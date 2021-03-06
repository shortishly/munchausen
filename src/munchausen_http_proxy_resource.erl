%% Copyright (c) 2016 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(munchausen_http_proxy_resource).

-export([info/3]).
-export([init/2]).
-export([metrics/0]).
-export([terminate/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-include_lib("kernel/include/inet.hrl").


%% munchaussen is a http proxy that can cope with chunked responses
%% and web socket upgrades - the kind of things a modern client might
%% expect.
%%
%% Accept a http request from a client, ask the load balancer which
%% endpoint to use, if none found reply with a not_found. Otherwise,
%% upgrade the request to a web socket if appropriate headers are
%% present. A http request is dealt with by firstly fetching any
%% request body which is forwarded to the origin, and then batching
%% any response as chunks.


-on_load(on_load/0).

-record(?MODULE, {
           key,
           value
          }).

on_load() ->
    crown_table:reuse(?MODULE, set).

init(Req, #{balancer := Balancer} = State) when is_atom(Balancer) ->
    %% When balancer is an atom we assume that it is a module name
    %% where use the pick/2 function
    init(Req, State#{balancer := fun Balancer:pick/2});

init(Req, #{prefix := _, balancer := _} = State) ->
    URL = #{host => cowboy_req:host(Req),
            path => cowboy_req:path(Req),
            port => cowboy_req:port(Req)},

    increment(#{request => URL}),

    %% Attempt to load balance the request
    case balance(Req, State) of
        not_found ->
            %% No endpoint found for this request, reply with
            %% not found 404.
            increment(#{request => URL, status => 404}),
            {stop, not_found(Req), undefined};

        #{host := Endpoint, port := Port, path := Path} ->
            %% Open and monitor a http connection to the origin
            %% endpoint, upgrading the request if headers are present
            %% to a web socket.

            increment(#{request => URL,
                        endpoint => #{host => Endpoint,
                                      port => Port,
                                      path => Path}}),

            {ok, Origin} = gun:open(inet_ip(Endpoint), Port, #{transport => tcp}),
            Monitor = erlang:monitor(process, Origin),
            QS = cowboy_req:qs(Req),
            case is_ws_upgrade(Req) of
                true ->
                    %% Web socket upgrade in header switch to
                    %% websocket loop.
                    loop(
                      cowboy_websocket,
                      Req,
                      State#{url => URL},
                      Endpoint,
                      Port,
                      Origin,
                      Monitor,
                      Path,
                      QS);

                false ->
                    %% Otherwise remain in normal http loop.
                    loop(
                      cowboy_loop,
                      Req,
                      State#{url => URL},
                      Endpoint,
                      Port,
                      Origin,
                      Monitor,
                      Path,
                      QS)
            end
    end;

init(Req, #{balancer := _} = State) ->
    %% Add an empty prefix if one is not present
    init(Req, State#{prefix => <<>>}).


info({gun_up, Origin, _},
     Req,
     #{url := URL,
       endpoint := Endpoint,
       path := Path,
       qs := QS,
       origin := Origin} = State) ->

    increment(#{request => URL,
                endpoint => Endpoint,
                info => up}),

    %% A http connection to origin is up and available, proxy
    %% client request through to the origin.
    {ok, Req, maybe_request_body(Req, State, Origin, Endpoint, Path, QS)};

info({gun_response, _, _, nofin, Status, Headers},
     Req,
     #{url := URL,
       endpoint := Endpoint} = State) when (Status div 100) == 2 ->
    %% We have an initial http response from the origin together with
    %% some headers to forward to the client.

    increment(#{request => URL,
                endpoint => Endpoint,
                info => response,
                status => Status}),

    {ok, cowboy_req:chunked_reply(Status, Headers, Req), State};

info({gun_response, _, _, nofin, Status, Headers},
     Req,
     #{url := URL,
       endpoint := Endpoint} = State) ->

    increment(#{request => URL,
                endpoint => Endpoint,
                info => response,
                status => Status}),

    case lists:keyfind(<<"content-length">>, 1, Headers) of
        false ->
            %% no content length, assume that we are done (despite
            %% nofin)
            {stop, cowboy_req:reply(Status, Headers, Req), State};
        _ ->
            %% We have an initial http response from the origin together with
            %% some headers to forward to the client.
            {ok, cowboy_req:chunked_reply(Status, Headers, Req), State}
    end;

info({gun_response, _, _, fin, Status, Headers},
     Req,
     #{url := URL,
       endpoint := Endpoint} = State) ->
    %% short and sweeet, we have final http response from the origin
    %% with just status and headers and no response body.

    increment(#{request => URL,
                endpoint => Endpoint,
                info => response,
                status => Status}),

    {stop, cowboy_req:reply(Status, Headers, Req), State};

info({gun_data, _, _, nofin, Data},
     Req,
     #{url := URL,
       endpoint := Endpoint} = State) ->
    %% we have received a response body chunk from the origin,
    %% chunk and forward to the client.

    increment(#{request => URL,
                endpoint => Endpoint,
                info => data}),

    case cowboy_req:chunk(Data, Req) of
        ok ->
            %% response has been chunked OK to the client, continue in
            %% loop
            {ok, Req, State};

        {error, _} ->
            %% some error while chunking the response to the client,
            %% time to hang up the connection and call it a day
            {stop, Req, State}
    end;

info({gun_data, _, _, fin, Data},
     Req,
     #{url := URL,
       endpoint := Endpoint} = State) ->
    %% we received a final response body chunk from the origin,
    %% chunk and forward to the client - and then hang up the
    %% connection.

    increment(#{request => URL,
                endpoint => Endpoint,
                info => data}),

    cowboy_req:chunk(Data, Req),
    {stop, Req, State};

info({request_body, #{complete := Data}}, Req, #{origin := Origin,
                                                 request := Request} = State) ->
    %% the client has streamed the http request body to us, and we
    %% have received the last chunk, forward the chunk to the origin
    %% letting them know not to expect any more
    gun:data(Origin, Request, fin, Data),
    {ok, Req, State};

info({request_body, #{more := More}}, Req, #{origin := Origin,
                                             request := Request} = State) ->
    %% the client is streaming the http request body to us, forward
    %% the chunk to the origin letting them know that we're not done
    %% yet.
    gun:data(Origin, Request, nofin, More),
    {ok, Req, request_body(Req, State)};

info({'DOWN', Monitor, _, _, _},
     Req,
     #{monitor := Monitor,
       url := URL,
       endpoint := Endpoint} = State) ->
    %% whoa, our monitor has noticed the http connection to the origin
    %% is emulating a Norwegian Blue parrot, time to declare to the
    %% client that the gateway has turned bad.

    BadGateway = 502,

    increment(#{request => URL,
                endpoint => Endpoint,
                info => response,
                status => BadGateway}),

    {stop, cowboy_req:reply(BadGateway, Req), State}.


terminate(_Reason, _Req, #{origin := Origin, monitor := Monitor}) ->
    %% we are terminating and have a monitored connection to the
    %% origin, try and be a nice citizen and demonitor and pull the
    %% plug on the connection to the origin.
    erlang:demonitor(Monitor),
    gun:close(Origin);

terminate(_Reason, _Req, #{origin := Origin}) ->
    %% we are terminating and just have a connection to the origin,
    %% try and pull the plug on it.
    gun:close(Origin);

terminate(_Reason, _Req, _) ->
    %% nothing to clean up here.
    ok.


websocket_info({'DOWN', M, _, _, _}, Req, #{monitor := M} = State) ->
    %% whoa, we've noticed that our websocket to the origin has
    %% failed, respond with a bad gateway to the client.
    {stop, cowboy_req:reply(502, Req), State};

websocket_info({gun_up, O, _}, Req, #{path := Path, origin := O} = State) ->
    %% The connection is now open to the origin. Let them know our
    %% intentions and request an upgrade to a websocket. We need to
    %% wait for a gun_ws_upgrade back from the origin to let us know
    %% that it is all OK (we put #{ws_upgrade => handshake} on the
    %% state when everyone is in agreement).
    gun:ws_upgrade(O, Path),
    {ok, Req, State};

websocket_info({gun_ws_upgrade, Origin, ok, _},
               Req,
               #{origin := Origin,
                 ws_send_backlog := Frames} = State) ->
    %% The origin has finally agreed to upgrade the connection to a
    %% web socket. In the meantime, we've been dealing with a racy
    %% client that has eagerly given us some frames for the
    %% origin. Now that everyone agrees that a web socket is a good
    %% thing: time to offload the frames and remember that everyone
    %% has a shaken on the deal (while forgetting the backlog of
    %% frames that we've just offloaded).
    gun:ws_send(Origin, lists:reverse(Frames)),
    {ok, Req, maps:without([ws_send_backlog], State#{ws_upgrade => handshake})};

websocket_info({gun_ws_upgrade, _, _,  _}, Req, State) ->
    %% The origin has agreed to upgrade to a web socket, while the
    %% client has been kind enough not send any frames, we all agree
    %% and shake on the deal.
    {ok, Req, State#{ws_upgrade => handshake}};

websocket_info({gun_ws, Origin, Frame}, Req, #{origin := Origin} = State) ->
    %% The origin has sent us a frame, proxy it onward to the client.
    {reply, Frame, Req, State};

websocket_info({gun_response, _, _, nofin, _, _}, Req, State) ->
    {ok, Req, State};

websocket_info({gun_response, _, _, fin, _, _}, Req, State) ->
    {stop, Req, State}.

websocket_handle(Frame, Req, #{origin := Origin,
                               ws_upgrade := handshake} = State) ->
    %% The client has sent us a frame, and we have already agreed with
    %% the origin that this is an upgraded web socket connection, so
    %% proxy the request straight through to the origin.
    gun:ws_send(Origin, Frame),
    {ok, Req, State};

websocket_handle(Frame, Req, #{ws_send_backlog := Backlog} = State) ->
    %% Another frame to add to the backlog from the client, and we
    %% still haven't reached agreement with the origin that this
    %% connection should be a web socket.
    {ok, Req, State#{ws_send_backlog := [Frame | Backlog]}};

websocket_handle(Frame, Req, State) ->
    %% A first frame from the client while we are waiting for the
    %% origin to confirm that this connection will be a web socket.
    {ok, Req, State#{ws_send_backlog => [Frame]}}.


balance(Req, #{prefix := Prefix, balancer := Balancer}) ->
    %% Load balancers expect two arguments the hostname from the
    %% request (with a prefix if one has been supplied), and the
    %% original request path to the origin.
    Host = cowboy_req:host(Req),
    Path = cowboy_req:path(Req),
    Balancer(<<Prefix/binary, Host/binary>>, Path).

is_ws_upgrade(Req) ->
    %% When is a http request actually a web socket in disguise? When
    %% it has an upgrade header of type websocket.
    [<<"websocket">>] == cowboy_req:parse_header(<<"upgrade">>, Req).


not_found(Req) ->
    %% A simple text, plain response that says 404 - "not found".
    TextPlain = [<<"content-type">>,  <<"text/plain">>],
    cowboy_req:reply(404, TextPlain, "Not found.", Req).

loop(Handler, Req, State, Host, Port, Origin, Monitor, Path, QS) ->
    %% We are either in the cowboy_websocket or cowboy_loop, the
    %% request and basic state are common between both loops.
    {Handler,
     Req,
     State#{origin => Origin,
            endpoint => #{host => Host, path => Path, port => Port},
            monitor => Monitor,
            path => Path,
            qs => QS}}.


maybe_request_body(Req, State, Origin, Endpoint, Path, QS) ->
    %% Proxy the http request through to the origin, and start
    %% streaming the request body from the client is there is one.
    maybe_request_body(Req, State#{request => proxy(Req, Origin, Endpoint, Path, QS)}).


proxy(Req, Origin, #{host := Host, port := Port}, Path, QS) ->
    %% Act as a proxy for a http request to the origin from the
    %% client.
    Method = cowboy_req:method(Req),
    Headers = lists:keyreplace(
                <<"host">>,
                1,
                cowboy_req:headers(Req),
                {<<"host">>,
                 <<(any:to_binary(Host))/bytes,
                   ":",
                   (any:to_binary(Port))/bytes>>}),
    request(Origin, Method, path(Path, QS), Headers).

request(Origin, Method, PathQS, Headers) ->
    gun:request(Origin, Method, PathQS, Headers).
    

path(Path, <<>>) ->
    Path;
path(Path, QS) ->
    <<Path/bytes, "?", QS/bytes>>.

maybe_request_body(Req, State) ->
    case cowboy_req:has_body(Req) of
        true ->
            %% We have a http request body, start streaming it from
            %% the client.
            request_body(Req, State);

        false ->
            %% There is no request body from the client, time to move
            %% on
            State
    end.


request_body(Req, State) ->
    %% We are streaming the request body from the client
    case cowboy_req:body(Req) of
        {ok, Data, _} ->
            %% We have streamed all of the request body from the
            %% client.
            self() ! {request_body, #{complete => Data}},
            State;

        {more, Data, _} ->
            %% We have part of the request body, but there is still
            %% more waiting for us.
            self() ! {request_body, #{more => Data}}
    end.

inet_ip(Endpoint) ->
    case inet:parse_ipv4_address(Endpoint) of
        {ok, _} ->
            Endpoint;

        {error, einval} ->
            case inet_res:gethostbyname(Endpoint) of
                {ok,
                 #hostent{
                    h_addrtype = inet,
                    h_addr_list = Addresses}} ->
                    inet:ntoa(munchausen_util:pick_one(Addresses));

                {error, _} ->
                    error([Endpoint])
            end
    end.

increment(Key) ->
    ets:update_counter(?MODULE, Key, 1, #?MODULE{value = 0}).

metrics() ->
    lists:foldl(
      fun
          (#?MODULE{key = #{request := Request, endpoint := Endpoint, info := response}, value = Value}, #{requests := Requests} = A) ->

              RequestURI = uri(Request),
              EndpointURI = uri(Endpoint),

              case maps:find(RequestURI, Requests) of
                  {ok, #{EndpointURI := Metric} = Endpoints} ->
                      A#{requests => Requests#{RequestURI => Endpoints#{EndpointURI => Metric + Value}}};

                  {ok, Endpoints} ->
                      A#{requests => Requests#{RequestURI => Endpoints#{EndpointURI => Value}}};

                  error ->
                      A#{requests => Requests#{RequestURI => #{EndpointURI => Value}}}
              end;

          (#?MODULE{}, A) ->
              A
      end,
      #{requests => #{}},
      ets:tab2list(?MODULE)).

uri(#{host := Host, port := Port}) ->
    <<"tcp://", (any:to_binary(Host))/bytes, ":", (any:to_binary(Port))/bytes>>.
