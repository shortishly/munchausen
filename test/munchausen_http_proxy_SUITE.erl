%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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

-module(munchausen_http_proxy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
    [{group, samples}].

groups() ->
    [{samples, [sequence], common:all(?MODULE)}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = cowboy:start_http(?MODULE,
                                       10,
                                       [{port, 0}],
                                       [{env, [dispatch(Config)]}]),
    Config.

end_per_suite(_Config) ->
    cowboy:stop_listener(?MODULE),
    application:stop(inets),
    application:stop(cowboy),
    application:stop(gun).

data_dir(Config) ->
    ?config(data_dir, Config).

http_port() ->
    ranch_server:get_port(?MODULE).

gun(Config) ->
    ?config(gun, Config).

dispatch(Config) ->
    {dispatch, cowboy_router:compile(resources(Config))}.

resources(Config) ->
    [{'_',
      [{<<"/sample/simple">>, proxy_simple_small_resource, []},

       {<<"/sample/mad_barbara">>,
        proxy_simple_resource, #{filename => mad_barbara(Config)}},

       {<<"/sample/war_and_peace">>,
        proxy_simple_resource, #{filename => war_and_peace(Config)}},

       {<<"/do/echo">>, proxy_echo_post_resource, #{}},

       {<<"/do/ws/echo">>, proxy_ws_echo_resource, #{}},

       {<<"/proxy/[...]">>,
        munchausen_http_proxy_resource,
        #{prefix => <<>>,
          balancer => balancer()}}]}].

balancer() ->
    fun
        (Host, <<"/proxy/", Path/binary>>) ->
            #{host => binary_to_list(Host),
              path => <<"/", Path/binary>>,
              port => http_port()}
    end.

simple_test(_Config) ->
    {ok,
     {{_, 200, _},
      _,
      <<"Hello world!">>}} = request(get, "/proxy/sample/simple"),
    ok.

mad_barbara(Config) ->
    filename:join(data_dir(Config),
                  "Mad_Barbara_by_Warwick_Deeping.txt").

war_and_peace(Config) ->
    filename:join(data_dir(Config),
                  "War_and_Peace_by_Leo_Tolstoy.txt").

lorem_ipsum_5k(Config) ->
    filename:join(data_dir(Config),
                  "lorem_ipsum_5k.txt").

mad_barbara_test(Config) ->
    {ok, MadBarbara} = file:read_file(mad_barbara(Config)),
    {ok,
     {{_, 200, _},
      _,
      MadBarbara}} = request(get, "/proxy/sample/mad_barbara"),
    ok.

war_and_peace_test(Config) ->
    {ok, WarAndPeace} = file:read_file(war_and_peace(Config)),
    {ok,
     {{_, 200, _},
      _,
      WarAndPeace}} = request(get, "/proxy/sample/war_and_peace"),
    ok.


echo_hello_world_test(_Config) ->
    tracing(),
    HelloWorld = <<"Hello world!">>,
    {ok,
     {{_, 200, _},
      _,
      HelloWorld}} = request(post, "/proxy/do/echo", HelloWorld),
    ok.


echo_lorem_ipsum_5k_test(Config) ->
    {ok, LoremIpsum} = file:read_file(lorem_ipsum_5k(Config)),
    {ok,
     {{_, 200, _},
      _,
      LoremIpsum}} = request(post, "/proxy/do/echo", LoremIpsum),
    ok.

ws_echo_test(_Config) ->
    tracing(),
    {ok, Gun} = gun:open("127.0.0.1",
                         http_port(),
                         #{transport => tcp}),
    gun:await_up(Gun),
    gun:ws_upgrade(Gun, "/proxy/do/ws/echo"),
    receive
        {gun_ws_upgrade, Gun, ok, _} ->
            ct:log("WS Upgrade")

    after 5000 ->
            error(timeout)
    end,
    Message = <<"Hello World!">>,
    gun:ws_send(Gun, {text, Message}),
    receive
        {gun_ws, Gun, {text, Message}} ->
            ct:log("message echod ~p", [Message])

    after 5000 ->
            error(timeout)
    end.


tracing() ->
    lists:foreach(fun code:ensure_loaded/1, modules()),
    (recon_trace:calls([m(Module) || Module <- modules()],
                       {1000, 500},
                       [{scope, local},
                        {pid, all}]) > 0) orelse error({debug, no_match}).

modules() ->
    [proxy_echo_post_resource,
     proxy_simple_resource,
     proxy_simple_small_resource,
     proxy_ws_echo_resource,
     haystack_http_proxy_resource].

m(Module) ->
    {Module, '_', '_'}.


request(get, URL) ->
    httpc:request(get,
                  {"http://127.0.0.1:" ++
                       integer_to_list(http_port()) ++
                       URL, []},
                  [],
                  [{body_format, binary}]).

request(post, URL, Body) ->
    httpc:request(post,
                  {"http://127.0.0.1:" ++
                       integer_to_list(http_port()) ++
                       URL,
                   [],
                  "text/plain",
                  Body},
                  [],
                  [{body_format, binary}]).
