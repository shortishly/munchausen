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


-module(munchausen_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    case munchausen_config:enabled(http) of
        true ->
            try
                {ok, Sup} = munchausen_sup:start_link(),
                [munchausen:trace(true) || munchausen_config:enabled(debug)],
                {ok, Sup, #{listeners => [start_http(http)]}}
            catch
                _:Reason ->
                    {error, {Reason, erlang:get_stacktrace()}}
            end;

        false ->
            munchausen_sup:start_link()
    end.

stop(#{listeners := Listeners}) ->
    lists:foreach(fun cowboy:stop_listener/1, Listeners);
stop(_State) ->
    ok.

start_http(Prefix) ->
    {ok, _} = cowboy:start_http(Prefix,
                                munchausen_config:acceptors(Prefix),
                                [{port, munchausen_config:port(Prefix)}],
                                [{env, [dispatch(Prefix)]}]),
    Prefix.

dispatch(Prefix) ->
    {dispatch, cowboy_router:compile(munchausen_config:resources(Prefix))}.
