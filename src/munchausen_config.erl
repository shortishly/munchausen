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


-module(munchausen_config).
-export([acceptors/1]).
-export([enabled/1]).
-export([port/1]).
-export([resources/0]).
-export([resources/1]).

port(http) ->
    envy:get_integer(munchausen, http_port, default(80)).

enabled(http) ->
    envy:get_boolean(munchausen, http_enabled, default(true)).

acceptors(http) ->
    envy:get_integer(munchausen, http_acceptors, default(100)).


default(Default) ->
    [os_env, app_env, {default, Default}].


resources(http) ->
    munchausen_route:compile(
      case munchausen:get_env(munchausen_routes, [os_env]) of
          undefined ->
              get_env(resources, resources());

          Routes ->
              resource(Routes)
      end).

resources() ->
    {ok, Binary} = file:read_file(
                     munchausen:priv_dir("etc/routes")),
    resource(binary_to_list(Binary)).

get_env(Name, Default) ->
    munchausen:get_env(Name, [app_env, {default, Default}]).

resource(Resources) ->
    {ok, Tokens, _} = erl_scan:string(Resources),
    [begin
         {ok, Route} = erl_parse:parse_term(Term),
         Route
     end || Term <- split(Tokens)].

split([]) ->
    [];
split(L1) ->
    L2 = lists:takewhile(
           fun
               ({dot, _}) ->
                   false;
               (_) ->
                   true
           end,
           L1),
    [{dot, _} = Dot | T] = L1 -- L2,
    [L2 ++ [Dot] | split(T)].
