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

-module(common).
-include_lib("common_test/include/ct.hrl").
-export([all/1]).
-export([consult/2]).
-export([purge_application/1]).
-export([read_file/2]).

is_a_test(is_a_test) ->
    false;
is_a_test(Function) ->
    hd(lists:reverse(string:tokens(atom_to_list(Function), "_"))) =:= "test".

all(Module) ->
    [Function || {Function, Arity} <- Module:module_info(exports),
                 Arity =:= 1,
                 is_a_test(Function)].

consult(Config, Name) ->
    {ok, Terms} = file:consult(filename:join(data_dir(Config), Name)),
    Terms.

read_file(Config, Name) ->
    {ok, Contents} = file:read_file(filename:join(data_dir(Config), Name)),
    Contents.

data_dir(Config) ->
    ?config(data_dir, Config).

purge_application(Application) ->
    application:stop(Application),
    case application:get_key(Application, modules) of
        undefined ->
            [];
        {ok, Modules} ->
            [begin
                 case code:is_loaded(Module) of
                     {file, _} ->
                         code:purge(Module),
                         code:delete(Module),
                         code:purge(Module);
                     false ->
                         not_loaded
                 end
             end || Module <- Modules]
    end,
    application:unload(Application).
