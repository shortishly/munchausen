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


-module(munchausen_route).
-export([compile/1]).


compile(Routes) ->
    lists:foldl(
      fun
          ({Domain, Route}, [{Domain, Existing} | A]) ->
              [{Domain, [Route | Existing]} | A];

          ({Domain, Route}, A) ->
              [{Domain, [Route]} | A]
      end,
      [],
      lists:sort(lists:map(fun route/1, Routes))).


route(#{domain := Domain, path := Path, host := Host, port := Port}) ->
    {Domain, {Path,
               munchausen_http_proxy_resource,
               #{balancer => fun
                                 (_ProxyHost, _ProxyPath) ->
                                     #{host => Host, port => Port, path => Path}
                             end}}};

route(#{path := _, host := _, port := _} = Route) ->
    route(Route#{domain => '_'}).
