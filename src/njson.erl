%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(njson).

%%% EXTERNAL EXPORTS
-export([decode/1, encode/1, encode/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
decode(<<>>) ->
    undefined;
decode(Json) ->
    {ok, Result, _Rest} = njson_decoder:decode(Json),
    Result.

encode(Erlang) ->
    njson_encoder:encode(Erlang, false).

encode(Erlang, AsIOList) ->
    njson_encoder:encode(Erlang, AsIOList).
