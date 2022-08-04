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
-module(njson_encoder).

%%% EXTERNAL EXPORTS
-export([encode/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
encode(Map, true) when is_map(Map) ->
    encode_map(Map);
encode(Map, false) when is_map(Map) ->
    iolist_to_binary(encode_map(Map));
encode(List, true) when is_list(List) ->
    encode_list(List);
encode(List, false) when is_list(List) ->
    iolist_to_binary(encode_list(List));
encode(Val, true) ->
    encode_val(Val);
encode(Val, false) ->
    iolist_to_binary(encode_val(Val)).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
encode_key(Key) when is_binary(Key) ->
    [$", Key, $"].

encode_val(true) ->
    <<"true">>;
encode_val(false) ->
    <<"false">>;
encode_val(null) ->
    <<"null">>;
encode_val(undefined) ->
    <<>>;
encode_val(Integer) when is_integer(Integer) ->
    erlang:integer_to_binary(Integer);
encode_val(Float) when is_float(Float) ->
    erlang:float_to_binary(Float, [short]);
encode_val(Bin) when is_binary(Bin) ->
    [$", escape(Bin), $"].

encode_map(Map) when is_map(Map) ->
    Encoded = maps:fold(fun map_fold_encode/3, <<>>, Map),
    [${, Encoded, $}].

map_fold_encode(_Key, undefined, <<>>) ->
    [];
map_fold_encode(_Key, undefined, AccIn) ->
    AccIn;
map_fold_encode(Key, Map, <<>>) when is_map(Map) ->
    [encode_key(Key), $:, encode_map(Map)];
map_fold_encode(Key, Map, AccIn) when is_map(Map) ->
    [AccIn, $,, encode_key(Key), $:, encode_map(Map)];
map_fold_encode(Key, List, <<>>) when is_list(List) ->
    [encode_key(Key), $:, encode_list(List)];
map_fold_encode(Key, List, AccIn) when is_list(List) ->
    [AccIn, $,, encode_key(Key), $:, encode_list(List)];
map_fold_encode(Key, Val, <<>>) ->
    [encode_key(Key), $:, encode_val(Val)];
map_fold_encode(Key, Val, AccIn) ->
    [AccIn, $,, encode_key(Key), $:, encode_val(Val)].

encode_list(List) when is_list(List) ->
    Encoded = lists:foldl(fun list_fold_encode/2, <<>>, List),
    [$[, Encoded, $]].

list_fold_encode(Map, <<>>) when is_map(Map) ->
    encode_map(Map);
list_fold_encode(Map, AccIn) when is_map(Map) ->
    [AccIn, $,, encode_map(Map)];
list_fold_encode(List, <<>>) when is_list(List) ->
    encode_list(List);
list_fold_encode(List, AccIn) when is_list(List) ->
    [AccIn, $,, encode_list(List)];
list_fold_encode(Val, <<>>) ->
    encode_val(Val);
list_fold_encode(Val, AccIn) ->
    [AccIn, $,, encode_val(Val)].

escape(Bin) ->
    escape(Bin, Bin, 0, []).

escape(<<>>, Base, _Len, []) ->
    Base;
escape(<<>>, Base, _Len, Acc) ->
    [Acc, Base];
escape(<<$", Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $"]);
escape(<<$/, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $/]);
escape(<<$\\, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $\\]);
escape(<<$\b, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $b]);
escape(<<$\f, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $f]);
escape(<<$\n, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $n]);
escape(<<$\r, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $r]);
escape(<<$\t, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $t]);
escape(<<_C, Bin/binary>>, Base, Len, Acc) ->
    escape(Bin, Base, Len + 1, Acc).
