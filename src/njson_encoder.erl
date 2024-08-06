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
-spec encode(Json, boolean()) -> {ok, Binary} | njson:encode_error() when
    Json :: njson:t(),
    Binary :: binary().
encode(Map, true) when is_map(Map) ->
    encode_map(Map);
encode(Map, false) when is_map(Map) ->
    case encode_map(Map) of
        {ok, Encoded} ->
            {ok, iolist_to_binary(Encoded)};
        Error ->
            Error
    end;
encode(List, true) when is_list(List) ->
    encode_list(List);
encode(List, false) when is_list(List) ->
    case encode_list(List) of
        {ok, Encoded} ->
            {ok, iolist_to_binary(Encoded)};
        Error ->
            Error
    end;
encode(Val, true) ->
    encode_val(Val);
encode(Val, false) ->
    case encode_val(Val) of
        {ok, Encoded} ->
            {ok, iolist_to_binary(Encoded)};
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec encode_key(binary()) -> {ok, iolist()} | {error, {invalid_key, any()}}.
encode_key(Key) when is_binary(Key) ->
    {ok, [$", Key, $"]};
encode_key(Key) ->
    {error, {invalid_key, Key}}.

-spec encode_val(any()) -> {ok, iolist()} | {error, {invalid_value, any()}}.
encode_val(true) ->
    {ok, <<"true">>};
encode_val(false) ->
    {ok, <<"false">>};
encode_val(null) ->
    {ok, <<"null">>};
encode_val(undefined) ->
    {ok, []};
encode_val(Integer) when is_integer(Integer) ->
    {ok, erlang:integer_to_binary(Integer)};
encode_val(Float) when is_float(Float) ->
    {ok, erlang:float_to_binary(Float, [short])};
encode_val(Bin) when is_binary(Bin) ->
    {ok, [$", escape(Bin), $"]};
encode_val(Other) ->
    {error, {invalid_value, Other}}.

-spec encode_map(map()) -> {ok, iolist()} | {error, {invalid_map, any()}}.
encode_map(Map) when is_map(Map) ->
    case mapbind(fun map_fold_encode/3, [], Map) of
        {ok, Encoded} ->
            {ok, [${, Encoded, $}]};
        {error, Error} ->
            {error, {invalid_map, Error}}
    end.

map_fold_encode(_Key, undefined, []) ->
    {ok, []};
map_fold_encode(_Key, undefined, AccIn) ->
    {ok, AccIn};
map_fold_encode(Key, Map, []) when is_map(Map) ->
    case {encode_key(Key), encode_map(Map)} of
        {{ok, EncodedKey}, {ok, EncodedMap}} ->
            {ok, [EncodedKey, $:, EncodedMap]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end;
map_fold_encode(Key, Map, AccIn) when is_map(Map) ->
    case {encode_key(Key), encode_map(Map)} of
        {{ok, EncodedKey}, {ok, EncodedMap}} ->
            {ok, [AccIn, $,, EncodedKey, $:, EncodedMap]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end;
map_fold_encode(Key, List, []) when is_list(List) ->
    case {encode_key(Key), encode_list(List)} of
        {{ok, EncodedKey}, {ok, EncodedList}} ->
            {ok, [EncodedKey, $:, EncodedList]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end;
map_fold_encode(Key, List, AccIn) when is_list(List) ->
    case {encode_key(Key), encode_list(List)} of
        {{ok, EncodedKey}, {ok, EncodedList}} ->
            {ok, [AccIn, $,, EncodedKey, $:, EncodedList]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end;
map_fold_encode(Key, Val, []) ->
    case {encode_key(Key), encode_val(Val)} of
        {{ok, EncodedKey}, {ok, EncodedVal}} ->
            {ok, [EncodedKey, $:, EncodedVal]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end;
map_fold_encode(Key, Val, AccIn) ->
    case {encode_key(Key), encode_val(Val)} of
        {{ok, EncodedKey}, {ok, EncodedVal}} ->
            {ok, [AccIn, $,, EncodedKey, $:, EncodedVal]};
        {{error, _Reason} = Error, _} ->
            Error;
        {_, {error, _Reason} = Error} ->
            Error
    end.

-spec encode_list(list()) -> {ok, iolist()} | {error, {invalid_list, any()}}.
encode_list(List) when is_list(List) ->
    case listbind(fun list_fold_encode/2, [], List) of
        {ok, Encoded} ->
            {ok, [$[, Encoded, $]]};
        Error ->
            Error
    end.

list_fold_encode(Map, []) when is_map(Map) ->
    encode_map(Map);
list_fold_encode(Map, AccIn) when is_map(Map) ->
    case encode_map(Map) of
        {ok, Encoded} ->
            {ok, [AccIn, $,, Encoded]};
        Error ->
            Error
    end;
list_fold_encode(List, []) when is_list(List) ->
    encode_list(List);
list_fold_encode(List, AccIn) when is_list(List) ->
    case encode_list(List) of
        {ok, Encoded} ->
            {ok, [AccIn, $,, Encoded]};
        Error ->
            Error
    end;
list_fold_encode(Val, []) ->
    encode_val(Val);
list_fold_encode(Val, AccIn) ->
    case encode_val(Val) of
        {ok, Encoded} ->
            {ok, [AccIn, $,, Encoded]};
        Error ->
            Error
    end.

escape(Bin) ->
    escape(Bin, Bin, 0, []).

escape(<<>>, Base, _Len, []) ->
    Base;
escape(<<>>, Base, _Len, Acc) ->
    [Acc, Base];
escape(<<$", Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $"]);
escape(<<$\\, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $\\]);
escape(<<$\b, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $b]);
escape(<<$\t, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $t]);
escape(<<$\n, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $n]);
escape(<<$\v, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $v]);
escape(<<$\f, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $f]);
escape(<<$\r, Bin/binary>>, Base, Len, Acc) ->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, $r]);
escape(<<C, Bin/binary>>, Base, Len, Acc) when
    C >= 16#00 andalso C =< 16#07 orelse
        C >= 16#0E andalso C =< 16#1F
->
    String = binary:part(Base, 0, Len),
    escape(Bin, Bin, 0, [Acc, String, $\\, C]);
escape(<<_C, Bin/binary>>, Base, Len, Acc) ->
    escape(Bin, Base, Len + 1, Acc).

%%%-----------------------------------------------------------------------------
%%% BIND FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec listbind(Fun, Acc0, List) -> {ok, Acc1} | {error, any()} when
    Fun :: fun((Elem :: T, AccIn) -> {ok, AccOut} | {error, any()}),
    Acc0 :: any(),
    Acc1 :: any(),
    AccIn :: any(),
    AccOut :: any(),
    List :: [T],
    T :: any().
listbind(F, AccIn, [Hd | Tail]) when is_function(F, 2) ->
    case F(Hd, AccIn) of
        {ok, AccOut} ->
            listbind_(F, AccOut, Tail);
        {error, Reason} ->
            {error, {Hd, Reason}}
    end;
listbind(F, AccIn, []) when is_function(F, 2) ->
    {ok, AccIn}.

listbind_(F, AccIn, [Hd | Tail]) ->
    case F(Hd, AccIn) of
        {ok, AccOut} ->
            listbind_(F, AccOut, Tail);
        {error, Reason} ->
            {error, {Hd, Reason}}
    end;
listbind_(_F, Acc, []) ->
    {ok, Acc}.

-spec mapbind(Fun, Init, MapOrIter) -> {ok, Acc} | {error, any()} when
    Fun :: fun((Key, Value, AccIn) -> {ok, AccOut} | {error, any()}),
    Init :: any(),
    Acc :: AccOut,
    AccIn :: Init | AccOut,
    MapOrIter :: #{Key => Value} | maps:iterator(Key, Value).
mapbind(Fun, Init, Map) when is_map(Map), is_function(Fun, 3) ->
    mapbind_(Fun, Init, maps:next(maps:iterator(Map)));
mapbind(_Fun, _Init, Map) ->
    {error, {bad_map, Map}}.

mapbind_(Fun, Acc, {K, V, Iter}) ->
    case Fun(K, V, Acc) of
        {ok, AccOut} ->
            mapbind_(Fun, AccOut, maps:next(Iter));
        {error, Reason} ->
            {error, {K, V, Reason}}
    end;
mapbind_(_Fun, Acc, none) ->
    {ok, Acc}.
