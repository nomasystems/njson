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
-module(njson_decoder).
-compile([
    {inline, [
        {next, 5},
        {array, 5},
        {array_close, 4},
        {finalize, 5},
        {key, 4},
        {close_key, 5},
        {object, 5},
        {chunk, 5},
        {chunk, 6},
        {do_unescape, 5},
        {dec, 1},
        {unicode, 4}
    ]}
]).

-dialyzer({no_improper_lists, [finalize_string/6, unescape/6]}).

%%% EXTERNAL EXPORTS
-export([decode/1]).

%%% MACROS
-define(END, 0).
-define(KEY, 1).
-define(OBJECT, 2).
-define(ARRAY, 3).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec decode(Binary) -> OK | Error when
    Binary :: binary(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
decode(Json) when is_binary(Json) ->
    decode(Json, Json).

-spec decode(Binary, Binary) -> OK | Error when
    Binary :: binary(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
%Ignore UTF8 BOM as suggested in RFC section 8.1
decode(<<239, 187, 191, Bin/binary>>, Json) ->
    val(Bin, Json, 3, [?END]);
decode(Bin, Json) ->
    val(Bin, Json, 0, [?END]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec val(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
val(<<C, Bin/binary>>, Original, Skip, Next) ->
    case C of
        $\t ->
            val(Bin, Original, Skip + 1, Next);
        $\n ->
            val(Bin, Original, Skip + 1, Next);
        $\r ->
            val(Bin, Original, Skip + 1, Next);
        $\s ->
            val(Bin, Original, Skip + 1, Next);
        $" ->
            string(Bin, Original, Skip + 1, Next, 0);
        $- ->
            number(Bin, Original, Skip, Next, 1);
        $+ ->
            number(Bin, Original, Skip, Next, 1);
        D when D >= $0, D =< $9 ->
            number(Bin, Original, Skip, Next, 1);
        $[ ->
            val(Bin, Original, Skip + 1, [?ARRAY, [] | Next]);
        $] ->
            array_close(Bin, Original, Skip, Next);
        $f ->
            alse(Bin, Original, Skip, Next);
        $n ->
            ull(Bin, Original, Skip, Next);
        $t ->
            rue(Bin, Original, Skip, Next);
        ${ ->
            key(Bin, Original, Skip + 1, [?KEY, #{} | Next]);
        _C ->
            {error, {invalid_value, [C], Skip}}
    end.

-spec alse(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
alse(<<"alse", Bin/binary>>, Original, Skip, Next) ->
    next(Bin, Original, Skip + 5, Next, false);
alse(<<_Bin/binary>>, _Original, Skip, _Next) ->
    {error, {invalid_value, [$f], Skip}}.

-spec ull(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
ull(<<"ull", Bin/binary>>, Original, Skip, Next) ->
    next(Bin, Original, Skip + 4, Next, null);
ull(<<_Bin/binary>>, _Original, Skip, _Next) ->
    {error, {invalid_value, [$n], Skip}}.

-spec rue(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
rue(<<"rue", Bin/binary>>, Original, Skip, Next) ->
    next(Bin, Original, Skip + 4, Next, true);
rue(<<_Bin/binary>>, _Original, Skip, _Next) ->
    {error, {invalid_value, [$t], Skip}}.

-spec next(Binary, Binary, Skip, Next, Value) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Value :: false | null | true | binary() | number() | list() | map(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
next(<<Bin/binary>>, Original, Skip, Next, Value) ->
    case Next of
        [?END | NewNext] ->
            finalize(Bin, Original, Skip, NewNext, Value);
        [?KEY | NewNext] ->
            close_key(Bin, Original, Skip, NewNext, Value);
        [?ARRAY | NewNext] ->
            array(Bin, Original, Skip, NewNext, Value);
        [?OBJECT | NewNext] ->
            object(Bin, Original, Skip, NewNext, Value)
    end.

-spec finalize(Binary, Binary, Skip, Next, Value) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Value :: njson:t(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
finalize(<<$\t, Bin/binary>>, Original, Skip, Next, Value) ->
    finalize(Bin, Original, Skip, Next, Value);
finalize(<<$\n, Bin/binary>>, Original, Skip, Next, Value) ->
    finalize(Bin, Original, Skip, Next, Value);
finalize(<<$\r, Bin/binary>>, Original, Skip, Next, Value) ->
    finalize(Bin, Original, Skip, Next, Value);
finalize(<<$\s, Bin/binary>>, Original, Skip, Next, Value) ->
    finalize(Bin, Original, Skip, Next, Value);
finalize(<<C, _Bin/binary>>, _Original, Skip, _Next, _Value) ->
    {error, {unexpected_trailing_char, [C], Skip}};
finalize(<<>>, _Original, _Skip, [], Value) ->
    {ok, Value}.

-spec key(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
key(<<C, Bin/binary>>, Original, Skip, Next) ->
    case C of
        $\t ->
            key(Bin, Original, Skip + 1, Next);
        $\n ->
            key(Bin, Original, Skip + 1, Next);
        $\r ->
            key(Bin, Original, Skip + 1, Next);
        $\s ->
            key(Bin, Original, Skip + 1, Next);
        $" ->
            string(Bin, Original, Skip + 1, Next, 0);
        $} ->
            empty_object(Bin, Original, Skip + 1, Next);
        C ->
            {error, {invalid_key, [C], Skip}}
    end.

-spec close_key(Binary, Binary, Skip, Next, Value) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Value :: njson:t(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
close_key(<<C, Bin/binary>>, Original, Skip, Next, Value) ->
    case C of
        $\t ->
            close_key(Bin, Original, Skip + 1, Next, Value);
        $\n ->
            close_key(Bin, Original, Skip + 1, Next, Value);
        $\r ->
            close_key(Bin, Original, Skip + 1, Next, Value);
        $\s ->
            close_key(Bin, Original, Skip + 1, Next, Value);
        $: ->
            val(Bin, Original, Skip + 1, [?OBJECT, Value | Next]);
        C ->
            {error, {invalid_key, [C], Skip}}
    end.

-spec array(Binary, Binary, Skip, Next, Value) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Value :: njson:t(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
array(<<$\t, Bin/binary>>, Original, Skip, Next, Value) ->
    array(Bin, Original, Skip + 1, Next, Value);
array(<<$\n, Bin/binary>>, Original, Skip, Next, Value) ->
    array(Bin, Original, Skip + 1, Next, Value);
array(<<$\r, Bin/binary>>, Original, Skip, Next, Value) ->
    array(Bin, Original, Skip + 1, Next, Value);
array(<<$\s, Bin/binary>>, Original, Skip, Next, Value) ->
    array(Bin, Original, Skip + 1, Next, Value);
array(<<$,, Bin/binary>>, Original, Skip, Next, Value) ->
    [Values | NewNext] = Next,
    val(Bin, Original, Skip + 1, [?ARRAY, [Value | Values] | NewNext]);
array(<<$], Bin/binary>>, Original, Skip, Next, Value) ->
    [Values | NewNext] = Next,
    next(Bin, Original, Skip + 1, NewNext, lists:reverse([Value | Values]));
array(<<C, _Bin/binary>>, _Original, Skip, _Next, _Value) ->
    {error, {invalid_array, [C], Skip}}.

-spec array_close(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
array_close(<<Bin/binary>>, Original, Skip, [?ARRAY, [] | Next]) ->
    next(Bin, Original, Skip + 1, Next, []);
array_close(<<_Bin/binary>>, _Original, Skip, _Next) ->
    {error, {invalid_array, [$,], Skip}}.

-spec object(Binary, Binary, Skip, Next, Value) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Value :: njson:t(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
object(<<$\t, Bin/binary>>, Original, Skip, Next, Value) ->
    object(Bin, Original, Skip + 1, Next, Value);
object(<<$\n, Bin/binary>>, Original, Skip, Next, Value) ->
    object(Bin, Original, Skip + 1, Next, Value);
object(<<$\r, Bin/binary>>, Original, Skip, Next, Value) ->
    object(Bin, Original, Skip + 1, Next, Value);
object(<<$\s, Bin/binary>>, Original, Skip, Next, Value) ->
    object(Bin, Original, Skip + 1, Next, Value);
object(<<$,, Bin/binary>>, Original, Skip, Next, Value) ->
    [Key, Object | NewNext] = Next,
    key(Bin, Original, Skip + 1, [?KEY, maps:put(Key, Value, Object) | NewNext]);
object(<<$}, Bin/binary>>, Original, Skip, Next, Value) ->
    [Key, Object | NewNext] = Next,
    next(Bin, Original, Skip + 1, NewNext, maps:put(Key, Value, Object));
object(<<C, _Bin/binary>>, _Original, Skip, _Next, _Value) ->
    {error, {invalid_object, [C], Skip}}.

-spec empty_object(Binary, Binary, Skip, Next) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
empty_object(<<Bin/binary>>, Original, Skip, [?KEY, Map | Next]) ->
    next(Bin, Original, Skip, Next, Map).

-spec number(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
number(<<$+, Bin/binary>>, Original, Skip, Next, Len) ->
    number(Bin, Original, Skip, Next, Len + 1);
number(<<$-, Bin/binary>>, Original, Skip, Next, Len) ->
    number(Bin, Original, Skip, Next, Len + 1);
number(<<$., Bin/binary>>, Original, Skip, Next, Len) ->
    fraction(Bin, Original, Skip, Next, Len + 1);
number(<<D, Bin/binary>>, Original, Skip, Next, Len) when D >= $0, D =< $9 ->
    number(Bin, Original, Skip, Next, Len + 1);
number(<<E, Bin/binary>>, Original, Skip, Next, Len) when E == $e; E == $E ->
    Prefix = erlang:binary_part(Original, Skip, Len),
    exponent(Bin, Original, Skip + Len + 1, Next, Prefix, 0);
number(<<Bin/binary>>, Original, Skip, Next, Len) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    next(Bin, Original, Skip + Len, Next, binary_to_integer(Chunk)).

-spec fraction(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
fraction(<<D, Bin/binary>>, Original, Skip, Next, Len) when D >= $0, D =< $9 ->
    fraction(Bin, Original, Skip, Next, Len + 1);
fraction(<<E, Bin/binary>>, Original, Skip, Next, Len) when E == $e; E == $E ->
    exponent(Bin, Original, Skip, Next, <<>>, Len + 1);
fraction(<<Bin/binary>>, Original, Skip, Next, Len) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    next(Bin, Original, Skip + Len, Next, binary_to_float(Chunk)).

-spec exponent(Binary, Binary, Skip, Next, Prefix, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Prefix :: binary(),
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
exponent(<<$+, Bin/binary>>, Original, Skip, Next, Prefix, Len) ->
    exponent(Bin, Original, Skip, Next, Prefix, Len + 1);
exponent(<<$-, Bin/binary>>, Original, Skip, Next, Prefix, Len) ->
    exponent(Bin, Original, Skip, Next, Prefix, Len + 1);
exponent(<<D, Bin/binary>>, Original, Skip, Next, Prefix, Len) when D >= $0, D =< $9 ->
    exponent(Bin, Original, Skip, Next, Prefix, Len + 1);
exponent(<<Bin/binary>>, Original, Skip, Next, Prefix, Len) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    Float = prepare_float(Prefix, Chunk),
    next(Bin, Original, Skip + Len, Next, binary_to_float(Float)).

-spec prepare_float(Binary, Chunk) -> Binary when
    Binary :: binary(),
    Chunk :: binary().
prepare_float(<<>>, Chunk) ->
    Chunk;
prepare_float(Prefix, Chunk) ->
    <<Prefix/binary, ".0e", Chunk/binary>>.

-spec string(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
string(<<Bin/binary>>, Original, Skip, Next, Len) ->
    chunk(Bin, Original, Skip, Next, Len).

-spec chunk(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
chunk(<<C, Bin/binary>>, Original, Skip, Next, Len) ->
    case C of
        $" ->
            finalize_string(Bin, Original, Skip, Next, Len);
        $\\ ->
            unescape(Bin, Original, Skip, Next, Len);
        _C ->
            chunk(Bin, Original, Skip, Next, Len + 1)
    end.

-spec chunk(Binary, Binary, Skip, Next, Len, Acc) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    Acc :: iolist(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
chunk(<<C, Bin/binary>>, Original, Skip, Next, Len, Acc) ->
    case C of
        $" ->
            finalize_string(Bin, Original, Skip, Next, Len, Acc);
        $\\ ->
            unescape(Bin, Original, Skip, Next, Len, Acc);
        _C ->
            chunk(Bin, Original, Skip, Next, Len + 1, Acc)
    end.

-spec finalize_string(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
finalize_string(<<Bin/binary>>, Original, Skip, Next, Len) ->
    String = erlang:binary_part(Original, Skip, Len),
    next(Bin, Original, Skip + Len + 1, Next, String).

-spec finalize_string(Binary, Binary, Skip, Next, Len, Acc) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    Acc :: maybe_improper_list(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
finalize_string(<<Bin/binary>>, Original, Skip, Next, Len, Acc) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    String = iolist_to_binary([Acc | Chunk]),
    next(Bin, Original, Skip + Len + 1, Next, String).

-spec unescape(Binary, Binary, Skip, Next, Len) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
unescape(<<Bin/binary>>, Original, Skip, Next, Len) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    do_unescape(Bin, Original, Skip + Len + 1, Next, [Chunk]).

-spec unescape(Binary, Binary, Skip, Next, Len, Acc) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Len :: non_neg_integer(),
    Acc :: maybe_improper_list(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
unescape(<<Bin/binary>>, Original, Skip, Next, Len, Acc) ->
    Chunk = erlang:binary_part(Original, Skip, Len),
    do_unescape(Bin, Original, Skip + Len + 1, Next, [Acc | Chunk]).

-spec do_unescape(Binary, Binary, Skip, Next, Acc) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Acc :: maybe_improper_list(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
do_unescape(<<C, Bin/binary>>, Original, Skip, Next, Acc) ->
    case C of
        $" ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $"]);
        $/ ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $/]);
        $\\ ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\\]);
        $b ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\b]);
        $f ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\f]);
        $n ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\n]);
        $r ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\r]);
        $t ->
            chunk(Bin, Original, Skip + 1, Next, 0, [Acc, $\t]);
        $u ->
            unescape_unicode(Bin, Original, Skip + 1, Next, Acc)
    end.

-spec unescape_unicode(Binary, Binary, Skip, Next, Acc) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Acc :: iolist(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
unescape_unicode(<<U0, U1, U2, U3, Bin/binary>>, Original, Skip, Next, Acc) ->
    case unicode(U0, U1, U2, U3) of
        Unicode when Unicode >= 16#D800, Unicode =< 16#DFFF ->
            surrogate(Bin, Original, Skip + 4, Next, Acc, Unicode);
        Unicode ->
            chunk(Bin, Original, Skip + 4, Next, 0, [Acc, <<Unicode/utf8>>])
    end.

-spec unicode(byte(), byte(), byte(), byte()) -> non_neg_integer().
unicode(U0, U1, U2, U3) ->
    dec(U3) bor (dec(U2) bsl 4) bor (dec(U1) bsl 8) bor (dec(U0) bsl 12).

-spec dec(byte()) -> byte().
dec($0) -> 0;
dec($1) -> 1;
dec($2) -> 2;
dec($3) -> 3;
dec($4) -> 4;
dec($5) -> 5;
dec($6) -> 6;
dec($7) -> 7;
dec($8) -> 8;
dec($9) -> 9;
dec($a) -> 10;
dec($b) -> 11;
dec($c) -> 12;
dec($d) -> 13;
dec($e) -> 14;
dec($f) -> 15;
dec($A) -> 10;
dec($B) -> 11;
dec($C) -> 12;
dec($D) -> 13;
dec($E) -> 14;
dec($F) -> 15.

-spec surrogate(Binary, Binary, Skip, Next, Acc, Unicode) -> OK | Error when
    Binary :: binary(),
    Skip :: non_neg_integer(),
    Next :: [?END | ?KEY | ?OBJECT | ?ARRAY | list() | map()],
    Acc :: iolist(),
    Unicode :: non_neg_integer(),
    OK :: {ok, Json},
    Json :: njson:t(),
    Error :: njson:decode_error().
surrogate(<<$\\, $u, U0, U1, U2, U3, Bin/binary>>, Original, Skip, Next, Acc, Unicode0) ->
    Unicode1 = unicode(U0, U1, U2, U3),
    Unicode = (Unicode0 - 16#D800) * 16#400 + (Unicode1 - 16#DC00) + 16#10000,
    chunk(Bin, Original, Skip + 6, Next, 0, [Acc, <<Unicode/utf8>>]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%% RFC GRAMMAR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%
%%       JSON-text = ws value ws
%% 
%%       begin-array     = ws %x5B ws  ; [ left square bracket
%%       begin-object    = ws %x7B ws  ; { left curly bracket
%%       end-array       = ws %x5D ws  ; ] right square bracket
%%       end-object      = ws %x7D ws  ; } right curly bracket
%%       name-separator  = ws %x3A ws  ; : colon
%%       value-separator = ws %x2C ws  ; , comma
%% 
%%       ws = *(
%%               %x20 /              ; Space
%%               %x09 /              ; Horizontal tab
%%               %x0A /              ; Line feed or New line
%%               %x0D )              ; Carriage return
%% 
%%       value = false / null / true / object / array / number / string
%%       false = %x66.61.6c.73.65   ; false
%%       null  = %x6e.75.6c.6c      ; null
%%       true  = %x74.72.75.65      ; true
%%
%%       object = begin-object [ member *( value-separator member ) ]
%%                end-object
%%       member = string name-separator value
%% 
%%       array = begin-array [ value *( value-separator value ) ] end-array
%% 
%%       number = [ minus ] int [ frac ] [ exp ]
%%       decimal-point = %x2E       ; .
%%       digit1-9 = %x31-39         ; 1-9
%%       e = %x65 / %x45            ; e E
%%       exp = e [ minus / plus ] 1*DIGIT
%%       frac = decimal-point 1*DIGIT
%%       int = zero / ( digit1-9 *DIGIT )
%%       minus = %x2D               ; -
%%       plus = %x2B                ; +
%%       zero = %x30                ; 0
%% 
%%       string = quotation-mark *char quotation-mark
%%       char = unescaped /
%%           escape (
%%               %x22 /          ; "    quotation mark  U+0022
%%               %x5C /          ; \    reverse solidus U+005C
%%               %x2F /          ; /    solidus         U+002F
%%               %x62 /          ; b    backspace       U+0008
%%               %x66 /          ; f    form feed       U+000C
%%               %x6E /          ; n    line feed       U+000A
%%               %x72 /          ; r    carriage return U+000D
%%               %x74 /          ; t    tab             U+0009
%%               %x75 4HEXDIG )  ; uXXXX                U+XXXX
%%       escape = %x5C              ; \
%%       quotation-mark = %x22      ; "
%%       unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
