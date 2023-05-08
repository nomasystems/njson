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
%%%
%%% We use CPS decoding style here. First heard about it on jsone
%%% https://github.com/sile/jsone
-module(njson_decoder).

%%% EXTERNAL EXPORTS
-export([decode/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------

%Ignore UTF8 BOM
decode(<<239, 187, 191, Json/binary>>) ->
    val(Json, []);
decode(Json) ->
    val(Json, []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
val(<<C, Bin/binary>>, Next) when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
    val(Bin, Next);
val(<<"false", Bin/binary>>, Next) ->
    next(Bin, false, Next);
val(<<"true", Bin/binary>>, Next) ->
    next(Bin, true, Next);
val(<<"null", Bin/binary>>, Next) ->
    next(Bin, undefined, Next);
val(<<$[, Bin/binary>>, Next) ->
    array(Bin, [{array, []} | Next]);
val(<<${, Bin/binary>>, Next) ->
    object(Bin, [{object, undefined, #{}} | Next]);
val(<<$", Bin/binary>>, Next) ->
    string(Bin, Next);
val(<<Bin/binary>>, Next) ->
    number(Bin, Next).

ws(<<C, Bin/binary>>, F, Next) when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
    ws(Bin, F, Next);
ws(<<Bin/binary>>, key, Next) ->
    key(Bin, Next);
ws(<<Bin/binary>>, val, Next) ->
    val(Bin, Next);
ws(<<Bin/binary>>, array, Next) ->
    array(Bin, Next);
ws(<<Bin/binary>>, object, Next) ->
    object(Bin, Next).

colon(<<$\s, Bin/binary>>, Next) ->
    colon(Bin, Next);
colon(<<$\t, Bin/binary>>, Next) ->
    colon(Bin, Next);
colon(<<$\r, Bin/binary>>, Next) ->
    colon(Bin, Next);
colon(<<$\n, Bin/binary>>, Next) ->
    colon(Bin, Next);
colon(<<$:, Bin/binary>>, Next) ->
    colon(Bin, Next);
colon(<<Bin/binary>>, Next) ->
    val(Bin, Next).

next(<<Bin/binary>>, Value, []) ->
    {ok, Value, Bin};
next(<<Bin/binary>>, Value, [{array, Values} | Next]) ->
    ws(Bin, array, [{array, [Value | Values]} | Next]);
next(<<Bin/binary>>, Value, [{object, undefined, Object} | Next]) ->
    colon(Bin, [{object, Value, Object} | Next]);
next(<<Bin/binary>>, undefined, [{object, _Key, Object} | Next]) ->
    ws(Bin, object, [{object, undefined, Object} | Next]);
next(<<Bin/binary>>, Value, [{object, Key, Object} | Next]) ->
    ws(Bin, object, [{object, undefined, maps:put(Key, Value, Object)} | Next]).

array(<<$], Bin/binary>>, [{array, Values} | Next]) ->
    next(Bin, lists:reverse(Values), Next);
array(<<$,, Bin/binary>>, Next) ->
    ws(Bin, val, Next);
array(<<Bin/binary>>, Next) ->
    ws(Bin, val, Next).

object(<<$}, Bin/binary>>, [{object, undefined, Object} | Next]) ->
    next(Bin, Object, Next);
object(<<$,, Bin/binary>>, Next) ->
    ws(Bin, key, Next);
object(<<Bin/binary>>, Next) ->
    ws(Bin, key, Next).

key(<<$", Bin/binary>>, Next) ->
    string(Bin, Next).

number(<<Bin/binary>>, Next) ->
    number(Bin, <<>>, Next).

number(<<D, Bin/binary>>, Acc, Next) when D > 47, D < 58 ->
    number(Bin, <<Acc/binary, D>>, Next);
number(<<$-, Bin/binary>>, Acc, Next) ->
    number(Bin, <<Acc/binary, $->>, Next);
number(<<$+, Bin/binary>>, Acc, Next) ->
    number(Bin, <<Acc/binary, $+>>, Next);
number(<<$., Bin/binary>>, Acc, Next) ->
    fraction(Bin, <<Acc/binary, $.>>, Next);
number(<<$e, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $., $0, $e>>, Next);
number(<<$E, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $., $0, $E>>, Next);
number(<<Bin/binary>>, Acc, Next) ->
    next(Bin, binary_to_integer(Acc), Next).

fraction(<<D, Bin/binary>>, Acc, Next) when D > 47, D < 58 ->
    fraction(Bin, <<Acc/binary, D>>, Next);
fraction(<<$e, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $e>>, Next);
fraction(<<$E, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $E>>, Next);
fraction(<<Bin/binary>>, Acc, Next) ->
    next(Bin, binary_to_float(Acc), Next).

exponent(<<D, Bin/binary>>, Acc, Next) when D > 47, D < 58 ->
    exponent(Bin, <<Acc/binary, D>>, Next);
exponent(<<$-, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $->>, Next);
exponent(<<$+, Bin/binary>>, Acc, Next) ->
    exponent(Bin, <<Acc/binary, $+>>, Next);
exponent(<<Bin/binary>>, Acc, Next) ->
    next(Bin, binary_to_float(Acc), Next).

string(<<Bin/binary>>, Next) ->
    {Count, Escaped} = string_len(Bin, 0, []),
    <<String:Count/binary, $", Rest/binary>> = Bin,
    if
        Escaped == [] ->
            next(Rest, String, Next);
        true ->
            Unescaped = unescape([String], Escaped),
            next(Rest, Unescaped, Next)
    end.

string_len(<<$", _Rest/binary>>, Len, Escaped) ->
    {Len, Escaped};
string_len(<<$\\, $", Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$">>, Pos, 2} | Escaped]);
string_len(<<$\\, $/, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$/>>, Pos, 2} | Escaped]);
string_len(<<$\\, $\\, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$\\>>, Pos, 2} | Escaped]);
string_len(<<$\\, $b, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$\b>>, Pos, 2} | Escaped]);
string_len(<<$\\, $f, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$\f>>, Pos, 2} | Escaped]);
string_len(<<$\\, $r, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$\r>>, Pos, 2} | Escaped]);
string_len(<<$\\, $n, Bin/binary>>, Pos, Escaped) ->
    string_len(Bin, Pos + 2, [{<<$\n>>, Pos, 2} | Escaped]);
string_len(<<$\\, $u, U0, U1, U2, U3, Bin/binary>>, Pos, Escaped) ->
    Unicode = unicode(U0, U1, U2, U3),
    case is_surrogate(Unicode) of
        true ->
            surrogate(Unicode, Bin, Pos, Escaped);
        false ->
            string_len(Bin, Pos + 6, [{<<Unicode/utf8>>, Pos, 6} | Escaped])
    end;
string_len(<<_C, Bin/binary>>, Len, Escaped) ->
    string_len(Bin, Len + 1, Escaped).

unescape(IoList, []) ->
    iolist_to_binary(IoList);
unescape([IoLPrefix | IoLRest], [{Replacement, Pos, Len} | Rest]) ->
    <<Prefix:Pos/binary, _Escaped:Len/binary, Suffix/binary>> = IoLPrefix,
    unescape([Prefix, Replacement, Suffix | IoLRest], Rest).

unicode(U0, U1, U2, U3) ->
    dec(U3) bor (dec(U2) bsl 4) bor (dec(U1) bsl 8) bor (dec(U0) bsl 12).

dec(C) when C >= $0, C =< $9 ->
    C - $0;
dec(C) when C >= $a, C =< $f ->
    C - $a + 10;
dec(C) when C >= $A, C =< $F ->
    C - $A + 10.

is_surrogate(Unicode) when Unicode >= 16#D800, Unicode =< 16#DFFF ->
    true;
is_surrogate(_Unicode) ->
    false.

surrogate(Unicode0, <<$\\, $u, U0, U1, U2, U3, Bin/binary>>, Pos, Escaped) ->
    Unicode1 = unicode(U0, U1, U2, U3),
    Unicode = (Unicode0 - 16#D800) * 16#400 + (Unicode1 - 16#DC00) + 16#10000,
    string_len(Bin, Pos + 12, [{<<Unicode/utf8>>, Pos, 12} | Escaped]).
