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
-module(njson_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% EXPORTS
-compile([export_all, nowarn_export_all]).

%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
%% Object keys are ASCII only
-type option() ::
    object_keys_ascii
    %% Strings contain no escaped characters
    | strings_no_escaped
    %% All strings are ASCII only
    | strings_ascii
    %% Numbers contain no exponentials
    | number_no_exp.

%%% Tests two properties:
%%% 1.- json:decode based on the JSON RFC 7159.
%%% 2.- symmetry of json_encode and json:decode
-spec properties(option()) -> term().
properties(Options) ->
    ?FORALL(
        JsonValue,
        triq_dom:sample(json_value(Options)),
        begin
            Json = json_value_pretty_print_binary(JsonValue),
            Result =
                try
                    Decoded = njson:decode(Json),
                    Json2 = njson:encode(Decoded),
                    Decoded2 = njson:decode(Json2),
                    {Decoded,
                        % Tests property 1
                        compare(JsonValue, Decoded),
                        % Tests property 2
                        compare(JsonValue, Decoded2)}
                of
                    R ->
                        R
                catch
                    Exception:Reason:St ->
                        {caught, Exception, Reason, St}
                end,
            ?WHENFAIL(
                case Result of
                    {caught, _, _} = E ->
                        io:format(
                            "Exception: ~p~nExpected abstract form: ~p~nJSON text input: ~p~n",
                            [E, JsonValue, Json]
                        );
                    {D, _, _} ->
                        io:format(
                            "Result of decode: ~p~nExpected abstract form: ~p~nJSON text input: ~p~n",
                            [D, JsonValue, Json]
                        )
                end,
                element(2, Result) == true andalso
                    element(3, Result) == true
            )
        end
    ).

compare({object, []}, #{}) ->
    true;
compare([], []) ->
    true;
compare({object, ExpectedMembers}, MapMembers) when
    is_map(MapMembers), length(ExpectedMembers) == map_size(MapMembers)
->
    Members = maps:to_list(MapMembers),
    SortedExpectedMembers = lists:keysort(2, ExpectedMembers),
    SortedMembers = lists:keysort(1, Members),
    lists:all(
        fun({{member, ExpectedKey, ExpectedValue}, {Key, Value}}) ->
            compare(ExpectedKey, Key) andalso compare(ExpectedValue, Value)
        end,
        lists:zip(SortedExpectedMembers, SortedMembers)
    );
compare(ExpectedArrayElements, ArrayElements) when
    is_list(ExpectedArrayElements),
    is_list(ArrayElements),
    length(ExpectedArrayElements) == length(ArrayElements)
->
    lists:all(fun({A, B}) -> compare(A, B) end, lists:zip(ExpectedArrayElements, ArrayElements));
compare({string, ExpectedCharList}, String) when is_binary(String) ->
    unicode:characters_to_binary(ExpectedCharList) == String;
compare({string, ExpectedCharList}, String) when is_atom(String) ->
    erlang:binary_to_atom(unicode:characters_to_binary(ExpectedCharList), utf8) == String;
compare({number, _, _, _, _} = ExpectedNumber, Number) when is_number(Number) ->
    ExpectedFloat = expected_number_to_float(ExpectedNumber),
    %% Check that the two numbers are roughly the same by comparing the distance of a representation
    %% that preserves the lexicographic order of IEEE floats
    lexicographic_distance(ExpectedFloat, Number) =< 3;
compare(ExpectedValue, Value) ->
    ExpectedValue == Value.

lexicographic_distance(F1, F2) ->
    lexicographic_point(F2) - lexicographic_point(F1).

lexicographic_point(F) ->
    list_to_integer(
        lists:flatten([io_lib:format("~8.2.0B", [C]) || C <- binary_to_list(<<F/float>>)]), 2
    ).

expected_number_to_float({number, Minus, Int, Frac, Exp}) ->
    Sign =
        case Minus of
            minus -> -1;
            _ -> +1
        end,
    FracNr =
        case is_number(Frac) of
            true ->
                list_to_float("0." ++ integer_to_list(Frac));
            _ ->
                0
        end,
    ExpNr =
        case Exp of
            {minus, F} -> math:pow(10, -F);
            {_, F} -> math:pow(10, F);
            _ -> 1
        end,
    Sign * (Int + FracNr) * ExpNr.

json_value_pretty_print_binary(JsonValue) ->
    case pretty_print_json(JsonValue) of
        Binary when is_binary(Binary) ->
            Binary;
        List when is_list(List) ->
            list_to_binary(List)
    end.

json_value(Options) when is_list(Options) ->
    json_value(Options, 5).

json_value(Options, 0) ->
    oneof([
        json_false(),
        json_true(),
        json_string(Options),
        json_number(Options)
    ]);
json_value(Options, Depth) when Depth > 0 ->
    oneof([
        json_object(Options, Depth - 1),
        json_array(Options, Depth - 1),
        json_false(),
        json_true(),
        json_string(Options),
        json_number(Options)
    ]).

json_object(Options, Depth) ->
    ?LET(L, list(json_object_member(Options, Depth)), {object, remove_duplicated_keys(L)}).

remove_duplicated_keys(L) ->
    lists:ukeysort(2, L).

json_object_member(Options, Depth) ->
    {member, json_object_member_name(Options), json_value(Options, Depth)}.

json_array(Options, Depth) ->
    list(json_value(Options, Depth)).

json_false() ->
    false.

json_true() ->
    true.

json_object_member_name(Options) ->
    case proplists:is_defined(object_keys_ascii, Options) of
        true ->
            json_string_ascii(Options);
        false ->
            json_string(Options)
    end.

json_string(Options) ->
    case proplists:is_defined(strings_ascii, Options) of
        true ->
            json_string_ascii(Options);
        false ->
            {string, list(json_char(Options))}
    end.

json_string_ascii(Options) ->
    {string, list(json_char_ascii(Options))}.

json_char(Options) ->
    case proplists:is_defined(strings_no_escaped, Options) of
        true ->
            elements([json_char_unescaped()]);
        false ->
            elements([json_char_unescaped(), json_char_escaped()])
    end.

json_char_ascii(Options) ->
    case proplists:is_defined(strings_no_escaped, Options) of
        true ->
            elements([json_char_ascii_unescaped()]);
        false ->
            elements([json_char_ascii_unescaped(), json_char_escaped()])
    end.

json_char_ascii_unescaped() ->
    oneof([
        choose(16#20, 16#21),
        choose(16#23, 16#5B),
        choose(16#5D, 16#7D)
    ]).

json_char_unescaped() ->
    %% This spec differs slightly from the range in the RFC grammar, as it does not make sense to
    %% generate characters in the range (16#D800 - 16#DFFF) - those are reserved for UTF-16 surrogate pairs
    oneof([
        choose(16#20, 16#21),
        choose(16#23, 16#5B),
        choose(16#5D, 16#D7FF),
        choose(16#E000, 16#10FFFF)
    ]).

json_char_escaped() ->
    oneof(json_char_escaped_list()).

json_char_escaped_list() ->
    [16#22, 16#5C, 16#2F, 16#8, 16#C, 16#A, 16#D, 16#9].

json_number(Options) ->
    ?SUCHTHAT(
        Number,
        case proplists:is_defined(number_no_exp, Options) of
            true ->
                {number, oneof([undefined, minus]), nat(), oneof([undefined, nat()]), undefined};
            false ->
                {number, oneof([undefined, minus]), nat(), oneof([undefined, nat()]),
                    oneof([undefined, json_number_exp()])}
        end,
        case Number of
            {number, minus, 0, _, _} ->
                false;
            _ ->
                true
        end
    ).

json_number_exp() ->
    {oneof([minus, plus, undefined]), nat_positive()}.

nat_positive() ->
    triq_dom:pos_integer().

nat() ->
    triq_dom:non_neg_integer().


pretty_print_json(false) ->
    <<"false">>;
pretty_print_json(true) ->
    <<"true">>;
pretty_print_json(null) ->
    <<"null">>;
pretty_print_json({string, CharacterList}) ->
    case
        unicode:characters_to_binary(pretty_print_json_escape_chars(CharacterList), unicode, utf8)
    of
        {error, _, RestData} -> throw({unicode_encode_failure, RestData});
        Bin when is_binary(Bin) -> [$\", Bin, $\"]
    end;
pretty_print_json({number, Minus, Int, Frac, Exp}) ->
    [
        pretty_print_json_minus_plus(Minus),
        integer_to_list(Int),
        pretty_print_json_frac(Frac),
        pretty_print_json_exp(Exp)
    ];
pretty_print_json({object, Members}) ->
    [${, pretty_print_json_object_members(Members), $}];
pretty_print_json(ArrayElements) when is_list(ArrayElements) ->
    [$[, pretty_print_json_array_elements(ArrayElements), $]].

pretty_print_json_minus_plus(minus) ->
    $-;
pretty_print_json_minus_plus(plus) ->
    $+;
pretty_print_json_minus_plus(_) ->
    <<>>.

pretty_print_json_frac(Int) when is_integer(Int) ->
    [$., integer_to_list(Int)];
pretty_print_json_frac(_) ->
    <<>>.

pretty_print_json_exp({MinusPlus, Int}) ->
    [$e, pretty_print_json_minus_plus(MinusPlus), integer_to_list(Int)];
pretty_print_json_exp(undefined) ->
    <<>>.

pretty_print_json_object_members([{member, String, Value}]) ->
    [pretty_print_json(String), $:, pretty_print_json(Value)];
pretty_print_json_object_members([H | T]) ->
    [pretty_print_json_object_members([H]), $,, pretty_print_json_object_members(T)];
pretty_print_json_object_members([]) ->
    <<>>.

pretty_print_json_array_elements([H]) ->
    pretty_print_json(H);
pretty_print_json_array_elements([H | T]) ->
    [pretty_print_json(H), $,, pretty_print_json_array_elements(T)];
pretty_print_json_array_elements([]) ->
    <<>>.

pretty_print_json_escape_chars(CharacterList) ->
    lists:map(fun escape_char/1, CharacterList).

escape_char(16#22) ->
    [16#5C, 16#22];
escape_char(16#5C) ->
    [16#5C, 16#5C];
escape_char(16#2F) ->
    [16#5C, 16#2F];
escape_char(16#8) ->
    [16#5C, 16#62];
escape_char(16#C) ->
    [16#5C, 16#66];
escape_char(16#A) ->
    [16#5C, 16#6E];
escape_char(16#D) ->
    [16#5C, 16#72];
escape_char(16#9) ->
    [16#5C, 16#74];
escape_char(C) ->
    C.
