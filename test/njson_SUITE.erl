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
-module(njson_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
% ¡
-define(UTF8_INVERTED_EXCLAMATION_MARK, <<16#C2, 16#A1>>).

% 中华人民共和国
-define(UTF8_COUNTRY1,
    <<16#E4, 16#B8, 16#AD, 16#E5, 16#8D, 16#8E, 16#E4, 16#BA, 16#BA, 16#E6, 16#B0, 16#91, 16#E5,
        16#85, 16#B1, 16#E5, 16#92, 16#8C, 16#E5, 16#9B, 16#BD>>
).

% Российская Федерация
-define(UTF8_COUNTRY2,
    <<16#D0, 16#A0, 16#D0, 16#BE, 16#D1, 16#81, 16#D1, 16#81, 16#D0, 16#B8, 16#D0, 16#B9, 16#D1,
        16#81, 16#D0, 16#BA, 16#D0, 16#B0, 16#D1, 16#8F, 16#20, 16#D0, 16#A4, 16#D0, 16#B5, 16#D0,
        16#B4, 16#D0, 16#B5, 16#D1, 16#80, 16#D0, 16#B0, 16#D1, 16#86, 16#D0, 16#B8, 16#D1, 16#8F>>
).
% 日本国
-define(UTF8_COUNTRY3, <<16#E6, 16#97, 16#A5, 16#E6, 16#9C, 16#AC, 16#E5, 16#9B, 16#BD>>).
% 日本語
-define(UTF8_LANGUAGE, <<16#E6, 16#97, 16#A5, 16#E6, 16#9C, 16#AC, 16#E8, 16#AA, 16#9E>>).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [properties, json_decode, json_encode, json_json, json_undefined_encoding].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
properties() ->
    [{userdata, [{doc, "Property testing njson"}]}].

properties(Conf) ->
    ct_property_test:quickcheck(
        njson_properties:properties([object_keys_ascii, strings_no_escaped]),
        Conf
    ).

json_decode() ->
    [{userdata, [{doc, "Registry API."}]}].

json_decode(_Conf) ->
    Test = fun({Json, Erlang}) ->
        try
            Erlang = njson:decode(Json)
        catch
            Error ->
                throw({error, [{decoding, Json}, {exptected, Erlang}, {received, Error}]})
        end
    end,
    lists:foreach(Test, json_decode_only_cases() ++ json_cases()).

json_encode() ->
    [{userdata, [{doc, "Registry API."}]}].

json_encode(_Conf) ->
    Test = fun({Json, Erlang}) ->
        try
            Json = njson:encode(Erlang)
        catch
            Error ->
                throw({error, [{encoding, Erlang}, {exptected, Json}, {received, Error}]})
        end
    end,
    lists:foreach(Test, json_encode_only_cases() ++ json_cases()),

    Test2 = fun({Json, Erlang}) ->
        try
            Json = iolist_to_binary(njson:encode(Erlang, true))
        catch
            Error ->
                throw({error, [{encoding, Erlang}, {exptected, Json}, {received, Error}]})
        end
    end,
    lists:foreach(Test2, json_encode_only_cases() ++ json_cases()).

json_decode_only_cases() ->
    [
        {<<"-1.0e-3">>, -0.001},
        {<<"+1.0E+3">>, 1.0e3},
        {<<"+1E+3">>, 1.0e3},
        {<<"\"\\\\, \\b, \\f, \\r, \\n\"">>, <<"\\, \b, \f, \r, \n">>},
        {<<"\s\t\r\n { \r\n   \"currency\" \t\r\n: \"\\u20AC\"}">>, #{
            <<"currency">> => <<"€"/utf8>>
        }},
        {<<"{\"currency\": \"\\u20ac\"}">>, #{<<"currency">> => <<"€"/utf8>>}},
        {<<"{\"key1\":\"Val1\", \"key2\":null, \"key3\":\"Val3\"}">>, #{
            <<"key1">> => <<"Val1">>, <<"key3">> => <<"Val3">>
        }},

        {<<"{\"d\":\"a6\\/\"}">>, #{<<"d">> => <<"a6/">>}},
        {<<"null">>, undefined}
    ].

json_encode_only_cases() ->
    [
        {<<"\"\\\\, \\t, \\b, \\f, \\r, \\n\"">>, <<"\\, \t, \b, \f, \r, \n">>},
        {<<"{\"listKey\":\"a6\"}">>, #{<<"listKey">> => <<"a6">>}},
        {<<"{\"binaryKey\":\"a6\"}">>, #{<<"binaryKey">> => <<"a6">>}},
        {<<"{}">>, #{}},
        {<<"{\"a\":{}}">>, #{<<"a">> => #{}}},
        {<<"2147483648">>, 2147483648},
        {<<"-2147483649">>, -2147483649},
        {<<"null">>, null},
        {<<"{\"key2\":\"Val2\"}">>, #{<<"key2">> => <<"Val2">>}},
        {<<"{\"key1\":\"Val1\"}">>, #{<<"key1">> => <<"Val1">>}},
        {<<"{\"key1\":\"Val1\",\"key3\":\"Val3\"}">>, #{
            <<"key1">> => <<"Val1">>, <<"key3">> => <<"Val3">>
        }},
        {<<"{\"key1\":\"Val1\",\"key3\":{\"key31\":\"Val31\",\"key32\":\"Val32\"}}">>, #{
            <<"key1">> => <<"Val1">>,
            <<"key3">> => #{<<"key31">> => <<"Val31">>, <<"key32">> => <<"Val32">>}
        }},
        {<<"{\"key1\":\"Val1\",\"key3\":{\"key31\":\"Val31\",\"key32\":[\"Val32\"],\"key33\":{}}}">>,
            #{
                <<"key1">> => <<"Val1">>,
                <<"key3">> => #{
                    <<"key31">> => <<"Val31">>, <<"key32">> => [<<"Val32">>], <<"key33">> => #{}
                }
            }}
    ].

json_cases() ->
    [
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"">>, undefined},
        {<<"1">>, 1},
        {<<"2.0">>, 2.0},
        {<<"\"hola\"">>, <<"hola">>},
        {<<"[]">>, []},
        {<<"[1,2,3]">>, [1, 2, 3]},
        {<<"{\"a\":\"b\"}">>, #{<<"a">> => <<"b">>}},
        {<<"{\"a\":\"|\"}">>, #{<<"a">> => <<"|">>}},
        {<<"{\"message\":\"", (?UTF8_INVERTED_EXCLAMATION_MARK)/binary, "Hola Mundo!\"}">>, #{
            <<"message">> => <<"¡Hola Mundo!"/utf8>>
        }},
        {<<"{\"country\":\"", (?UTF8_COUNTRY1)/binary, "\"}">>, #{
            <<"country">> => <<"中华人民共和国"/utf8>>
        }},
        {<<"{\"country\":\"", (?UTF8_COUNTRY2)/binary, "\"}">>, #{
            <<"country">> => <<"Российская Федерация"/utf8>>
        }},
        {<<"{\"country\":\"", (?UTF8_COUNTRY3)/binary, "\"}">>, #{<<"country">> => <<"日本国"/utf8>>}},
        {
            <<"{\"country\":\"", (?UTF8_COUNTRY3)/binary, "\",\"language\":\"",
                (?UTF8_LANGUAGE)/binary, "\"}">>,
            #{<<"country">> => <<"日本国"/utf8>>, <<"language">> => <<"日本語"/utf8>>}
        }
    ].

json_json() ->
    [{userdata, [{doc, "Registry API."}]}].

json_json(_Conf) ->
    Test = fun({Json, Erlang}) -> Erlang = njson:decode(Json) end,
    ok = lists:foreach(Test, json_json_cases()),
    Test2 = fun({Json, Erlang}) -> Json = njson:encode(Erlang) end,
    ok = lists:foreach(Test2, json_json_cases()).

json_json_cases() ->
    [
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"[]">>, []},
        {<<"">>, undefined},
        {<<"\"ho\\\"l2\"">>, <<"ho\"l2">>},
        {<<"\"ho\\\"l\\\"2\"">>, <<"ho\"l\"2">>},
        {<<"[true]">>, [true]},
        {<<"[true,false]">>, [true, false]},
        {<<"1">>, 1},
        {<<"1.0">>, 1.0},
        {<<"[1,2]">>, [1, 2]},
        {<<"[\"a\",\"b\",7]">>, [<<"a">>, <<"b">>, 7]},
        {<<"9">>, 9}
    ].

json_undefined_encoding() ->
    [{userdata, [{doc, "Properly undefined management in encoding"}]}].

json_undefined_encoding(_Conf) ->
    <<"{}">> = njson:encode(#{}),
    <<"{\"foo\":\"bar\"}">> =
        njson:encode(#{<<"undefined">> => undefined, <<"foo">> => <<"bar">>}),
    <<"{\"foo\":{}}">> = njson:encode(#{<<"foo">> => #{<<"undefined">> => undefined}}),
    <<"{\"foo\":{\"bar\":\"baz\"}}">> =
        njson:encode(#{<<"foo">> => #{<<"undefined">> => undefined, <<"bar">> => <<"baz">>}}),
    ok.
