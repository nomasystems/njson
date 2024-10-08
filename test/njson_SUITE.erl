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

%%% INCLUDES
-include_lib("stdlib/include/assert.hrl").

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
    [
        properties,
        json_decode,
        json_encode,
        json_json,
        json_undefined_encoding,
        json_emoji,
        json_errors
    ].

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
        ?assertEqual({ok, Erlang}, njson:decode(Json))
    end,
    lists:foreach(Test, json_decode_only_cases() ++ json_cases()).

json_encode() ->
    [{userdata, [{doc, "Registry API."}]}].

json_encode(_Conf) ->
    Test = fun({Json, Erlang}) ->
        ?assertEqual({ok, Json}, njson:encode(Erlang))
    end,
    lists:foreach(Test, json_encode_only_cases() ++ json_cases()),

    Test2 = fun({Json, Erlang}) ->
        {ok, Encoded} = njson:encode(Erlang),
        ?assertEqual(Json, iolist_to_binary(Encoded))
    end,
    lists:foreach(Test2, json_encode_only_cases() ++ json_cases()),
    {ok, _Json} = njson:encode(#{<<"key2">> => <<"Val2">>}, true),
    {ok, _Json2} = njson:encode([<<"val1">>, <<"val2">>], true),
    {ok, _Json3} = njson:encode(<<"val1">>, true).

json_decode_only_cases() ->
    [
        {<<239, 187, 191, "-1.0e-3">>, -0.001},
        {<<"-1.0e-3">>, -0.001},
        {<<"+1.0E+3">>, 1.0e3},
        {<<"+1E+3">>, 1.0e3},
        {<<"false", $\s, $\t, $\r, $\n>>, false},
        {<<"[true, false \t \n \r \s]">>, [true, false]},
        {<<"\s\t\r\n { \r\n   \"currency\" \t\r\n: \"\\u20AC\"}">>, #{
            <<"currency">> => <<"€"/utf8>>
        }},
        {<<"{\"currency\": \"\\u20ac\"}">>, #{<<"currency">> => <<"€"/utf8>>}},
        {<<"{\"key1\":\"Val1\", \"key2\":null, \"key3\":\"Val3\"}">>, #{
            <<"key1">> => <<"Val1">>, <<"key2">> => null, <<"key3">> => <<"Val3">>
        }},
        {<<"{\"key1\":\"Val1\"\t \n \r \s, \t \n \r \s}">>, #{
            <<"key1">> => <<"Val1">>
        }},
        {<<"{\"d\":\"a6/\"}">>, #{<<"d">> => <<"a6/">>}},
        {<<"null">>, null}
    ].

json_encode_only_cases() ->
    [
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
        {<<"\"\\\\, \\\", \\t, \\b, \\f, \\r, \\n \"">>, <<"\\, \", \t, \b, \f, \r, \n ">>},
        {<<"\" \\u0000, \\t, \\u000B, \\r, \\u000E \"">>, <<" \x00, \t, \v, \x0D, \x0E ">>},
        {<<"\" \\u000F, \\u001D, \\u001F, ¹, ï \"">>, <<" \x0F, \x1D, \x1F, \xb9, \xef ">>},
        {<<"\" \\u0019, \\u000B, \\u000E, \\u000F \"">>, <<" \x19, \x0b, \x0e, \x0f ">>},
        {<<"true">>, true},
        {<<"false">>, false},
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
    Test = fun({Json, Erlang}) ->
        ?assertEqual({ok, Erlang}, njson:decode(Json))
    end,
    ok = lists:foreach(Test, json_json_cases()),
    Test2 = fun({Json, Erlang}) ->
        ?assertEqual({ok, Json}, njson:encode(Erlang))
    end,
    ok = lists:foreach(Test2, json_json_cases()).

json_json_cases() ->
    [
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"[]">>, []},
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
    ?assertEqual(
        {ok, <<"{}">>},
        njson:encode(#{}, false)
    ),
    ?assertEqual(
        {ok, <<"{\"foo\":\"bar\"}">>},
        njson:encode(#{<<"undefined">> => undefined, <<"foo">> => <<"bar">>})
    ),
    ?assertEqual(
        {ok, <<"{\"foo\":{}}">>},
        njson:encode(#{<<"foo">> => #{<<"undefined">> => undefined}})
    ),
    ?assertEqual(
        {ok, <<"{\"foo\":{\"bar\":\"baz\"}}">>},
        njson:encode(#{<<"foo">> => #{<<"undefined">> => undefined, <<"bar">> => <<"baz">>}})
    ),
    ok.

json_emoji() ->
    [{userdata, [{doc, "Properly decoding json with emojis"}]}].

json_emoji(_Conf) ->
    HoFBin = <<"{\"text\":{\"body\":\"\\u2764\\u200d\\ud83d\\udd25\\u0bef\"}}">>,
    DecodedHoF = #{
        <<"text">> => #{
            <<"body">> => <<226, 157, 164, 226, 128, 141, 240, 159, 148, 165, 224, 175, 175>>
        }
    },
    ?assertEqual({ok, DecodedHoF}, njson:decode(HoFBin)),
    EncodedHoF =
        <<123, 34, 116, 101, 120, 116, 34, 58, 123, 34, 98, 111, 100, 121, 34, 58, 34, 226, 157,
            164, 226, 128, 141, 240, 159, 148, 165, 224, 175, 175, 34, 125, 125>>,
    ?assertEqual({ok, EncodedHoF}, njson:encode(DecodedHoF)),
    io:format("~tp~n", [DecodedHoF]),
    io:format("~ts~n", [EncodedHoF]).

json_errors() ->
    [{userdata, [{doc, "Check errors"}]}].

json_errors(_Conf) ->
    {error, _} = njson:decode(<<"not-a-json">>),
    {error, _} = njson:decode(<<"folse">>),
    {error, _} = njson:decode(<<"troe">>),
    {error, _} = njson:decode(<<"other">>),
    {error, _} = njson:decode(<<"{,}">>),
    {error, _} = njson:decode(<<"false,">>),
    {error, _} = njson:encode(test),
    ok.
