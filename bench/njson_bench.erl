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
-module(njson_bench).

-define(SEPARATOR, io:format("--------------------------------------------------------------------------------------~n")).

%%% EXTERNAL EXPORTS
-export([bench/0, bench_decode/0, bench_encode/0, profile_decode/0, profile_encode/0]).

%%% MACROS
-define(TIMES , 1000).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
bench() ->
    bench_decode(),
    bench_encode().

bench_decode() ->
    Times = ?TIMES,
    ?SEPARATOR,
    io:format("Decoder:~n"),
    ?SEPARATOR,
    head(),
    bench_decode("bench/test14.json", Times),
    bench_decode("bench/test.json", Times),
    bench_decode("bench/test33.json", Times),
    bench_decode("bench/test66.json", Times),
    bench_decode("bench/test133.json", Times),
    ?SEPARATOR.
 
bench_encode() ->
    Times = ?TIMES,
    ?SEPARATOR,
    io:format("Encoder:~n"),
    ?SEPARATOR,
    head(),
    bench_encode("bench/test14.json", Times),
    bench_encode("bench/test.json", Times),
    bench_encode("bench/test33.json", Times),
    bench_encode("bench/test66.json", Times),
    bench_encode("bench/test133.json", Times),
    ?SEPARATOR.



profile_decode() ->
    Path = "bench/test.json",
    {ok, Bin} = file:read_file(Path),
    eflambe:apply({njson, decode, [Bin]}, [{output_format, brendan_gregg}]).

profile_encode() ->
    Path = "bench/test.json",
    {ok, Bin} = file:read_file(Path),
    {ok, J, _} = njson:decode(Bin),
    eflambe:apply({njson, encode, [J]}, [{output_format, brendan_gregg}]).



%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
head() ->
    io:format("~20.. s  ~20.. s  ~20.. s  ~20.. s~n",
              ["File size (bytes)", "NJson time(us)", "Jsone time (us)", "Jason time (us)"]).
 
bench_decode(Path, Times) ->
    {ok, Bin} = file:read_file(Path),
    JasonDecodeTime = erlperf:time(fun() -> jason:decode(Bin) end, Times),
    JsoneDecodeTime = erlperf:time(fun() -> jsone:decode(Bin,[]) end, Times),
    NJsonDecodeTime = erlperf:time(fun() -> njson:decode(Bin) end, Times),

    io:format("~20.. B  ~20.. B  ~20.. B  ~20.. B~n",
              [byte_size(Bin),
               round(NJsonDecodeTime/Times),
               round(JsoneDecodeTime/Times),
               round(JasonDecodeTime/Times)]).
   
bench_encode(Path, Times) ->
    {ok, Bin} = file:read_file(Path),
    J = njson:decode(Bin),


    JasonEncodeTime = erlperf:time(fun() -> jason:encode(J) end, Times),
    JsoneEncodeTime = erlperf:time(fun() -> jsone:encode(J, []) end, Times),
    NJsonEncodeTime = erlperf:time(fun() -> njson:encode(J, true) end, Times),
    io:format("~20.. B  ~20.. B  ~20.. B  ~20.. B~n",
              [byte_size(Bin),
               round(NJsonEncodeTime/Times),
               round(JsoneEncodeTime/Times),
               round(JasonEncodeTime/Times)]).
