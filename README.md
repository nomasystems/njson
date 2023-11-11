# njson
[![njson](https://github.com/nomasystems/njson/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/njson/actions/workflows/ci.yml)

`njson` is an OTP library to pragmatically and efficiently encode and decode JSON to Erlang maps.

## Setup

Add `njson` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {njson, {git, "git@github.com:nomasystems/njson.git", {branch, "main"}}}
]}.
```

## Features

`njson` exposes utilities via its API that allows you to:

| Function | Description |
| --------  | ------------ |
| `njson:decode/1` | Decodes a JSON binary as Erlang map |
| `njson:encode/2` | Encodes a map with binary keys as JSON |


## Implementation

`njson` aims to be pragmatic and efficient.
To do so, it decodes JSON to erlang maps with binary keys.
Someone might argue about duplicated keys.
We don't see any practical use for this feature,
and the [RFC support this claim](https://datatracker.ietf.org/doc/html/rfc8259#section-4) ``The names within an object SHOULD be unique.`` 

## A simple example

```erl
%%% Decode

1> njson:decode(<<"{\"a\":\"b\"}">>).
{ok,#{<<"a">> => <<"b">>},<<>>}

%%% Encode as binary
2> njson:encode(#{<<"a">> => <<"b">>}).
<<"{\"a\":\"b\"}">>

%%% Encode as iolist (even faster)
3> njson:encode(#{<<"a">> => <<"b">>}, true).
[123,[[34,<<"a">>,34],58,[34,<<"b">>,34]],125]

```

## Benchmarks

```
1> njson_bench:bench().
--------------------------------------------------------------------------------------
Decoder:
--------------------------------------------------------------------------------------
   File size (bytes)        NJson time(us)       Thoas time (us)       Jsone time (us)
               13405                 12096                 28063                 24249
               31793                 20188                 21051                 22784
               33785                 21844                 26516                 36027
               67540                 43974                 53933                 70363
              135685                 89314                106720                145824
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
Encoder:
--------------------------------------------------------------------------------------
   File size (bytes)        NJson time(us)       Thoas time (us)       Jsone time (us)
               13405                  9912                 12793                 11114
               31793                 12233                 15060                 13950
               33785                 23184                 28485                 23792
               67540                 45248                 63248                 48199
              135685                 90410                125532                101918
--------------------------------------------------------------------------------------```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/njson/issues).
