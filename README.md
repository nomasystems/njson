# njson
[![njson](https://github.com/nomasystems/njson/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/njson/actions/workflows/ci.yml)

`njson` is a robust and efficient Erlang library that seamlessly encodes and decodes JSON
data into Erlang terms, encoding JSON objects as Erlang maps.
It is designed to be pragmatic and efficient, making it an ideal choice for a wide range
of Erlang applications.


## Features and implementation decisions

njson offers a comprehensive set of features for handling JSON data in Erlang:

- **Easy-to-use API**: njson provides a straightforward API that makes it easy to encode
and decode JSON data.
- **Efficient decoding**: njson's decoding is optimized for performance, ensuring
efficient processing of large JSON payloads.
- **Binary key support**: njson encodes and decodes JSON data using binary keys,
improving memory usage and performance. This also avoids atom table overflow vulnerability.
- **Duplicated key handling**: njson adheres to the [RFC recommendation](https://datatracker.ietf.org/doc/html/rfc8259#section-4)
that object keys should be unique, eliminating potential conflicts.
- **Flexible encoding options**: njson supports encoding as both binary and iolist,
catering to different performance requirements.

## Setup

Add `njson` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {njson, {git, "git@github.com:nomasystems/njson.git", {branch, "main"}}}
]}.
```

## Usage

`njson` provides two primary functions for encoding and decoding JSON data:

| Function | Description |
| --------  | ------------ |
| `njson:decode/1` | Decodes a JSON binary as Erlang map |
| `njson:encode/2` | Encodes a map with binary keys as JSON |

## A simple example

```erl
%%% Decode

1> njson:decode(<<"{\"a\":\"b\"}">>).
{ok,#{<<"a">> => <<"b">>}}

%%% Encode as binary
2> njson:encode(#{<<"a">> => <<"b">>}).
{ok,<<"{\"a\":\"b\"}">>}

%%% Encode as iolist (even faster)
3> njson:encode(#{<<"a">> => <<"b">>}, true).
{ok,[123,[[34,<<"a">>,34],58,[34,<<"b">>,34]],125]}

```

## Error handling

See [details](https://github.com/nomasystems/njson/blob/94c586b92a7e24c403089cdbe2994b7e7c87b9cc/src/njson.erl#L22)

```erl

%%% Decode 

1> njson:decode(<<"{\"a\":\"b\}">>).
{error,{unexpected_end_of_string,[],6}}


%%% Encode
2> njson:encode(#{<<"a">> => a}).   
{error,{invalid_map,{<<"a">>,a,{invalid_value,a}}}}
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
--------------------------------------------------------------------------------------
```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/njson/issues).
