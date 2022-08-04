# njson
![njson](https://github.com/nomasystems/njson/actions/workflows/build.yml/badge.svg)

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

`njson` aims to be pragmatic and efficient. To do so, it considers some assumptions:

- It decodes JSON to erlang maps. Someone might argue about duplicated keys. We don't see any practical use for this feature, and the [RFC support this claim](https://datatracker.ietf.org/doc/html/rfc8259#section-4) ``The names within an object SHOULD be unique.`` 
- It decodes JSON `null` values as Erlang `undefined`. Given we use maps, this is as not decoding `null` values. Following this, it doesn't encode `null`. It's been a long debate, but no one has ever shown a practical case where not encoding an attribute or encoding an attribute with `null` value makes a difference. Previous considerations will make `njson` somehow asymmetric, but that is not a problem in practical cases.
- The decoder is implemented using CPS style. Thanks `jsone` team for showing us about it.




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
   File size (bytes)        NJson time(us)       Jsone time (us)       Jason time (us)
               13405                   166                   205                   581
               33785                   414                   498                  1322
               67540                   826                   985                  2798
              135685                  1701                  2024                  5870
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
Encoder:
--------------------------------------------------------------------------------------
   File size (bytes)        NJson time(us)       Jsone time (us)       Jason time (us)
               13405                    59                   134                   412
               33785                   193                   321                  1143
               67540                   333                   661                  2374
              135685                   847                  1389                  5343
--------------------------------------------------------------------------------------
```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/njson/issues).
