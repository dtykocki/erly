# erly

## Overview
**Erly** A URL shortening service backed by OTP and Mnesia.

## How to compile:

The first thing you have to do is to compile all the Erlang files using `rebar`.

```bash
$ ./rebar get-deps compile
```
    
## How to use with rebar:

You can use erly as a dependency in your rebar.config:

```erlang
{deps , [
    {erly, ".*", {git, "https://github.com/dtykocki/erly.git", {tag, "0.0.1"}}}
]}.
```
    
## How to run the application:

```bash
$ erl -pa deps/*/ebin -pa ebin
```

```erlang
1> application:start(mnesia).
ok
2> application:start(erly).
ok
```
## Usage

```erlang
1> erly:create_url("www.google.com").
{ok,{url,3,"www.google.com","68PXaMWx"}}

2> erly:lookup_url("68PXaMWx").
{ok,{url,3,"www.google.com","68PXaMWx"}}
```

## License

This software is licensed under [the MIT license](license).
