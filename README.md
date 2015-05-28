# erly

## Overview
**Erly** A URL shortening service backed by OTP and Mnesia.

### How to compile:

The first thing you have to do is to compile all the Erlang files using `rebar`.

    $ ./rebar get-deps compile
    
### How to use with rebar:

You can use erly as a dependency in your rebar.config:

    {deps , [
        {erly, ".*", {git, "https://github.com/dtykocki/erly.git", {tag, "0.0.1"}}}
    ]}.
    
### How to run the application:

    $ erl -pa deps/*/ebin -pa ebin
    1> application:start(mnesia).
    ok
    2> application:start(erly).
    ok

