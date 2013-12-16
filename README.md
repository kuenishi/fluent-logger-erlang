# fluent-logger-erlang

[![Build Status](https://travis-ci.org/kuenishi/fluent-logger-erlang.png?branch=master)](https://travis-ci.org/kuenishi/fluent-logger-erlang)

erlang logger using gen_event to output to fluent.

## Examples

```erl
1> {ok,_Pid} = gen_event:start({local, yourlogger}),
2> ok = gen_event:add_handler(yourlogger, fluent_event, debug),
3> ok = gen_event:notify(yourlogger, {debug, {[{<<"hoge">>,<<"data">>}]}}),
```
