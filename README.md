# fluent-logger-erlang

[![Build Status](https://travis-ci.org/kuenishi/fluent-logger-erlang.png?branch=master)](https://travis-ci.org/kuenishi/fluent-logger-erlang)

erlang logger using gen_event to output to fluent.

## Examples

```erl
1> {ok,_Pid} = gen_event:start({local, yourlogger}),
2> ok = gen_event:add_handler(yourlogger, fluent_event, myapp),
```

### eep18

```erl
3> ok = gen_event:notify(yourlogger, {access, {[{<<"agent">>,<<"foo">>}]}}),
% 2013-12-17 22:55:43 +0900 myapp.access: {"agent":"foo"}
```

### proplist

```erl
4> ok = gen_event:notify(yourlogger, {access, [{<<"agent">>,<<"foo">>}]}),
% 2013-12-17 22:55:53 +0900 myapp.access: {"agent":"foo"}
```

### with Lager

In `app.config` or `sys.config`:

```erl
{lager, [
  {handlers, [
     {fluent_event, {yourappname, hostname, 24224}}
]}]},
```
