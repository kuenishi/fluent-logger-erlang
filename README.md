# fluent-logger-erlang

[![Build Status](https://travis-ci.org/kuenishi/fluent-logger-erlang.png?branch=master)](https://travis-ci.org/kuenishi/fluent-logger-erlang)

erlang logger using gen_event to output to fluent.

## Examples

```erl
1> {ok,_Pid} = gen_event:start({local, yourlogger}),
2> ok = gen_event:add_handler(yourlogger, fluent_event, debug),
3> ok = gen_event:notify(yourlogger, {debug, {[{<<"hoge">>,<<"data">>}]}}),
```

Now I am thinking of better interface.

## TODO

* better interface (accepting proplist-like map only, binary() or string()? )
* tests
* unknown failure in Lion: 

```
=ERROR REPORT==== 9-Oct-2011::00:07:27 ===
** Generic server inet_gethost_native_sup terminating 
** Last message in was {'EXIT',<0.94.0>,killed}
** When Server state == {state,inet_gethost_native,undefined,<0.94.0>,
                               {local,inet_gethost_native_sup}}
```
