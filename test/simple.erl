-module(simple).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

aaa_test() ->
    {ok,Pid}=gen_event:start({local, ?MODULE}),
    ?debugHere,
    ok = gen_event:add_handler(?MODULE, fluent_event, debug),
    ?debugHere,
    ?debugVal(Pid),
    ok = gen_event:notify(?MODULE, {debug, {[{<<"hoge">>,<<"data">>}]}}),
    ok.

-endif.
