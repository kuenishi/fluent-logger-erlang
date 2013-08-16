-module(simple).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

aaa_test() ->
    {ok,Pid}=gen_event:start({local, ?MODULE}),
    ok = gen_event:add_handler(?MODULE, fluent_event, debug),
    ok = gen_event:notify(?MODULE, {debug, {[{<<"hoge">>,<<"data">>}]}}),
%    error_logger:error_msg("hogehoge"),

    ok = gen_event:stop(?MODULE),
    ok.

lager_test() ->
    {ok,Pid}=gen_event:start({local, ?MODULE}),
    ok = gen_event:add_handler(?MODULE, fluent_event, debug),
    ok = gen_event:notify(?MODULE, {<<"log">>, {lager_msg,
                                                [],
                                                [],
                                                error,
                                                {["2013",45,"08",45,"16"],["13",58,"41",58,"38",46,"275"]},
                                                {1376,628098,275271},
                                                ["\"a\""]}}),

    ok = gen_event:stop(?MODULE),
    ok.

-endif.
