-module(fluent_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fluent_test_() ->
  {foreach,
   fun start/0,
   fun stop/1,
   [{"normal format",
     fun() ->
         ?assertEqual(ok, post({debug, {[{<<"hoge">>,<<"data">>}]}}))
     end},
    {"lager format",
     fun() ->
        ?assertEqual(ok, post({<<"log">>, {lager_msg,
                                          [],
                                          [],
                                          error,
                                          {["2013",45,"08",45,"16"],["13",58,"41",58,"38",46,"275"]},
                                          {1376,628098,275271},
                                          ["\"a\""]}}))
    end}
   ]}.

post(Data) ->
  Result = gen_event:notify(?MODULE, Data),
  meck:wait(gen_tcp, send, '_', 1000),
  ?assert(meck:validate(gen_tcp)),
  Result.

start() ->
    meck:new(gen_tcp, [passthrough, unstick]),
    meck:expect(gen_tcp, connect, [{['_', '_', '_'], {ok, socket}}]),
    meck:expect(gen_tcp, send, fun(_, Bin) ->
      ?assertMatch({ok, _}, msgpack:unpack(Bin)),
      ok
    end),
    meck:expect(gen_tcp, close, [{['_'], ok}]),
    {ok,_Pid}=gen_event:start({local, ?MODULE}),
    ok = gen_event:add_handler(?MODULE, fluent_event, debug).

stop(_) ->
    ok = gen_event:stop(?MODULE),
    meck:unload(gen_tcp).

-endif.
