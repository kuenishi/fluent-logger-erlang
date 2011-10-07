-module(simple).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    %% ?assertEqual(
    %%    "No, but I will!",
    %%    "Have you written any tests?"),
    ok.

-endif.
