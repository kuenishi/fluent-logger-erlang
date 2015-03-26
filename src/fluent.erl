-module(fluent).
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(fluent).

-spec stop() -> ok.
stop() ->
    application:stop(fluent).
