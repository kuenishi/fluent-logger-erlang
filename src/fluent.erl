%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%% @copyright (C) 2011, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2011 by UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%%-------------------------------------------------------------------
-module(fluent).
-author('@kuenishi').
-export([start/0, stop/0]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(fluent).

-spec stop() -> ok.
stop() ->
    application:stop(fluent).

-spec add_sender(atom(), inet:host(), inet:port_number()) -> ok.
add_sender(Tag,Host,Port)->
    okkkk.
