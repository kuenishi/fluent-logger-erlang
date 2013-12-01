%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%% @copyright (C) 2011, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2011 by UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%%-------------------------------------------------------------------
-module(fluent_event).

-behaviour(gen_event).

%% API
-export([add_handler/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  tag  :: atom(),
	  tagbd :: binary(), % binary of "tag."
	  host :: inet:host(),
	  port :: inet:port_number(),
	  sock :: inet:socket()
	 }).

% for lager 2.0 format
-record(lager_msg,{
        destinations :: list(),
        metadata :: [tuple()],
        severity :: atom(),
        datetime :: {string(), string()},
        timestamp :: erlang:timestamp(),
        message :: list()
    }).

%% Adds an event handler
%% -spec add_handler(atom()) -> ok.
%% add_handler(Tag) ->
%%     add_handler(Tag,localhost,24224)

-spec add_handler(atom(), inet:host(), inet:port_number()) -> ok | {'EXIT', term()} | term().
add_handler(Tag,Host,Port) ->
    gen_event:add_handler(?SERVER, ?MODULE, {Tag,Host,Port}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
-spec init({atom(),inet:host(),inet:port_number()}) -> {ok, #state{}}.
init({Tag,Host,Port}) when is_atom(Tag) ->
    {ok,S} = gen_tcp:connect(Host,Port,[binary,{packet,0}]),
    TagBD = <<(atom_to_binary(Tag, latin1))/binary, ".">>,
    {ok,#state{tag=Tag,tagbd=TagBD,host=Host,port=Port,sock=S}};
init(Tag) when is_atom(Tag) ->
    init({Tag,localhost,24224}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
-spec handle_event({ atom() | string() | binary(), tuple() % => msgpack_term().
		   }, #state{}) -> {ok, #state{}} | remove_handler.
handle_event({<<"log">>, #lager_msg{datetime={Date, Time}, message=Message}}, State) ->
    Label = <<"lager_log">>,
    Data = {[{<<"lager_date">>, list_to_binary(Date)},
             {<<"later_time">>, list_to_binary(Time)},
             {<<"txt">>, list_to_binary(Message)}]},
    {Msec,Sec,_} = erlang:now(),
    Package = [<<(State#state.tagbd)/binary, Label/binary>>, Msec*1000000+Sec, Data],
    try_send(State, msgpack:pack(Package, [{enable_str,false}]), 3);

handle_event({Label,Data}, State) when is_atom(Label) ->
    handle_event({atom_to_binary(Label, latin1),Data}, State);

handle_event({Label,Data}, State) when is_list(Label) ->
    handle_event({list_to_binary(Label),Data}, State);

handle_event({Label,Data}, State) when is_binary(Label) , is_tuple(Data) -> % Data should be map
    {Msec,Sec,_} = os:timestamp(),
    Package = [<<(State#state.tagbd)/binary, Label/binary>>, Msec*1000000+Sec, Data],
    try_send(State, msgpack:pack(Package, [{enable_str,false}]), 3);

handle_event({log, _N, {Date, Time}, Data0}, State) ->
    Label = <<"lager_log">>,
    Data = {[{<<"lager_date">>, list_to_binary(Date)},
             {<<"later_time">>, list_to_binary(Time)},
             {<<"txt">>, list_to_binary(Data0)}]},
    {Msec,Sec,_} = os:timestamp(),
    Package = [<<(State#state.tagbd)/binary, Label/binary>>, Msec*1000000+Sec, Data],
    try_send(State, msgpack:pack(Package, [{enable_str,false}]), 3);

handle_event(Other, State) ->
    Label = <<"other">>,
    io:format("~p~n", [Other]),
    Data = {[{<<"log">>, list_to_binary(io_lib:format("~p", [Other]))}]},
    {Msec,Sec,_} = os:timestamp(),
    Package = [<<(State#state.tagbd)/binary, Label/binary>>, Msec*1000000+Sec, Data],
    try_send(State, msgpack:pack(Package, [{enable_str,false}]), 3).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
-spec terminate(atom(), #state{}) -> term().
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
try_send(_State, _, 0) -> throw({error, retry_over});
try_send(State, Bin, N) ->
    case gen_tcp:send(State#state.sock, Bin) of
	ok -> {ok, State};
	{error, closed} ->
	    Host = State#state.host,
	    Port = State#state.port,
	    {ok,S} = gen_tcp:connect(Host,Port,[binary,{packet,0}]),
	    try_send(State#state{sock=S}, Bin, N-1);
	Other ->
	    throw(Other)
    end.
