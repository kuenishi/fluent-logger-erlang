-module(fluent_wm_event).

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

%% copied from webmachine_logger.hrl
-record(wm_log_data,
        {resource_module :: atom(),
         start_time :: tuple(),
         method :: atom(),
         headers,
         peer,
         sock,
         path :: string(),
         version,
         response_code,
         response_length,
         end_time :: tuple(),
         finish_time :: tuple(),
         notes}).

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
init(Any) ->
    fluent_event:init(Any).

%% @private
%%-spec handle_event({ atom() | string() | binary(), tuple()}, #state{}) ->
%%                          {ok, #state{}} | remove_handler.
handle_event({log_access, LogData}, State) ->
    #wm_log_data{method=Method,
                 headers=Headers,
                 peer=Peer,
                 path=Path,
                 version=_Version,
                 response_code=ResponseCode,
                 response_length=ResponseLength,
                 start_time=StartTime,
                 end_time=EndTime} = LogData,
    User = <<"-">>,
    Time = list_to_binary(httpd_util:rfc1123_date()),
    %%% time_format %d/%b/%Y:%H:%M:%S %z
    Status = case ResponseCode of
                 {Code, _ReasonPhrase} when is_integer(Code)  ->
                     Code;
                 _ ->
                     ResponseCode
             end,
    Referer =
        case mochiweb_headers:get_value("Referer", Headers) of
            undefined -> [];
            R -> [{<<"referer">>, R}]
        end,
    UserAgent =
        case mochiweb_headers:get_value("User-Agent", Headers) of
            undefined -> [];
            U -> [{<<"agent">>, U}]
        end,
    Obj = {Referer ++ UserAgent ++
               [{<<"method">>, list_to_binary(atom_to_list(Method))},
                {<<"host">>, list_to_binary(Peer)},
                {<<"user">>, User},
                {<<"time">>, Time},
                {<<"path">>, list_to_binary(Path)},
                {<<"status">>, Status},
                {<<"size">>, ResponseLength},
                {<<"latency">>, timer:now_diff(EndTime, StartTime)/1000.0}]},
    fluent_event:handle_event({access_log, Obj}, State);

handle_event(_Other, State) ->
    {ok, State}.

%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
-spec terminate(atom(), #state{}) -> term().
terminate(Reason, State) ->
    fluent_event:terminate(Reason, State).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
