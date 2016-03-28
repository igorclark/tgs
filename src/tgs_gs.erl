-module(tgs_gs).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, tgs_gs_sup).
-define(REGISTRY, global).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/1,
	start_server/1,
	stop_server/1,
	get_servers/0,
	send_message/2,
	call_message/2
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% this gets called by ?SUPERVISOR module
% to start a new simple_one_for_one ?MODULE process
start_link( Name ) ->
	gen_server:start_link(
		{ via, ?REGISTRY, { ?MODULE, Name } },
		?MODULE,
		[ Name ],
		[]
	).

% call this to have the ?SUPERVISOR
% start a named ?MODULE process
start_server( Name ) ->
	supervisor:start_child( ?SUPERVISOR, [ Name ] ).

% stop a named ?MODULE process
stop_server( Name ) ->
	Pid = ?REGISTRY:whereis_name( { ?MODULE, Name } ),
	case supervisor:terminate_child( ?SUPERVISOR, Pid ) of
		{ error, simple_one_for_one } ->
			{ error, { name_not_found, Name } };
		OtherResponse -> OtherResponse
	end.

% get a list of names under which
% ?MODULE processes are registered
get_servers() ->
	F=fun(T) ->
		try ?MODULE = element(1,T) of
			_ -> true
			catch _:_ -> false
		end
	end,
	AllNames = ?REGISTRY:registered_names(),
	lists:reverse(
		[ X || {?MODULE, X} <- lists:filter( F, AllNames )]
	).

send_message(Name, Msg) ->
	gen_server:cast({via, ?REGISTRY, {?MODULE, Name}}, Msg).

call_message(Name, Msg) ->
	gen_server:call({via, ?REGISTRY, {?MODULE, Name}}, Msg).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Name]) ->
	io:format("module ~p process ~p init(~p)", [?MODULE, self(), Name]),
	tgs_eh:add_server(Name),
    {ok, Name}.

handle_call(Msg, _From, State) ->
    {reply, [{call_msg,Msg},{state,State}], State}.

handle_cast(_Msg, State) ->
	error_logger:info_msg("module ~p process ~p (~p) got message: ~p", [?MODULE, self(), State, _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("module ~p process ~p terminating", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

