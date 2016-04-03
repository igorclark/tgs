-module( tgs_eh ).
-behaviour( gen_event ).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

% this handler is added by the tgs_sup supervisor
% so we don't need these convenience functions
-export( [
%	start_link/0,
%	add_handler/2
] ).

-export( [
	add_server/1,
	get_servers/0,
	update_server_list/0,
	time_sending_message_via_name_list/1,
	time_sending_message_via_registry/1
] ).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export( [
	init/1,
	handle_event/2,
	handle_call/2,
	handle_info/2,
	terminate/2,
	code_change/3
] ).

% name of the event manager this works with
-define( MANAGER, tgs_em ).

% simple state with list of tgs_gs server names
% to test performance of list vs registry lookup
-record( state, { servers = [] } ).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% add a tgs_gs server name
% called in tgs_gs:start_server/1 to support
% testing the performance vs registry lookups
add_server( Name ) ->
	gen_event:notify( { global, ?MANAGER }, { add_server, Name } ).

% get the stored list of tgs_gs server names
get_servers() ->
	gen_event:call( { global, ?MANAGER }, tgs_eh, get_servers ).

% update the stored list from the global registry
update_server_list() ->
	gen_event:call( { global, ?MANAGER }, tgs_eh, update_server_list ).

% send a message to all tgs_gs processes we know about
% according to the list we have in #state
time_sending_message_via_name_list( Msg ) ->
	gen_event:call( { global, ?MANAGER }, tgs_eh, { time_sending_message_via_name_list, Msg } ).

% send a message to all tgs_gs processes we know about
% according to the tgs_gs entries in global registry
time_sending_message_via_registry( Msg ) ->
	gen_event:call( { global, ?MANAGER }, tgs_eh, { time_sending_message_via_registry, Msg } ).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init( [] ) ->
    { ok, #state{} }.

handle_event( { add_server, ServerName }, State ) ->
	error_logger:info_msg( "~p adding server ~p", [ ?MODULE, ServerName ] ),
	NewServers = lists:reverse( [ ServerName | State#state.servers ] ),
	NewState = State#state{ servers = NewServers },
	{ ok, NewState };

handle_event( Event, State ) ->
	F = fun( T ) -> try tgs_gs = element( 1,T ) of _ -> true catch _:_ -> false end end,
	Servers = lists:filter( F, global:registered_names() ),
	[ tgs_gs:send_message( GS, Event ) || { tgs_gs, GS } <- Servers ],
	{ ok, State }.

handle_call( { time_sending_message_via_name_list, Event }, State) ->
%	error_logger:info_msg( "module ~p manager proc ~p got event ~p", [ ?MODULE, self(), Event ] ),
	T1 = erlang:timestamp(),
	[ tgs_gs:send_message( GS, Event ) || GS <- State#state.servers ],
	T2 = erlang:timestamp(),
	TD = timer:now_diff( T2,T1 ),
%	error_logger:info_msg( "time to send ~p messages via name_list: ~pµs", [ length(State#state.servers), TD ] ),
    { ok, TD, State };

handle_call({time_sending_message_via_registry, Event}, State) ->
%	error_logger:info_msg( "module ~p manager proc ~p got event ~p", [ ?MODULE, self(), Event ] ),
	T1 = erlang:timestamp(),
	[ tgs_gs:send_message(GS, Event) || GS <- tgs_gs:get_servers() ],
	T2 = erlang:timestamp(),
	TD = timer:now_diff(T2,T1),
%	error_logger:info_msg( "time to send ~p messages via registry: ~pµs", [ length(State#state.servers), TD ] ),
    {ok, TD, State};

handle_call( get_servers, State ) ->
	{ ok, State#state.servers, State };

handle_call( update_server_list, State ) ->
	NewState = State#state{
		servers = tgs_gs:get_servers()
	},
	{ ok, NewState#state.servers, NewState };

handle_call( Request, State ) ->
    Reply = [ { call, Request }, { state, State } ],
    { ok, Reply, State }.

handle_info( _Info, State ) ->
    { ok, State }.

terminate( _Reason, _State ) ->
    ok.

code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

