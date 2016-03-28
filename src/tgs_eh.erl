-module(tgs_eh).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         add_handler/2]).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
		 add_server/1,
		 get_servers/0,
		 send_message_via_name_list/1,
		 send_message_via_registry/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(MANAGER, tgs_em).

-record(state, {servers=[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

add_server(Name) ->
	gen_event:notify({global, ?MANAGER}, {add_server, Name}).

get_servers() ->
	gen_event:call({global, ?MANAGER}, tgs_eh, get_servers).

send_message_via_name_list(Msg) ->
	gen_event:call({global, ?MANAGER}, tgs_eh, {send_message_via_name_list, Msg}).

send_message_via_registry(Msg) ->
	gen_event:call({global, ?MANAGER}, tgs_eh, {send_message_via_registry, Msg}).

start_link() ->
    gen_event:start_link({via, global, ?MANAGER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_event({add_server, ServerName}, State) ->
	io:format("~p adding server ~p", [?MODULE, ServerName]),
	NewServers = lists:reverse([ServerName|State#state.servers]),
	NewState = State#state{
		servers=NewServers
	},
	{ok, NewState};

handle_event(Event, State) ->
	F=fun(T) -> try tgs_gs = element(1,T) of _ -> true catch _:_ -> false end end,
	Servers = lists:filter(F,global:registered_names()),
	[ tgs_gs:send_message(GS, Event) || {tgs_gs, GS} <- Servers ],
	{ok, State}.

handle_call({send_message_via_name_list, Event}, State) ->
%	io:format("module ~p manager proc ~p got event ~p", [?MODULE, self(), Event]),
	T1 = erlang:timestamp(),
	[ tgs_gs:send_message(GS, Event) || GS <- State#state.servers ],
	T2 = erlang:timestamp(),
	TD = timer:now_diff(T2,T1),
%	io:format("time to send ~p messages via name_list: ~pus", [length(State#state.servers), TD]),
    {ok, TD, State};

handle_call({send_message_via_registry, Event}, State) ->
%	io:format("module ~p manager proc ~p got event ~p", [?MODULE, self(), Event]),
	F=fun(T) -> try tgs_gs = element(1,T) of _ -> true catch _:_ -> false end end,
	T1 = erlang:timestamp(),
	[ tgs_gs:send_message(GS, Event) || {tgs_gs, GS} <- lists:filter(F,global:registered_names())],
	T2 = erlang:timestamp(),
	TD = timer:now_diff(T2,T1),
%	io:format("time to send ~p messages via registry: ~pus", [length(State#state.servers), TD]),
    {ok, TD, State};

handle_call(get_servers, State) ->
	{ok, State#state.servers, State};

handle_call(Request, State) ->
    Reply = [{call, Request},{state, State}],
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

