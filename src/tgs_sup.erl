-module( tgs_sup ).

-behaviour( supervisor ).

%% API
-export( [
	start_link/0
] ).

%% Supervisor callbacks
-export( [
	init/1
] ).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	% only registering the overall supervisor locally
    { ok, Pid } = supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ),

	% once the supervisor is started, all its children are running
	% so now's the time to add the tgs_eh event handler for events
	% managed by the tgs_em event manager
	ok = gen_event:add_handler( { global, tgs_em }, tgs_eh, [] ),

	{ ok, Pid }.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init( [] ) ->
	error_logger:info_msg( "module ~p process ~p init( ~p )", [ ?MODULE, self(), [] ]),

	% tgs_gs_sup supervisor to
	% supervise tgs_gs gen_servers
	TgsGsSup = {
		tgs_gs_sup,
		{ tgs_gs_sup, start_link, [] },
		permanent,
		5000,
		supervisor,
		[ tgs_gs_sup ]
	},

	% gen_event manager for tgs_em events
	% registered globally because ...
	% i'm learning about using 'global' registry
	TgsEmSup = {
		{ global, tgs_em },
		{ gen_event, start_link, [ { global, tgs_em } ] },
		permanent,
		5000,
		worker,
		[ dynamic ]
	},

    { ok, { { one_for_one, 5, 10 }, [ TgsGsSup, TgsEmSup ] } }.

