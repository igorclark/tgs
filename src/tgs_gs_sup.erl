-module( tgs_gs_sup ).

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
	% only start the tgs_gs_sup supervisor locally
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

% start a simple_one_for_one supervisor
% for creation of dynamic tgs_gs gen_servers
init( [] ) ->
	error_logger:info_msg( "module ~p process ~p init( ~p )", [ ?MODULE, self(), [] ] ),

	TgsGsSpec = {
		tgs_gs, { tgs_gs, start_link, [] },
			permanent, 5000, worker, [ tgs_gs ]
	},

    { ok, { { simple_one_for_one, 5, 10 }, [ TgsGsSpec ] } }.
