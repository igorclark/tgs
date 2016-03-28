-module(tgs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	ok = gen_event:add_handler({global,tgs_em}, tgs_eh, []),
	{ok, Pid}.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("module ~p process ~p init(~p)", [?MODULE, self(), []]),
	TgsGsSup = {
		tgs_gs_sup,
		{tgs_gs_sup, start_link, []},
		permanent,
		5000,
		supervisor,
		[tgs_gs_sup]
	},
	TgsEmSup = {
		{global, tgs_em},
		{gen_event, start_link, [{global, tgs_em}]},
		permanent,
		5000,
		worker,
		[dynamic]
	},
    {ok, { {one_for_one, 5, 10}, [TgsGsSup, TgsEmSup]} }.

