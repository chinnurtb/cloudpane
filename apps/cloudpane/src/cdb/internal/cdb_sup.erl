-module(cdb_sup).
-include("cdb.hrl").
-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).
-debug(true).

start_link() ->
    start_link([]).

%%
%% startargs example:
% [{adapter,"pgsql"},
% {db_host,"localhost"},
% {db_port,5432},
% {db_username,"cloudpane"},{db_password,"cloudpane"}
% {db_database,"cloudpane"}
% ] 
%%
start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    Args = [{name,{local,my_cdb_pool}},
            {worker_module,cdb_worker},
            {size,2},
            {max_overflow,4}| 
            StartArgs
            ],

    PoolSpec = {
    	cdb_controller_handler, %Id
    	{poolboy,start_link,[Args]}, 
        permanent, 
        2000, %Shutdown
        worker,
        [poolboy]
	},
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.