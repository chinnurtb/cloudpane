-module(cdb_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker). %start_link/1
-include("cdb.hrl").

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-debug(true).

start_link() ->
	start_link([]).

start_link(Args) ->
	?cinfo(Args),
	gen_server:start_link(?MODULE,Args,[]).

%[{adapter,"pgsql"},{db_host},"localhost"},{db_port,5432},{db_username,""}]
init(Args) -> 
    process_flag(trap_exit, true),
    %start adapter:start(X)
    AdapterName = proplists:get_value(adapter, Args, pgsql),
    ?cinfo({adapter_name,AdapterName}),
    Adapter = list_to_atom(lists:concat(["cdb_adapter_", AdapterName])),
    ?cinfo({adapter_module,Adapter}),
    {ok, Conn} = Adapter:init(Args),
    ?cinfo({conn,Conn}),
    {ok,#state
            {
                work_id = erlang:now(),
                connection = Conn, 
                adapter = Adapter
            }
    }.

handle_call({Action,Record}, _From, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Result = Adapter:Action(Conn,Record),
    ?cinfo({Action,State#state.work_id,Result}),
    {reply, Result, State};

handle_call(Others,_From,State) ->
    ?cwarn({unknow_handler_call_params,Others}),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
	?cinfo(Reason),
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Adapter:terminate(Conn),
    ok.
    % close all shards connections
    % lists:map(
    % 	fun({A, C}) ->
    %             A:terminate(C)
    %     end, 
    % State#state.shards).

handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.
