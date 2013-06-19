-module(cdb).
-compile(export_all).
-include("cdb.hrl"). 
-behaviour(application).
-export([start/0,start/1,call/1,stop/0]).
-compile(export_all).

version() ->
	{1,0,0,0}.

start() ->
	start([]).

start(Options) ->
    AdapterName = proplists:get_value(adapter, Options, pgsql),
    Adapter = list_to_atom(lists:concat(["cdb_adapter_", AdapterName])),
    Adapter:start(Options),
	cdb_sup:start_link(Options).
 
stop() ->
	% we do not design to close db here.
	ok.

call(Msg) ->
	cdb_pool:call(my_cdb_pool,Msg,30*1000).

