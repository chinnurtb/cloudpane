-module(cdb_adapter_mnesia).
-behaviour(cdb_adapter).

-include("cdb.hrl"). 
-export([start/1,stop/0,init/1,terminate/1]).
-export([create_object/2,destroy_object/2,show_object/2,update_object/2,patch_object/2,list_object/2]).
-export([create_schema/2,destroy_schema/2,show_schema/2,patch_schema/2,list_schema/2]).
-export([add_column_schema/2,del_column_schema/2]).
 
start(_) ->
	application:start(mnesia).

stop() ->
	application:stop(mnesia).

init(_Options) ->
    ok.

terminate(_Conn) ->
	ok.

create_object(Conn,Record) ->
	?cinfo({Conn,Record}),

	ok.

destroy_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

show_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

patch_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

update_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

list_object(Conn,Conditions) ->
	?cinfo({Conn,Conditions}),
	ok.

create_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

destroy_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

show_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

patch_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

list_schema(Conn,Conditions) ->
	?cinfo({Conn,Conditions}),
	ok.

add_column_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.

del_column_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	ok.
%% private 
