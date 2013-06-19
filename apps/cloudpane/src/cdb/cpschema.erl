-module(cpschema).
-compile(export_all).

create(Record) ->
	cdb:call({create_schema,Record}).

destroy(Record) ->
	cdb:call({destroy_schema,Record}).

show(Record) ->
	cdb:call({show_schema,Record}).

add_column(Record) ->
	cdb:call({add_column_schema,Record}).

del_column(Record) ->
	cdb:call({del_column_schema,Record}).

 
patch(Record) ->
	cdb:call({patch_schema,Record}).

list(Conditoins) ->
	cdb:call({list_schema,Conditoins}).


%% for history backup object
show_log(Record) ->
	cdb:call({show_log_schema,Record}).

list_log(Conditoins) ->
	cdb:call({list_log_schema,Conditoins}).

destroy_log(Record) ->
	cdb:call({destroy_log_schema,Record}).
