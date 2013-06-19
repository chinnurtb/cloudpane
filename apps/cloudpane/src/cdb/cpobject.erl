-module(cpobject).
-compile(export_all).

create(Record) ->
	cdb:call({create_object,Record}).

destroy(Record) ->
	cdb:call({destroy_object,Record}).

patch(Record) ->
	cdb:call({patch_object,Record}).

show(Record) ->
	cdb:call({show_object,Record}).

%% if you do not mention an object column attr, 
%% which will set empty
put(Record) ->
	cdb:call({put_object,Record}).

list(Conditoins) ->
	cdb:call({list_object,Conditoins}).

%% for history backup object
show_log(Record) ->
	cdb:call({show_log_object,Record}).

list_log(Conditoins) ->
	cdb:call({list_log_object,Conditoins}).

destroy_log(Record) ->
	cdb:call({destroy_log_object,Record}).
