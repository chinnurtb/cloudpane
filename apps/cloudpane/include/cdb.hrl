-define(
	record_to_tuplelist(Rec, Ref),
begin
	lists:zip(
				record_info(fields, Rec),
				tl(tuple_to_list(Ref))
			)
end).

-record(state, {
        adapter, 
        connection,
        work_id
        %shards = [],
        %model_dict = dict:new(),
        %cache_enable,
        %cache_ttl,
        %cache_prefix,
        %depth = 0
        }).

-record(error_object,{
		code,
		message
	}).

-record(raw_object,{
		record_genre = raw_object, % or raw_schema 
		id,
		alias,
		pin,
		pin_type,
		name,
		timestamp,
		columns=[], %%[{name,value}]
		schema_id, % % version 2.*
		schema_version,% version 2.*
		creator, % version 2.*
		domain % version 2.*
	}).

-record(raw_column,{
		name,
		v,
		%type,
		db_type
	}).

-record(object_schema,{
		id,
		name,
		columns=[], %% tuple list   {id,name,type,db_type,schema_id,default_value}
		domain % version 2.*
	}).

-record(column_schema,{
		id,
		name,
		type,
		db_type,
		schema_id,
		default_value
	}).

-define(debugme,
	begin
		[true] =:= proplists:get_value(debug,proplists:get_value(attributes,?MODULE:module_info()))
	end).

-define(cinfo(Msg),
	begin
		case proplists:get_value(debug,proplists:get_value(attributes,?MODULE:module_info())) of
			[true] ->
				error_logger:info_report([
					process_info(self(), current_function),
					Msg,
					{file,?FILE},
					{line,?LINE}
				])
		end
	end).

-define(cwarn(Msg),
	begin 
			error_logger:warning_report([
				process_info(self(), current_function),
				Msg,
				{file,?FILE},
				{line,?LINE}
			])
	end).

-define(cerror(Msg),
	begin
		error_logger:error_report([
			process_info(self(), current_function),
			Msg,
			{file,?FILE},
			{line,?LINE}
		])
	end).

-define(R2P(Record),
	begin

	end).


