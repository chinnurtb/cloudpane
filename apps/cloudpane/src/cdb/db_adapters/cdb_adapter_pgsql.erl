-module(cdb_adapter_pgsql).
-behaviour(cdb_adapter).

-include("cdb.hrl"). 
-export([start/1,stop/0,init/1,terminate/1]).
-export([create_object/2,destroy_object/2,show_object/2,update_object/2,patch_object/2,list_object/2]).
-export([create_schema/2,destroy_schema/2,show_schema/2,patch_schema/2,list_schema/2]).
-export([add_column_schema/2,del_column_schema/2]).
 
-debug(true).

start(_) ->
	ok.

stop() ->
	ok.

%must return {ok,Conn}
init(Options) ->
    DBHost = proplists:get_value(db_host, Options, "localhost"),
    DBPort = proplists:get_value(db_port, Options, 5433),
    DBUsername = proplists:get_value(db_username, Options, "postgres"),
    DBPassword = proplists:get_value(db_password, Options, "postgres"),
    DBDatabase = proplists:get_value(db_database, Options, "cloudpane"),

    Result = pgsql:connect(DBHost, DBUsername, DBPassword, 
        [{port, DBPort}, {database, DBDatabase}]),

    {ok,Conn} = Result,

	CreateObjectSchema = pgsql:squery(Conn,
			"CREATE TABLE IF NOT EXISTS cp_object_schema (id text NOT NULL, name text NOT NULL, CONSTRAINT cp_object_schema_pkey PRIMARY KEY (id), CONSTRAINT cp_object_schema_name_key UNIQUE (name));"),
 	?cinfo(CreateObjectSchema),
	CreateColumnSceham = pgsql:squery(Conn,
			"CREATE TABLE IF NOT EXISTS cp_column_schema (id text NOT NULL, name text NOT NULL, db_type text NOT NULL,type text NOT NULL,default_value text,object_schema_id text NOT NULL,CONSTRAINT cp_column_schema_pkey PRIMARY KEY (id),UNIQUE(object_schema_id,name));"),
	?cinfo(CreateColumnSceham),
    Result.

terminate(Conn) ->
	pgsql:close(Conn).

create_uuid() ->
	uuid:to_string(uuid:uuid4()).

create_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	UUID = create_uuid(),
	%Alias = proplists:get_value(alias,Record,undefined),
	Id = proplists:get_value(id,Record,UUID),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	Name = proplists:get_value(name,Record,""),
	Columns = proplists:get_value(columns,Record,[]),
	TableName = "t_"++ SchemaId,
	CreateObject = pgsql:equery(Conn,
		"insert into \"" ++ TableName ++"\" (id,name) values($1,$2);",
		[Id, Name]),

	case CreateObject of
		{ok,1} ->
			case Columns of
				[] ->
					{ok,[]};
				Columns ->
					update_object(Conn,Record)
			end;
		Others ->
			?cerror(Others),
			{error,#error_object{}}
	end. 

destroy_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	Id = proplists:get_value(id,Record,undefined),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	case Id of
		undefined ->	
			{error,#error_object{message="unsupported api params right now"}};
		Id ->
			%not check SchemaId == undefined
			TableName = "t_"++ SchemaId,
			DelResult = pgsql:equery(Conn,
			"delete from \""++ TableName ++"\" where id = $1 or alias = $1",
			[Id]),
			case DelResult of
				{error,Reason} ->
					?cerror(Reason),
					{error,#error_object{}};
				{ok,1} ->
					{ok,#raw_object{id=Id}};
				{ok,_} ->
					?cwarn(["clear more than one object with same id"]),
					{ok,#raw_object{id=Id}};
				Others ->
					?cerror(Others),
					{error,#error_object{}}
			end
	end.

%cpobject:show([{schema_id,"28953000-0000-0000-0002-000000000002"},{id,"28953000-0000-0000-0003-000000000002"}]).
show_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	ObjectId = proplists:get_value(id,Record,undefined),
	%%TODO check and filter pin(security) field
	TableName = "t_"++SchemaId,
	ShowResult = pgsql:equery(
		Conn,
		"select * from \""++ TableName ++"\" where id = $1",
		[ObjectId]
	),
	case ShowResult of
		{ok,_Columns,[]} ->
			{error,#error_object{message="not such object"}};
		{ok,Columns,Rows} ->
			?cinfo(Columns),
			?cinfo(Rows),
			[Row] = Rows,
			ListRow = tuple_to_list(Row),
			?cinfo(ListRow),
			 
			Combine = fun(X,Y) ->
				{column,Name,Type,_,_,_} = X,
				?cinfo(X),
				?cinfo(Y), 
				#raw_column{name=Name,db_type=Type,v=Y}
			end,
			ColumnsCombine = lists:zipwith(Combine,Columns,ListRow),
			{ok,#raw_object{id=ObjectId,schema_id=SchemaId,columns=ColumnsCombine}};
		
		Others ->
			?cerror(Others),
			{error,#error_object{message="unknown error in show object"}}
	end.

patch_object(Conn,Record) ->
	update_object(Conn,Record).

update_object(Conn,Record) ->
	?cinfo({Conn,Record}),
	Id = proplists:get_value(id,Record,undefined),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	Columns = proplists:get_value(columns,Record,[]),
	TableName = "t_" ++ SchemaId,
	P = fun(Item,Acc) ->
		ColumnName = proplists:get_value(name,Item,undefined),
		ColumnValue = proplists:get_value(value,Item,undefined),
		 
		Result = pgsql:equery(Conn,"update \""++ TableName ++"\" set "
			++ ColumnName ++" = $1 where id = $2",
			[ColumnValue,Id]),
		case Result of
			{ok,_} ->
				Acc;
			{error,Reason} ->
				?cerror(Reason),
				Acc+1
		end
	end,
	FailureCount = lists:foldr(P,0,Columns),
	case FailureCount of
		0 ->
			{ok,#raw_object{id=Id,schema_id=SchemaId,columns=Columns}};
		N ->
			?cerror(N),
			{error,#error_object{message="update object error"}}
	end.
	

 
list_object(Conn,Conditions) ->
	?cinfo({Conn,Conditions}),
	?cerror(todo),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	NewUUID = create_uuid(),
	Id = proplists:get_value(id,Record,NewUUID),
	Name = proplists:get_value(name,Record,"UnamedSchema"),
	%Creator = proplists:get_value(creator,Record,"system"),
	%Domain = proplists:get_value(domain,Record,"default"),
	TableName = "t_"++ Id,
	PK = "CONSTRAINT \""++ TableName ++ "_pkey\" PRIMARY KEY (id), CONSTRAINT \""++TableName ++"_alias_key\" UNIQUE (alias)",
    CreateTable = pgsql:squery(Conn,
    	"CREATE TABLE \""
    	++ TableName 
    	++"\" (id text NOT NULL,alias text, name text NOT NULL, domain text, creator text, timestamp int,"
    	++ PK ++");"),

	case CreateTable of
		{ok,[],[]} ->
			AddRecResult = pgsql:equery(Conn,"insert into cp_object_schema (id,name) values($1,$2);",[Id,Name]),
			case AddRecResult of
				{ok,1} ->
					{ok,#object_schema{id=Id,name=Name}};
				{error,Reason} ->
					?cerror(Reason),
					{error,#error_object{message="add schema data failure"}}
			end;
		{error,Reason} ->
			?cerror(Reason),
			{error,#error_object{message="create schema table failure"}}
	end.

%%%%%%%%%%%%%%%%%%TODO
destroy_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	Id = proplists:get_value(id,Record,undefined),
	DelSchemaRecordResult = pgsql:equery(Conn,"delete from cp_object_schema where id = $1",[Id]),
	?cinfo(DelSchemaRecordResult),
	case DelSchemaRecordResult of
		{ok,1} ->
			DelColumnsResult = pgsql:equery(Conn,"delete from cp_column_schema where object_schema_id = $1",[Id]),
			case DelColumnsResult of
				{ok,0} ->
					?cinfo(there_is_no_columns_in_this_schema),
					{ok,#object_schema{id=Id}};
				{ok,N} ->
					?cinfo(N),
					{ok,#object_schema{id=Id}};
				UnknowReason ->
					?cerror(UnknowReason),
					{error,#error_object{message="unknow reason"}}
			end;
		Others ->
			?cerror(Others),
			{error,#error_object{message="destroy_schema failure with unknown reason"}}
	end.


show_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	Id = proplists:get_value(id,Record,undefined),
	case Id of
		undefined ->
			{error,#error_object{message="missing id in condition"}};
		UUID -> 
			SchemaResult = pgsql:equery(Conn,"select id,name from cp_object_schema where id = $1",[Id]),
			case SchemaResult of
				{ok,_C,[]} ->
					{error,#error_object{message="can not find schema in database"}};
				{ok,_SchemaColumn,SchemaRows} ->
					[SchemaRow] = SchemaRows,
					{SchemaId,SchemaName} = SchemaRow,


					ColumnsSchemaResult = pgsql:equery(Conn,
						"select id,name,db_type,type,default_value,object_schema_id from cp_column_schema where object_schema_id = $1",[Id]),
					case ColumnsSchemaResult of
						{ok,_Columns,[]} ->
							{ok,#object_schema{id=UUID,columns=[]}};
						{ok,_Columns,ColumnRows} ->
							SF = fun(Item,Acc) ->
								{ColumnId,ColumnName,
								ColumnDbType,ColumnType,
								DefaultValue,SchemaId} = Item,
								lists:append(Acc,[#column_schema{
										id= ColumnId,
										name=ColumnName,
										type = ColumnType,
										db_type = ColumnDbType,
										default_value = DefaultValue
										%schema_id = SchemaId
									}])
							end,
							Combine = lists:foldr(SF,[],ColumnRows),
							{ok,#object_schema{id=UUID,name=SchemaName,columns=Combine}};
						Others ->
							?cerror(Others),
							{error,#error_object{}}
					end;
				UnknowReason ->
					?cerror(UnknowReason),
					{error,#error_object{}}
			end
	end.

%%%%%%%%%%%%%%%%%%TODO
patch_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	?cerror(todo),
	ok.

list_schema(Conn,Conditions) ->
	?cinfo(Conditions),
    Result = pgsql:squery(Conn,"select id,name from cp_object_schema"),
    ?cinfo(Result),
    case Result of
		{ok,_Columns,Rows} ->
		 	?cinfo(Rows),
			P = fun(Item, AccIn) ->
				{Id,_Name} = Item,
				lists:append(AccIn,[show_schema(Conn,[{id,Id}])])
			end,
			SchemaObjectList = lists:foldr(P,[],Rows),
			{ok,SchemaObjectList};
		_Others ->
			{error,#error_object{}}
    end.

%[{id,XXX} ,{columns,[{xxxx_id,"age","int","int"}]}]  // {ColumnName,DBType,WebType}
add_column_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	UUID = create_uuid(),
 	ColumnId = proplists:get_value(id,Record,UUID),
 	ColumnName = proplists:get_value(name,Record,""),
 	ColumnType = proplists:get_value(type,Record,"string"),
	ColumnDbType = proplists:get_value(db_type,Record,"text"),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	ColumnDefaultValue = proplists:get_value(default_value,Record,undefined),

	%ALTER TABLE t_94b105afa797458a873239e72c53d8b9 ADD COLUMN id uuid;
	TableName = "t_"++SchemaId,
	AlterResult = pgsql:squery(Conn,"ALTER TABLE \""
			++ TableName ++"\" ADD COLUMN "++ ColumnName ++ " "++ ColumnDbType),
	?cinfo(AlterResult),
	case AlterResult of
		{ok,[],[]} ->
			AddRecResult = pgsql:equery(Conn,
				"insert into cp_column_schema (id,name,type,db_type,default_value,object_schema_id) values($1,$2,$3,$4,$5,$6)",
				[ColumnId,ColumnName,ColumnType,ColumnDbType,ColumnDefaultValue,SchemaId]),
			?cinfo(AddRecResult),
			case AddRecResult of
				{ok,1} ->
					{ok,#column_schema{id = ColumnId,name=ColumnName,
							type=ColumnType,db_type=ColumnDbType,
							schema_id= SchemaId
							}};
				AddRecError ->
					?cerror(AddRecError),
					{error,#error_object{}}
			end;
		Others ->
			?cinfo(Others),
			{error,#error_object{message="add column schema failure"}}
	end.
 
del_column_schema(Conn,Record) ->
	?cinfo({Conn,Record}),
	%Id = proplists:get_value(id,Record,undefined),
	SchemaId = proplists:get_value(schema_id,Record,undefined),
	ColumnName = proplists:get_value(name,Record,undefined),
	TableName = "t_"++ SchemaId,
 
	AlterResult = pgsql:squery(Conn,"ALTER TABLE \""++TableName++"\" DROP COLUMN \""++ColumnName++"\""),
	?cinfo(AlterResult),
	case AlterResult of
		{ok,[],[]} ->
			RemoveRecordResult = pgsql:equery(Conn,
				"delete from cp_column_schema where object_schema_id = $1 and name = $2",
				[SchemaId,ColumnName]),
			case RemoveRecordResult of
				{ok,N} ->
					?cinfo(N),
					{ok,#column_schema{schema_id=SchemaId}};
				OtherResult ->
					?cerror(OtherResult),
					{error,#error_object{}}
			end;
		OtherResult ->
			?cerror(OtherResult),
			{error,#error_object{}}
	end.
 
