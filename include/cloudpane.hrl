%% @author zhang weizhong <@zhangweizhong>
%% @doc cloudpane global macro resources.

%%-include_lib("stdlib/include/qlc.hrl").
 
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