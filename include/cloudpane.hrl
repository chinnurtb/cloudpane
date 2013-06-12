%% @author zhang weizhong <@zhangweizhong>
%% @doc cloudpane global macro resources.

-include_lib("stdlib/include/qlc.hrl").
-include("ej.hrl").

-define(cinfo(FunName,Msg),
	error_logger:info_report([
		{module,?MODULE},
		{function,FunName},
		Msg,
		{file,?FILE},
		{line,?LINE}
	])).

-define(cwarn(FunName,Msg),
	error_logger:warning_report([
		{module,?MODULE},
		{function,FunName},
		Msg,
		{file,?FILE},
		{line,?LINE}
	])).

-define(cerror(FunName,Msg),
	error_logger:error_report([
		{module,?MODULE},
		{function,FunName},
		Msg,
		{file,?FILE},
		{line,?LINE}
	])).

% %%cloudpane standard return (like json rpc result and error)
% -define(okobj(Result),{cp:to_binary(Result),null}).
% -define(errobj(Code,Msg),{[],cp_json:make_error_object(Code,Msg)}).









