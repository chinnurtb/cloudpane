%% @author zhang weizhong <@zhangweizhong>
%% @doc cloudpane global macro resources.
%%-include_lib("stdlib/include/qlc.hrl").
-include("ej.hrl").

-define(cinfo(Msg),
			{current_function, {_M, F, A}} = process_info(self(), current_function),
			error_logger:info_report([
				{mfa,?MODULE,F,A},
				Msg,
				{file,?FILE},
				{line,?LINE}
			])
		).

-define(cwarn(Msg),
			{current_function, {_M, F, A}} = process_info(self(), current_function),
			error_logger:warning_report([
				{mfa,?MODULE,F,A},
				Msg,
				{file,?FILE},
				{line,?LINE}
			])
		).

-define(cerror(Msg),
			{current_function, {_M, F, A}} = process_info(self(), current_function),
			error_logger:error_report([
				{mfa,?MODULE,F,A},
				Msg,
				{file,?FILE},
				{line,?LINE}
			])
		).




