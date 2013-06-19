-module(cp_time).
-compile(export_all).
 

timestamp()->
	TS = erlang:now(),
	{Mega,Sec,Micro} = TS,
	Mega*1000000*1000000 + Sec*1000000 + Micro.

time_after({Year,Mon,Day},{Hour,Minus,Sec}) ->
	{{Y,M,D},{H,Min,S}} = calendar:local_time(),
	After = {{Y+Year,Mon+M,D+Day},{H+Hour,Min+Minus,S+Sec}},
	After.

datetime() ->
	{Y,M,D} = erlang:date(),
	{Hour,Min,Sec} = erlang:time(),
	integer_to_list(Y) ++ "/" ++ 
	integer_to_list(M) ++ "/" ++
	integer_to_list(D) ++ " " ++
	integer_to_list(Hour) ++ ":" ++
	integer_to_list(Min) ++ ":" ++
	integer_to_list(Sec).	

is_expired({{Year,Mon,Day},{Hour,Min,Sec}}) ->
	TheTime = {{Year,Mon,Day},{Hour,Min,Sec}},
	RightNow = calendar:local_time(),
	RightNow > TheTime.
