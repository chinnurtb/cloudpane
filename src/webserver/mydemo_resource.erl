%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(mydemo_resource).
-export([init/1, to_html/2]).
-debug(true).

-include_lib("webmachine/include/webmachine.hrl").
-include("cloudpane.hrl").

init([]) -> 
	?cinfo(ok),
	{{trace, "/tmp"}, undefined}.

to_html(ReqData, State) ->
    {"<html><body>Hello, new world</body></html>", ReqData, State}.
