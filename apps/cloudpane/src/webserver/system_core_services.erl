-module(system_core_services).
-compile(export_all).
-include("cloudpane.hrl").

-record(context,{
		session = undefined,
		json_rpc_method,
		json_rpc_id
	}).
-debug(true).

init(Arg) ->
	%?cinfo(Arg),
    case ?debugme of
        true ->
            {{trace, "/tmp"}, #context{}};
        false ->
            {ok, #context{}}
    end.

allowed_methods(RD,Ctx) ->
	{['POST'],RD,Ctx}.

is_authorized(RD,Ctx) ->
	Body = wrq:req_body(RD),
	?cinfo(Body),
	RefreshCookie = cp_uuid:v4(),
	NewHeader = wrq:set_resp_header("Set-Cookie","cp_session="++RefreshCookie++";",RD),
	{true,NewHeader,Ctx}.

content_types_accepted(Req,State) ->
	{[{"*/*",from_json}],Req,State}.

content_types_provided(Req, State) ->
    {[{"*/*", to_json}], Req, State}.

from_json(Req,State) ->
	{true, Req, State}.

to_json(ReqData, State) ->
    {true, ReqData, State}.

process_post(ReqData, State) ->
	  
	StructBody = 
	{
		struct,
		[
			{<<"id">>,1},
			{<<"method">>,<<"login">>},
			{<<"result">>,[]},
			{<<"error">>,null}
		]
	},
	Body = mochijson2:encode(StructBody),
    {true, wrq:append_to_response_body(Body, ReqData), State}.
