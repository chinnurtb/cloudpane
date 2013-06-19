-module(data_domain_controller).
-compile(export_all).
-include("cloudpane.hrl").
-debug(true).
-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
   	case ?debugme of
   		true ->
		   	{{trace, "/tmp"}, Config};  %% debugging code
		false ->
			{ok,Config}
   	end.

allowed_methods(RD,Ctx) ->
	{['POST','GET'],RD,Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

content_types_accepted(RD,Ctx) ->
	{[{"application/json"},from_json],RD,Ctx}.

from_json(RD,Ctx) ->
	{true,RD,Ctx}.

to_json(RD,Ctx) ->
	{true,RD,Ctx}. 

process_post(RD,Ctx) ->
	?cinfo(running),
	StructBody = 
	{
		struct,
		[
			{<<"id">>,111},
			{<<"method">>,<<"test_method">>},
			{<<"result">>,["22222"]},
			{<<"error">>,null}
		]
	},
	EncodedBody = mochijson2:encode(StructBody),
	{true,wrq:append_to_response_body(EncodedBody,RD),Ctx}.