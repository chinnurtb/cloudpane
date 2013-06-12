-module(data_domain_controller).
-compile(export_all).
-include("cloudpane.hrl").
-include_lib("webmachine/include/webmachine.hrl").

% init(_Arg) ->
% 	{ok,undefined}.

init(Config) ->
   {{trace, "/tmp"}, Config}.  %% debugging code

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
	StructBody = 
	{
		struct,
		[
			{<<"id">>,111},
			{<<"method">>,<<"test_method">>},
			{<<"result">>,""},
			{<<"error">>,null}
		]
	},
	EncodedBody = mochijson2:encode(StructBody),
	{true,wrq:append_to_response_body(EncodedBody,RD),Ctx}.