-module(cp_uuid).
-compile(export_all).

%% using deps/uuid project

%% v4 uuid for short (with out '-')
%% e.g. "79f492f813374200abcd92bada1cacao"
v4s() ->
	uuid:to_string(uuid:uuid4()).
%% v4 uuid for short (with out '-')
%% e.g. "79f492f8-1337-4200-abcd-92bada1cacao"
v4() ->
	uuid:to_string(uuid:uuid4()).