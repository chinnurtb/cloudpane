%%
%% The module(ref ej.erl) is intended to make it easy to work with the Erlang
%% structure used by `mochijson2' to represent JSON.  You can use
%% `ej:get' to walk an object and return a particular value, or
%% `ej:set' to update a value.
%%

-module(cp_json).
-export([
         get/2,
         get/3,
         set/3,
         set_p/3,
         delete/2,
         valid/2
         ]).

get(Keys,Obj) ->
	ej:get(Keys,Obj).

get(Keys,Obj,Default) ->
	ej:get(Keys,Obj,Default).

%% @doc Set a value in `Obj'
%%
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure.  If `Value' is the atom `EJ_DELETE',
%% then the path specified by `Keys' is removed (but see `delete/2').
%%

set(Keys,Obj,Value) ->
	ej:set(Keys,Obj,Value).

%% @doc Set a value in `Obj' and create missing intermediate
%%      nodes if need be.
%%
%% This resembles the -p option in mkdir. If the intermediate
%% elements in the structure are missing, then they are created.
%% This is useful when creating complex JSON structures from scratch.
%%
%% The arguments are the same as for `set'.
%%

set_p(Keys,Obj,Value) ->
	ej:set_p(Keys,Obj,Value).

% TODO: support setting list elements as well as a means to add new
% elements to a list.

%% @doc Remove the item specified by `Keys'.
delete(Keys,Obj) ->
	ej:delete(Keys,Obj).

%% @doc Validate JSON terms. Validity is determined by the
%% `ej_json_spec()` provided which has the shape of EJSON terms but
%% with keys and values describing what is expected. `Obj' is the
%% EJSON term to be validated. This function will return `ok' if all
%% validation rules succeed and a `#ej_invalid{}' record when the
%% first failure is encountered (validation specs are processed in
%% order, depth first).  NOTE: this function is experimental and the
%% API and definition of specs is subject to change.
valid(Spec,Obj)->
	ej:valid(Spec,Obj).


