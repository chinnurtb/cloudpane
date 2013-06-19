-module(cdb_pool).
-export([call/2,call/3]).
-include("cdb.hrl").

call(Pool, Msg) ->
    Worker = poolboy:checkout(Pool),
    ?cinfo(Worker),
    Reply = gen_server:call(Worker, Msg),
    poolboy:checkin(Pool, Worker),
    Reply.

call(Pool, Msg, Timeout) ->
    Worker = poolboy:checkout(Pool),
    Reply = gen_server:call(Worker, Msg, Timeout),
    poolboy:checkin(Pool, Worker),
    Reply.
 