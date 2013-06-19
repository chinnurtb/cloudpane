%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cloudpane application.

-module(cloudpane_app).
-author('@zhangweizhong').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cloudpane.
start(_Type, _StartArgs) ->
    cloudpane_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cloudpane.
stop(_State) ->
    Res = application:stop(cloudpane),
    application:stop(webmachine),
    application:stop(mochiweb),
 
    %application:stop(cdb),
    cdb:stop(),

    application:stop(crypto),
    application:stop(inets),
    Res.
    %ok.
