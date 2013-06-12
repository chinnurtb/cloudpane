%% @author zhang weizhong <tony@ctyle.com>
%% @copyright 2013.

%% @doc cloudpane startup code

-module(cloudpane).
-author('zhangweizhong <tony@ctyle.com>').
-export([start/0, start_link/0, stop/0, version/0]).
-compile(export_all).
-define(COPYRIGHT_MESSSAGE,"Copyright (C) 2009-2013 Ctyle Corporation.").
-define(INFO_MESSAGE,"Licensed under the Commerical Usage. More info, please read at http://www.ctyle.com/legal").
-define(ABOUT_CLOUDPANE,"Welcome to visit cloudpane.com to get more help.").
-define(CP_VERSION,"Cloudpane 2.0.0").
version() ->
    '{"Cloudpane Lite":{"version":"1.0.0"}}'.

print_copyrights() ->
    io:format("~n"
        "Cloudpane <Version 2.0.0>~n"
        "DB Adapter  : [X] PostgreSQL Edition~n"
        "App Engine  : [ ] None~n"
        "Cloudpane MQ: [ ] None~n"
        "       .__                   .___                           ~n"
        "  ____ |  |   ____  __ __  __| _/__________    ____   ____   ~n"
        "_/ ___\\|  |  /  _ \\|  |  \\/ __ |\\____ \\__  \\  /    \\_/ __ \\  ~n"
        "\\  \\___|  |_(  <_> )  |  / /_/ ||  |_> > __ \\|   |  \\  ___/ ~n"
        " \\___  >____/\\____/|____/\\____ ||   __(____  /___|  /\\___  >~n"
        "     \\/                       \\/|__|       \\/     \\/     \\/ ~n"
        "~n"
        "~p~n~p~n~p~n~n",[?COPYRIGHT_MESSSAGE,?INFO_MESSAGE,?ABOUT_CLOUDPANE]),
    "".


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    %code:add_patha("deps/*/ebin"),
    print_copyrights(),
    ensure_started(inets),
    ensure_started(crypto),
    %cp_mnesia:start(),

    %application:set_env(mnesia,dir,"db/development"),

    ensure_started(mochiweb),
   
    ensure_started(webmachine),
    application:set_env(webmachine,server_name,?CP_VERSION),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    cloudpane_sup:start_link().

%% @spec start() -> ok
%% @doc Start the cloudpane server.
start() ->
    print_copyrights(),
    ensure_started(inets),
    ensure_started(crypto),
    %%ensure_started(mnesia),
    %%change to advance mnesia start function
    %cp_mnesia:start(),

    ensure_started(mochiweb),
    application:set_env(webmachine,server_name,?CP_VERSION),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(cloudpane).

%% @spec stop() -> ok
%% @doc Stop the cloudpane server.
stop() ->
    Res = application:stop(cloudpane),
    application:stop(webmachine),
    application:stop(mochiweb),
 
    %%application:stop(mnesia),
    %%change to advance mnesia stop function
    %cp_mnesia:stop(),
    application:stop(crypto),
    application:stop(inets),
    Res.

start_debug() ->
    wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp").

open_log() ->
    webmachine_log:add_handler(webmachine_perf_log_handler, ["log"]).

close_log() ->
    webmachine_log:delete_handler(webmachine_perf_log_handler).
