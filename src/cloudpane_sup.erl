%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the cloudpane application.

-module(cloudpane_sup).
-author('@zhangweizhong').
-behaviour(supervisor).
-include("cloudpane.hrl").
%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).
-compile(export_all).
%-debug(true).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->

  cloudpane:print_copyrights(),
  cloudpane:ensure_started(inets),
  cloudpane:ensure_started(crypto),

  
  %ensure_started(cdb),

  cloudpane:ensure_started(mochiweb),

  application:set_env(webmachine,server_name, "Cloudpane"),
  application:set_env(webmachine, webmachine_logger_module, 
                      webmachine_logger),
  cloudpane:ensure_started(webmachine),
  cdb:start(),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "127.0.0.1"; Any -> Any end,
    {ok, App} = application:get_application(?MODULE),
    %?cinfo(App), 
    %%sometime App = undefined, -_-!
    {ok, Dispatch} = file:consult(filename:join([priv_dir(App), "dispatch.conf"])),
    Port = case os:getenv("WEBMACHINE_PORT") of
            false -> 8000;
            AnyPort -> AnyPort
          end,
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {
            webmachine_mochiweb,
            {webmachine_mochiweb, start, [WebConfig]},
            permanent, 5000, worker, [mochiweb_socket_server]
          },
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.

%%
%% @doc return the priv dir
%% @fixme something wrong here in windows
priv_dir(Mod) ->
  %?cinfo(Mod),
  case code:priv_dir(Mod) of

    {error, bad_name} ->
      Ebin = filename:dirname(code:which(Mod)),
      filename:join(filename:dirname(Ebin), "priv");
    PrivDir ->
      %?cinfo(PrivDir),
      PrivDir
  end.
