%% @doc cloudpane static resources.

-module(static_resources).
-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2]).
%-export([generate_etag/2]).
-export_all(compile).

-debug(false).

-include("cloudpane.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {
     docroot=undefined,
     fullpath=undefined,
     fileinfo=undefined,
     response_body=undefined,
     session = undefined,
     id=0,
     authorized = false,
     method = undefined,
     rest = false,
     params = undefined,
     result = undefined,
     error = null
}).

init([]) ->
    %%{ok, App}= application:get_application(), 
    %?info2(init,{privDir,PrivDir}),
    PrivDir = code:priv_dir(cloudpane),
    {{trace, "/tmp"}, #context{docroot=PrivDir}}.
    %{ok, #context{docroot=PrivDir}}.

allowed_methods(ReqData, Context) ->
    ?cinfo(ok),
    {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
    ?cinfo(ok),
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Path = get_full_path(Ctx, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

% generate_etag(ReqData, Context) -> 
%     ETag = wrq:raw_path(ReqData),
%     io:format("cloudpane_resource_static:generate_etag Etag~p~n",[ETag]),
%     {ETag, ReqData, Context}.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body, ReqData, Context};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case file_exists(Context, Path) of
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Context#context{response_body=Value}};
                false ->
                    {false, Context}
            end;
        _Body ->
            {true, Context}
    end.

file_exists(Context, Path) ->
    FPath = get_full_path(Context, Path),
    case filelib:is_regular(filename:absname(FPath)) of
        true ->
            {true, FPath};
        false ->
            false
    end.

get_full_path(Context, Path) ->
    Root = Context#context.docroot,
    %?cinfo({root,Root}),
    %?cinfo({safe_relative_path,mochiweb_util:safe_relative_path(Path)}),
    case mochiweb_util:safe_relative_path(Path) of
        undefined -> undefined;
        RelPath ->
            FullPath = filename:join([Root, RelPath]),
            ?cinfo({fullpath,FullPath}),
            case filelib:is_dir(FullPath) of
                true ->
                    filename:join([FullPath, "index.html"]);
                false ->
                    FullPath
            end
    end.
