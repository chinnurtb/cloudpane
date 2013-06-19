%% @doc cloudpane static resources.

-module(static_resources).
-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2]).
-export([generate_etag/2]).
-export_all(compile).

-debug(true).

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
     etag = undefined,
     authorized = false,
     method = undefined,
     rest = false,
     params = undefined,
     result = undefined,
     error = null
}).

init([]) ->
    PrivDir = code:priv_dir(cloudpane),
    ?cinfo(PrivDir),
    case ?debugme of
        true ->
            {{trace, "/tmp"}, #context{docroot=PrivDir}};
        false ->
            {ok, #context{docroot=PrivDir}}
    end.

allowed_methods(ReqData, Context) ->
    ?cinfo(ok),
    {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
    ?cinfo(ok),
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Path = get_full_path(Ctx, wrq:disp_path(ReqData)),
    ?cinfo(Path),
    ?cinfo(webmachine_util:guess_mime(Path)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

generate_etag(ReqData, Context) -> 
    %ETag = wrq:raw_path(ReqData),
    %?cinfo(ETag),
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, NewContext} ->
            ETag = NewContext#context.etag,
        
            {ETag, ReqData, Context};
        {false, NewContext} ->
            {undefined, ReqData, NewContext}
    end.

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
                    HashValue = cp_crypto:md5(Value),
                    ?cinfo(HashValue),
                    {true, Context#context{response_body=Value,etag=HashValue}};
                false ->
                    ?cinfo(false),
                    {false, Context}
            end;
        _Body ->
            ?cinfo(body_true),
            {true, Context}
    end.

file_exists(Context, Path) ->
    FPath = get_full_path(Context, Path),
    ?cinfo(FPath),
    case filelib:is_regular(filename:absname(FPath)) of
        true ->
            ?cinfo(true),
            {true, FPath};
        false ->
            ?cinfo(false),
            false
    end.

get_full_path(Context, Path) ->
    Root = Context#context.docroot,
    ?cinfo({root,Root}),
    ?cinfo({safe_relative_path,mochiweb_util:safe_relative_path(Path)}),
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
