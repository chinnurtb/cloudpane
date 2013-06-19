-module(cdb_adapter).
-export([behaviour_info/1]).

%% @spec behaviour_info( atom() ) -> [ {Function::atom(), Arity::integer()} ] | undefined
behaviour_info(callbacks) ->
    [
        {start, 1}, {stop, 0}, {init, 1}, 

        {create_object,2},
        {destroy_object,2},
        {show_object,2},

        {patch_object,2},
        {update_object,2},
        
        {list_object,2},

        % {destory_log_object,2},
        % {show_log_object,2},
        % {list_log_object,2},

        {create_schema,2},
        {destroy_schema,2},
        {show_schema,2},

        {patch_schema,2},
        {list_schema,2},

        {add_column_schema,2},
        {del_column_schema,2},

        % {destroy_log_schema,2},
        % {show_log_schema,2},
        % {list_log_scehma,2},   

        {terminate, 1}
    ];
behaviour_info(_Other) ->
    undefined.
