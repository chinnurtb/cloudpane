-module(cp_crypto).
-compile(export_all).

%%pretty output md5

md5(Str)
    when is_list(Str)   ->  ?MODULE:md5(iolist_to_binary(Str));
md5(BinStr)         ->  <<N:128>> = erlang:md5(BinStr),
                            [Md5] = io_lib:format("~.16b",[N]),
                            Md5.