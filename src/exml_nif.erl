-module(exml_nif).

-export([create/0, create/1, parse/1, parse_next/2, escape_cdata/1,
         to_binary/2, reset_parser/1]).
-on_load(load/0).

-spec load() -> any().
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), none).

create() ->
    erlang:nif_error(not_loaded).

create(_) ->
    erlang:nif_error(not_loaded).

escape_cdata(_Bin) ->
     erlang:nif_error(not_loaded).

to_binary(_Bin, _Indent) ->
    erlang:nif_error(not_loaded).

parse(_) ->
    erlang:nif_error(not_loaded).

parse_next(_, _) ->
    erlang:nif_error(not_loaded).

reset_parser(_) ->
    erlang:nif_error(not_loaded).
