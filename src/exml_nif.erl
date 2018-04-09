%%%-------------------------------------------------------------------
%%% @author Konrad Zemek <konrad.zemek@erlang-solutions.com>
%%% @copyright (C) 2018, Erlang Solutions Ltd.
%%%-------------------------------------------------------------------

-module(exml_nif).

-include("exml.hrl").
-include("exml_stream.hrl").

-type parser() :: term().
-type stream_element() :: exml:element() | exml_stream:start() | exml_stream:stop().
-type continue_term() :: {Data :: binary(), Offset :: non_neg_integer()}.

-export([create/2, parse/1, parse_next/2, escape_cdata/1,
         to_binary/2, reset_parser/1]).
-export_type([parser/0, continue_term/0]).

-on_load(load/0).

%%%===================================================================
%%% Public API
%%%===================================================================

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

-spec create(MaxChildSize :: non_neg_integer(), InfiniteStream :: boolean()) ->
                    {ok, parser()} | {error, Reason :: any()}.
create(_, _) ->
    erlang:nif_error(not_loaded).

-spec escape_cdata(Bin :: iodata()) -> binary().
escape_cdata(_Bin) ->
     erlang:nif_error(not_loaded).

-spec to_binary(Elem :: exml:element(), pretty | not_pretty) -> binary().
to_binary(_Elem, _Pretty) ->
    erlang:nif_error(not_loaded).

-spec parse(Bin :: iodata()) -> {ok, exml:element()} | {error, Reason :: any()}.
parse(_) ->
    erlang:nif_error(not_loaded).

-spec parse_next(parser(), Data :: iodata() | continue_term()) ->
                        {ok, stream_element() | undefined, continue_term() | parsed_all} |
                        {error, Reason :: any()}.
parse_next(_, _) ->
    erlang:nif_error(not_loaded).

-spec reset_parser(parser()) -> any().
reset_parser(_) ->
    erlang:nif_error(not_loaded).
