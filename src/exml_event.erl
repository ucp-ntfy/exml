%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Event-based XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(exml_event).

-include("exml_event.hrl").

-export([load/0]).
-export([new_parser/0,
         new_parser/1,
         new_parser/2,
         reset_parser/1,
         free_parser/1,
         parse/2,
         parse_final/2]).

-export_type([c_parser/0, event/0]).

-opaque c_parser() :: binary().
-type xmlns() :: {URI :: binary(), Prefix :: none | binary()}.
-type attr() :: {Key :: binary(), Value :: binary()}.
-type event() ::
    {xml_element_start, Name :: binary(), [xmlns()], [attr()]} |
    {xml_element_end, Name :: binary()} |
    {xml_cdata, Data :: binary()} |
    {error, Msg :: string()} |
    xml_stream_reset.

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
    erlang:load_nif(filename:join(PrivDir, "exml_event"), none).

-spec new_parser() -> {ok, c_parser()}.
new_parser() ->
    new_parser(0).

-spec new_parser(MaxChildSize :: non_neg_integer()) -> {ok, c_parser()}.
new_parser(_MaxChildSize) ->
    erlang:nif_error(not_loaded).

-spec new_parser(MaxChildSize :: non_neg_integer(), StartTag :: binary()) -> {ok, c_parser()}.
new_parser(_MaxChildSize, _StartTag) ->
    erlang:nif_error(not_loaded).

-spec reset_parser(c_parser()) -> ok.
reset_parser(Parser) ->
    erlang:nif_error(not_loaded, [Parser]).

-spec free_parser(c_parser()) -> ok.
free_parser(Parser) ->
    erlang:nif_error(not_loaded, [Parser]).

-spec parse(c_parser(), binary()) -> {ok, [event()]} | {error, string()}.
parse(Parser, Data) ->
    do_parse(Parser, Data, 0).

-spec parse_final(c_parser(), binary()) -> {ok, [event()]} | {error, string()}.
parse_final(Parser, Data) ->
    do_parse(Parser, Data, 1).

-spec do_parse(c_parser(), binary(), 0 | 1) -> {ok, [event()]} | {error, string()}.
do_parse(Parser, Data, Final) ->
    case parse_nif(Parser, Data, Final) of
        {ok, Res} -> {ok, lists:reverse(Res)};
        Error     -> Error
    end.

-spec parse_nif(c_parser(), binary(), integer()) -> {ok, [event()]} | {error, string()}.
parse_nif(Parser, Data, Final) ->
    erlang:nif_error(not_loaded, [Parser, Data, Final]).
