%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc XML stream parser
%%%
%%% @end
%%% Created : 21 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_stream).

-include("exml_stream.hrl").

-export([new_parser/0,
         new_parser/1,
         parse/2,
         reset_parser/1,
         free_parser/1]).

-export_type([element/0,
              parser/0,
              parser_opt/0]).

-record(parser, {event_parser :: exml_nif:parser(), buffer :: binary()}).

-type element() :: exml_nif:stream_element().
-type parser() :: #parser{}.
%% infinite_stream - no distinct "stream start" or "stream end", only #xmlel{} will be returned
%% max_child_size - specifies maximum byte size of any child of the root element. The byte size is
%%                  counted from the start tag until the opening character of its end tag. Disabled
%%                  if set to 0 (default).
-type parser_opt() :: {infinite_stream, boolean()} | {max_child_size, non_neg_integer()}.

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new_parser() -> {ok, parser()} | {error, any()}.
new_parser() ->
    new_parser([]).

-spec new_parser([parser_opt()]) -> {ok, parser()} | {error, any()}.
new_parser(Opts)->
    MaxChildSize = proplists:get_value(max_child_size, Opts, 0),
    InfiniteStream = proplists:get_value(infinite_stream, Opts, false),
    case exml_nif:create(MaxChildSize, InfiniteStream) of
        {ok, EventParser} ->
            {ok, #parser{event_parser = EventParser, buffer = <<>>}};
        Error ->
            Error
    end.

-spec parse(parser(), binary()) -> {ok, parser(), [exml_stream:element()]} | {error, Reason :: any()}.
parse(Parser, Input) when is_binary(Input) ->
    #parser{event_parser = EventParser, buffer = OldBuf} = Parser,
    Buffer = <<OldBuf/binary, Input/binary>>,
    case parse_all(EventParser, Buffer, []) of
        {ok, Elems, RestBuffer} ->
            {ok, Parser#parser{buffer = RestBuffer}, Elems};
        Other ->
            Other
    end.

-spec reset_parser(parser()) -> {ok, parser()}.
reset_parser(#parser{event_parser = NifParser} = Parser) ->
    exml_nif:reset_parser(NifParser),
    {ok, Parser#parser{buffer = <<>>}}.

-spec free_parser(parser()) -> ok.
free_parser(#parser{}) ->
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

parse_all(Parser, Buffer, Acc) ->
    case exml_nif:parse_next(Parser, Buffer) of
        {ok, undefined, {B, Offset}} ->
            {ok, lists:reverse(Acc), binary_part(B, Offset, byte_size(B) - Offset)};
        {ok, undefined, parsed_all} ->
            {ok, lists:reverse(Acc), <<>>};
        {ok, Element, parsed_all} ->
            {ok, lists:reverse([Element | Acc]), <<>>};
        {ok, Element, Continue} ->
            parse_all(Parser, Continue, [Element | Acc]);
        {error, _} = Error ->
            Error
    end.
