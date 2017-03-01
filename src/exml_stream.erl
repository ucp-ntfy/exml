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

-export_type([start/0,
              stop/0,
              element/0,
              parser/0,
              parser_opt/0]).

-record(config, {infinite_stream :: boolean(),
                 autoreset :: boolean(),
                 start_tag :: undefined | binary()}).

-record(parser, {event_parser :: exml_event:c_parser(),
                 config :: parser_cfg(),
                 stack = [] :: list()}).

-type start() :: #xmlstreamstart{}.
-type stop() :: #xmlstreamend{}.
-type element() :: exml:element() | start() | stop().
-type parser_cfg() :: #config{}.
-type parser() :: #parser{}.
%% infinite_stream - no distinct "stream start" or "stream end", only #xmlel{} will be returned
%% autoreset - will reset expat after each parsed document
%%             use only when complete xml document is sent to the parser
%%             for example XMPP over WebSocekts - http://tools.ietf.org/html/draft-ietf-xmpp-websocket
%% start_tag - if set, the stream will reset its state after encountering the start_tag, de-facto
%%             resetting the whole stream and notifying the caller of new xmlstreamstart. Any
%%             unclosed tags that are not the start_tag will be treated as an error. Has no effect
%%             if infinite_stream is true.
-type parser_property() :: infinite_stream | autoreset.
-type parser_opt() :: {parser_property(), boolean()} | {start_tag, undefined | binary()}.

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new_parser() -> {ok, parser()} | {error, any()}.
new_parser() ->
    new_parser([]).

-spec new_parser([parser_opt()]) -> {ok, parser()} | {error, any()}.
new_parser(Opts)->
    try
        StartTag = proplists:get_value(start_tag, Opts),
        {ok, EventParser} =
            case StartTag of
                undefined -> exml_event:new_parser();
                _ -> exml_event:new_parser(StartTag)
            end,

        {ok, #parser{
            event_parser = EventParser,
            config = #config{
                        infinite_stream = bool_opt(infinite_stream, Opts, false),
                        autoreset = bool_opt(autoreset, Opts, false),
                        start_tag = StartTag
                        }}}
    catch
        E:R -> {error, {E, R}}
    end.

-spec parse(parser(), binary()) -> {ok, parser(), [exml_stream:element()]} | {error, Reason :: any()}.
parse(Parser, Input) ->
    #parser{event_parser = EventParser, stack = OldStack, config = Config} = Parser,
    #config{autoreset = Autoreset, infinite_stream = InfiniteStream, start_tag = StartTag} = Config,
    case exml_event:parse(EventParser, Input) of
        {error, Reason} -> {error, Reason};
        {ok, Events} ->
            case parse_events(Events, OldStack, InfiniteStream, StartTag) of
                {error, Reason} -> {error, Reason};
                {[], Elements} when Autoreset =:= true ->
                    {ok, NewParser} = reset_parser(Parser),
                    {ok, NewParser, lists:reverse(Elements)};
                {NewStack, Elements} ->
                    {ok, Parser#parser{stack = NewStack}, lists:reverse(Elements)}
            end
    end.

-spec reset_parser(parser()) -> {ok, parser()} | {error, any()}.
reset_parser(#parser{event_parser = EventParser, config = Config}) ->
    try
        exml_event:reset_parser(EventParser),
        %% drop all the state except event_parser
        {ok, #parser{event_parser = EventParser, config = Config}}
    catch
        E:R ->
            {error, {E, R}}
    end.

-spec free_parser(parser()) -> ok | {error, any()}.
free_parser(#parser{event_parser = EventParser}) ->
    exml_event:free_parser(EventParser).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec parse_events([exml_event:event()], [exml_stream:element()], boolean(), undefined | binary()) ->
        {[exml_stream:element()], [exml_stream:element()]} | {error, Reason :: any()}.
parse_events(Events, Stack, InfiniteStream, StartTag) ->
    ParseEvent =
        fun
            (_, {error, Reason}) -> {error, Reason};
            (Event, {St, Acc}) -> parse_event(Event, St, Acc, InfiniteStream, StartTag)
        end,
    lists:foldl(ParseEvent, {Stack, []}, Events).

-spec parse_event(exml_event:event(), [exml_stream:element()], [exml_stream:element()],
                  boolean(), undefined | binary()) ->
        {[exml_stream:element()], [exml_stream:element()]} | {error, Reason :: any()}.
parse_event({xml_element_start, Name, NSs, Attrs}, [], Acc, false, StartTag)
  when StartTag =:= Name; StartTag =:= undefined ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    NewStack = [#xmlel{name = Name, attrs = NewAttrs}],
    {NewStack, [#xmlstreamstart{name = Name, attrs = NewAttrs} | Acc]};
parse_event({xml_element_start, Name, _, _}, [], _Acc, false, _StartTag) ->
    {error, "invalid start tag <" ++ unicode:characters_to_list(Name) ++ ">"};
parse_event(xml_stream_reset, [_], Acc, false, _StartTag) ->
    {[], Acc};
parse_event(xml_stream_reset, Stack, _Acc, false, _StartTag) ->
    [#xmlel{name = Name} | _] = Stack,
    {error, "unclosed tag on stream restart: <" ++ unicode:characters_to_list(Name) ++ ">"};
parse_event({xml_element_start, Name, NSs, Attrs}, Stack, Acc, _InfiniteStream, _StartTag) ->
    NewAttrs = nss_to_fake_attrs(NSs, []) ++ Attrs,
    {[#xmlel{name = Name, attrs = NewAttrs} | Stack], Acc};
parse_event({xml_element_end, Name}, [#xmlel{name = Name}], Acc, false, _StartTag) ->
    {[], [#xmlstreamend{name = Name} | Acc]};
parse_event({xml_element_end, Name}, [#xmlel{name = Name} = Element], Acc, true, _StartTag) ->
    {[], [xml_element(Element) | Acc]};
parse_event({xml_element_end, Name}, [#xmlel{name = Name} = Element, Top], Acc, false, _StartTag) ->
    {[Top], [xml_element(Element) | Acc]};
parse_event({xml_element_end, _}, [Element, Parent | Stack], Acc, _InfiniteStream, _StartTag) ->
    NewElement = Element#xmlel{children = lists:reverse(Element#xmlel.children)},
    NewParent = Parent#xmlel{children = [NewElement | Parent#xmlel.children]},
    {[NewParent | Stack], Acc};
parse_event({xml_cdata, _CData}, [Top], Acc, false, _StartTag) ->
    {[Top], Acc};
parse_event({xml_cdata, CData},
             [#xmlel{children = [#xmlcdata{content = Content} | RestChildren]} = XML | Stack],
             Acc, _InfiniteStream, _StartTag) ->
    NewChildren = [#xmlcdata{content = list_to_binary([Content, CData])} | RestChildren],
    {[XML#xmlel{children = NewChildren} | Stack], Acc};
parse_event({xml_cdata, CData}, [Element | Stack], Acc, _InfiniteStream, _StartTag) ->
    NewChildren = [#xmlcdata{content = CData} | Element#xmlel.children],
    {[Element#xmlel{children = NewChildren} | Stack], Acc};
parse_event({error, Reason}, Stack, Acc, _InfiniteStream, _StartTag) ->
    {Stack, [{error, Reason} | Acc]}.

-spec xml_element(exml:element()) -> exml:element().
xml_element(#xmlel{children = Children} = Element) ->
    Element#xmlel{children = lists:reverse(Children)}.

-spec nss_to_fake_attrs([{binary(), binary() | none}], [exml:attr()]) ->
        [exml:attr()].
nss_to_fake_attrs([{Uri, none} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns">>, Uri} | Acc]);
nss_to_fake_attrs([{Uri, Prefix} | Rest], Acc) ->
    nss_to_fake_attrs(Rest, [{<<"xmlns:", Prefix/binary>>, Uri} | Acc]);
nss_to_fake_attrs([], Acc) ->
    Acc. %% no lists:reverse, as we got the argument list in reversed order


-spec bool_opt(parser_property(), [parser_opt()], boolean()) -> boolean().
bool_opt(Val, Opts, Default) ->
    Got = proplists:get_value(Val, Opts, Default),
    case is_boolean(Got) of
        true -> Got;
        false -> error({invalid_parser_opt, {Val, Got}})
    end.
