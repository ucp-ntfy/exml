%%%-------------------------------------------------------------------
%%% @author Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%% @copyright (C) 2017, Erlang Solutions Ltd.
%%% @doc
%%%
%%% This module contains utilities that are not part of the core
%%% functionality provided by this application.
%%%
%%% @end
%%% Created : 4 Aug 2017 by Szymon Mentel <szymon.mentel@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_utils).

-include_lib("exml/include/exml_stream.hrl").

-type xmlstreamelement() :: #xmlel{} | #xmlstreamstart{} | #xmlstreamend{}.

-export([xml_sort/1]).

%% @doc Sort 'xmlel()` children and attributes.
xml_sort({xmlcdata, _} = CData) -> CData;
xml_sort(#xmlstreamstart{attrs = Attrs} = StreamStart) ->
    StreamStart#xmlstreamstart{attrs = lists:sort(Attrs)};
xml_sort(#xmlel{} = El) ->
    #xmlel{attrs = Attrs, children = Children} = El,
    El#xmlel{attrs = lists:sort(Attrs),
             children = [ xml_sort(C) || C <- Children ]};
xml_sort({xml_element_start, Name, NSs, Attrs} = _ElementStart) ->
    {xml_element_start, Name, lists:sort(NSs), lists:sort(Attrs)};
xml_sort({xmlstreamend, _} = StreamEnd) ->
    StreamEnd;
xml_sort(Elements) when is_list(Elements) ->
    [ xml_sort(E) || E <- Elements ].
