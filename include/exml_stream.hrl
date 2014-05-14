-include("exml.hrl").

-record(xmlstreamstart, {name :: binary(),
                         attrs = [] :: [exml:xmlattr()]}).

-record(xmlstreamend, {name :: binary()}).
