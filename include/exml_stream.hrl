-include("exml.hrl").

-record(xmlstreamstart, {name :: iodata(),
                         attrs = [] :: [exml:attr()]}).

-record(xmlstreamend, {name :: iodata()}).
