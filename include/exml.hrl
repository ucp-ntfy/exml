-ifndef(EXML_HEADER).
-define(EXML_HEADER, true).

-record(xmlcdata, {content = [] :: iodata()}).

-record(xmlel, {name :: binary(),
                attrs = [] :: [exml:xmlattr()],
                children =  [] :: [exml:xmlel() | exml:xmlcdata()]}).

-endif.
