-ifndef(EXML_HEADER).
-define(EXML_HEADER, true).

-record(xmlcdata, {content = [] :: iodata()}).

-record(xmlel, {name :: iodata(),
                attrs = [] :: [exml:attr()],
                children =  [] :: [exml:element() | exml:cdata()]}).

-endif.
