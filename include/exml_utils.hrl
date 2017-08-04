%% See assertEqual in $ERLANG/lib/stdlib-2.6/include/assert.hrl for the original.
-define(exmlAssertEqual(Example, Expr),
        begin
            ((fun (__X, __V) ->
                      case __V of
                          __X -> ok;
                          __V -> erlang:error({exmlAssertEqual,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {expected, __X},
                                                {value, __V}]})
                      end
              end)(exml_utils:xml_sort((Example)), exml_utils:xml_sort((Expr))))
        end).
