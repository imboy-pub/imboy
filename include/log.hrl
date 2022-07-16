
-define(DEBUG(Msg), lager:debug(Msg)).

-define(debug, ok).
-ifdef(debug).
-define(LOG(X), io:format("pid:~p , {~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-else.
-define(LOG(X), true).
-endif.
