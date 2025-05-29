
-define(DEBUG(Msg), lager:debug(Msg)).

-define(debug, ok).
-ifdef(debug).

-define(LOG(X), io:format("pid:~p , {~p,~p}: ~p~n~n~n~n~n", [self(), ?MODULE, ?LINE, X])).

%% 定义支持一个参数或两个参数的 DEBUG_LOG 宏 (重命名为 DEBUG_LOG 避免冲突)
-define(DEBUG_LOG(Msg), imboy_log:internal_log(debug, Msg, ?MODULE, ?LINE)).
-define(DEBUG_LOG(Format, Args), imboy_log:internal_log(debug, Format, Args, ?MODULE, ?LINE)).

-else.

-define(LOG(X), true).

-define(DEBUG_LOG(Msg), true).
-define(DEBUG_LOG(Format, Args), true).

-endif.
