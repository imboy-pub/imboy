 % Key 必须都为256比特，也就是32字节
-define(AES_KEY, "eadfghkl;'][poi?eadfghkl;'][poi?").
 % IV 必须都为128比特，也就是16字节
-define(AES_IV, "bsefg;'][poi?hkl").
% refreshtoken有效期 2小时 单位毫秒 7200000 = 3600 * 1000 * 2
-define(TOKEN_VALID, 7200000).
% refreshtoken有效期 10天 单位毫秒 864000000 = 86400 * 1000 * 10
-define(REFRESHTOKEN_VALID, 864000000).

% 群成员系统限制
-define (GROUP_MEMBER_LIMIT, 10000).

% 离线消息限制
-define (OFFLINE_MSG_LIMIT, 1000).

% 用户聊天状态
-type user_chat_state() :: online | offline | hide.

-define(debug, ok).
-ifdef(debug).
-define(LOG(X), io:format("pid:~p , {~p,~p}: ~p~n", [self(), ?MODULE, ?LINE, X])).
-else.
-define(LOG(X), true).
-endif.
