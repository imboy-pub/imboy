
% syn scope name
-define(CHAT_SCOPE, imboy_chat).
-define(ROOM_SCOPE, imboy_room).
-define(CACHE_SCOPE, imboy_cache).

% 群成员系统限制
-define (GROUP_MEMBER_LIMIT, 10000).

% 存储消息调试显示
-define (SAVE_MSG_LIMIT, 5000).

%%% 消息重试间隔配置（单位：毫秒）
%%% 默认策略：0ms立即投递 -> 5s -> 7s -> 11s -> 17s停止
-define(MSG_RETRY_DELAYS_C2C, [0, 5000, 7000, 11000, 17000]).
-define(MSG_RETRY_DELAYS_C2G, [0, 3500, 7000, 11000, 17000]).
-define(MSG_RETRY_DELAYS_C2S, [0, 5000, 7000, 11000]).
-define(MSG_RETRY_DELAYS_PULL, [0, 10000, 20000]).
-define(MSG_RETRY_DELAYS_NOTICE, [0, 5000, 10000]).

% 用户聊天状态
-type user_chat_state() :: online | offline | hide |
    % <<"online">> | <<"offline">> | <<"hide">>.
    binary().
