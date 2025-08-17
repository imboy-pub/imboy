
% syn scope name
-define(CHAT_SCOPE, imboy_chat).
-define(ROOM_SCOPE, imboy_room).
-define(CACHE_SCOPE, imboy_cache).

% 群成员系统限制
-define (GROUP_MEMBER_LIMIT, 10000).

% 存储消息调试显示
-define (SAVE_MSG_LIMIT, 5000).

% 用户聊天状态
-type user_chat_state() :: online | offline | hide |
    % <<"online">> | <<"offline">> | <<"hide">>.
    binary().
