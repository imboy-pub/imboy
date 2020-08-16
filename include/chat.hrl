% 群成员系统限制
-define (GROUP_MEMBER_LIMIT, 10000).

% 存储消息调试显示
-define (SAVE_MSG_LIMIT, 5000).

% 用户聊天状态
-type user_chat_state() :: online | offline | hide.
