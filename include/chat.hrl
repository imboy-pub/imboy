
% syn scope name
-define(CHAT_SCOPE, imboy_chat).
-define(ROOM_SCOPE, imboy_room).
-define(CACHE_SCOPE, imboy_cache).
%% PR-2γ: QR 扫码登录会话推送 scope（独立 scope，不污染 ?CHAT_SCOPE）
%% 由 qr_login_event_ds:subscribe/unsubscribe/notify 使用，PR-3 SSE handler 监听
-define(QR_LOGIN_SCOPE, imboy_qr_login).

% 群成员系统限制
-define (GROUP_MEMBER_LIMIT, 10000).

% 存储消息调试显示
-define (SAVE_MSG_LIMIT, 5000).

% 用户聊天状态
-type user_chat_state() :: online | offline | hide |
    % <<"online">> | <<"offline">> | <<"hide">>.
    binary().
