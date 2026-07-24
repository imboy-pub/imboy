% syn scope name
-define(CHAT_SCOPE, imboy_chat).
-define(ROOM_SCOPE, imboy_room).
-define(CACHE_SCOPE, imboy_cache).
%% PR-2γ: QR 扫码登录会话推送 scope（独立 scope，不污染 ?CHAT_SCOPE）
%% 由 qr_login_event_ds:subscribe/unsubscribe/notify 使用，PR-3 SSE handler 监听
-define(QR_LOGIN_SCOPE, imboy_qr_login).

% 群成员系统限制
-define(GROUP_MEMBER_LIMIT, 10000).

% 存储消息调试显示
-define(SAVE_MSG_LIMIT, 5000).

%% S0-1 消息信封版本号（架构保险）
%% 出站信封统一打上此版本；入站缺省视为当前版本，旧客户端无 ver 字段时向后兼容。
%% 演进规则：协议不兼容变更时递增；仅增字段不递增（接收方按缺省处理未知字段）。
-define(CUR_MSG_VER, 2).

% 用户聊天状态
-type user_chat_state() ::
    online
    | offline
    | hide
    % <<"online">> | <<"offline">> | <<"hide">>.
    | binary().
