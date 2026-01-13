%%%-------------------------------------------------------------------
%%% @doc
%%% 全局常量定义文件
%%%
%%% 本文件定义了项目中使用的全局常量，用于替代代码中的魔法数字和字符串。
%%% 提高代码可维护性，遵循 DRY 原则。
%%%
%%% 合并自:
%%%   - constants.hrl (业务常量)
%%%   - def_const.hrl (技术常量)
%%%
%%% @author Imboy Team
%%% @copyright 2026 Imboy Project
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% API 相关常量
%% ===================================================================

%% @doc API 版本
-define(API_VERSION, <<"v1">>).

%% @doc HTTP 状态码
-define(HTTP_OK, 200).
-define(HTTP_CREATED, 201).
-define(HTTP_BAD_REQUEST, 400).
-define(HTTP_UNAUTHORIZED, 401).
-define(HTTP_FORBIDDEN, 403).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_INTERNAL_ERROR, 500).


%% ===================================================================
%% 分页相关常量
%% ===================================================================

%% @doc 默认分页大小
-define(DEFAULT_PAGE_SIZE, 20).

%% @doc 最大分页大小（防止过大的查询）
-define(MAX_PAGE_SIZE, 1000).

%% @doc 分页偏移量（从第 1 页开始）
-define(PAGE_OFFSET, 1).

%% @doc 分页大小限制
-define(PAGE_SIZE_MIN, 1).
-define(PAGE_SIZE_DEFAULT, 20).
-define(PAGE_SIZE_MAX, 100).


%% ===================================================================
%% 缓存相关常量
%% ===================================================================

%% @doc 短期缓存 TTL（5 分钟）
-define(CACHE_TTL_SHORT, 300).

%% @doc 中期缓存 TTL（1 小时）
-define(CACHE_TTL_MEDIUM, 3600).

%% @doc 长期缓存 TTL（1 天）
-define(CACHE_TTL_LONG, 86400).

%% @doc 缓存 1 周
-define(CACHE_TTL_WEEK, 604800).

%% @doc 验证码缓存 TTL（10 分钟）
-define(CACHE_TTL_VERIFICATION, 600).


%% ===================================================================
%% WebSocket 相关常量
%% ===================================================================

%% @doc WebSocket 空闲超时时间（毫秒）
-define(WS_IDLE_TIMEOUT, 128000).

%% @doc WebSocket 消息最大大小（字节）
-define(WS_MAX_FRAME_SIZE, 16#100000).  % 16MB


%% ===================================================================
%% 异步执行相关常量
%% ===================================================================

%% @doc 默认异步任务超时时间（毫秒）
-define(DEFAULT_ASYNC_TIMEOUT, 5000).

%% @doc 默认重试延迟（毫秒）
-define(DEFAULT_RETRY_DELAY, 1000).

%% @doc 默认重试次数
-define(DEFAULT_RETRY_COUNT, 3).


%% ===================================================================
%% 时间相关常量
%% ===================================================================

%% @doc 时间单位（秒）
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(SECONDS_PER_WEEK, 604800).

%% @doc 时间单位（毫秒）
-define(MILLISECONDS_PER_SECOND, 1000).

%% @doc 消息重试间隔（毫秒）
-define(MSG_RETRY_INTERVAL_1, 2000).  % 2秒
-define(MSG_RETRY_INTERVAL_2, 5000).  % 5秒
-define(MSG_RETRY_INTERVAL_3, 7000).  % 7秒
-define(MSG_RETRY_INTERVAL_4, 11000). % 11秒
-define(MSG_RETRY_MAX_TIMES, 4).


%% ===================================================================
%% 认证相关常量
%% ===================================================================

%% @doc 登录类型
-define(TYPE_EMAIL, <<"email">>).
-define(TYPE_MOBILE, <<"mobile">>).
-define(TYPE_ACCOUNT, <<"account">>).
-define(TYPE_SMS, <<"sms">>).

%% @doc RSA 加密标志
-define(RSA_ENCRYPT_YES, <<"1">>).
-define(RSA_ENCRYPT_NO, <<"0">>).

%% @doc Token 有效期（秒）
-define(TOKEN_VALID, 86400).           % 访问令牌：1 天
-define(REFRESHTOKEN_VALID, 2592000).  % 刷新令牌：30 天

%% @doc Token 时钟偏差容错（秒）
-define(TOKEN_LEEWAY, 300).  % 5 分钟

%% @doc Token 刷新时间（秒）
-define(TOKEN_REFRESH_DEADLINE, 8).


%% ===================================================================
%% 验证码相关常量
%% ===================================================================

%% @doc 验证码场景
-define(SCENE_SIGNUP, <<"signup">>).
-define(SCENE_FORGOT_PWD, <<"forgot_pwd">>).
-define(SCENE_BIND_MAIL, <<"bind_mail">>).
-define(SCENE_BIND_MOBILE, <<"bind_mobile">>).

%% @doc 验证码有效期（秒）
-define(VERIFICATION_CODE_VALID, 300).  % 5 分钟

%% @doc 验证码长度
-define(VERIFICATION_CODE_LENGTH, 6).


%% ===================================================================
%% 用户相关常量
%% ===================================================================

%% @doc 用户状态
-define(USER_STATUS_DELETED, -1).   % 已删除
-define(USER_STATUS_DISABLED, 0).   % 已禁用
-define(USER_STATUS_ACTIVE, 1).     % 正常

%% @doc 用户在线状态（整数）
-define(USER_STATUS_ONLINE, 1).
-define(USER_STATUS_OFFLINE, 0).
-define(USER_STATUS_HIDDEN, 2).

%% @doc 用户在线状态（字符串）
-define(USER_ONLINE_STATUS_ONLINE, <<"online">>).
-define(USER_ONLINE_STATUS_OFFLINE, <<"offline">>).
-define(USER_ONLINE_STATUS_HIDDEN, <<"hidden">>).

%% @doc 用户名称长度限制
-define(USER_NICKNAME_MIN_LENGTH, 1).
-define(USER_NICKNAME_MAX_LENGTH, 50).
-define(NICKNAME_MAX_LEN, 50).

%% @doc 密码长度限制
-define(PASSWORD_MIN_LENGTH, 6).
-define(PASSWORD_MAX_LENGTH, 50).


%% ===================================================================
%% 好友相关常量
%% ===================================================================

%% @doc 好友状态
-define(FRIEND_STATUS_PENDING, 0).   % 待确认
-define(FRIEND_STATUS_ACCEPTED, 1).  % 已接受
-define(FRIEND_STATUS_BLOCKED, 2).   % 已拉黑


%% ===================================================================
%% 群组相关常量
%% ===================================================================

%% @doc 群组类型
-define(GROUP_TYPE_PUBLIC, 1).      % 公开群组
-define(GROUP_TYPE_PRIVATE, 2).     % 私有群组

%% @doc 群组加入限制
-define(GROUP_JOIN_LIMIT_ANYONE, 1).      % 任何人可加入
-define(GROUP_JOIN_LIMIT_APPROVAL, 2).    % 需要审批
-define(GROUP_JOIN_LIMIT_INVITE_ONLY, 3). % 仅邀请可加入

%% @doc 群成员角色（整数 - 用于数据库存储）
-define(GROUP_ROLE_MEMBER_INT, 1).    % 普通成员
-define(GROUP_ROLE_GUEST_INT, 2).     % 嘉宾
-define(GROUP_ROLE_ADMIN_INT, 3).     % 管理员
-define(GROUP_ROLE_OWNER_INT, 4).     % 群主

%% @doc 群成员角色（字符串 - 用于 API 和消息）
-define(GROUP_ROLE_MEMBER, <<"member">>).
-define(GROUP_ROLE_ADMIN, <<"admin">>).
-define(GROUP_ROLE_OWNER, <<"owner">>).

%% @doc 群组名称长度限制
-define(GROUP_NAME_MIN_LENGTH, 1).
-define(GROUP_NAME_MAX_LENGTH, 50).
-define(GROUP_NAME_MAX_LEN, 100).

%% @doc 群组最大成员数
-define(GROUP_MAX_MEMBERS, 500).
-define(GROUP_MAX_MEMBERS_LARGE, 5000).


%% ===================================================================
%% 消息相关常量
%% ===================================================================

%% @doc 消息方向
-define(MSG_TYPE_C2C, <<"C2C">>).     % 单聊
-define(MSG_TYPE_C2G, <<"C2G">>).     % 群聊
-define(MSG_TYPE_S2C, <<"S2C">>).     % 系统消息

%% @doc 消息状态
-define(MSG_STATUS_SENDING, 0).     % 发送中
-define(MSG_STATUS_SENT, 1).        % 已发送
-define(MSG_STATUS_DELIVERED, 2).   % 已送达
-define(MSG_STATUS_READ, 3).        % 已读
-define(MSG_STATUS_FAILED, 4).      % 发送失败

%% @doc 系统消息类型
-define(MSG_SYSTEM_GROUP_EDIT, <<"group_eidt">>).
-define(MSG_SYSTEM_GROUP_DISSOLVE, <<"group_dissolve">>).
-define(MSG_SYSTEM_FRIEND_ADD, <<"friend_add">>).


%% ===================================================================
%% 文件相关常量
%% ===================================================================

%% @doc 文件类型
-define(FILE_TYPE_IMAGE, <<"image">>).
-define(FILE_TYPE_VIDEO, <<"video">>).
-define(FILE_TYPE_AUDIO, <<"audio">>).
-define(FILE_TYPE_DOCUMENT, <<"document">>).
-define(FILE_TYPE_OTHER, <<"other">>).

%% @doc 文件大小限制（字节）
-define(FILE_SIZE_MAX, 100 * 1024 * 1024).  % 100MB
-define(IMAGE_SIZE_MAX, 10 * 1024 * 1024).  % 10MB

%% @doc 头像和各类型文件大小限制
-define(MAX_FILE_SIZE_AVATAR, 5 * 1024 * 1024).      % 头像：5MB
-define(MAX_FILE_SIZE_IMAGE, 10 * 1024 * 1024).      % 图片：10MB
-define(MAX_FILE_SIZE_VIDEO, 100 * 1024 * 1024).     % 视频：100MB
-define(MAX_FILE_SIZE_DOCUMENT, 50 * 1024 * 1024).   % 文档：50MB


%% ===================================================================
%% 限流相关常量
%% ===================================================================

%% @doc 限流策略名称
-define(THROTTLE_THREE_SECOND_ONCE, three_second_once).
-define(THROTTLE_ONE_MINUTE_TEN, one_minute_ten).
-define(THROTTLE_ONE_HOUR_HUNDRED, one_hour_hundred).
-define(THROTTLE_PER_MINUTE, per_minute_once).
-define(THROTTLE_PER_HOUR, per_hour_once).


%% ===================================================================
%% 位置服务相关常量
%% ===================================================================

%% @doc 默认附近人搜索半径（米）
-define(NEARBY_DEFAULT_RADIUS, 5000).

%% @doc 附近人搜索最大半径（米）
-define(NEARBY_MAX_RADIUS, 50000).

%% @doc 地球半径（米）
-define(EARTH_RADIUS, 6371000).


%% ===================================================================
%% 其他常量
%% ===================================================================

%% @doc 签名长度限制
-define(SIGN_MAX_LEN, 200).
