# 阶段 2: 设备间传输

> **预计工期**: 5-7 天
> **依赖**: 阶段 1（准备工作）
> **安全等级**: ⭐⭐⭐⭐⭐（最安全）

---

## 目标

实现设备间直接传输私钥功能：
1. 后端：传输会话管理、路由转发
2. 前端：二维码生成/扫描、密钥导出/导入
3. 完整的传输流程测试

---

## 安全原则

```
┌────────────────────────────────────────────────────────────┐
│                      安全设计原则                           │
├────────────────────────────────────────────────────────────┤
│ 1. 服务器永不解密私钥 - 仅转发加密数据                      │
│ 2. 二维码短期有效（5 分钟）- 防止截获重放                   │
│ 3. 使用传输方公钥加密 - 只有新设备能解密                    │
│ 4. 会话状态跟踪 - 防止重复接受/确认                         │
│ 5. 设备绑定验证 - 确保是目标设备                            │
└────────────────────────────────────────────────────────────┘
```

---

## 传输流程

```
旧设备 (From)                  服务器                    新设备 (To)
     │                          │                          │
     │  1. 生成会话 ID           │                          │
     │  (UUID v4)               │                          │
     │                          │                          │
     │  2. 加密密钥包            │                          │
     │  (RSA-OAEP-256)          │                          │
     │                          │                          │
     │  3. 创建传输会话 ────────►│                          │
     │  POST /transfer/create    │                          │
     │                          │  存储:                    │
     │                          │  - session_id             │
     │                          │  - encrypted_key_bundle   │
     │                          │  - expires_at (5min)      │
     │                          │                          │
     │  4. 显示二维码             │                          │
     │  (session_id)             │                          │
     │                          │                          │
     │                          │          5. 扫描二维码     │
     │                          │◄─────────────────────────│
     │                          │  GET /transfer/info      │
     │                          │  ?session_id=xxx         │
     │                          │                          │
     │                          │  6. 返回会话信息 ────────►│
     │                          │  - from_uid               │
     │                          │  - encrypted_key_bundle   │
     │                          │                          │
     │                          │  7. 接受传输 ────────────►│
     │                          │  POST /transfer/accept    │
     │                          │                          │
     │                          │  更新状态: accepted       │
     │                          │                          │
     │  8. 等待确认              │      9. 解密私钥          │
     │                          │      10. 存储到安全存储    │
     │                          │                          │
     │                          │  11. 确认完成 ───────────►│
     │                          │  POST /transfer/confirm   │
     │                          │                          │
     │ ◄────────────────────────│                          │
     │  12. 更新状态: confirmed  │                          │
     │  13. 通知传输成功         │                          │
     │                          │                          │
```

---

## 步骤 1: 后端 - Repo 层

### 1.1 创建 Repo 文件

```bash
# 在后端项目目录执行
cd /Users/leeyi/project/imboy.pub/imboy

# 创建 repo 文件
vim src/repo/e2ee_transfer_repo.erl
```

### 1.2 Repo 实现代码

```erlang
-module(e2ee_transfer_repo).
%%%===================================================================
%%% @doc
%%% e2ee_transfer_repo - E2EE 设备传输数据仓库层
%%%
%%% 功能：
%%% - 传输会话的 CRUD 操作
%%% - 会话状态管理
%%% - 过期会话清理
%%%
%%% 使用示例：
%%% ```
%%% {ok, Session} = e2ee_transfer_repo:create(SessionMap),
%%% {ok, Session} = e2ee_transfer_repo:find_by_session_id(SessionId),
%%% ok = e2ee_transfer_repo:update_status(SessionId, <<"accepted">>),
%%% ```
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([create/1]).
-export([find_by_session_id/1]).
-export([find_pending_by_from_uid/1]).
-export([update_status/2]).
-export([update_to_device/2]).
-export([mark_confirmed/1]).
-export([delete_expired/0]).

%% 类型定义
-type transfer_session() :: #{
    id => integer(),
    session_id => binary(),
    from_uid => integer(),
    from_device_id => binary(),
    to_uid => integer(),
    to_device_id => binary(),
    status => binary(),
    encrypted_key_bundle => binary(),
    expires_at => binary(),
    created_at => binary(),
    confirmed_at => binary()
}.
-type transfer_result() :: {ok, transfer_session()} | {error, term()}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建传输会话
%% @param SessionMap 会话数据 Map
%% @returns {ok, Session} | {error, Reason}
-spec create(map()) -> transfer_result().
create(SessionMap) ->
    SessionId = maps:get(<<"session_id">>, SessionMap),
    FromUid = maps:get(<<"from_uid">>, SessionMap),
    FromDeviceId = maps:get(<<"from_device_id">>, SessionMap),
    ToUid = maps:get(<<"to_uid">>, SessionMap),
    EncryptedKeyBundle = maps:get(<<"encrypted_key_bundle">>, SessionMap),
    ExpiresAt = maps:get(<<"expires_at">>, SessionMap),

    Sql = <<"INSERT INTO e2ee_transfer_sessions (
            session_id, from_uid, from_device_id, to_uid,
            encrypted_key_bundle, expires_at, status
        ) VALUES ($1, $2, $3, $4, $5, $6, 'pending')
        RETURNING id, session_id, from_uid, from_device_id, to_uid,
                  to_device_id, status, encrypted_key_bundle,
                  expires_at, created_at, confirmed_at">>,

    case elib_pg:query(Sql, [
        SessionId, FromUid, FromDeviceId, ToUid,
        EncryptedKeyBundle, ExpiresAt
    ]) of
        {ok, _, [{Result}]} ->
            {ok, row_to_map(Result)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_repo, create_failed, Reason]),
            {error, Reason}
    end.

%% @doc 根据 session_id 查找传输会话
%% @param SessionId 会话 ID
%% @returns {ok, Session} | {error, not_found}
-spec find_by_session_id(binary()) -> transfer_result().
find_by_session_id(SessionId) ->
    Sql = <<"SELECT id, session_id, from_uid, from_device_id, to_uid,
                    to_device_id, status, encrypted_key_bundle,
                    expires_at, created_at, confirmed_at
             FROM e2ee_transfer_sessions
             WHERE session_id = $1">>,

    case elib_pg:query(Sql, [SessionId]) of
        {ok, _, [Result]} ->
            {ok, row_to_map(Result)};
        {ok, _, []} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_repo, find_failed, Reason]),
            {error, Reason}
    end.

%% @doc 查找用户的待处理传输会话
%% @param FromUid 发送方用户 ID
%% @returns {ok, [Session]}
-spec find_pending_by_from_uid(integer()) -> {ok, [transfer_session()]}.
find_pending_by_from_uid(FromUid) ->
    Sql = <<"SELECT id, session_id, from_uid, from_device_id, to_uid,
                    to_device_id, status, encrypted_key_bundle,
                    expires_at, created_at, confirmed_at
             FROM e2ee_transfer_sessions
             WHERE from_uid = $1 AND status = 'pending'
             ORDER BY created_at DESC">>,

    case elib_pg:query(Sql, [FromUid]) of
        {ok, _, Results} ->
            {ok, lists:map(fun row_to_map/1, Results)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_repo, find_pending_failed, Reason]),
            {error, Reason}
    end.

%% @doc 更新会话状态
%% @param SessionId 会话 ID
%% @param Status 新状态
%% @returns ok | {error, Reason}
-spec update_status(binary(), binary()) -> ok | {error, term()}.
update_status(SessionId, Status) ->
    Sql = <<"UPDATE e2ee_transfer_sessions
             SET status = $1
             WHERE session_id = $2">>,

    case elib_pg:query(Sql, [Status, SessionId]) of
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新接收方设备 ID
%% @param SessionId 会话 ID
%% @param ToDeviceId 接收方设备 ID
%% @returns ok | {error, Reason}
-spec update_to_device(binary(), binary()) -> ok | {error, term()}.
update_to_device(SessionId, ToDeviceId) ->
    Sql = <<"UPDATE e2ee_transfer_sessions
             SET to_device_id = $1
             WHERE session_id = $2">>,

    case elib_pg:query(Sql, [ToDeviceId, SessionId]) of
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 标记会话为已确认
%% @param SessionId 会话 ID
%% @returns ok | {error, Reason}
-spec mark_confirmed(binary()) -> ok | {error, term()}.
mark_confirmed(SessionId) ->
    Sql = <<"UPDATE e2ee_transfer_sessions
             SET status = 'confirmed', confirmed_at = CURRENT_TIMESTAMP
             WHERE session_id = $1">>,

    case elib_pg:query(Sql, [SessionId]) of
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 删除过期的传输会话
%% @returns {ok, DeletedCount}
-spec delete_expired() -> {ok, non_neg_integer()}.
delete_expired() ->
    Sql = <<"DELETE FROM e2ee_transfer_sessions
             WHERE expires_at < CURRENT_TIMESTAMP AND status != 'confirmed'
             RETURNING id">>,

    case elib_pg:query(Sql, []) of
        {ok, _, Results} ->
            {ok, length(Results)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_repo, delete_expired_failed, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 将数据库行转换为 Map
%% @private
-spec row_to_map(map()) -> transfer_session().
row_to_map(Row) ->
    #{
        id => maps:get(<<"id">>, Row),
        session_id => maps:get(<<"session_id">>, Row),
        from_uid => maps:get(<<"from_uid">>, Row),
        from_device_id => maps:get(<<"from_device_id">>, Row),
        to_uid => maps:get(<<"to_uid">>, Row),
        to_device_id => maps:get(<<"to_device_id">>, Row, <<>>),
        status => maps:get(<<"status">>, Row),
        encrypted_key_bundle => maps:get(<<"encrypted_key_bundle">>, Row),
        expires_at => maps:get(<<"expires_at">>, Row),
        created_at => maps:get(<<"created_at">>, Row),
        confirmed_at => maps:get(<<"confirmed_at">>, Row, <<>>)
    }.
```

### 1.3 创建 Repo 测试

```bash
vim test/repo/e2ee_transfer_repo_tests.erl
```

```erlang
-module(e2ee_transfer_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-include("common.hrl").

%% 测试辅助宏
-define(TEST_WITH_APP, fun() ->
    application:ensure_all_started(imboy),
    fun() ->
        application:set_env(imboy, env, test)
    end()
end).

%% ================================================================
%% 测试用例
%% ================================================================

e2ee_transfer_repo_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"创建传输会话", fun create_transfer_session/0},
      {"查找传输会话", fun find_transfer_session/0},
      {"更新会话状态", fun update_session_status/0},
      {"删除过期会话", fun delete_expired_sessions/0}
     ]}.

%% ================================================================
%% setup/cleanup
%% ================================================================

setup() ->
    ?TEST_WITH_APP(),
    % 清理测试数据
    elib_pg:query(<<"DELETE FROM e2ee_transfer_sessions">>, []),
    ok.

cleanup(_State) ->
    % 清理测试数据
    elib_pg:query(<<"DELETE FROM e2ee_transfer_sessions">>, []),
    ok.

%% ================================================================
%% 测试函数
%% ================================================================

create_transfer_session() ->
    SessionMap = #{
        <<"session_id">> => <<"test-session-123">>,
        <<"from_uid">> => 10001,
        <<"from_device_id">> => <<"device-001">>,
        <<"to_uid">> => 10002,
        <<"encrypted_key_bundle">> => <<"encrypted-data">>,
        <<"expires_at">> => elib_dt:add(elib_dt:now(), {5, minute})
    },

    % 创建会话
    {ok, Session} = e2ee_transfer_repo:create(SessionMap),

    % 验证结果
    ?assertEqual(<<"test-session-123">>, maps:get(<<"session_id">>, Session)),
    ?assertEqual(10001, maps:get(<<"from_uid">>, Session)),
    ?assertEqual(10002, maps:get(<<"to_uid">>, Session)),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Session)).

find_transfer_session() ->
    % 先创建会话
    SessionId = <<"test-session-456">>,
    SessionMap = #{
        <<"session_id">> => SessionId,
        <<"from_uid">> => 10001,
        <<"from_device_id">> => <<"device-001">>,
        <<"to_uid">> => 10002,
        <<"encrypted_key_bundle">> => <<"encrypted-data">>,
        <<"expires_at">> => elib_dt:add(elib_dt:now(), {5, minute})
    },
    {ok, _Created} = e2ee_transfer_repo:create(SessionMap),

    % 查找会话
    {ok, Found} = e2ee_transfer_repo:find_by_session_id(SessionId),

    % 验证结果
    ?assertEqual(SessionId, maps:get(<<"session_id">>, Found)),
    ?assertEqual(10001, maps:get(<<"from_uid">>, Found)).

update_session_status() ->
    % 创建会话
    SessionId = <<"test-session-789">>,
    SessionMap = #{
        <<"session_id">> => SessionId,
        <<"from_uid">> => 10001,
        <<"from_device_id">> => <<"device-001">>,
        <<"to_uid">> => 10002,
        <<"encrypted_key_bundle">> => <<"encrypted-data">>,
        <<"expires_at">> => elib_dt:add(elib_dt:now(), {5, minute})
    },
    {ok, _Created} = e2ee_transfer_repo:create(SessionMap),

    % 更新状态
    ok = e2ee_transfer_repo:update_status(SessionId, <<"accepted">>),

    % 验证状态
    {ok, Updated} = e2ee_transfer_repo:find_by_session_id(SessionId),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Updated)).

delete_expired_sessions() ->
    % 创建已过期会话
    ExpiredSessionMap = #{
        <<"session_id">> => <<"expired-session">>,
        <<"from_uid">> => 10001,
        <<"from_device_id">> => <<"device-001">>,
        <<"to_uid">> => 10002,
        <<"encrypted_key_bundle">> => <<"encrypted-data">>,
        <<"expires_at">> => elib_dt:add(elib_dt:now(), {-10, minute})
    },
    {ok, _} = e2ee_transfer_repo:create(ExpiredSessionMap),

    % 创建有效会话
    ValidSessionMap = #{
        <<"session_id">> => <<"valid-session">>,
        <<"from_uid">> => 10001,
        <<"from_device_id">> => <<"device-001">>,
        <<"to_uid">> => 10002,
        <<"encrypted_key_bundle">> => <<"encrypted-data">>,
        <<"expires_at">> => elib_dt:add(elib_dt:now(), {5, minute})
    },
    {ok, _} = e2ee_transfer_repo:create(ValidSessionMap),

    % 删除过期会话
    {ok, DeletedCount} = e2ee_transfer_repo:delete_expired(),
    ?assertEqual(1, DeletedCount).

%% ================================================================
%% 内部函数
%% ================================================================
```

---

## 步骤 2: 后端 - DS 层

### 2.1 创建 DS 文件

```bash
vim src/ds/e2ee_transfer_ds.erl
```

### 2.2 DS 实现代码

```erlang
-module(e2ee_transfer_ds).
%%%===================================================================
%%% @doc
%%% e2ee_transfer_ds - E2EE 设备传输数据服务层
%%%
%%% 功能：
%%% - 传输会话管理
%%% - 会话验证和状态检查
%%% - 过期会话清理
%%% - 会话安全性检查
%%%
%%% 使用示例：
%%% ```
%%% {ok, Session} = e2ee_transfer_ds:create_session(Uid, DeviceId, ToUid, KeyBundle),
%%% {ok, Session} = e2ee_transfer_ds:get_session(SessionId),
%%% ok = e2ee_transfer_ds:accept_session(SessionId, Uid, DeviceId),
%%% ```
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([create_session/4]).
-export([get_session/1]).
-export([accept_session/3]).
-export([confirm_session/2]).
-export([check_session_valid/1]).
-export([cleanup_expired_sessions/0]).
-export([get_user_pending_sessions/1]).

%% 类型定义
-type session_result() :: {ok, map()} | {error, integer(), binary()}.
-type validation_result() :: ok | {error, integer(), binary()}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建传输会话
%% @param FromUid 发送方用户 ID
%% @param FromDeviceId 发送方设备 ID
%% @param ToUid 接收方用户 ID（可以是自己的另一个账号，也可以是其他用户）
%% @param EncryptedKeyBundle 加密的密钥包（使用接收方公钥加密）
%% @returns {ok, Session} | {error, Code, Msg}
-spec create_session(integer(), binary(), integer(), binary()) -> session_result().
create_session(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) ->
    % 生成会话 ID（UUID v4）
    SessionId = elib_str:uuid(),

    % 计算过期时间（5 分钟后）
    ExpiresAt = elib_dt:add(elib_dt:now(), {5, minute}),

    SessionMap = #{
        <<"session_id">> => SessionId,
        <<"from_uid">> => FromUid,
        <<"from_device_id">> => FromDeviceId,
        <<"to_uid">> => ToUid,
        <<"encrypted_key_bundle">> => EncryptedKeyBundle,
        <<"expires_at">> => ExpiresAt
    },

    case e2ee_transfer_repo:create(SessionMap) of
        {ok, Session} ->
            ?INFO_LOG([e2ee_transfer_ds, session_created, SessionId, FromUid, ToUid]),
            {ok, Session};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_ds, create_failed, Reason]),
            {error, ?ERR_INTERNAL_ERROR, error_msg(?ERR_INTERNAL_ERROR)}
    end.

%% @doc 获取传输会话信息
%% @param SessionId 会话 ID
%% @returns {ok, Session} | {error, Code, Msg}
-spec get_session(binary()) -> session_result().
get_session(SessionId) ->
    case e2ee_transfer_repo:find_by_session_id(SessionId) of
        {ok, Session} ->
            {ok, Session};
        {error, not_found} ->
            {error, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND, error_msg(?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND)};
        {error, Reason} ->
            ?ERROR_LOG([e2ee_transfer_ds, get_failed, Reason]),
            {error, ?ERR_INTERNAL_ERROR, error_msg(?ERR_INTERNAL_ERROR)}
    end.

%% @doc 接受传输会话
%% @param SessionId 会话 ID
%% @param ToUid 接收方用户 ID
%% @param ToDeviceId 接收方设备 ID
%% @returns ok | {error, Code, Msg}
-spec accept_session(binary(), integer(), binary()) -> ok | {error, integer(), binary()}.
accept_session(SessionId, ToUid, ToDeviceId) ->
    % 1. 检查会话是否存在且有效
    case check_session_valid(SessionId) of
        ok ->
            ok;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end,

    % 2. 获取会话信息
    {ok, Session} = get_session(SessionId),

    % 3. 验证接收方用户 ID
    SessionToUid = maps:get(<<"to_uid">>, Session),
    case SessionToUid =:= ToUid of
        false ->
            {error, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH, error_msg(?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH)};
        true ->
            ok
    end,

    % 4. 检查会话状态
    Status = maps:get(<<"status">>, Session),
    case Status of
        <<"pending">> ->
            % 5. 更新设备 ID 和状态
            ok = e2ee_transfer_repo:update_to_device(SessionId, ToDeviceId),
            ok = e2ee_transfer_repo:update_status(SessionId, <<"accepted">>),
            ?INFO_LOG([e2ee_transfer_ds, session_accepted, SessionId, ToUid, ToDeviceId]),
            ok;
        <<"accepted">> ->
            {error, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED, error_msg(?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED)};
        _ ->
            {error, ?ERR_E2EE_TRANSFER_INVALID_SESSION, error_msg(?ERR_E2EE_TRANSFER_INVALID_SESSION)}
    end.

%% @doc 确认传输完成
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID
%% @returns ok | {error, Code, Msg}
-spec confirm_session(binary(), integer()) -> ok | {error, integer(), binary()}.
confirm_session(SessionId, FromUid) ->
    % 1. 获取会话信息
    case get_session(SessionId) of
        {error, Code, Msg} ->
            {error, Code, Msg};
        {ok, Session} ->
            % 2. 验证发送方用户 ID
            SessionFromUid = maps:get(<<"from_uid">>, Session),
            case SessionFromUid =:= FromUid of
                false ->
                    {error, ?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH, error_msg(?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH)};
                true ->
                    % 3. 检查会话状态
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"accepted">> ->
                            ok = e2ee_transfer_repo:mark_confirmed(SessionId),
                            ?INFO_LOG([e2ee_transfer_ds, session_confirmed, SessionId, FromUid]),
                            ok;
                        _ ->
                            {error, ?ERR_E2EE_TRANSFER_CANNOT_CONFIRM, error_msg(?ERR_E2EE_TRANSFER_CANNOT_CONFIRM)}
                    end
            end
    end.

%% @doc 检查会话是否有效（未过期且状态正确）
%% @param SessionId 会话 ID
%% @returns ok | {error, Code, Msg}
-spec check_session_valid(binary()) -> validation_result().
check_session_valid(SessionId) ->
    case get_session(SessionId) of
        {error, Code, Msg} ->
            {error, Code, Msg};
        {ok, Session} ->
            % 检查是否过期
            ExpiresAt = maps:get(<<"expires_at">>, Session),
            Now = elib_dt:now(),
            case Now >= ExpiresAt of
                true ->
                    {error, ?ERR_E2EE_TRANSFER_SESSION_EXPIRED, error_msg(?ERR_E2EE_TRANSFER_SESSION_EXPIRED)};
                false ->
                    ok
            end
    end.

%% @doc 清理过期的传输会话（定时任务调用）
%% @returns {ok, DeletedCount}
-spec cleanup_expired_sessions() -> {ok, non_neg_integer()}.
cleanup_expired_sessions() ->
    e2ee_transfer_repo:delete_expired().

%% @doc 获取用户的待处理传输会话列表
%% @param FromUid 发送方用户 ID
%% @returns {ok, [Session]}
-spec get_user_pending_sessions(integer()) -> {ok, [map()]}.
get_user_pending_sessions(FromUid) ->
    e2ee_transfer_repo:find_pending_by_from_uid(FromUid).
```

---

## 步骤 3: 后端 - Logic 层

### 3.1 创建 Logic 文件

```bash
vim src/logic/e2ee_transfer_logic.erl
```

### 3.2 Logic 实现代码

```erlang
-module(e2ee_transfer_logic).
%%%===================================================================
%%% @doc
%%% e2ee_transfer_logic - E2EE 设备传输业务逻辑层
%%%
%%% 功能：
%%% - 处理传输会话的业务逻辑
%%% - 验证用户权限和设备信息
%%% - 协调传输流程
%%%
%%% 使用示例：
%%% ```
%%% {ok, Session} = e2ee_transfer_logic:create_transfer(Uid, DeviceId, ToUid, KeyBundle),
%%% {ok, Session} = e2ee_transfer_logic:get_transfer_info(SessionId, Uid),
%%% ok = e2ee_transfer_logic:accept_transfer(SessionId, Uid, DeviceId),
%%% ```
%%%===================================================================

-include("error_code.hrl").
-include("log.hrl").
-include("common.hrl").

%% API 函数
-export([create_transfer/4]).
-export([get_transfer_info/2]).
-export([accept_transfer/3]).
-export([confirm_transfer/2]).
-export([list_pending_transfers/1]).
-export([cancel_transfer/2]).

%% 类型定义
-type transfer_result() :: {ok, map()} | {error, integer(), binary()}.

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建传输会话
%% @param FromUid 发送方用户 ID
%% @param FromDeviceId 发送方设备 ID
%% @param ToUid 接收方用户 ID
%% @param EncryptedKeyBundle 加密的密钥包
%% @returns {ok, Session} | {error, Code, Msg}
-spec create_transfer(integer(), binary(), integer(), binary()) -> transfer_result().
create_transfer(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) ->
    % 1. 验证输入参数
    case validate_transfer_input(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) of
        ok ->
            ok;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end,

    % 2. 验证发送方设备是否存在
    case validate_device_exists(FromUid, FromDeviceId) of
        ok ->
            ok;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end,

    % 3. 创建会话
    case e2ee_transfer_ds:create_session(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) of
        {ok, Session} ->
            % 4. 返回会话信息（包含二维码数据）
            ResponseData = #{
                <<"session_id">> => maps:get(<<"session_id">>, Session),
                <<"from_uid">> => elib_hashids:encode(FromUid),
                <<"to_uid">> => elib_hashids:encode(ToUid),
                <<"expires_at">> => maps:get(<<"expires_at">>, Session),
                <<"qr_code_data">> => build_qr_code_data(Session)
            },
            {ok, ResponseData};
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%% @doc 获取传输会话信息
%% @param SessionId 会话 ID
%% @param RequestUid 请求用户 ID
%% @returns {ok, SessionInfo} | {error, Code, Msg}
-spec get_transfer_info(binary(), integer()) -> transfer_result().
get_transfer_info(SessionId, RequestUid) ->
    case e2ee_transfer_ds:get_session(SessionId) of
        {ok, Session} ->
            % 检查权限：必须是发送方或接收方
            FromUid = maps:get(<<"from_uid">>, Session),
            ToUid = maps:get(<<"to_uid">>, Session),

            case RequestUid =:= FromUid orelse RequestUid =:= ToUid of
                true ->
                    % 返回会话信息（不包含加密密钥包）
                    SessionInfo = #{
                        <<"session_id">> => maps:get(<<"session_id">>, Session),
                        <<"from_uid">> => elib_hashids:encode(FromUid),
                        <<"to_uid">> => elib_hashids:encode(ToUid),
                        <<"status">> => maps:get(<<"status">>, Session),
                        <<"expires_at">> => maps:get(<<"expires_at">>, Session),
                        <<"created_at">> => maps:get(<<"created_at">>, Session)
                    },
                    {ok, SessionInfo};
                false ->
                    {error, ?ERR_UNAUTHORIZED, error_msg(?ERR_UNAUTHORIZED)}
            end;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%% @doc 接受传输会话
%% @param SessionId 会话 ID
%% @param ToUid 接收方用户 ID
%% @param ToDeviceId 接收方设备 ID
%% @returns {ok, Session} | {error, Code, Msg}
-spec accept_transfer(binary(), integer(), binary()) -> transfer_result().
accept_transfer(SessionId, ToUid, ToDeviceId) ->
    % 1. 验证输入参数
    case validate_device_id(ToDeviceId) of
        ok ->
            ok;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end,

    % 2. 验证接收方设备是否存在
    case validate_device_exists(ToUid, ToDeviceId) of
        ok ->
            ok;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end,

    % 3. 接受会话
    case e2ee_transfer_ds:accept_session(SessionId, ToUid, ToDeviceId) of
        ok ->
            % 4. 返回会话信息和加密密钥包
            {ok, Session} = e2ee_transfer_ds:get_session(SessionId),
            ResponseData = #{
                <<"session_id">> => maps:get(<<"session_id">>, Session),
                <<"from_uid">> => elib_hashids:encode(maps:get(<<"from_uid">>, Session)),
                <<"from_device_id">> => maps:get(<<"from_device_id">>, Session),
                <<"encrypted_key_bundle">> => maps:get(<<"encrypted_key_bundle">>, Session),
                <<"expires_at">> => maps:get(<<"expires_at">>, Session)
            },
            {ok, ResponseData};
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%% @doc 确认传输完成
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID
%% @returns {ok, confirmed} | {error, Code, Msg}
-spec confirm_transfer(binary(), integer()) -> transfer_result().
confirm_transfer(SessionId, FromUid) ->
    case e2ee_transfer_ds:confirm_session(SessionId, FromUid) of
        ok ->
            {ok, #{<<"status">> => <<"confirmed">>}};
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%% @doc 列出用户的待处理传输会话
%% @param FromUid 发送方用户 ID
%% @returns {ok, [Session]}
-spec list_pending_transfers(integer()) -> transfer_result().
list_pending_transfers(FromUid) ->
    case e2ee_transfer_ds:get_user_pending_sessions(FromUid) of
        {ok, Sessions} ->
            ResponseList = lists:map(fun(Session) ->
                #{
                    <<"session_id">> => maps:get(<<"session_id">>, Session),
                    <<"to_uid">> => elib_hashids:encode(maps:get(<<"to_uid">>, Session)),
                    <<"status">> => maps:get(<<"status">>, Session),
                    <<"created_at">> => maps:get(<<"created_at">>, Session),
                    <<"expires_at">> => maps:get(<<"expires_at">>, Session)
                }
            end, Sessions),
            {ok, ResponseList};
        {error, Reason} ->
            {error, ?ERR_INTERNAL_ERROR, error_msg(?ERR_INTERNAL_ERROR)}
    end.

%% @doc 取消传输会话
%% @param SessionId 会话 ID
%% @param FromUid 发送方用户 ID
%% @returns {ok, cancelled} | {error, Code, Msg}
-spec cancel_transfer(binary(), integer()) -> transfer_result().
cancel_transfer(SessionId, FromUid) ->
    case e2ee_transfer_ds:get_session(SessionId) of
        {ok, Session} ->
            % 验证权限
            SessionFromUid = maps:get(<<"from_uid">>, Session),
            case SessionFromUid =:= FromUid of
                false ->
                    {error, ?ERR_UNAUTHORIZED, error_msg(?ERR_UNAUTHORIZED)};
                true ->
                    % 只能取消 pending 状态的会话
                    Status = maps:get(<<"status">>, Session),
                    case Status of
                        <<"pending">> ->
                            ok = e2ee_transfer_repo:update_status(SessionId, <<"cancelled">>),
                            {ok, #{<<"status">> => <<"cancelled">>}};
                        _ ->
                            {error, ?ERR_E2EE_TRANSFER_INVALID_SESSION, error_msg(?ERR_E2EE_TRANSFER_INVALID_SESSION)}
                    end
            end;
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 验证传输输入参数
%% @private
-spec validate_transfer_input(integer(), binary(), integer(), binary()) -> ok | {error, integer(), binary()}.
validate_transfer_input(FromUid, FromDeviceId, ToUid, EncryptedKeyBundle) ->
    case FromUid > 0 of
        false ->
            {error, ?ERR_BAD_REQUEST, <<"无效的发送方用户 ID"/utf8>>};
        true ->
            case ToUid > 0 of
                false ->
                    {error, ?ERR_BAD_REQUEST, <<"无效的接收方用户 ID"/utf8>>};
                true ->
                    case validate_device_id(FromDeviceId) of
                        ok ->
                            case byte_size(EncryptedKeyBundle) > 0 of
                                true ->
                                    ok;
                                false ->
                                    {error, ?ERR_BAD_REQUEST, <<"密钥包不能为空"/utf8>>}
                            end;
                        {error, Code, Msg} ->
                            {error, Code, Msg}
                    end
            end
    end.

%% @doc 验证设备 ID 格式
%% @private
-spec validate_device_id(binary()) -> ok | {error, integer(), binary()}.
validate_device_id(DeviceId) when is_binary(DeviceId), byte_size(DeviceId) > 0 ->
    ok;
validate_device_id(_) ->
    {error, ?ERR_E2EE_TRANSFER_INVALID_DEVICE, error_msg(?ERR_E2EE_TRANSFER_INVALID_DEVICE)}.

%% @doc 验证设备是否存在
%% @private
-spec validate_device_exists(integer(), binary()) -> ok | {error, integer(), binary()}.
validate_device_exists(Uid, DeviceId) ->
    case user_device_ds:find_by_device_id(Uid, DeviceId) of
        {ok, _Device} ->
            ok;
        {error, not_found} ->
            {error, ?ERR_E2EE_TRANSFER_INVALID_DEVICE, error_msg(?ERR_E2EE_TRANSFER_INVALID_DEVICE)};
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_ERROR, error_msg(?ERR_INTERNAL_ERROR)}
    end.

%% @doc 构建二维码数据
%% @private
-spec build_qr_code_data(map()) -> binary().
build_qr_code_data(Session) ->
    SessionId = maps:get(<<"session_id">>, Session),
    % 二维码数据格式: imboy:e2ee:transfer:{session_id}
    QrData = <<"imboy:e2ee:transfer:", SessionId/binary>>,
    base64:encode(QrData).
```

---

## 步骤 4: 后端 - Handler 层

### 4.1 编辑 Handler 文件

```bash
vim src/api/e2ee_handler.erl
```

### 4.2 添加新端点

在现有 `e2ee_handler.erl` 中添加新端点：

```erlang
%% ================================================================
%% 设备传输相关端点
%% ================================================================

%% @doc 创建传输会话
-spec create_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
create_transfer(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    CurrentDeviceId = auth_ds:current_device_id(State),

    % 解析请求参数
    {ok, Body} = elib_req:body(Req0, []),
    ToUidEnc = maps:get(<<"to_uid">>, Body, <<>>),
    EncryptedKeyBundle = maps:get(<<"encrypted_key_bundle">>, Body, <<>>),

    % 验证参数
    ToUid = case elib_hashids:decode(ToUidEnc) of
        invalid -> {error, invalid_uid};
        Uid when is_integer(Uid) -> {ok, Uid}
    end,

    case ToUid of
        {ok, ToUidInt} when ToUidInt > 0 ->
            case e2ee_transfer_logic:create_transfer(CurrentUid, CurrentDeviceId, ToUidInt, EncryptedKeyBundle) of
                {ok, ResponseData} ->
                    elib_response:success(Req0, ResponseData);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        _ ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.

%% @doc 获取传输会话信息
-spec get_transfer_info(cowboy_req:req(), map()) -> cowboy_req:req().
get_transfer_info(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    SessionId = elib_param:get(<<"session_id">>, Req0, <<>>),

    case byte_size(SessionId) > 0 of
        true ->
            case e2ee_transfer_logic:get_transfer_info(SessionId, CurrentUid) of
                {ok, ResponseData} ->
                    elib_response:success(Req0, ResponseData);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.

%% @doc 接受传输会话
-spec accept_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
accept_transfer(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    CurrentDeviceId = auth_ds:current_device_id(State),

    {ok, Body} = elib_req:body(Req0, []),
    SessionId = maps:get(<<"session_id">>, Body, <<>>),

    case byte_size(SessionId) > 0 of
        true ->
            case e2ee_transfer_logic:accept_transfer(SessionId, CurrentUid, CurrentDeviceId) of
                {ok, ResponseData} ->
                    elib_response:success(Req0, ResponseData);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.

%% @doc 确认传输完成
-spec confirm_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
confirm_transfer(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    {ok, Body} = elib_req:body(Req0, []),
    SessionId = maps:get(<<"session_id">>, Body, <<>>),

    case byte_size(SessionId) > 0 of
        true ->
            case e2ee_transfer_logic:confirm_transfer(SessionId, CurrentUid) of
                {ok, ResponseData} ->
                    elib_response:success(Req0, ResponseData);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.

%% @doc 列出待处理的传输会话
-spec list_transfers(cowboy_req:req(), map()) -> cowboy_req:req().
list_transfers(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    case e2ee_transfer_logic:list_pending_transfers(CurrentUid) of
        {ok, ResponseList} ->
            elib_response:success(Req0, #{<<"transfers">> => ResponseList});
        {error, Msg, Code} ->
            elib_response:error(Req0, Msg, Code)
    end.

%% @doc 取消传输会话
-spec cancel_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
cancel_transfer(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),

    {ok, Body} = elib_req:body(Req0, []),
    SessionId = maps:get(<<"session_id">>, Body, <<>>),

    case byte_size(SessionId) > 0 of
        true ->
            case e2ee_transfer_logic:cancel_transfer(SessionId, CurrentUid) of
                {ok, ResponseData} ->
                    elib_response:success(Req0, ResponseData);
                {error, Msg, Code} ->
                    elib_response:error(Req0, Msg, Code)
            end;
        false ->
            elib_response:error(Req0, error_msg(?ERR_BAD_REQUEST), ?ERR_BAD_REQUEST)
    end.
```

### 4.3 添加路由

编辑 `src/imboy_router.erl`，添加新路由：

```erlang
{"e2ee", e2ee_handler, #{
    % 现有路由
    {"/user_keys", user_keys},
    {"/group_member_keys", group_member_keys},

    % 新增路由 - 设备传输
    {"/transfer/create", create_transfer},
    {"/transfer/info", get_transfer_info},           % GET
    {"/transfer/accept", accept_transfer},
    {"/transfer/confirm", confirm_transfer},
    {"/transfer/list", list_transfers},              % GET
    {"/transfer/cancel", cancel_transfer}
}}
```

---

## 步骤 5: 前端实现

### 5.1 创建 API 服务

在前端项目目录执行：

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp

# 创建 API 文件
vim lib/store/api/e2ee_transfer_api.dart
```

```dart
import 'dart:convert';
import 'package:imboy/store/api/base_api.dart';

/// E2EE 设备传输 API
class E2EETransferApi extends BaseApi {
  /// 创建传输会话
  static Future<Map<String, dynamic>> createTransfer({
    required String toUid,
    required String encryptedKeyBundle,
  }) async {
    final response = await post('/v1/e2ee/transfer/create', data: {
      'to_uid': toUid,
      'encrypted_key_bundle': encryptedKeyBundle,
    });
    return response.data;
  }

  /// 获取传输会话信息
  static Future<Map<String, dynamic>> getTransferInfo({
    required String sessionId,
  }) async {
    final response = await get(
      '/v1/e2ee/transfer/info',
      queryParameters: {'session_id': sessionId},
    );
    return response.data;
  }

  /// 接受传输会话
  static Future<Map<String, dynamic>> acceptTransfer({
    required String sessionId,
  }) async {
    final response = await post('/v1/e2ee/transfer/accept', data: {
      'session_id': sessionId,
    });
    return response.data;
  }

  /// 确认传输完成
  static Future<Map<String, dynamic>> confirmTransfer({
    required String sessionId,
  }) async {
    final response = await post('/v1/e2ee/transfer/confirm', data: {
      'session_id': sessionId,
    });
    return response.data;
  }

  /// 列出待处理的传输会话
  static Future<List<dynamic>> listTransfers() async {
    final response = await get('/v1/e2ee/transfer/list');
    return response.data['transfers'] ?? [];
  }

  /// 取消传输会话
  static Future<Map<String, dynamic>> cancelTransfer({
    required String sessionId,
  }) async {
    final response = await post('/v1/e2ee/transfer/cancel', data: {
      'session_id': sessionId,
    });
    return response.data;
  }
}
```

### 5.2 创建传输服务

```bash
vim lib/service/e2ee_transfer_service.dart
```

```dart
import 'dart:convert';
import 'package:imboy/store/api/e2ee_transfer_api.dart';
import 'package:imboy/store/api/e2ee_api.dart';
import 'package:imboy/service/rsa_service.dart';
import 'package:imboy/service/storage_service.dart';

/// E2EE 设备传输服务
class E2EETransferService {
  /// 创建传输会话
  static Future<Map<String, dynamic>> createTransfer({
    required String toUid,
  }) async {
    // 1. 获取当前用户的私钥
    final privateKey = await StorageService.getE2EEPrivateKey();
    if (privateKey == null) {
      throw Exception('私钥不存在');
    }

    // 2. 获取接收方的公钥
    final deviceKeys = await E2EEApi.userKeys(uid: toUid);
    if (deviceKeys.isEmpty) {
      throw Exception('接收方设备未找到');
    }

    final receiverPublicKey = deviceKeys[0]['public_key'];
    final receiverDeviceId = deviceKeys[0]['device_id'];

    // 3. 使用接收方公钥加密私钥
    final encryptedKeyBundle = await _encryptPrivateKeyForDevice(
      privateKey: privateKey,
      publicKey: receiverPublicKey,
    );

    // 4. 创建传输会话
    final session = await E2EETransferApi.createTransfer(
      toUid: toUid,
      encryptedKeyBundle: encryptedKeyBundle,
    );

    return session;
  }

  /// 接受传输会话
  static Future<bool> acceptTransfer({
    required String sessionId,
  }) async {
    // 1. 接受会话
    final session = await E2EETransferApi.acceptTransfer(
      sessionId: sessionId,
    );

    // 2. 解密私钥
    final encryptedKeyBundle = session['encrypted_key_bundle'];
    final privateKey = await RSAService.decryptWithPrivateKey(
      encryptedKeyBundle,
    );

    // 3. 存储私钥
    await StorageService.saveE2EEPrivateKey(privateKey);

    // 4. 确认传输完成
    await E2EETransferApi.confirmTransfer(sessionId: sessionId);

    return true;
  }

  /// 加密私钥（使用接收方公钥）
  static Future<String> _encryptPrivateKeyForDevice({
    required String privateKey,
    required String publicKey,
  }) async {
    // 构建密钥包
    final keyBundle = json.encode({
      'private_key': privateKey,
      'timestamp': DateTime.now().toIso8601String(),
    });

    // 使用接收方公钥加密
    final encrypted = await RSAService.encryptWithPublicKey(
      keyBundle,
      publicKey,
    );

    // Base64 编码
    return base64.encode(utf8.encode(encrypted));
  }

  /// 解析二维码数据
  static String? parseQrCodeData(String qrData) {
    try {
      final decoded = base64.decode(qrData);
      final data = utf8.decode(decoded);
      if (data.startsWith('imboy:e2ee:transfer:')) {
        return data.substring('imboy:e2ee:transfer:'.length);
      }
      return null;
    } catch (e) {
      return null;
    }
  }
}
```

---

## 步骤 6: 测试

### 6.1 后端测试

```bash
# 运行单元测试
make eunit

# 或运行特定测试
erl -noshell -eval "eunit:test(e2ee_transfer_repo_tests, [verbose])" -s init stop
```

### 6.2 集成测试

```erlang
% 在节点 shell 中执行

% 1. 创建传输会话
{ok, Session} = e2ee_transfer_logic:create_transfer(
    10001,
    <<"device-001">>,
    10002,
    <<"encrypted-key-bundle">>
).

% 2. 获取会话信息
{ok, Info} = e2ee_transfer_logic:get_transfer_info(
    maps:get(<<"session_id">>, Session),
    10001
).

% 3. 接受传输
{ok, Accepted} = e2ee_transfer_logic:accept_transfer(
    maps:get(<<"session_id">>, Session),
    10002,
    <<"device-002">>
).

% 4. 确认传输
{ok, Confirmed} = e2ee_transfer_logic:confirm_transfer(
    maps:get(<<"session_id">>, Session),
    10001
).
```

### 6.3 前端测试

```dart
// 测试文件: test/service/e2ee_transfer_service_test.dart

void main() {
  testWidgets('创建传输会话', (tester) async {
    // 测试创建传输会话
  });

  testWidgets('接受传输会话', (tester) async {
    // 测试接受传输会话
  });
}
```

---

## 完成检查清单

- [ ] Repo 层实现完成
- [ ] DS 层实现完成
- [ ] Logic 层实现完成
- [ ] Handler 层实现完成
- [ ] 路由配置完成
- [ ] 前端 API 服务完成
- [ ] 前端传输服务完成
- [ ] 单元测试通过
- [ ] 集成测试通过
- [ ] 文档更新完成

---

## 下一阶段

完成本阶段后，请继续执行：
- [阶段 3: 社交恢复](./phase-03-social-recovery.md)

---

**最后更新**: 2026-01-30
**作者**: Claude AI Planning Agent
