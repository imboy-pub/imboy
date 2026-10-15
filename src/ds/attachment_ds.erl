-module(attachment_ds).
-moduledoc "附件数据服务层 / attachment data service。".
%%%
% attachment_ds 是附件数据服务层
% 封装附件的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([tablename/0]).
-export([save/4, save_confirmed_tx/4]).
-export([stats/0]).
-export([page/3]).
-export([disable/1, enable/1, soft_delete/1]).
-export([orphan_stats/1, orphan_cleanup/1]).
-export([pending_add/4, pending_remove/1, pending_cleanup/1]).
-export([find_by_path_and_uid/2]).
-export([find_by_path/1]).
-export([bind_moment_scope_ref/4]).

-export([find_path_by_id/1]).
-export([find_path_by_group_file/2]).
-export([authorize_group_access/2]).
-export([authorize_channel_scope/2]).
-export([authorize_group_scope/2]).
-export([ensure_upload_scope_tx/3]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取附件表名
%% @returns binary() 表名
-spec tablename() -> binary().
tablename() ->
    attachment_repo:tablename().

%% @doc 保存附件信息
%% 保存附件信息，如果 MD5 已存在则更新引用次数
%% @param Conn 数据库连接
%% @param CreatedAt 创建时间
%% @param Uid 用户ID
%% @param Attach 附件信息列表
%% @returns ok
-spec save(pid(), binary(), integer(), [map()]) -> ok.
save(Conn, CreatedAt, Uid, Attach) ->
    attachment_repo:save(Conn, CreatedAt, Uid, Attach).

%% Human group/channel confirmations audit only the first durable metadata row.
%% Scope qualification is locked by the caller; audit failure aborts that same transaction.
-spec save_confirmed_tx(pid(), binary(), integer(), [map()]) -> ok.
save_confirmed_tx(_Conn, _CreatedAt, _Uid, []) ->
    ok;
save_confirmed_tx(Conn, CreatedAt, Uid, [Attach | Tail]) ->
    Path = maps:get(<<"path">>, Attach),
    {ok, Before} = confirmation_result(attachment_repo:confirmation_row_tx(Conn, Path)),
    ok = ensure_confirmed_scope(Before, Uid, Attach),
    %% W2-02：metadata insert-if-absent 与真实引用新增拆开——confirm 重放
    %% 复用既有行且不递增 referer（referer 只属于 save/4 的新引用写入）。
    case attachment_repo:confirm_if_absent_tx(Conn, CreatedAt, Uid, Attach) of
        {ok, _Id, _First} -> ok;
        {error, Reason} -> throw({abort_tx, Reason})
    end,
    case Before of
        not_found ->
            {ok, Row} = confirmation_result(attachment_repo:confirmation_row_tx(Conn, Path)),
            ok = ensure_confirmed_scope(Row, Uid, Attach),
            %% W2-02：V5 confirm 对全部 scope 复用本入口；workspace 级审计
            %% 只对 group/channel（个人域首确认以元数据行为记录，无治理审计）。
            case
                case maps:get(<<"scope">>, Row) of
                    <<"group">> -> group;
                    <<"channel">> -> channel;
                    _ -> none
                end
            of
                none ->
                    ok;
                Kind ->
                    Ref = elib_cnv:safe_to_integer(maps:get(<<"scope_ref">>, Row)),
                    ok = workspace_guard:abort_on_error(
                        enterprise_audit_event_repo:append_scope_file_tx(
                            Conn,
                            {Kind, Ref},
                            Uid,
                            #{
                                resource_type => <<"attachment">>,
                                resource_id => maps:get(<<"id">>, Row),
                                action => <<"file.confirmed">>
                            }
                        )
                    )
            end;
        _ ->
            ok
    end,
    save_confirmed_tx(Conn, CreatedAt, Uid, Tail).

ensure_confirmed_scope(not_found, _Uid, _Attach) ->
    ok;
ensure_confirmed_scope(Row, Uid, Attach) ->
    case
        {
            maps:get(<<"creator_user_id">>, Row),
            maps:get(<<"scope">>, Row),
            maps:get(<<"scope_ref">>, Row)
        }
    of
        {Uid, Scope, Ref} ->
            case {maps:get(<<"scope">>, Attach), maps:get(<<"scope_ref">>, Attach)} of
                {Scope, Ref} -> ok;
                _ -> throw({abort_tx, forbidden})
            end;
        _ ->
            throw({abort_tx, forbidden})
    end,
    %% W2-02：conflicting cipher 拒绝——重放请求的加密形态必须与既有行一致
    %% （历史行 cipher=NULL，旧客户端重放 normalize 后同为 null，兼容不受影响）。
    case maps:get(<<"cipher">>, Row, null) of
        Cipher when is_atom(Cipher); is_binary(Cipher) ->
            case maps:get(<<"cipher">>, Attach, null) of
                Cipher -> ok;
                _ -> throw({abort_tx, cipher_conflict})
            end;
        _ ->
            throw({abort_tx, cipher_conflict})
    end.

confirmation_result({ok, _} = Result) -> Result;
confirmation_result({error, Reason}) -> throw({abort_tx, Reason}).

-spec authorize_group_scope(integer(), integer()) -> boolean().
authorize_group_scope(Gid, Uid) ->
    attachment_repo:authorize_group_scope(Gid, Uid).

-spec find_path_by_group_file(integer(), integer()) -> {ok, binary()} | {error, term()}.
find_path_by_group_file(Gid, FileId) ->
    attachment_repo:find_path_by_group_file(Gid, FileId).

%% G3: adm_attach_handler 不应直调 attachment_repo
-spec stats() -> map().
stats() -> attachment_repo:stats().

-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Opts) -> attachment_repo:page(Page, Size, Opts).

-spec disable(integer() | binary()) -> ok | {error, term()}.
disable(Id) -> attachment_repo:update_status(Id, 0).

-spec enable(integer() | binary()) -> ok | {error, term()}.
enable(Id) -> attachment_repo:update_status(Id, 1).

-spec soft_delete(integer() | binary()) -> ok | {error, term()}.
soft_delete(Id) -> attachment_repo:update_status(Id, -1).

-spec orphan_stats(map()) -> {ok, map()} | {error, term()}.
orphan_stats(Opts) -> attachment_repo:orphan_stats(Opts).

%% Commit metadata deletion and its durable job before deleting object bytes.
%% W4-01 入口收口（02 §4/§5）：orphan 清理不再删字节——对每个孤儿发
%% metadata 域删除意图（fn_v5_intent_ordinary_delete 锁后复核 gc_verified/
%% 治理/绑定；不可证明对象保守保留），字节由 Worker 删；行删除由队列 ack
%% 协议负责。cleaned 计数语义 = 已发删除意图数。
-spec orphan_cleanup(map()) -> {ok, #{cleaned := integer(), errors := integer()}} | {error, term()}.
orphan_cleanup(Opts) ->
    case attachment_repo:orphan_list_for_delete(Opts) of
        {ok, Rows} ->
            {Queued, Errors} = lists:foldl(
                fun(Row, {Q, E}) ->
                    case queue_orphan(maps:get(<<"id">>, Row), Opts) of
                        queued -> {Q + 1, E};
                        skipped -> {Q, E};
                        _ -> {Q, E + 1}
                    end
                end,
                {0, 0},
                Rows
            ),
            {ok, #{cleaned => Queued, errors => Errors}};
        {error, R} ->
            {error, R}
    end.

queue_orphan(Id, Opts) ->
    elib_pg:with_tx(fun(Conn) ->
        case attachment_repo:orphan_path_tx(Conn, Id, Opts) of
            {ok, []} ->
                skipped;
            {ok, [Row]} ->
                Path = maps:get(<<"path">>, Row),
                case
                    attachment_repo:intent_ordinary_delete_tx(
                        Conn, Path, <<"orphan cleanup">>
                    )
                of
                    ok -> queued;
                    %% 资格不满足（gc_verified=false/治理/绑定）或已在队列：
                    %% 保守保留，不计错误
                    {error, intent_conflict} -> skipped;
                    {error, tombstone} -> skipped;
                    {error, R} -> throw({abort_tx, R})
                end;
            {error, R} ->
                throw({abort_tx, R})
        end
    end).

%% W4-01 入口收口：旧 gc repo 的 flush_orphan_jobs/delete_orphan_object
%% （业务侧直接删字节）已移除——删除意图统一走 fn_v5_intent_ordinary_delete
%% / fn_v5_intent_pending_delete（迁移 179/184/186），字节由删除队列 Worker
%% 执行；旧 attachment_gc 表存量行的接管属 W4-02（preserve legacy attempts）。

%% ===================================================================
%% 待确认 presign 登记（#20）
%% ===================================================================

%% @doc presign 签发时登记，使未 confirm 的对象对清理器可见
-spec pending_add(binary(), binary(), binary(), integer()) -> ok | {error, term()}.
pending_add(ObjectKey, Bucket, Scope, Uid) ->
    attach_pending_repo:add(ObjectKey, Bucket, Scope, Uid).

%% @doc confirm 成功后销账
-spec pending_remove(binary()) -> ok | {error, term()}.
pending_remove(ObjectKey) ->
    attach_pending_repo:remove(ObjectKey).

%% Pending garbage shares the confirmation path lock and durable deletion queue.
%% W4-01 入口收口（02 §2/§5）：pending 清理不再删 pending 行+字节——对每个
%% 超龄 session 发 pending 域删除意图（fn_v5_intent_pending_delete sweep：
%% registered→expired→delete_intent，同事务 ready job+审计），字节由 Worker
%% 删。cleaned 计数语义 = 已发删除意图数。
-spec pending_cleanup(integer()) ->
    {ok, #{cleaned := integer(), errors := integer()}} | {error, term()}.
pending_cleanup(AgeHours) ->
    case attach_pending_repo:list_expired(AgeHours) of
        {ok, Rows} ->
            {Queued, Errors} = lists:foldl(
                fun(Row, {Q, E}) ->
                    case queue_pending(maps:get(<<"object_key">>, Row), AgeHours) of
                        queued -> {Q + 1, E};
                        skipped -> {Q, E};
                        _ -> {Q, E + 1}
                    end
                end,
                {0, 0},
                Rows
            ),
            {ok, #{cleaned => Queued, errors => Errors}};
        {error, R} ->
            {error, R}
    end.

queue_pending(Key, _AgeHours) ->
    elib_pg:with_tx(fun(Conn) ->
        case
            attach_pending_repo:intent_pending_delete_tx(
                Conn, Key, <<"sweep">>, <<"pending cleanup">>
            )
        of
            ok -> queued;
            %% 资格不满足（能力窗未耗尽等）或已在队列/已清：保守保留
            {error, intent_conflict} -> skipped;
            {error, tombstone} -> skipped;
            {error, not_found} -> skipped;
            {error, R} -> throw({abort_tx, R})
        end
    end).

%% @doc 根据 object_key(path) 和上传者 uid 查找附件（归属校验）
-spec find_by_path_and_uid(binary(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_path_and_uid(ObjectKey, Uid) ->
    attachment_repo:find_by_path_and_uid(ObjectKey, Uid).

%% @doc 根据 object_key(path) 查找附件元数据（读鉴权 authorize/2 用）
-spec find_by_path(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_path(ObjectKey) ->
    attachment_repo:find_by_path(ObjectKey).

%% @doc 群附件按消息 anchor 与当前 membership generation 授权。
-spec authorize_group_access(binary(), integer()) -> boolean().
authorize_group_access(ObjectKey, Uid) ->
    attachment_repo:authorize_group_access(ObjectKey, Uid).

%% @doc 在同一发帖事务连接上绑定作者的全部媒体附件
-spec bind_moment_scope_ref(
    epgsql:connection() | pid(), integer(), [binary()], integer() | binary()
) -> ok | {error, term()}.
bind_moment_scope_ref(Conn, Uid, ObjectKeys, MomentId) ->
    attachment_repo:bind_moment_scope_ref(Conn, Uid, ObjectKeys, MomentId).

%% @doc 按 id 查询附件 path（admin 下载端点签发 presign GET 用）
-spec find_path_by_id(integer() | binary()) -> {ok, binary()} | {error, not_found | term()}.
find_path_by_id(Id) ->
    attachment_repo:find_path_by_id(Id).

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec authorize_channel_scope(integer(), integer()) -> boolean().
authorize_channel_scope(ChannelId, Uid) ->
    attachment_repo:authorize_channel_scope(ChannelId, Uid).

%% 锁父级资格后复用原归档守卫；最终检查和元数据保存使用同一连接。
-spec ensure_upload_scope_tx(any(), {group | channel, integer() | binary()}, integer()) ->
    ok | {error, term()}.
ensure_upload_scope_tx(Conn, {Kind, Ref}, Uid) ->
    Id = elib_cnv:safe_to_integer(Ref),
    Target = {Kind, Id},
    case attachment_repo:lock_upload_organization_tx(Conn, Target, Uid) of
        ok ->
            case workspace_guard:ensure_writable_tx(Conn, Target) of
                ok ->
                    case attachment_repo:authorize_upload_scope_tx(Conn, Kind, Id, Uid) of
                        true -> ok;
                        false -> {error, forbidden}
                    end;
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.
