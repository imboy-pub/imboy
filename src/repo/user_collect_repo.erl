-module(user_collect_repo).
%%%
% user_collect 相关操作都放到该模块，存储库模块
% collect related operations are put in this module, repository module
% 用户收藏数据仓库层，提供用户收藏信息的基础数据库操作
%%%

-export([tablename/0]).
-export([count_by_uid_kind_id/2]).
-export([delete/2]).
-export([update/3]).
-export([find_any_by_uid_kind_id/2, find_any_by_uid_kind_id_tx/3]).
-export([sealed_exists/2]).
-export([key_state_gate_tx/3, key_state_lock_tx/2]).
-export([insert_sealed_tx/7]).
-export([cas_update_sealed_tx/8]).
-export([revive_sealed_tx/7]).
-export([tombstone_sealed_tx/3]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取用户收藏表的表名
%% @return 返回用户收藏表的完整表名
-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"user_collect">>).

%% @doc 统计用户指定类型的收藏数量
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return Count 收藏数量
%% @example user_collect_repo:count_by_uid_kind_id(2, <<"cqi6od1pa9gjnlt3a5a0">>).
-spec count_by_uid_kind_id(integer(), binary()) -> non_neg_integer().
count_by_uid_kind_id(Uid, KindId) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql =
        <<"SELECT count(*) as count FROM ", (tablename())/binary,
            " WHERE user_id = $1 AND status = 1 AND kind_id = $2">>,
    case elib_pg:query(Sql, [Uid, KindId]) of
        {ok, [#{<<"count">> := Count}]} ->
            Count;
        _ ->
            0
    end.

%% @doc 删除用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @return {ok, Count} 删除成功返回影响行数 | {error, Reason} 删除失败
%% @example user_collect_repo:delete(2, <<"cqi6od1pa9gjnlt3a5a0">>).
-spec delete(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete(Uid, KindId) ->
    Tb = tablename(),
    Where = <<" WHERE user_id = $1 AND kind_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    case elib_pg:execute_count(Sql, [Uid, KindId]) of
        {ok, Count} -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新用户收藏
%% @param Uid 用户ID
%% @param KindId 收藏类型ID
%% @param Data 要更新的数据map
%% @return {ok, Count} 更新成功返回影响行数 | {error, Reason} 更新失败
%% @example user_collect_repo:update(2, <<"cqi6od1pa9gjnlt3a5a0">>, #{<<"updated_at">> => elib_dt:now()}).
-spec update(integer(), binary(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update(Uid, KindId, Data) ->
    Table = tablename(),
    Where = <<"user_id = $1 AND kind_id = $2">>,
    elib_pg:update(Table, Data, Where, [Uid, KindId]).

%% ===================================================================
%% E2EE 收藏 v1（迁移 227 / PLAN 2026-10-24-e2ee-collect §7.2/§7.3）
%% 单行模型：每 (user_id, kind_id) 至多一行（uk_user_collect_uid_kind 跨
%% status），行内 status 表达活动/墓碑；sealed 判据 key_version IS NOT NULL。
%% ===================================================================

-define(SEALED_ROW_COLUMNS,
    <<"id, status, generation, key_version, info, attach_file_hash256, updated_at">>
).

%% @doc 取该 (user_id, kind_id) 的任意 status 行（单行模型：至多一行）。
%% 事务版供 v1 事务内读取（与后续 UPDATE 同连接保证可见性）。
-spec find_any_by_uid_kind_id(integer(), binary()) -> not_found | {ok, map()}.
find_any_by_uid_kind_id(Uid, KindId) ->
    find_any_by_uid_kind_id_tx(undefined, Uid, KindId).

-spec find_any_by_uid_kind_id_tx(pid() | undefined, integer(), binary()) ->
    not_found | {ok, map()}.
find_any_by_uid_kind_id_tx(undefined, Uid, KindId) ->
    handle_find_any(elib_pg:query(find_any_sql(), [Uid, KindId]));
find_any_by_uid_kind_id_tx(Conn, Uid, KindId) when is_pid(Conn) ->
    handle_find_any(elib_pg:query(Conn, find_any_sql(), [Uid, KindId])).

handle_find_any({ok, [Row | _]}) ->
    {ok, Row};
handle_find_any({ok, []}) ->
    not_found;
handle_find_any({error, Reason}) ->
    erlang:error({collect_find_failed, Reason}).

find_any_sql() ->
    <<"SELECT ", ?SEALED_ROW_COLUMNS/binary, " FROM ", (tablename())/binary,
        " WHERE user_id = $1 AND kind_id = $2">>.

%% @doc sealed 行存在性（任意 status）——legacy add/change 守卫用（§9）。
-spec sealed_exists(integer(), binary()) -> boolean().
sealed_exists(Uid, KindId) ->
    Sql =
        <<"SELECT 1 FROM ", (tablename())/binary,
            " WHERE user_id = $1 AND kind_id = $2 AND key_version IS NOT NULL LIMIT 1">>,
    case elib_pg:query(Sql, [Uid, KindId]) of
        {ok, [_ | _]} -> true;
        {ok, []} -> false;
        _ -> false
    end.

%% @doc 用户级 key_version 高水位门（§7.2）：
%% INSERT ON CONFLICT DO UPDATE 对冲突行取行锁（等价按用户写锁，串行化
%% 同用户全部 sealed 写）；提交值 >= 高水位方可继续，> 则同事务推进。
%% 锁顺序约定：先 key_state 行，后 user_collect 行（全路径一致）。
-spec key_state_gate_tx(pid(), integer(), integer()) -> ok | no_return().
key_state_gate_tx(Conn, Uid, KeyVersion) ->
    {ok, Hw} = key_state_lock_tx(Conn, Uid),
    case KeyVersion >= Hw of
        true when KeyVersion > Hw ->
            AdvanceSql =
                <<"UPDATE public.user_collect_key_state ",
                    "SET max_key_version = $2, updated_at = now() WHERE user_id = $1">>,
            {ok, _} = elib_pg:execute(Conn, AdvanceSql, [Uid, KeyVersion]),
            ok;
        true ->
            ok;
        false ->
            throw({abort_tx, key_version_rejected})
    end.

%% @doc 仅取用户级行锁（INSERT ON CONFLICT DO UPDATE 对冲突行加锁）并
%% 返回当前高水位，不判定不推进——供不携带新 key_version 的写路径
%% （墓碑化）串行化用（§7.2 锁顺序：先 key_state，后 user_collect）。
-spec key_state_lock_tx(pid(), integer()) -> {ok, integer()} | no_return().
key_state_lock_tx(Conn, Uid) ->
    LockSql =
        <<"INSERT INTO public.user_collect_key_state (user_id, max_key_version) ",
            "VALUES ($1, 0) ON CONFLICT (user_id) DO UPDATE SET updated_at = now() ",
            "RETURNING max_key_version">>,
    case elib_pg:query(Conn, LockSql, [Uid]) of
        {ok, [#{<<"max_key_version">> := Hw}]} ->
            {ok, Hw};
        {error, Reason} ->
            erlang:error({key_state_lock_failed, Reason})
    end.

%% @doc v1 创建：INSERT 新行（generation=1）。触发器 INSERT 分支无操作；
%% 引用计数 +1 归应用层 save（调用方在事务内先 attachment_ds:save）。
-spec insert_sealed_tx(pid(), integer(), integer(), binary(), binary(), integer(), [binary()]) ->
    ok | no_return().
insert_sealed_tx(Conn, Uid, Kind, KindId, InfoSealed, KeyVersion, AttachHashes) ->
    NowTs = elib_dt:now(),
    Data = #{
        <<"id">> => elib_tsid:generate(user_collect),
        <<"user_id">> => Uid,
        <<"kind">> => Kind,
        <<"kind_id">> => KindId,
        <<"source">> => <<>>,
        <<"remark">> => <<>>,
        <<"info">> => InfoSealed,
        <<"attach_file_hash256">> => elib_cnv:implode(<<",">>, AttachHashes),
        <<"created_at">> => NowTs,
        <<"updated_at">> => NowTs,
        <<"status">> => 1,
        <<"generation">> => 1,
        <<"key_version">> => KeyVersion
    },
    {Sql, Params} = elib_pg_sql:insert(<<"public.user_collect">>, Data, <<>>),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({collect_sealed_insert_failed, Reason})
    end.

%% 单行模型 UPDATE 计数归一：1 行命中为 Hit 标签，0 行幂等 noop，多行兜底 Hit。
update_count_result({ok, 1}, Hit) -> {ok, Hit};
update_count_result({ok, 0}, _Hit) -> {ok, noop};
update_count_result({ok, _}, Hit) -> {ok, Hit}.

%% @doc v1 编辑（CAS）：UPDATE ... WHERE generation = ExpectedGen AND status = 1。
%% 引用差集由触发器差分完成（§7.4，不调 save 防双计）。
%% 返回 {ok, updated} | {ok, noop}（0 行，由调用方读行判定幂等/冲突）。
-spec cas_update_sealed_tx(pid(), integer(), binary(), integer(), integer(), binary(), integer(), [
    binary()
]) ->
    {ok, updated} | {ok, noop}.
cas_update_sealed_tx(Conn, Uid, KindId, ExpectedGen, NewGen, InfoSealed, KeyVersion, AttachHashes) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET generation = $3, info = $4, key_version = $5, ",
            "attach_file_hash256 = $6, updated_at = now() ",
            "WHERE user_id = $1 AND kind_id = $2 AND status = 1 AND generation = $7">>,
    run_sealed_update(
        Conn, Sql, Uid, KindId, NewGen, InfoSealed, KeyVersion, AttachHashes, ExpectedGen
    ).

%% @doc v1 复活：复用墓碑行 UPDATE（禁止 INSERT——跨 status 唯一约束，
%% §7.3）。客户端已重新 seal（seal_generation=墓碑G+1）。触发器对差集 +1。
-spec revive_sealed_tx(pid(), integer(), binary(), integer(), binary(), integer(), [binary()]) ->
    {ok, updated} | {ok, noop}.
revive_sealed_tx(Conn, Uid, KindId, TombstoneGen, InfoSealed, KeyVersion, AttachHashes) ->
    NewGen = TombstoneGen + 1,
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 1, generation = $3, info = $4, key_version = $5, ",
            "attach_file_hash256 = $6, updated_at = now() ",
            "WHERE user_id = $1 AND kind_id = $2 AND status = 0 AND generation = $7">>,
    run_sealed_update(
        Conn, Sql, Uid, KindId, NewGen, InfoSealed, KeyVersion, AttachHashes, TombstoneGen
    ).

%% cas/revive 共用执行体：$1 uid $2 kind_id $3 目标 generation $4 sealed info
%% $5 key_version $6 attach 哈希串 $7 守护 generation（cas=期望值，revive=墓碑值）。
run_sealed_update(Conn, Sql, Uid, KindId, NewGen, InfoSealed, KeyVersion, AttachHashes, GuardGen) ->
    update_count_result(
        elib_pg:execute(Conn, Sql, [
            Uid,
            KindId,
            NewGen,
            InfoSealed,
            KeyVersion,
            elib_cnv:implode(<<",">>, AttachHashes),
            GuardGen
        ]),
        updated
    ).

%% @doc sealed 行墓碑化（§7.3/§9）：status=0 + generation+1 + attach=''
%% （触发器对 OLD −1 释放引用）；绝不物理 DELETE。幂等：已 status=0 为 noop。
-spec tombstone_sealed_tx(pid(), integer(), binary()) -> {ok, tombstoned} | {ok, noop}.
tombstone_sealed_tx(Conn, Uid, KindId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 0, generation = generation + 1, attach_file_hash256 = '', ",
            "updated_at = now() WHERE user_id = $1 AND kind_id = $2 AND status = 1 ",
            "AND key_version IS NOT NULL">>,
    update_count_result(elib_pg:execute(Conn, Sql, [Uid, KindId]), tombstoned).
%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%

%% ===================================================================
%% EUnit tests.
%% ===================================================================
