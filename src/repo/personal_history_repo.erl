-module(personal_history_repo).
%%%
%%% personal_history_repo —— 个人消息历史域仓库层（MH-01 合同 v1.1 / MH-02）
%%%
%%% 迁移 276-279 四表（authorization / archive / cursor / key_package）的
%%% 参数化 SQL。本模块零业务策略：状态机裁决、幂等分类、代次比较在
%%% ds/logic 层；此处只提供行级读写与事务内原子原语。
%%%
%%% 合同锚点：
%%% - §1   授权四格 + generation CAS（写入与关闭锁同一授权行）
%%% - §2.3 upload 幂等键 (account,domain,conv,msg_id) 全局唯一（W1：幂等键
%%%        与归档同事务落库，由调用方在 with_tx 内组合本模块原语）
%%% - §10.4 nonce 唯一 (account,domain,key_version,nonce)
%%% - W3    disable 的 OFF_PURGE_PENDING 可重入清理
%%%

-export([auth_tablename/0, archive_tablename/0]).
%% cursor 与 key package 组已拆至 personal_history_cursor_repo / personal_history_key_package_repo
%%（elvis no_god_modules：37 函数超 30；调用面与 SQL 逐字未动，仅换模块前缀）。
% authorization
-export([find_authorization/2, lock_authorization_tx/3, insert_authorization_tx/5]).
-export([enable_authorization_tx/5, cas_disable_tx/4, mark_purged_tx/4, reenter_purge_tx/3]).
% archive
-export([insert_archive_tx/2, find_archive_tx/5, find_msg_id_by_nonce_tx/5]).
-export([find_accepted_message_tx/5]).
-export([advance_archive_tx/2, mark_deleted_tx/5, mark_revoked_tx/4]).
-export([changes_since/4, changes_since_tx/6, page_archive/6, max_event_seq/2]).
-export([changes_exceed_window_tx/6, delete_archive_by_account_tx/3]).
-export([snapshot_bounds_tx/4, snapshot_rows_tx/7]).
% cursor
% key package

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Tablenames
%% ===================================================================

-spec auth_tablename() -> binary().
auth_tablename() ->
    elib_pg_sql:public_tablename(<<"personal_history_authorization">>).

-spec archive_tablename() -> binary().
archive_tablename() ->
    elib_pg_sql:public_tablename(<<"personal_history_archive">>).

%% ===================================================================
%% Authorization（§1 状态机 / generation CAS）
%% ===================================================================

%% @doc 读授权行；无行 = 默认 OFF（零授权，MSG-H01）。
-spec find_authorization(integer(), binary()) -> {ok, map() | undefined} | {error, term()}.
find_authorization(AccountId, Domain) ->
    Sql = authorization_select(),
    elib_pg:query_judged(Sql, [AccountId, Domain], {ok, undefined}).

%% @doc 事务内 FOR UPDATE 锁授权行（写入与关闭锁同一行，W1/W4 竞态定序）。
-spec lock_authorization_tx(pid(), integer(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
lock_authorization_tx(Conn, AccountId, Domain) ->
    Sql = <<(authorization_select())/binary, " FOR UPDATE">>,
    elib_pg:query_judged(Conn, Sql, [AccountId, Domain], {ok, undefined}).

authorization_select() ->
    <<
        "SELECT state, generation, enabled_at, purge_started_at, purge_finished_at,",
        " client_instance_id FROM ",
        (auth_tablename())/binary,
        " WHERE account_id = $1 AND domain = $2"
    >>.

%% @doc 首次 opt-in：插入 ON 行（generation=1 起点）。唯一键兜底并发。
-spec insert_authorization_tx(pid(), integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
insert_authorization_tx(Conn, AccountId, Domain, Generation, ClientInstanceId) ->
    Sql =
        <<"INSERT INTO ", (auth_tablename())/binary,
            " (account_id, domain, state, generation, enabled_at, client_instance_id)"
            " VALUES ($1, $2, 'ON', $3, CURRENT_TIMESTAMP, $4)"
            " ON CONFLICT (account_id, domain) DO NOTHING"
            " RETURNING state, generation, enabled_at">>,
    Params = account_params(AccountId, Domain, Generation, ClientInstanceId),
    elib_pg:query_judged(Conn, Sql, Params, {error, conflict}).

%% @doc 重新启用（OFF / OFF_PURGED → ON，原子提升 generation+1，新代次）。
-spec enable_authorization_tx(pid(), integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
enable_authorization_tx(Conn, AccountId, Domain, FromGeneration, ClientInstanceId) ->
    Sql =
        <<"UPDATE ", (auth_tablename())/binary,
            " SET state = 'ON', generation = generation + 1, enabled_at = CURRENT_TIMESTAMP,",
            " purge_started_at = NULL, purge_finished_at = NULL, client_instance_id = $3,",
            " updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND state IN ('OFF','OFF_PURGED')"
            " AND generation = $4"
            " RETURNING state, generation, enabled_at">>,
    Params = account_params(AccountId, Domain, ClientInstanceId, FromGeneration),
    elib_pg:query_judged(Conn, Sql, Params, {error, conflict}).

%% @doc disable 第一步（W3 删账先于密文删除）：ON → OFF_PURGE_PENDING，
%% 原子 generation+1；CAS 条件 state='ON' AND generation=$3。
-spec cas_disable_tx(pid(), integer(), binary(), integer()) ->
    {ok, map()} | {ok, none} | {error, term()}.
cas_disable_tx(Conn, AccountId, Domain, FromGeneration) ->
    Sql =
        <<"UPDATE ", (auth_tablename())/binary,
            " SET state = 'OFF_PURGE_PENDING', generation = generation + 1,",
            " purge_started_at = CURRENT_TIMESTAMP, enabled_at = NULL,",
            " client_instance_id = NULL, updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND state = 'ON' AND generation = $3"
            " RETURNING state, generation, purge_started_at">>,
    Params = [AccountId, Domain, FromGeneration],
    elib_pg:query_judged(Conn, Sql, Params, {ok, none}).

%% @doc purge 完成落账：OFF_PURGE_PENDING → OFF_PURGED（防复活标记本体：
%% account/domain/generation/purged_at 留存，跨代复活被拒）。
-spec mark_purged_tx(pid(), integer(), binary(), integer()) -> ok | {error, term()}.
mark_purged_tx(Conn, AccountId, Domain, Generation) ->
    Sql =
        <<"UPDATE ", (auth_tablename())/binary,
            " SET state = 'OFF_PURGED', purge_finished_at = CURRENT_TIMESTAMP,",
            " updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND state = 'OFF_PURGE_PENDING'"
            " AND generation = $3">>,
    elib_pg:execute_ok(Conn, Sql, [AccountId, Domain, Generation]).

%% @doc W3 可重入：崩在 purge 中途时再次进入同一代次的清理（不动 generation）。
-spec reenter_purge_tx(pid(), integer(), binary()) ->
    {ok, integer()} | {ok, none} | {error, term()}.
reenter_purge_tx(Conn, AccountId, Domain) ->
    Sql =
        <<"UPDATE ", (auth_tablename())/binary,
            " SET updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND state = 'OFF_PURGE_PENDING'"
            " RETURNING generation">>,
    case elib_pg:query(Conn, Sql, [AccountId, Domain]) of
        {ok, [#{<<"generation">> := Generation}]} -> {ok, Generation};
        {ok, []} -> {ok, none};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Archive（§2.3 幂等键 / §10.4 nonce / event_seq 锚）
%% ===================================================================

-spec find_accepted_message_tx(pid(), integer(), binary(), binary(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
find_accepted_message_tx(Conn, AccountId, Domain, ConversationId, MsgId) ->
    Sql = <<
        "SELECT fact.conv_seq, fact.action_version FROM (",
        (accepted_message_sql(<<"$1">>))/binary,
        ") fact JOIN ",
        (auth_tablename())/binary,
        " auth ON auth.account_id = $2 AND auth.domain = $3 AND auth.state='ON'",
        " WHERE $2 IN (fact.from_id, fact.to_id) AND fact.conversation_id = $4",
        " AND fact.accepted_at >= auth.enabled_at"
    >>,
    Params = [MsgId, AccountId, Domain, ConversationId],
    elib_pg:query_judged(Conn, Sql, Params, {ok, undefined}).

%% Only the immutable accept time qualifies. Conflicting copies or edit versions fail closed.

accepted_message_sql(MsgRef) ->
    <<
        "SELECT f.from_id, f.to_id, f.conv_seq, f.accepted_at, r.action_version,",
        " 'c2c:' || least(f.from_id,f.to_id) || ':' || greatest(f.from_id,f.to_id) AS conversation_id",
        " FROM (SELECT min(from_id) AS from_id, min(to_id) AS to_id, min(conv_seq) AS conv_seq,",
        " min(accepted_at) AS accepted_at, min(formal_version) AS formal_version",
        " FROM (SELECT from_id,to_id,conv_seq,accepted_at,expire_at,action_version AS formal_version",
        " FROM public.msg_c2c WHERE msg_id=",
        MsgRef/binary,
        " UNION ALL SELECT from_id,to_id,conv_seq,accepted_at,expire_at,NULL::bigint",
        " FROM public.msg_store_staging WHERE type='c2c' AND processed_at IS NULL AND msg_id=",
        MsgRef/binary,
        " UNION ALL SELECT from_id,to_id,conv_seq,accepted_at,expire_at,NULL::bigint",
        " FROM public.msg_store WHERE chat_type='c2c' AND msg_id=",
        MsgRef/binary,
        ") copies HAVING count(from_id)=count(*) AND count(to_id)=count(*)",
        " AND min(from_id)=max(from_id) AND min(to_id)=max(to_id)",
        " AND min(conv_seq)=max(conv_seq) AND min(conv_seq)>0 AND min(accepted_at)=max(accepted_at)",
        " AND min(formal_version) IS NOT DISTINCT FROM max(formal_version)",
        " AND (min(expire_at) IS NULL OR min(expire_at)>statement_timestamp())) f",
        " CROSS JOIN LATERAL (SELECT count(*) AS action_version FROM public.c2c_action_receipt",
        " WHERE original_msg_id=",
        MsgRef/binary,
        " AND actor_uid=f.from_id AND peer_uid=f.to_id AND action='edit' AND consumed_at IS NOT NULL) r",
        " WHERE (f.formal_version IS NULL OR f.formal_version=r.action_version)",
        " AND NOT EXISTS (SELECT 1 FROM public.c2c_action_receipt WHERE original_msg_id=",
        MsgRef/binary,
        " AND actor_uid=f.from_id AND peer_uid=f.to_id AND action='revoke')",
        " AND NOT EXISTS (SELECT 1 FROM public.enterprise_message_origin",
        " WHERE conversation_kind='direct' AND msg_id=",
        MsgRef/binary,
        ")"
    >>.

%% 事件流/快照查询共享：SQL 先经 with_archive_proof 注入授权行 CTE。
stream_query(Order, Sql, Params) ->
    elib_pg:query(with_archive_proof(Sql, Order), Params).

stream_query_tx(Conn, Order, Sql, Params) ->
    elib_pg:query(Conn, with_archive_proof(Sql, Order), Params).

with_archive_proof(Sql, Order) ->
    <<
        "SELECT a.*, CASE WHEN fact.from_id IS NOT NULL THEN jsonb_build_object(",
        " 'from_id',fact.from_id::text, 'to_id',fact.to_id::text,",
        " 'conv_seq',fact.conv_seq, 'action_version',fact.action_version,",
        " 'accepted_at_ms',floor(extract(epoch FROM fact.accepted_at)*1000)::bigint)",
        " END AS accepted_message FROM (",
        Sql/binary,
        ") a",
        " LEFT JOIN ",
        (auth_tablename())/binary,
        " auth ON auth.account_id=a.account_id",
        " AND auth.domain=a.domain AND auth.generation=a.generation AND auth.state='ON'",
        " LEFT JOIN LATERAL (",
        (accepted_message_sql(<<"a.msg_id">>))/binary,
        ") fact ON a.deleted_at IS NULL",
        " AND a.account_id IN (fact.from_id,fact.to_id) AND fact.conversation_id=a.conversation_id",
        " AND fact.conv_seq=a.conv_seq AND fact.action_version=a.action_version AND fact.accepted_at>=auth.enabled_at",
        " ORDER BY a.",
        Order/binary
    >>.

%% @doc 事务内插入归档行；幂等键与归档同一 INSERT（W1）。
%% 语义键冲突经 ON CONFLICT DO NOTHING 吸收（0 行=已存在，禁止在事务内
%% 撞 23505 中止后续命令）；仅 nonce 约束仍可能抛 unique_violation
%%（调用方须停止后续查询并按 nonce_reused 归一）。
%% Row 需含 account_id/domain/conversation_id/msg_id/action_version/conv_seq/
%% generation/key_version/nonce/ciphertext/envelope_meta。
-spec insert_archive_tx(pid(), map()) ->
    {ok, map()} | {ok, exists} | {error, unique_violation} | {error, term()}.
insert_archive_tx(Conn, Row) ->
    Sql =
        <<"INSERT INTO ", (archive_tablename())/binary,
            " (account_id, domain, conversation_id, msg_id, action_version, conv_seq,",
            " generation, key_version, nonce, ciphertext, envelope_meta)",
            " VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11::jsonb)",
            " ON CONFLICT (account_id, domain, conversation_id, msg_id) DO NOTHING",
            " RETURNING id, action_version, conv_seq, event_seq, created_at">>,
    Params = archive_row_params(Row),
    archive_write_result(elib_pg:query_judged(Conn, Sql, Params, {ok, exists})).

%% 归档插入专用 11 列参数表（$1..$10 + envelope_meta::jsonb；含 generation）。
archive_row_params(Row) ->
    [
        maps:get(account_id, Row),
        maps:get(domain, Row),
        maps:get(conversation_id, Row),
        maps:get(msg_id, Row),
        maps:get(action_version, Row),
        maps:get(conv_seq, Row),
        maps:get(generation, Row)
        | archive_envelope_params(Row)
    ].

archive_envelope_params(Row) ->
    [
        maps:get(key_version, Row),
        maps:get(nonce, Row),
        maps:get(ciphertext, Row),
        jsone:encode(maps:get(envelope_meta, Row))
    ].

%% @doc 事务内按 nonce 唯一维度定位既有行（§10.4 前置检查，FOR UPDATE 定序）。
-spec find_msg_id_by_nonce_tx(pid(), integer(), binary(), integer(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
find_msg_id_by_nonce_tx(Conn, AccountId, Domain, KeyVersion, Nonce) ->
    Sql =
        <<"SELECT msg_id FROM ", (archive_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2 AND key_version = $3 AND nonce = $4",
            " FOR UPDATE">>,
    Params = account_params(AccountId, Domain, KeyVersion, Nonce),
    elib_pg:query_judged(Conn, Sql, Params, {ok, undefined}).

archive_select() ->
    <<
        "SELECT id, account_id, domain, conversation_id, msg_id, action_version, conv_seq, generation,",
        " key_version, nonce, ciphertext, envelope_meta, event_seq, deleted_at,",
        " created_at, updated_at FROM ",
        (archive_tablename())/binary
    >>.

%% @doc 幂等键冲突后的原语义记录回读（重复上传返回原结果不覆盖）。
-spec find_archive_tx(pid(), integer(), binary(), binary(), binary()) ->
    {ok, map() | undefined} | {error, term()}.
find_archive_tx(Conn, AccountId, Domain, ConversationId, MsgId) ->
    Sql =
        <<
            (archive_select())/binary,
            " WHERE account_id = $1 AND domain = $2 AND conversation_id = $3 AND msg_id = $4"
        >>,
    Params = account_params(AccountId, Domain, ConversationId, MsgId),
    elib_pg:query_judged(Conn, Sql, Params, {ok, undefined}).

%% @doc 终态推进（编辑/撤回走 action_version 终态）：仅当已有版本更低时
%% 更新，并发放新的事件序；nonce 唯一性冲突原样返回。
-spec advance_archive_tx(pid(), map()) ->
    {ok, map()} | {ok, none} | {error, unique_violation} | {error, term()}.
advance_archive_tx(Conn, Row) ->
    Sql =
        <<"UPDATE ", (archive_tablename())/binary,
            " SET action_version = $5, conv_seq = $6, key_version = $7, nonce = $8,",
            " ciphertext = $9, envelope_meta = $10::jsonb, event_seq = DEFAULT,",
            " updated_at = CURRENT_TIMESTAMP"
            " WHERE account_id = $1 AND domain = $2 AND conversation_id = $3 AND msg_id = $4"
            " AND action_version < $5 AND deleted_at IS NULL"
            " RETURNING id, action_version, conv_seq, event_seq, created_at">>,
    %% 10 列参数表（$1..$4 幂等键 + $5..$10 新版本载荷）：与 insert 的 11 列
    %% 参数表不同源（UPDATE 无 generation 列），不得共享（聚焦 EUnit 曾抓到
    %% 共享导致的参数错位回归，特此注记）。
    Params = [
        maps:get(account_id, Row),
        maps:get(domain, Row),
        maps:get(conversation_id, Row),
        maps:get(msg_id, Row),
        maps:get(action_version, Row),
        maps:get(conv_seq, Row)
        | archive_envelope_params(Row)
    ],
    archive_write_result(elib_pg:query_judged(Conn, Sql, Params, {ok, none})).

archive_write_result({error, {error, _, <<"23505">>, unique_violation, _, _}}) ->
    {error, unique_violation};
archive_write_result(Result) ->
    Result.

%% @doc 墓碑（MSG-H15）：按幂等键置 deleted_at（撤回终态在归档侧的投影）。
-spec mark_deleted_tx(pid(), integer(), binary(), binary(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
mark_deleted_tx(Conn, AccountId, Domain, ConversationId, MsgId) ->
    Sql =
        <<"UPDATE ", (archive_tablename())/binary,
            " SET deleted_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP, event_seq = DEFAULT"
            " WHERE account_id = $1 AND domain = $2 AND conversation_id = $3 AND msg_id = $4"
            " AND deleted_at IS NULL">>,
    Params = account_params(AccountId, Domain, ConversationId, MsgId),
    elib_pg:execute_count(Conn, Sql, Params).

%% Caller holds the C2C original lock; account locks preserve commit order for each cursor.
-spec mark_revoked_tx(pid(), integer(), integer(), binary()) -> ok | {error, term()}.
mark_revoked_tx(Conn, ActorUid, PeerUid, MsgId) ->
    Conv = msg_archive_repo:conv_key(<<"c2c">>, ActorUid, PeerUid),
    Archive = archive_tablename(),
    Sql = <<
        "WITH enabled AS MATERIALIZED (SELECT a.account_id,a.domain,a.generation FROM ",
        (auth_tablename())/binary,
        " a WHERE a.account_id IN ($1,$2) AND a.state='ON' AND EXISTS (SELECT 1 FROM ",
        Archive/binary,
        " p WHERE p.account_id=a.account_id AND p.domain=a.domain AND p.generation=a.generation",
        " AND p.conversation_id=$3 AND p.msg_id=$4 AND p.deleted_at IS NULL)",
        " ORDER BY a.account_id,a.domain FOR UPDATE OF a) UPDATE ",
        Archive/binary,
        " p SET deleted_at=CURRENT_TIMESTAMP,updated_at=CURRENT_TIMESTAMP,event_seq=DEFAULT",
        " FROM enabled a WHERE p.account_id=a.account_id AND p.domain=a.domain AND p.generation=a.generation",
        " AND p.conversation_id=$3 AND p.msg_id=$4 AND p.deleted_at IS NULL"
    >>,
    case elib_pg:execute_count(Conn, Sql, [ActorUid, PeerUid, Conv, MsgId]) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%% @doc changes 源：event_seq 单调锚（W2：只读已 commit 行，天然满足）。
%% 含墓碑行（删除事件进变更流）。
-spec changes_since(integer(), binary(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
changes_since(AccountId, Domain, AfterEventSeq, Limit) ->
    Sql =
        <<
            (archive_select())/binary,
            " WHERE account_id = $1 AND domain = $2 AND event_seq > $3"
            " ORDER BY event_seq ASC LIMIT $4"
        >>,
    elib_pg:query(Sql, [AccountId, Domain, AfterEventSeq, Limit]).

%% @doc 事务内版 changes 源（与授权锁同一快照）。
-spec changes_since_tx(pid(), integer(), binary(), integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
changes_since_tx(Conn, AccountId, Domain, Generation, AfterEventSeq, Limit) ->
    Sql =
        <<
            (archive_select())/binary,
            " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND event_seq > $4"
            " ORDER BY event_seq ASC LIMIT $5"
        >>,
    Params = event_window_params(AccountId, Domain, Generation, AfterEventSeq, Limit),
    stream_query_tx(Conn, <<"event_seq ASC">>, Sql, Params).

%% @doc page 分页快照：conv_seq DESC；墓碑行一并返回（稳定合并版本+tombstone）。
-spec page_archive(integer(), binary(), integer(), binary(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page_archive(AccountId, Domain, Generation, ConversationId, Before, Limit) ->
    Sql =
        <<
            (archive_select())/binary,
            " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND conversation_id = $4",
            " AND ($5::bigint = 0 OR conv_seq < $5)"
            " ORDER BY conv_seq DESC LIMIT $6"
        >>,
    Params = archive_page_params(AccountId, Domain, Generation, ConversationId, Before, Limit),
    stream_query(<<"conv_seq DESC">>, Sql, Params).

%% Holding the authorization row makes the cut independent of uncommitted mutations.
-spec snapshot_bounds_tx(pid(), integer(), binary(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | {error, term()}.
snapshot_bounds_tx(Conn, AccountId, Domain, Generation) ->
    Sql =
        <<"SELECT COALESCE(MAX(id), 0) AS cut, COALESCE(MAX(event_seq), 0) AS watermark FROM ",
            (archive_tablename())/binary, " WHERE account_id=$1 AND domain=$2 AND generation=$3">>,
    Params = [AccountId, Domain, Generation],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"cut">> := Cut, <<"watermark">> := Watermark}]} -> {ok, Cut, Watermark};
        {error, _} = Error -> Error
    end.

-spec snapshot_rows_tx(pid(), integer(), binary(), integer(), integer(), integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
snapshot_rows_tx(Conn, AccountId, Domain, Generation, Cut, After, Limit) ->
    Sql =
        <<
            (archive_select())/binary,
            " WHERE account_id=$1 AND domain=$2 AND generation=$3 AND id>$4 AND id<=$5",
            " ORDER BY id ASC LIMIT $6"
        >>,
    Params = archive_page_params(AccountId, Domain, Generation, After, Cut, Limit),
    stream_query_tx(Conn, <<"id ASC">>, Sql, Params).

%% The identity sequence is global. A cursor window is measured in this account's pending rows.
-spec changes_exceed_window_tx(pid(), integer(), binary(), integer(), integer(), pos_integer()) ->
    {ok, boolean()} | {error, term()}.
changes_exceed_window_tx(Conn, AccountId, Domain, Generation, Cursor, Window) ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM ", (archive_tablename())/binary,
            " WHERE account_id = $1 AND domain = $2 AND generation = $3 AND event_seq > $4",
            " ORDER BY event_seq OFFSET $5 LIMIT 1) AS outside_window">>,
    Params = event_window_params(AccountId, Domain, Generation, Cursor, Window),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [#{<<"outside_window">> := Outside}]} -> {ok, Outside};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 池连接版最大事件序（status 的 upload_cursor）。
-spec max_event_seq(integer(), binary()) -> {ok, integer()} | {error, term()}.
max_event_seq(AccountId, Domain) ->
    case elib_pg:query(max_event_seq_sql(), [AccountId, Domain]) of
        {ok, [#{<<"max_seq">> := MaxSeq}]} -> {ok, MaxSeq};
        {error, Reason} -> {error, Reason}
    end.

max_event_seq_sql() ->
    <<"SELECT COALESCE(MAX(event_seq), 0) AS max_seq FROM ", (archive_tablename())/binary,
        " WHERE account_id = $1 AND domain = $2">>.

%% @doc purge：事务内删除该账号域全部归档密文（在线清理部分）。
-spec delete_archive_by_account_tx(pid(), integer(), binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
delete_archive_by_account_tx(Conn, AccountId, Domain) ->
    Sql =
        <<"DELETE FROM ", (archive_tablename())/binary, " WHERE account_id = $1 AND domain = $2">>,
    elib_pg:execute_count(Conn, Sql, [AccountId, Domain]).

account_params(AccountId, Domain, Third, Fourth) ->
    [AccountId, Domain, Third, Fourth].

event_window_params(AccountId, Domain, Generation, Cursor, Limit) ->
    [AccountId, Domain, Generation, Cursor, Limit].

archive_page_params(AccountId, Domain, Generation, FirstKey, SecondKey, Limit) ->
    [AccountId, Domain, Generation, FirstKey, SecondKey, Limit].

%% ===================================================================
%% EUnit tests.
%% ===================================================================
