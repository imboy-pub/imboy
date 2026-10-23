-module(attachment_reference_guard_repo).
-moduledoc "附件引用锁、GC资格与孤儿候选查询；保留统一意图入口的资格合同。".
-export([
    bind_reference_lock_tx/2,
    refresh_gc_verified_tx/2,
    orphan_predicate/0,
    orphan_stats/1,
    orphan_list_for_delete/1,
    delete_orphan_tx/3,
    orphan_path_tx/3
]).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"attachment">>).

%% W3-01 引用首绑/释放同锁（02 §4）：业务关系写入口先取与 intent/GC 相同的
%% 对象事务锁（迁移 168 的 hashtextextended(path,168)），锁内复核 lifecycle——
%% delete_intent/deleted（墓碑）拒绝新增引用，绑定事务改变零业务行。
-spec bind_reference_lock_tx(any(), binary()) -> ok | {error, tombstone | term()}.
bind_reference_lock_tx(Conn, Path) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT pg_advisory_xact_lock(hashtextextended($1,168)),"
                " lifecycle_state FROM public.attachment WHERE path=$1"
                " FOR UPDATE"
            >>,
            [Path]
        )
    of
        {ok, [#{<<"lifecycle_state">> := <<"live">>}]} ->
            ok;
        {ok, [#{<<"lifecycle_state">> := _}]} ->
            {error, tombstone};
        {ok, []} ->
            %% 尚无元数据行（上传绑定前）：仅取对象锁，与 intent 锁域对齐
            case
                elib_pg:query(
                    Conn,
                    <<"SELECT pg_advisory_xact_lock(hashtextextended($1,168))">>,
                    [Path]
                )
            of
                {ok, _} -> ok;
                {error, R} -> {error, R}
            end;
        {error, R} ->
            {error, R}
    end.

%% W3-01 gc_verified 覆盖证明（02 §5）：gc_verified = 「引用零」的可证明态。
%% 收藏哈希为逗号列表（迁移 26 列语义，视频/多图收藏多个哈希拼接），
%% 匹配口径与 imboy_user_collect_fun 触发器一致：ANY(string_to_array)。
%% W3-02 两域统一资格（02 §5 统一判据 + 企业应用附件行）：proven 与 intent
%% 函数锁后复核同一引用集（group_file 绑定 / 收藏 hash / moment 媒体 /
%% WAR 行 / EAR hold 或未 purged / talk_record 有效引用[00000212]）+ 明文 +
%% 完整位置；绑定活跃/密文/位置缺失/治理 hold 一律 false。资格标志陈旧 true
%% 不破坏安全——intent 在对象锁内重查全部引用。
-spec refresh_gc_verified_tx(any(), binary()) -> {ok, boolean()} | {error, term()}.
refresh_gc_verified_tx(Conn, Path) ->
    Sql =
        <<
            "UPDATE public.attachment a SET gc_verified = c.proven"
            " FROM (SELECT a2.path,"
            "   (a2.cipher IS NULL"
            "    AND a2.storage_driver IS NOT NULL AND a2.storage_namespace IS NOT NULL"
            "    AND a2.storage_container IS NOT NULL AND a2.storage_locator_sha256 IS NOT NULL"
            "    AND (a2.group_file_id IS NULL OR NOT EXISTS"
            "      (SELECT 1 FROM public.group_file gf WHERE gf.id = a2.group_file_id))"
            "    AND NOT EXISTS (SELECT 1 FROM public.user_collect uc"
            "      WHERE uc.attach_file_hash256 <> ''"
            "        AND a2.file_hash256 = ANY(string_to_array(uc.attach_file_hash256, ','))"
            "        AND uc.status = 1)"
            "    AND NOT EXISTS (SELECT 1 FROM public.moment_post mp"
            "      WHERE position(a2.path in mp.media::text) > 0)"
            "    AND NOT fn_talk_record_war_blocks(a2.id)"
            "    AND NOT EXISTS (SELECT 1 FROM public.enterprise_attachment_retention ear"
            "      WHERE ear.attachment_id = a2.id"
            "        AND (ear.hold_state = 'held'"
            "             OR ear.purge_state IS DISTINCT FROM 'purged'))"
            %% T06（00000212）：talk_record 有效引用（含未到期 processing lease）
            "    AND NOT EXISTS (SELECT 1 FROM talk_record_attachment_ref trar"
            "      WHERE trar.attachment_id = a2.id AND trar.state = 'bound'"
            "        AND (trar.lease_until IS NULL OR trar.lease_until > now()))"
            "   ) AS proven"
            "  FROM public.attachment a2 WHERE a2.path = $1) c"
            " WHERE a.path = c.path RETURNING a.gc_verified"
        >>,
    case elib_pg:query(Conn, Sql, [Path]) of
        {ok, [#{<<"gc_verified">> := V}]} -> {ok, V};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% One predicate keeps preview, candidate selection and transactional deletion aligned.
%% T06（00000212）：talk_record 有效业务引用（bound 且 lease 未到期）阻止孤儿
%% 清理——谓词片段与 talk_record_attachment_ref_repo:valid_reference_sql/1、
%% 迁移 00000212 的 fn_v5_intent_ordinary_delete 守卫逐字同义。
orphan_predicate() ->
    <<
        " a.status = 1 AND a.referer_time = 0"
        " AND a.created_at < NOW() - ($1 * INTERVAL '1 day')"
        " AND (a.last_referer_at IS NULL"
        " OR a.last_referer_at < NOW() - ($1 * INTERVAL '1 day'))"
        " AND NOT fn_talk_record_war_blocks(a.id)"
        " AND NOT EXISTS (SELECT 1 FROM enterprise_attachment_retention r"
        " WHERE r.attachment_id = a.id)"
        " AND NOT EXISTS (SELECT 1 FROM talk_record_attachment_ref trar"
        " WHERE trar.attachment_id = a.id AND trar.state = 'bound'"
        " AND (trar.lease_until IS NULL OR trar.lease_until > now()))"
    >>.

-spec orphan_stats(map()) -> {ok, map()} | {error, term()}.
orphan_stats(Opts) ->
    elib_pg:one(
        [
            <<"SELECT COUNT(*) AS count, COALESCE(SUM(a.size), 0) AS total_size FROM ">>,
            tablename(),
            <<" a WHERE ">>,
            orphan_predicate()
        ],
        [maps:get(age_days, Opts, 30)]
    ).

-spec orphan_list_for_delete(map()) -> {ok, [map()]} | {error, term()}.
orphan_list_for_delete(Opts) ->
    elib_pg:query(
        [
            <<"SELECT a.id, a.path FROM ">>,
            tablename(),
            <<" a WHERE ">>,
            orphan_predicate(),
            <<" LIMIT 500">>
        ],
        [maps:get(age_days, Opts, 30)]
    ).

%% Delete inside the caller's transaction before touching object storage.
%% FK retention checks and row locks remain effective until commit/rollback.
-spec delete_orphan_tx(pid(), integer(), map()) -> {ok, [map()]} | {error, term()}.
delete_orphan_tx(Conn, Id, Opts) ->
    elib_pg:query(
        Conn,
        [
            <<"DELETE FROM ">>,
            tablename(),
            <<" a WHERE a.id = $2 AND ">>,
            orphan_predicate(),
            <<" RETURNING a.path,a.scope">>
        ],
        [maps:get(age_days, Opts, 30), Id]
    ).

%% 孤儿意图的只读定位（收口后行删除由队列 ack 协议负责，意图阶段不删行）。
-spec orphan_path_tx(pid() | any(), integer(), map()) -> {ok, [map()]} | {error, term()}.
orphan_path_tx(Conn, Id, Opts) ->
    elib_pg:query(
        Conn,
        [
            <<"SELECT a.path, a.scope FROM ">>,
            tablename(),
            <<" a WHERE a.id = $2 AND ">>,
            orphan_predicate(),
            <<" LIMIT 1">>
        ],
        [maps:get(age_days, Opts, 30), Id]
    ).
