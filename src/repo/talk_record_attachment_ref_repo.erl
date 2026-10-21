-module(talk_record_attachment_ref_repo).
-moduledoc "talk_record 附件业务引用仓储（DESIGN §10.2；绑定/释放/processing lease/GC 资格谓词）。".
%%%
% talk_record_attachment_ref（迁移 00000211 建表，00000212 扩 lease_until）
% 的仓储层。全部核心写路径是事务内形态（Conn 直连），与附件生命周期 V5
% 的对象锁协议（attachment_repo:bind_reference_lock_tx，迁移 168 的
% hashtextextended(path,168)）同事务线性化：
%
%   * bind_tx/2     —— 首绑/幂等重放/business_key 冲突裁决；
%                       显式"confirmed 真实资产行存在"检查分支，不静默跳过
%                       （02 §4：普通不存在资产行的写入必须锁真实行）。
%   * release_tx/4  —— 幂等释放（删除/移交路径；治理门在 adapter 层）。
%   * grant_processing_lease_tx/5 —— 有期限处理引用（§10.3 complete/合成/
%                       转码；取消=release、超时=release_expired_leases_tx）。
%   * valid_reference_exists_tx/2 —— GC 资格谓词复用查询（bound 且未到期）。
%
% 表约束（209）：UNIQUE(record_id, role, business_key)、
% i_trar_attachment (attachment_id) WHERE state='bound'。
% 有效引用定义（00000212）：state='bound' AND (lease_until IS NULL
% OR lease_until > now())。
%%%

-export([tablename/0]).
-export([
    bind_tx/2,
    release_tx/4,
    release_by_id_tx/2,
    release_record_tx/2,
    grant_processing_lease_tx/6,
    release_job_processing_tx/3,
    release_expired_leases_tx/1,
    valid_reference_exists_tx/2,
    valid_reference_sql/1,
    find_tx/4,
    active_roles_tx/2
]).
%% 企业附件管理页资源分类查询（DESIGN §10.4；workspace_attachment_repo
% 现仅 group/channel，本查询补 talk_record 分类，接线 blocker 见 T06 报告）
-export([scan_organization/2, scan_organization_tx/3]).

-define(TABLE, <<"talk_record_attachment_ref">>).
-define(VALID_ROLES, [
    <<"audio_part">>,
    <<"original_audio">>,
    <<"manifest">>,
    <<"transcoded_audio">>,
    <<"export">>
]).

%% ===================================================================
%% 表名
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(?TABLE).

%% ===================================================================
%% 绑定（§10.2 同事务协议）
%% ===================================================================

%% Bind :: #{
%%   record_id := integer(), attachment_id := integer(),
%%   capture_id => integer() | null,      % audio_part 必带；其余产物可空
%%   role := binary(), business_key := binary(),
%%   lease_until => binary() | null,      % processing lease 到期（timestamptz 文本）
%%   id => integer()                      % 测试注入；缺省 elib_tsid
%% }
%% 返回 {ok, RefId, First :: boolean()}；冲突 fail-closed：
%%   attachment_not_found / confirm_missing / tombstone / attachment_disabled /
%%   scope_mismatch / business_key_conflict / already_released
-spec bind_tx(any(), map()) -> {ok, integer() | binary(), boolean()} | {error, term()}.
bind_tx(
    Conn,
    #{
        record_id := RecordId,
        attachment_id := AttachmentId,
        role := Role,
        business_key := _BusinessKey
    } = Bind
) when
    is_integer(RecordId), RecordId > 0, is_integer(AttachmentId), AttachmentId > 0
->
    case lists:member(Role, ?VALID_ROLES) of
        true -> bind_locked_tx(Conn, Bind);
        false -> {error, invalid_role}
    end;
bind_tx(_Conn, _Bind) ->
    {error, invalid_bind}.

bind_locked_tx(Conn, Bind) ->
    #{attachment_id := AttachmentId} = Bind,
    %% 第一步：定位对象键（无锁读，仅为进入对象锁协议）
    case
        elib_pg:query(
            Conn, <<"SELECT path FROM public.attachment WHERE id=$1">>, [AttachmentId]
        )
    of
        {ok, [#{<<"path">> := Path}]} ->
            bind_with_object_lock_tx(Conn, Bind, Path);
        {ok, []} ->
            {error, attachment_not_found};
        {error, R} ->
            {error, R}
    end.

bind_with_object_lock_tx(Conn, Bind, Path) ->
    %% 第二步：V5 对象锁（advisory 168 + attachment 行 FOR UPDATE；墓碑拒绝）
    case attachment_repo:bind_reference_lock_tx(Conn, Path) of
        ok ->
            bind_verified_tx(Conn, Bind, Path);
        {error, tombstone} ->
            {error, tombstone};
        {error, R} ->
            {error, R}
    end.

bind_verified_tx(Conn, #{record_id := _RecordId} = Bind, Path) ->
    %% 第三步：锁后显式复核"confirmed 真实资产行存在"（§10.2：不能仅调
    %% bind_reference_lock_tx 后忽略不存在元数据的分支）+ scope 归属守卫
    %% （§6：禁止拿其他记录/聊天 attachment_id 拼接——scope=talk_record 且
    %% scope_ref 锁定 record_id，跨 record 复用在此处拒绝）。
    Sql =
        <<
            "SELECT id, scope, scope_ref, lifecycle_state, status FROM public.attachment"
            " WHERE id=$1 AND path=$2 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [maps:get(attachment_id, Bind), Path]) of
        {ok, [Row = #{<<"lifecycle_state">> := <<"live">>, <<"status">> := Status}]} ->
            case Status >= 0 of
                false ->
                    {error, attachment_disabled};
                true ->
                    case
                        {
                            maps:get(<<"scope">>, Row),
                            maps:get(<<"scope_ref">>, Row),
                            ec_cnv:to_binary(maps:get(record_id, Bind))
                        }
                    of
                        {<<"talk_record">>, ScopeRef, RecordIdBin} when
                            ScopeRef =:= RecordIdBin
                        ->
                            governed_insert_tx(Conn, Bind);
                        _ ->
                            {error, scope_mismatch}
                    end
            end;
        {ok, [#{<<"lifecycle_state">> := _}]} ->
            %% 锁检查已拒绝非 live；此处防御性兜底（锁与复核间理论不可达）
            {error, tombstone};
        {ok, []} ->
            %% 对象锁 ok 但资产行不存在 = confirm 丢失/伪造 attachment_id。
            %% 显式失败分支，不静默跳过。
            {error, confirm_missing};
        {error, R} ->
            {error, R}
    end.

governed_insert_tx(C, Bind) ->
    case maps:get(lease_until, Bind, null) of
        null -> governed_permanent_tx(C, Bind);
        _ -> insert_ref_tx(C, Bind)
    end.
governed_permanent_tx(C, Bind) ->
    case elib_pg:query(C, <<"SAVEPOINT talk_record_governed_bind">>, []) of
        {ok, _} ->
            Result =
                case
                    talk_record_governance_repo:register_tx(
                        C, maps:get(record_id, Bind), maps:get(attachment_id, Bind)
                    )
                of
                    ok -> insert_ref_tx(C, Bind);
                    E -> E
                end,
            finish_governed_tx(C, Result);
        Error ->
            Error
    end.

finish_governed_tx(C, {error, _} = Result) ->
    case elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT talk_record_governed_bind">>, []) of
        {ok, _} -> release_governed_tx(C, Result);
        E -> E
    end;
finish_governed_tx(C, Result) ->
    release_governed_tx(C, Result).
release_governed_tx(C, Result) ->
    case elib_pg:query(C, <<"RELEASE SAVEPOINT talk_record_governed_bind">>, []) of
        {ok, _} -> Result;
        E -> E
    end.

insert_ref_tx(Conn, Bind) ->
    #{
        record_id := RecordId,
        attachment_id := AttachmentId,
        role := Role,
        business_key := BusinessKey
    } = Bind,
    CaptureId = maps:get(capture_id, Bind, null),
    LeaseUntil = maps:get(lease_until, Bind, null),
    %% TSID 发号沿用 attachment 命名空间（elib_tsid_inventory 注册集；
    %% id 仅是 bigint 主键，生成器命名空间不承载业务语义）
    RefId = maps:get(id, Bind, elib_tsid:generate(attachment)),
    Now = elib_dt:now(),
    {Sql, Params} = elib_pg_sql:insert(
        tablename(),
        #{
            <<"id">> => RefId,
            <<"record_id">> => RecordId,
            <<"attachment_id">> => AttachmentId,
            <<"capture_id">> => CaptureId,
            <<"role">> => Role,
            <<"business_key">> => BusinessKey,
            <<"state">> => <<"bound">>,
            <<"lease_until">> => LeaseUntil,
            <<"created_at">> => Now
        }
    ),
    Insert =
        [
            Sql,
            <<" ON CONFLICT (record_id, role, business_key) DO NOTHING RETURNING id">>
        ],
    case elib_pg:query(Conn, Insert, Params) of
        {ok, [#{<<"id">> := NewId}]} ->
            {ok, NewId, true};
        {ok, []} ->
            replay_tx(Conn, Bind);
        {error, R} ->
            {error, R}
    end.

%% 幂等重放裁决：同 business_key 已有行——同资产 bound 重放幂等（lease 只
%% 延长）；同 key 异资产 = 真实冲突；已 released = 不可复活（fail-closed）。
replay_tx(
    Conn,
    #{
        record_id := RecordId,
        attachment_id := AttachmentId,
        role := Role,
        business_key := BusinessKey
    } = Bind
) ->
    LeaseUntil = maps:get(lease_until, Bind, null),
    Sql =
        <<"SELECT id, attachment_id, state, lease_until FROM ", (tablename())/binary,
            " WHERE record_id=$1 AND role=$2 AND business_key=$3 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [RecordId, Role, BusinessKey]) of
        {ok, [#{<<"id">> := Id, <<"attachment_id">> := AttachmentId, <<"state">> := <<"bound">>}]} ->
            extend_lease_tx(Conn, Id, LeaseUntil),
            {ok, Id, false};
        {ok, [#{<<"attachment_id">> := AttachmentId, <<"state">> := <<"released">>}]} ->
            {error, already_released};
        {ok, [_]} ->
            {error, business_key_conflict};
        {ok, []} ->
            %% ON CONFLICT 命中后行消失（并发删除）——fail-closed 重试入口
            {error, business_key_conflict};
        {error, R} ->
            {error, R}
    end.

%% lease 只延长不缩短（对齐 EAR retention 只延长语义；普通 bound 重放
%% lease_until 均为 NULL，GREATEST 不改变 NULL）。
extend_lease_tx(Conn, Id, LeaseUntil) when LeaseUntil =/= null, LeaseUntil =/= undefined ->
    _ =
        elib_pg:query(
            Conn,
            <<
                "UPDATE ",
                (tablename())/binary,
                " SET lease_until = GREATEST(lease_until, $2::timestamptz)"
                " WHERE id=$1 AND state='bound' AND $2::timestamptz > lease_until"
            >>,
            [Id, LeaseUntil]
        ),
    ok;
extend_lease_tx(_Conn, _Id, _Null) ->
    ok.

%% ===================================================================
%% 释放
%% ===================================================================

%% 幂等释放（同 business key）：bound→released；已 released 幂等返回。
-spec release_tx(any(), integer(), binary(), binary()) ->
    {ok, released | already_released} | {error, term()}.
release_tx(Conn, RecordId, Role, BusinessKey) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='released', released_at=NOW()",
            " WHERE record_id=$1 AND role=$2 AND business_key=$3 AND state='bound'",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [RecordId, Role, BusinessKey]) of
        {ok, [_]} -> {ok, released};
        {ok, []} -> {ok, already_released};
        {error, R} -> {error, R}
    end.

-spec release_by_id_tx(any(), integer()) -> {ok, released | already_released} | {error, term()}.
release_by_id_tx(Conn, RefId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET state='released', released_at=NOW()"
            " WHERE id=$1 AND state='bound' RETURNING id">>,
    case elib_pg:query(Conn, Sql, [RefId]) of
        {ok, [_]} -> {ok, released};
        {ok, []} -> {ok, already_released};
        {error, R} -> {error, R}
    end.

%% 记录删除/移交：释放该记录全部 bound 引用（processing lease 一并释放）。
%% 返回释放行数。治理门（retention/hold 阻止释放）在 adapter 层先行裁决，
%% 本函数保持机械释放语义。
-spec release_record_tx(any(), integer()) -> {ok, non_neg_integer()} | {error, term()}.
release_record_tx(Conn, RecordId) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='released', released_at=NOW()",
            " WHERE record_id=$1 AND state='bound' RETURNING id">>,
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, R} -> {error, R}
    end.

%% Whole-job cancellation; never touches permanent audio_part business refs.
release_job_processing_tx(Conn, RecordId, JobId) ->
    Prefix = <<"assemble_job:", (integer_to_binary(JobId))/binary, ":%">>,
    elib_pg:execute(
        Conn,
        <<
            "UPDATE talk_record_attachment_ref SET state='released',"
            " released_at=now() WHERE record_id=$1 AND role='audio_part'"
            " AND state='bound' AND lease_until IS NOT NULL AND business_key LIKE $2"
        >>,
        [RecordId, Prefix]
    ).

%% ===================================================================
%% Processing lease（§10.3）
%% ===================================================================

%% 授予/延长有期限处理引用：同 key 已 bound 同资产 → 只延长 lease（GREATEST）；
%% 异资产 → business_key_conflict；不存在 → 首绑。LeaseUntil 为 timestamptz
%% 文本（如 <<"2030-01-01 00:00:00+00">>），必填——无期限引用走 bind_tx。
-spec grant_processing_lease_tx(any(), integer(), integer(), binary(), binary(), binary()) ->
    {ok, integer() | binary(), boolean()} | {error, term()}.
grant_processing_lease_tx(Conn, RecordId, AttachmentId, Role, BusinessKey, LeaseUntil) when
    is_binary(LeaseUntil), LeaseUntil =/= <<>>
->
    bind_tx(Conn, #{
        record_id => RecordId,
        attachment_id => AttachmentId,
        role => Role,
        business_key => BusinessKey,
        lease_until => LeaseUntil
    });
grant_processing_lease_tx(_Conn, _RecordId, _AttachmentId, _Role, _BusinessKey, _LeaseUntil) ->
    {error, lease_until_required}.

%% 到期处理引用清扫（惰性转 released；GC 谓词按时间比较，不依赖本清扫先行）。
-spec release_expired_leases_tx(any()) -> {ok, non_neg_integer()} | {error, term()}.
release_expired_leases_tx(Conn) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET state='released', released_at=NOW()",
            " WHERE state='bound' AND lease_until IS NOT NULL AND lease_until <= NOW()",
            " RETURNING id">>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, Rows} -> {ok, length(Rows)};
        {error, R} -> {error, R}
    end.

%% ===================================================================
%% GC 资格谓词（供测试与 adapter 复用；生产资格权威在
%% attachment_repo:refresh_gc_verified_tx / fn_v5_intent_ordinary_delete）
%% ===================================================================

%% 有效引用 SQL 片段（Alias = 附件行别名，如 <<"a">> 或 <<"a2">>）。
%% 与 00000212 迁移 fn 内守卫、attachment_repo 谓词扩展逐字同义。
-spec valid_reference_sql(binary()) -> binary().
valid_reference_sql(Alias) ->
    <<
        " AND NOT EXISTS (SELECT 1 FROM talk_record_attachment_ref trar"
        " WHERE trar.attachment_id = ",
        Alias/binary,
        ".id"
        " AND trar.state = 'bound'"
        " AND (trar.lease_until IS NULL OR trar.lease_until > now()))"
    >>.

-spec valid_reference_exists_tx(any(), integer()) -> {ok, boolean()} | {error, term()}.
valid_reference_exists_tx(Conn, AttachmentId) ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM ", (tablename())/binary,
            " WHERE attachment_id=$1 AND state='bound'"
            " AND (lease_until IS NULL OR lease_until > now())) AS exists_ok">>,
    case elib_pg:query(Conn, Sql, [AttachmentId]) of
        {ok, [#{<<"exists_ok">> := V}]} -> {ok, V};
        {ok, []} -> {ok, false};
        {error, R} -> {error, R}
    end.

-spec find_tx(any(), integer(), binary(), binary()) -> {ok, map() | not_found} | {error, term()}.
find_tx(Conn, RecordId, Role, BusinessKey) ->
    Sql =
        <<
            "SELECT id, record_id, attachment_id, capture_id, role, business_key,"
            " state, lease_until, released_at, created_at FROM ",
            (tablename())/binary,
            " WHERE record_id=$1 AND role=$2 AND business_key=$3 LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [RecordId, Role, BusinessKey]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, not_found};
        {error, R} -> {error, R}
    end.

-spec active_roles_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
active_roles_tx(Conn, AttachmentId) ->
    Sql =
        <<"SELECT id, record_id, capture_id, role, business_key, lease_until FROM ",
            (tablename())/binary, " WHERE attachment_id=$1 AND state='bound'",
            " AND (lease_until IS NULL OR lease_until > now())">>,
    case elib_pg:query(Conn, Sql, [AttachmentId]) of
        {ok, Rows} -> {ok, Rows};
        {error, R} -> {error, R}
    end.

%% ===================================================================
%% 企业附件管理页资源分类（§10.4：workspace=NULL 也不逃过治理检索）
%% ===================================================================

%% 按 organization（可选 workspace 过滤）列出 talk_record 附件：
%%   source='talk_record'、来源记录元信息、有效引用角色聚合、治理行存在性。
%% 与 workspace_attachment_repo:scan/2（group/channel）并列的 talk_record
%% 分类查询；管理页接线（workspace_attachment_logic/API/UI）为 T06 范围外
%% blocker，本查询为其就绪的只读入口。
-spec scan_organization(integer(), map()) -> {ok, [map()]} | {error, term()}.
scan_organization(OrgId, Filters) ->
    elib_pg:with_conn(fun(Conn) -> scan_organization_tx(Conn, OrgId, Filters) end).

%% 直连形态（隔离测试/既有事务复用）。
%% T06-L2 修复（Q-review-T06 #L2，T20 顺手修复——本文件在续写范围）：
%%   * JOIN 不再依赖 scope_ref 恒为数字的 cast 洁净假设（原
%%     tr.id = a.scope_ref::bigint 在脏数据下整查询抛错）——改文本等值
%%     比较 tr.id::text = a.scope_ref，非数字脏行自然不匹配（跳行不炸）；
%%   * keyword ILIKE 通配符转义（elib_pg:escape_like，%/_ 字面量化）；
%%   * LIMIT 参数化钳制 1..100（原硬编码 100 无分页边界）。
-spec scan_organization_tx(any(), integer(), map()) -> {ok, [map()]} | {error, term()}.
scan_organization_tx(Conn, OrgId, Filters) ->
    WsId = maps:get(workspace_id, Filters, null),
    Keyword = maps:get(keyword, Filters, <<>>),
    Limit = clamp_limit(maps:get(limit, Filters, 100)),
    Sql =
        <<
            "SELECT a.id, a.name, a.size, a.mime_type, a.created_at, a.creator_user_id,",
            " a.scope, a.scope_ref, a.path, a.status, a.cipher,",
            " u.nickname AS creator_name, tr.id AS record_id, tr.title AS record_title,",
            " tr.workspace_id AS record_workspace_id, tr.deleted_at AS record_deleted_at,",
            " COALESCE(string_agg(DISTINCT trar.role, ',' ORDER BY trar.role), '') AS active_roles,",
            " CASE WHEN ear.attachment_id IS NOT NULL THEN 'managed' ELSE 'unmanaged' END",
            "   AS ear_state",
            " FROM attachment a",
            " JOIN talk_record tr ON tr.id::text = a.scope_ref",
            " LEFT JOIN talk_record_attachment_ref trar",
            "   ON trar.attachment_id = a.id AND trar.state='bound'",
            "   AND (trar.lease_until IS NULL OR trar.lease_until > now())",
            " LEFT JOIN enterprise_attachment_retention ear ON ear.attachment_id = a.id",
            " LEFT JOIN \"user\" u ON u.id = a.creator_user_id",
            " WHERE a.scope='talk_record' AND a.status>=0",
            "   AND tr.organization_id = $1",
            "   AND ($2::bigint IS NULL OR tr.workspace_id = $2)",
            "   AND ($3::text='' OR a.name ILIKE '%' || $3 || '%' ESCAPE '\\')",
            " GROUP BY a.id, u.nickname, tr.id, ear.attachment_id",
            " ORDER BY a.id DESC LIMIT $4"
        >>,
    case elib_pg:query(Conn, Sql, [OrgId, WsId, escaped_keyword(Keyword), Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, R} -> {error, R}
    end.

%% @private keyword ILIKE 转义（%/_ 字面量化；ESCAPE '\' 声明默认转义符）。
-spec escaped_keyword(binary()) -> binary().
escaped_keyword(Keyword) when is_binary(Keyword), Keyword =/= <<>> ->
    elib_pg:escape_like(Keyword);
escaped_keyword(_) ->
    <<>>.

%% @private LIMIT 钳制 1..100（默认 100 兼容既有调用形状）。
-spec clamp_limit(term()) -> pos_integer().
clamp_limit(N) when is_integer(N), N >= 1, N =< 100 ->
    N;
clamp_limit(N) when is_integer(N), N > 100 ->
    100;
clamp_limit(_) ->
    100.
