-module(moya_invite_repo).
%%%
% 墨芽班级邀请码数据仓库（W3：老师邀请码 → 家长加入班级）
% Class invite code repository
%
% 职责：
%   - 邀请码 CRUD SQL（_tx 变体直传连接，join 事务与生产同一代码路径）
%   - 班级学员名单 / 班级摘要（group→workspace→organization）只读查询
%   - guardian_learner 的 join 插入（upsert 复活语义）
%   - 码生成：Crockford Base32 CSPRNG（见 generate_code 注释）
%
% 隐私纪律（代码级约束）：
%   - 本模块不产生任何日志含 display_name / 学员名——日志在 logic/handler
%     层且仅记 uid + code 指纹（sha256 前 12 位）
%   - 学员名单查询结果仅供"持有效码的确认页"使用，调用方（logic）负责
%     码校验前置；repo 不做权限判断（判断集中在 logic 层）
%
% 复用评估结论（organization_invite_code_*）：
%   既有 organization_invite_code_pg/_app 面向"组织成员邀请"（group_member/
%   organization_member 域：码→org→join 编排写 organization_member），其
%   治理门（owner/admin）、错误码（981/982）、加入编排（organization_join_
%   orchestrator）均为组织成员语义；本任务是"教学监护关系"（guardian_
%   learner 域：码→group→显式选 learner），语义不同不直接复用，仅在
%   码生成（CSPRNG + 32 字符集无偏取模）、ON CONFLICT 换码重试、
%   SQL 内计算 expired 布尔三个手法上镜像其实现。
%%%

-export([tablename/1]).
-export([
    find_code/1,
    find_code_tx/2,
    upsert_active_code/2,
    revoke_code/1,
    class_learners/1,
    class_brief/1,
    learner_active_in_class_tx/3,
    guardian_active_tx/3,
    insert_guardian_tx/3
]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% Crockford Base32：0-9 + A-Z 排除 I/L/O/U，恰 32 字符（256 rem 32 = 0，
%% `Byte rem 32` 无取模偏置，无需拒绝采样）；排除易手抄混淆字符。
-define(INVITE_CHARSET, <<"0123456789ABCDEFGHJKMNPQRSTVWXYZ">>).
%% 10 位 × 5 bit = 50 bit 熵：全班共用一码 + 可随时撤销，50 bit 足以
%% 阻挡穷举猜测（对照 organization_invite_code 8 位码，本码暴露面更大
%% （群发）故取高两档）。
-define(INVITE_CODE_LEN, 10).
%% code 主键碰撞重试上限（CSPRNG 50 bit 空间，碰撞概率天文级小，
%% 上限仅防御性兜底）。
-define(CODE_RETRY_LIMIT, 3).

%%%===================================================================
%%% API
%%%===================================================================

-spec tablename(binary()) -> binary().
tablename(Tb) ->
    elib_pg_sql:public_tablename(Tb).

%% @doc 按码查行（自动连接版，invite_info 读路径）。
%% 返回行含 code / group_id / status / expires_at / expired（SQL 内同行
%% 计算 `(expires_at < CURRENT_TIMESTAMP) AS expired`，避免 Erlang 侧
%% 解析 timestamptz 格式；expires_at NULL → expired 为 NULL，由 logic
%% 按 `=:= true` 判定，NULL 不算过期）。
-spec find_code(binary()) -> {ok, map() | undefined} | {error, term()}.
find_code(Code) ->
    find_code_tx(undefined, Code).

%% @doc 按码查行（事务内版，join 在同一事务快照内校验码）。
-spec find_code_tx(any(), binary()) -> {ok, map() | undefined} | {error, term()}.
find_code_tx(undefined, Code) ->
    Sql = code_row_sql(),
    case elib_pg:query(Sql, [Code]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end;
find_code_tx(Conn, Code) ->
    Sql = code_row_sql(),
    case elib_pg:query(Conn, Sql, [Code]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 取该班可用码：已有 active 且未过期的码则复用返回；否则生成新码
%% 插入。一码多人共用（全班家长），故复用优先于新建。
%% 碰撞（code 主键，概率 ~2^-50）时 ON CONFLICT DO NOTHING 落 0 行，
%% 换码重试 ≤3 次；穷尽 → {error, code_retry_exhausted}。
-spec upsert_active_code(integer(), integer()) -> {ok, binary()} | {error, term()}.
upsert_active_code(GroupId, CreatedBy) ->
    ReuseSql =
        <<"SELECT code FROM ", (tb(moya_invite_code))/binary,
            " WHERE group_id = $1 AND status = 'active' ",
            "AND (expires_at IS NULL OR expires_at >= CURRENT_TIMESTAMP) ",
            "ORDER BY created_at DESC LIMIT 1">>,
    case elib_pg:query(ReuseSql, [GroupId]) of
        {ok, [#{<<"code">> := Code} | _]} ->
            {ok, Code};
        {ok, []} ->
            insert_new_code(GroupId, CreatedBy, ?CODE_RETRY_LIMIT);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 撤销码（幂等：不存在/已撤销 → false）。
%% DB 错误也返回 false——fail-visible：错误已 LOG_ERROR，不静默；
%% 调用方（管理动作）凭 false 决定提示，不误报成功。
-spec revoke_code(binary()) -> boolean().
revoke_code(Code) ->
    Sql =
        <<"UPDATE ", (tb(moya_invite_code))/binary,
            " SET status = 'revoked' WHERE code = $1 AND status = 'active'">>,
    case elib_pg:execute(Sql, [Code]) of
        {ok, Count} when is_integer(Count), Count > 0 ->
            true;
        {ok, _} ->
            false;
        {error, Reason} ->
            ?LOG_ERROR("moya_invite revoke_code db error ~p", [Reason]),
            false
    end.

%% @doc 班级学员名单（确认页用）：class_enrollment active JOIN learner
%% active。按 display_name 排序、id 作次序 tiebreaker——稳定输出，
%% 家长两次刷新名单顺序一致。
%% 注意：该名单对"持有效码的人"可见是产品语义（选自家孩子），隐私
%% 缓解在码本身（不可猜测/可撤销）与日志纪律（不记学员名），见模块头。
-spec class_learners(integer()) -> {ok, [map()]} | {error, term()}.
class_learners(GroupId) ->
    Sql =
        <<
            "SELECT l.id, l.display_name FROM ",
            (tb(class_enrollment))/binary,
            " e "
            "JOIN ",
            (tb(learner))/binary,
            " l ON l.id = e.learner_id AND l.status = 'active' ",
            "WHERE e.group_id = $1 AND e.status = 'active' ",
            "ORDER BY l.display_name, l.id"
        >>,
    elib_pg:query(Sql, [GroupId]).

%% @doc 班级摘要（确认页标题「加入 逸云硬笔 · 周五班」用）：
%% group JOIN workspace JOIN organization。
%% 教学班必须挂在已归属机构的 workspace 下（00000096 DB-ORG-03 触发器
%% 约束），机构解析失败的群按不存在处理（deny-by-default，与
%% moya_context_repo:group_org 同口径）→ {ok, undefined}。
-spec class_brief(integer()) -> {ok, map() | undefined} | {error, term()}.
class_brief(GroupId) ->
    Sql =
        <<
            "SELECT o.name AS org_name, g.title AS group_name FROM ",
            (tbq(group))/binary,
            " g "
            "JOIN ",
            (tb(workspace))/binary,
            " w ON w.id = g.workspace_id "
            "JOIN ",
            (tb(organization))/binary,
            " o ON o.id = w.organization_id "
            "WHERE g.id = $1 LIMIT 1"
        >>,
    case elib_pg:query(Sql, [GroupId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {ok, undefined};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 学员是否在该班 active 在读（join 前置校验：码不能推断"谁家
%% 孩子"，家长必须从名单里选，选错/越权选他班学员 → learner_not_in_class）。
-spec learner_active_in_class_tx(any(), integer(), integer()) ->
    true | false | {error, term()}.
learner_active_in_class_tx(Conn, GroupId, LearnerId) ->
    Sql =
        <<
            "SELECT 1 FROM ",
            (tb(class_enrollment))/binary,
            " WHERE group_id = $1 AND learner_id = $2 AND status = 'active' LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [GroupId, LearnerId]) of
        {ok, [_ | _]} -> true;
        {ok, []} -> false;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 该监护关系是否已 active（join 幂等判断：不重复插、返回
%% already_joined）。
-spec guardian_active_tx(any(), integer(), integer()) ->
    true | false | {error, term()}.
guardian_active_tx(Conn, Uid, LearnerId) ->
    Sql =
        <<
            "SELECT 1 FROM ",
            (tb(guardian_learner))/binary,
            " WHERE guardian_uid = $1 AND learner_id = $2 AND status = 'active' LIMIT 1"
        >>,
    case elib_pg:query(Conn, Sql, [Uid, LearnerId]) of
        {ok, [_ | _]} -> true;
        {ok, []} -> false;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 绑定监护关系（事务内）：guardian_learner upsert。
%% ON CONFLICT (guardian_uid, learner_id) DO UPDATE SET status='active',
%% can_submit=true, can_view_review=true——removed 后重新凭码加入即复活
%% （权限回满：能重新提交/查看，镜像首次加入的授予面）；relation 保留
%% 原值（曾被人工细分为 'other' 的关系不因重新加入被覆盖回 'guardian'）。
%% 正常路径由 logic 保证仅在无 active 行时调用；并发双击双插时第二个
%% upsert 走 DO UPDATE 分支，结果幂等（双方都报 joined，最终状态一致）。
-spec insert_guardian_tx(any(), integer(), integer()) -> ok | {error, term()}.
insert_guardian_tx(Conn, Uid, LearnerId) ->
    Sql =
        <<
            "INSERT INTO ",
            (tb(guardian_learner))/binary,
            " (guardian_uid, learner_id, relation, can_submit, can_view_review, "
            "status, created_at, updated_at) "
            "VALUES ($1, $2, 'guardian', true, true, 'active', "
            "CURRENT_TIMESTAMP, CURRENT_TIMESTAMP) "
            "ON CONFLICT (guardian_uid, learner_id) DO UPDATE SET "
            "status = 'active', can_submit = true, can_view_review = true, "
            "updated_at = CURRENT_TIMESTAMP"
        >>,
    case elib_pg:execute(Conn, Sql, [Uid, LearnerId]) of
        {ok, Count} when is_integer(Count), Count > 0 -> ok;
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec code_row_sql() -> binary().
code_row_sql() ->
    <<
        "SELECT code, group_id, status, expires_at, ",
        "(expires_at < CURRENT_TIMESTAMP) AS expired FROM ",
        (tb(moya_invite_code))/binary,
        " WHERE code = $1 LIMIT 1"
    >>.

-spec insert_new_code(integer(), integer(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
insert_new_code(_GroupId, _CreatedBy, 0) ->
    {error, code_retry_exhausted};
insert_new_code(GroupId, CreatedBy, Left) ->
    Code = generate_code(),
    Sql =
        <<
            "INSERT INTO ",
            (tb(moya_invite_code))/binary,
            " (code, group_id, created_by, status) ",
            "VALUES ($1, $2, $3, 'active') ",
            "ON CONFLICT (code) DO NOTHING RETURNING code"
        >>,
    case elib_pg:query(Sql, [Code, GroupId, CreatedBy]) of
        {ok, [#{<<"code">> := Inserted} | _]} ->
            {ok, Inserted};
        {ok, []} ->
            %% 码主键碰撞（~2^-50）：DO NOTHING 落 0 行，换码重试
            insert_new_code(GroupId, CreatedBy, Left - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 生成 10 位 Crockford Base32 码（CSPRNG）。
%% 与 organization_invite_code_pg:generate_code 同一手法：每字节
%% crypto:strong_rand_bytes(1)（CSPRNG，非 rand:uniform 可预测 PRNG），
%% 字符集恰 32 字符 → `Byte rem 32` 无取模偏置。
-spec generate_code() -> binary().
generate_code() ->
    generate_code_chars(?INVITE_CODE_LEN, <<>>).

-spec generate_code_chars(non_neg_integer(), binary()) -> binary().
generate_code_chars(0, Acc) ->
    Acc;
generate_code_chars(N, Acc) ->
    Chars = ?INVITE_CHARSET,
    <<Byte:8>> = crypto:strong_rand_bytes(1),
    Pos = Byte rem byte_size(Chars),
    <<_:Pos/binary, Char:1/binary, _/binary>> = Chars,
    generate_code_chars(N - 1, <<Acc/binary, Char/binary>>).

%% 表名包裹（镜像 moya_learner_bind_repo:tb/1 的 atom 兼容处理：
%% 直连测试模式下 public_tabename 可能返回 atom）。
%% binary 分支已随 CI-00 成功类型移除（2026-09-28）：本仓调用点全为
%% atom 字面量，binary 子句为 dialyzer 判定死代码；learner_bind 版
%% 仍存 binary 调用点故保留，两仓不再完全镜像。
-spec tb(atom()) -> binary().
tb(group) ->
    %% GROUP 是保留字：只引末段 → public."group"（整段加引号 → 42P01）
    elib_pg_sql:public_tablename_quoted(<<"group">>);
tb(Tb) when is_atom(Tb) ->
    tablename(ec_cnv:to_binary(Tb)).

%% 显式 quoted 变体（可读性：JOIN 段直接用）
-spec tbq(atom()) -> binary().
tbq(Tb) ->
    tb(Tb).
