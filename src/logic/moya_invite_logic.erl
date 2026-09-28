-module(moya_invite_logic).
%%%
% 墨芽班级邀请码逻辑（W3：老师邀请码 → 家长加入班级）
% Class invite code logic
%
% 场景：县城书法培训机构，老师在群里发"邀请码"，家长扫码/复制进小程序
% → 看到「加入 逸云硬笔 · 周五班」确认页 + 该班学员名单 → 选自家孩子
% → 绑定为监护人（guardian_learner：can_submit=true, can_view_review=true,
% relation='guardian'）。
%
% 设计（产品语义已定，勿自行更改）：
%   - 邀请码绑定**班级**（group），一码多人共用（全班家长），可撤销
%   - 家长加入必须显式选择 learner——码不能推断"谁家孩子"
%   - 码校验统一折叠：不存在/revoked/expired → not_found（防探测，
%     细分原因仅入日志，不进响应）
%
% 隐私缓解（日志纪律，代码级强制）：
%   - 学员 display_name **绝不入日志**（儿童 PII）
%   - code 入日志一律 sha256 前 12 位指纹（code_fingerprint/1）
%   - 访问日志仅记 uid + 指纹（谁在何时用哪个码看了名单/加入）
%
% 复用评估（organization_invite_code_*）：
%   organization_invite_code_app/_pg 是"组织成员邀请"域（码→org→
%   organization_member 写入 + join 编排），治理门/错误码/编排均组织成员
%   语义；本模块是"教学监护关系"域（码→group→显式选 learner→
%   guardian_learner），不复用其应用层，仅镜像其码生成/重试/expired 手法
%   （见 moya_invite_repo 模块头）。
%
% 路由：本波不挂路由（禁改 imboy_router.erl）；所需路由三元组见任务报告。
%%%

-export([create_or_get_code/2, invite_info/1, join/3]).
-export([code_fingerprint/1]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 生成或复用班级邀请码（守卫：该班 active class_staff 老师或 org
%% owner，deny-by-default；班不存在 → not_found）。
%% 守卫与写入不在同一事务（upsert 自带重试自治）——TOCTOU 窗口内被
%% 撤职的老师多生成一个码的后果 = 可被 revoke 的普通码，风险可接受。
%% 错误：not_authorized | not_found | db_error
-spec create_or_get_code(integer(), integer()) ->
    {ok, binary()} | {error, not_authorized | not_found | db_error}.
create_or_get_code(Uid, GroupId) ->
    case guard_class_operator(Uid, GroupId) of
        ok ->
            case moya_invite_repo:upsert_active_code(GroupId, Uid) of
                {ok, Code} ->
                    ?LOG_INFO(
                        "[moya_invite] code_ready uid=~p group_id=~p fp=~p",
                        [Uid, GroupId, code_fingerprint(Code)]
                    ),
                    {ok, Code};
                {error, Reason} ->
                    ?LOG_ERROR(
                        "moya_invite upsert code failed group_id=~p uid=~p reason=~p",
                        [GroupId, Uid, Reason]
                    ),
                    {error, db_error}
            end;
        {error, Reason} ->
            ?LOG_INFO(
                "[moya_invite] code_create rejected uid=~p group_id=~p why=~p",
                [Uid, GroupId, Reason]
            ),
            {error, Reason}
    end.

%% @doc 凭码读确认页信息：机构名 + 班名 + 该班 active 学员名单。
%% 码校验统一折叠：不存在/revoked/过期 → not_found（细分原因仅入日志，
%% 响应不区分——持码人无法探测码的死因，撤销即"码不存在"）。
%% 错误：not_found | db_error
-spec invite_info(binary()) ->
    {ok, #{org_name => binary(), group_name => binary(), learners => [map()]}}
    | {error, not_found | db_error}.
invite_info(Code) ->
    Fp = code_fingerprint(Code),
    case moya_invite_repo:find_code(Code) of
        {ok, undefined} ->
            ?LOG_INFO("[moya_invite] info rejected fp=~p why=not_found", [Fp]),
            {error, not_found};
        {ok, #{<<"status">> := <<"revoked">>}} ->
            ?LOG_INFO("[moya_invite] info rejected fp=~p why=revoked", [Fp]),
            {error, not_found};
        {ok, #{<<"expired">> := true}} ->
            ?LOG_INFO("[moya_invite] info rejected fp=~p why=expired", [Fp]),
            {error, not_found};
        {ok, #{<<"group_id">> := GroupId}} when is_integer(GroupId) ->
            info_of_group(Fp, GroupId);
        {ok, _BadRow} ->
            ?LOG_ERROR("moya_invite info bad row fp=~p", [Fp]),
            {error, db_error};
        {error, Reason} ->
            ?LOG_ERROR("moya_invite find_code db error fp=~p reason=~p", [Fp, Reason]),
            {error, db_error}
    end.

%% @doc 凭码加入班级（绑定所选 learner 为监护人）。
%% - 码校验同 invite_info 折叠口径（同一事务快照内）
%% - LearnerId 必须在该班 class_enrollment active（码不能推断孩子，
%%   选了不在班的学员 → learner_not_in_class）
%% - 幂等：已有 active 监护关系 → already_joined（不重复插）；
%%   removed 行存在 → upsert 复活（status/can_submit/can_view_review 回满）
%% 错误：not_found | learner_not_in_class | db_error
-spec join(integer(), binary(), integer()) ->
    {ok, joined | already_joined} | {error, not_found | learner_not_in_class | db_error}.
join(Uid, Code, LearnerId) ->
    Fp = code_fingerprint(Code),
    Tx = fun(Conn) ->
        case moya_invite_repo:find_code_tx(Conn, Code) of
            {ok, undefined} ->
                ?LOG_INFO("[moya_invite] join rejected uid=~p fp=~p why=not_found", [Uid, Fp]),
                {error, not_found};
            {ok, #{<<"status">> := <<"revoked">>}} ->
                ?LOG_INFO("[moya_invite] join rejected uid=~p fp=~p why=revoked", [Uid, Fp]),
                {error, not_found};
            {ok, #{<<"expired">> := true}} ->
                ?LOG_INFO("[moya_invite] join rejected uid=~p fp=~p why=expired", [Uid, Fp]),
                {error, not_found};
            {ok, #{<<"group_id">> := GroupId}} when is_integer(GroupId) ->
                join_dispatch(Conn, Uid, Fp, GroupId, LearnerId);
            {ok, _BadRow} ->
                ?LOG_ERROR("moya_invite join bad row uid=~p fp=~p", [Uid, Fp]),
                {error, db_error};
            {error, Reason} ->
                ?LOG_ERROR(
                    "moya_invite join find_code db error uid=~p fp=~p reason=~p",
                    [Uid, Fp, Reason]
                ),
                {error, db_error}
        end
    end,
    case elib_pg:with_tx(Tx, [{reraise, false}]) of
        {rollback, Reason} ->
            ?LOG_ERROR("moya_invite join tx rollback uid=~p fp=~p reason=~p", [Uid, Fp, Reason]),
            {error, db_error};
        Result ->
            Result
    end.

%% @doc code 的日志指纹：sha256 hex 前 12 位。
%% code 本身是活凭证（可换资格），不整体入日志；uid+指纹足够审计
%% "谁用过哪个码"，且不可逆推码。导出供 handler 访问日志复用同一实现
%% （避免两处指纹算法漂移）。
-spec code_fingerprint(binary()) -> binary().
code_fingerprint(Code) when is_binary(Code) ->
    <<Fp:12/binary, _/binary>> = binary:encode_hex(crypto:hash(sha256, Code)),
    Fp;
code_fingerprint(_) ->
    <<"invalid">>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 守卫：Uid 是该班 active class_staff（任一教学角色，参照 moya_acl:
%% resolve_staff 默认口径）或该班所属机构 owner（T5：owner 不授予儿童
%% 资源，但"生成班级邀请码"是教学管理动作，与 moya_learner_bind 的
%% owner 放行口径一致）。
%% 班不存在（含机构解析失败的群，class_brief 内 JOIN 折叠）→ not_found。
-spec guard_class_operator(integer(), integer()) ->
    ok | {error, not_authorized | not_found | db_error}.
guard_class_operator(Uid, GroupId) ->
    case moya_invite_repo:class_brief(GroupId) of
        {ok, undefined} ->
            {error, not_found};
        {ok, _Brief} ->
            case moya_acl:resolve_staff(Uid, GroupId) of
                {ok, _Staff} ->
                    ok;
                {error, db_error} ->
                    {error, db_error};
                {error, _NotStaff} ->
                    guard_org_owner(Uid, GroupId)
            end;
        {error, Reason} ->
            ?LOG_ERROR(
                "moya_invite class_brief db error group_id=~p reason=~p",
                [GroupId, Reason]
            ),
            {error, db_error}
    end.

%% org owner 兜底（staff 不中时）：group→workspace→organization 解析
%% owner。机构解析失败（理论不可达：class_brief 已验证机构存在）→
%% not_authorized（deny-by-default）。
-spec guard_org_owner(integer(), integer()) ->
    ok | {error, not_authorized | db_error}.
guard_org_owner(Uid, GroupId) ->
    case moya_context_repo:group_org(GroupId) of
        {ok, OrgId} when is_integer(OrgId) ->
            case moya_acl:resolve_org_owner(Uid, OrgId) of
                ok ->
                    ok;
                {error, not_owner} ->
                    {error, not_authorized};
                {error, Reason} ->
                    ?LOG_ERROR("moya_invite org owner check db error ~p", [Reason]),
                    {error, db_error}
            end;
        {ok, undefined} ->
            {error, not_authorized};
        {error, Reason} ->
            ?LOG_ERROR("moya_invite group_org db error ~p", [Reason]),
            {error, db_error}
    end.

%% invite_info 的班级信息聚合（码已验证）。learner_id 出现在日志里是
%% ID 而非姓名（display_name 不入日志，儿童 PII 纪律）。
-spec info_of_group(binary(), integer()) ->
    {ok, map()} | {error, not_found | db_error}.
info_of_group(Fp, GroupId) ->
    case moya_invite_repo:class_brief(GroupId) of
        {ok, undefined} ->
            %% 码仍 active 但班已删（FK CASCADE 应已连带删码，防御性兜底）
            ?LOG_INFO(
                "[moya_invite] info rejected fp=~p why=group_gone group_id=~p",
                [Fp, GroupId]
            ),
            {error, not_found};
        {ok, #{<<"org_name">> := OrgName, <<"group_name">> := GroupName}} ->
            case moya_invite_repo:class_learners(GroupId) of
                {ok, Learners} ->
                    ?LOG_INFO(
                        "[moya_invite] info_access fp=~p group_id=~p learner_count=~p",
                        [Fp, GroupId, length(Learners)]
                    ),
                    {ok, #{
                        org_name => OrgName,
                        group_name => GroupName,
                        %% 原子键内聚投影（handler 负责 TSID→string 与 binary 键）
                        learners => [learner_brief(L) || L <- Learners]
                    }};
                {error, Reason} ->
                    ?LOG_ERROR(
                        "moya_invite class_learners db error fp=~p group_id=~p reason=~p",
                        [Fp, GroupId, Reason]
                    ),
                    {error, db_error}
            end;
        {ok, _BadBrief} ->
            ?LOG_ERROR("moya_invite class_brief bad row fp=~p group_id=~p", [Fp, GroupId]),
            {error, db_error};
        {error, Reason} ->
            ?LOG_ERROR(
                "moya_invite class_brief db error fp=~p group_id=~p reason=~p",
                [Fp, GroupId, Reason]
            ),
            {error, db_error}
    end.

-spec learner_brief(map()) -> #{id => integer(), display_name => binary()}.
learner_brief(#{<<"id">> := Id, <<"display_name">> := Name}) ->
    #{id => Id, display_name => Name};
learner_brief(#{<<"id">> := Id}) ->
    #{id => Id, display_name => <<>>};
learner_brief(_) ->
    #{id => 0, display_name => <<>>}.

%% join 的班级内校验 + 监护写入（事务体内部）。
-spec join_dispatch(any(), integer(), binary(), integer(), integer()) ->
    {ok, joined | already_joined} | {error, not_found | learner_not_in_class | db_error}.
join_dispatch(Conn, Uid, Fp, GroupId, LearnerId) ->
    case moya_invite_repo:learner_active_in_class_tx(Conn, GroupId, LearnerId) of
        true ->
            join_guardian(Conn, Uid, Fp, GroupId, LearnerId);
        false ->
            %% 学员不在该班 active：码不能推断"谁家孩子"，选错即拒
            %% （learner_id 是 ID，非 PII，可入日志）
            ?LOG_INFO(
                "[moya_invite] join rejected uid=~p fp=~p why=learner_not_in_class "
                "group_id=~p learner_id=~p",
                [Uid, Fp, GroupId, LearnerId]
            ),
            {error, learner_not_in_class};
        {error, Reason} ->
            ?LOG_ERROR(
                "moya_invite enrollment check db error uid=~p fp=~p reason=~p",
                [Uid, Fp, Reason]
            ),
            {error, db_error}
    end.

-spec join_guardian(any(), integer(), binary(), integer(), integer()) ->
    {ok, joined | already_joined} | {error, db_error}.
join_guardian(Conn, Uid, Fp, GroupId, LearnerId) ->
    case moya_invite_repo:guardian_active_tx(Conn, Uid, LearnerId) of
        true ->
            ?LOG_INFO(
                "[moya_invite] join idempotent uid=~p fp=~p learner_id=~p outcome=already_joined",
                [Uid, Fp, LearnerId]
            ),
            {ok, already_joined};
        false ->
            case moya_invite_repo:insert_guardian_tx(Conn, Uid, LearnerId) of
                ok ->
                    ?LOG_INFO(
                        "[moya_invite] joined uid=~p fp=~p group_id=~p learner_id=~p",
                        [Uid, Fp, GroupId, LearnerId]
                    ),
                    {ok, joined};
                {error, Reason} ->
                    ?LOG_ERROR(
                        "moya_invite insert guardian failed uid=~p fp=~p learner_id=~p reason=~p",
                        [Uid, Fp, LearnerId, Reason]
                    ),
                    {error, db_error}
            end;
        {error, Reason} ->
            ?LOG_ERROR(
                "moya_invite guardian check db error uid=~p fp=~p learner_id=~p reason=~p",
                [Uid, Fp, LearnerId, Reason]
            ),
            {error, db_error}
    end.
