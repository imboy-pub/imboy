-module(organization_invitation_app).

%% Organization Invitation commands（application 层）。
%%
%% Core Contract C11 + 计划 ORG-03 冻结实现：
%%   * create / accept / reject / revoke / list 是不同 command；
%%     Expire 是 lazy 状态转移（读写路径按需 sweep，非定时任务）。
%%   * token 明文只在 create 响应返回一次；库中/日志/响应投影只有 digest，
%%     且 digest 不出本模块（view 白名单不含 token_digest，与 cs_access_app 同口径）。
%%   * accept 一次性消费：同事务先 lazy-expire sweep，再单条 CAS UPDATE
%%     （WHERE status='pending'）；target-only（digest 查找同语句锁 target 作用域，
%%     非目标用户命中不了行）；幂等（重复 accept 读到 accepted 终态返回同一视图，
%%     不产生第二次消费副作用）。
%%   * accept_targeted（P0 定向邀请免口令）：target 身份（JWT）即凭据，行定位
%%     换成 (org, target) 最新 pending/accepted 行，其余语义与 accept/4 同构；
%%     口令路径保留向后兼容（旧版客户端凭 token accept 不受影响）。
%%   * 与 membership 的衔接 = membership_hook（同事务、consume 成功后调用）；
%%     V1 默认不注入（invitation 侧边界，membership adapter 由后续 Gate 集成）。
%%     hook 失败 → 整个事务回滚（消费 + 成员变更原子）。
%%   * 锁序与既有治理写一致：组织行先、成员/邀请行后。
%%   * SQL 真实行为由一次性 PG 上的 organization_invitation_behavior_harness.escript 覆盖。

-export([
    create/4,
    accept/4,
    accept_targeted/3,
    reject/3,
    revoke/3,
    list_for_target/2,
    list_for_org/3
]).

-include("log.hrl").

-type opts() :: map().

%% ===================================================================
%% create —— Invite command（明文 token 只返回一次）
%% ===================================================================

%% @doc 创建邀请。
%% Opts：
%%   expires_at     :: integer()  epoch 秒，缺省 now + 7 天；
%%   invitation_id  :: integer()  测试/编排注入；缺省在插入前经 elib_tsid:generate/0
%%                  惰性生成（被拒绝的 create 不消耗 id）。
%% 返回 {ok, View}，View 含 token（明文，唯一一次）与不含 token_digest 的投影。
-spec create(integer(), integer(), integer(), opts()) ->
    {ok, map()} | {error, {integer(), binary()}}.
create(InviterUid, OrgId, TargetUid, Opts) when is_map(Opts) ->
    case organization_invitation:valid_create(OrgId, InviterUid, TargetUid) of
        {error, _} = Rejected ->
            Rejected;
        ok ->
            ExpiresAt = maps:get(expires_at, Opts, default_expires_at()),
            InvitationId = maps:get(invitation_id, Opts, undefined),
            Token = organization_invitation:new_token(),
            Row = #{
                id => InvitationId,
                organization_id => OrgId,
                target_user_id => TargetUid,
                invited_by => InviterUid,
                token_digest => organization_invitation:token_digest(Token),
                expires_at => ExpiresAt
            },
            Tx = fun(Conn) -> create_tx(Conn, Row) end,
            case elib_pg:with_tx(Tx) of
                {ok, View} ->
                    %% 触达（P1）：邀请已落库，离线推送 fire-and-forget；
                    %% 发送结果绝不影响 create 结果。
                    organization_invitation_notify:notify_created(TargetUid),
                    {ok, View#{token => Token}};
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {error, Reason} ->
                    ?ERROR_LOG([
                        organization_invitation_create_failed,
                        OrgId,
                        InviterUid,
                        TargetUid,
                        Reason
                    ]),
                    {error, {500, <<"创建邀请失败，请稍后重试"/utf8>>}}
            end
    end;
create(_, _, _, _) ->
    {error, {400, <<"organization_id、invited_by、target_user_id 必须是正整数"/utf8>>}}.

create_tx(Conn, Row) ->
    OrgId = maps:get(organization_id, Row),
    InviterUid = maps:get(invited_by, Row),
    TargetUid = maps:get(target_user_id, Row),
    %% 1) 锁组织行 + 生命周期裁决（C16：archived 禁新写）
    case organization_invitation_pg:lock_organization_tx(Conn, OrgId) of
        {ok, #{<<"status">> := <<"active">>}} -> ok;
        {ok, _Archived} -> abort(409, <<"Organization 已归档，不能创建邀请"/utf8>>);
        {error, not_found} -> abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
    end,
    %% 2) inviter 必须是同 Org active 治理成员（owner/admin）
    case organization_invitation_pg:member_tx(Conn, OrgId, InviterUid) of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} when
            Role =:= <<"owner">>; Role =:= <<"admin">>
        ->
            ok;
        {ok, _} ->
            abort(403, <<"仅组织 Owner 或 Admin 可邀请成员"/utf8>>);
        {error, not_found} ->
            abort(403, <<"仅组织 Owner 或 Admin 可邀请成员"/utf8>>);
        {error, Reason2} ->
            throw({abort_tx, {internal, Reason2}})
    end,
    %% 3) target 已是 active 成员 → 邀请无意义（C11：立即加人走 legacy direct-add，
    %%    不是 invitation；restore 属独立 member command）
    case organization_invitation_pg:member_tx(Conn, OrgId, TargetUid) of
        {ok, #{<<"status">> := <<"active">>}} ->
            abort(409, <<"该用户已是组织成员"/utf8>>);
        {ok, _} ->
            ok;
        {error, not_found} ->
            ok;
        {error, Reason3} ->
            throw({abort_tx, {internal, Reason3}})
    end,
    %% 3.5) target 必须是已注册用户（与 member legacy direct-add 同口径同话术）；
    %%    校验后到插入之间 target 被删的并发窗口由第 5 步的 23503 映射兜底，
    %%    两条路径最终都是同一业务 404，不冒泡成 500。
    case organization_invitation_pg:target_user_tx(Conn, TargetUid) of
        {ok, _} ->
            ok;
        {error, not_found} ->
            abort(404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>);
        {error, Reason35} ->
            throw({abort_tx, {internal, Reason35}})
    end,
    %% 4) lazy expire sweep：先释放已到期占位，再插入（唯一索引是最终裁决）
    case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, Reason4} -> throw({abort_tx, {internal, Reason4}})
    end,
    %% 5) 插入；id 未注入时在插入前惰性生成（被拒绝的 create 不消耗 id）；
    %%    同 (org,target) 已有 pending → 部分唯一索引拒绝
    FinalRow =
        case maps:get(id, Row, undefined) of
            undefined -> Row#{id => elib_tsid:generate()};
            _ -> Row
        end,
    case organization_invitation_pg:insert_tx(Conn, FinalRow) of
        ok -> ok;
        {error, pending_conflict} -> abort(409, <<"该用户已有待处理邀请"/utf8>>);
        %% 并发窗口兜底：第 3.5 步校验通过后 target 仍可能在插入前被删，
        %% 外键违规（23503）转同一业务 404，不让用户看到 500。
        {error, target_user_missing} -> abort(404, <<"用户不存在（仅支持邀请已注册用户）"/utf8>>);
        {error, Reason5} -> throw({abort_tx, {internal, Reason5}})
    end,
    {ok,
        view(FinalRow#{
            status => <<"pending">>,
            responded_at => null,
            created_at => os:system_time(second)
        })}.

%% ===================================================================
%% accept —— Accept command（target-only / 一次性 / 幂等）
%% ===================================================================

%% @doc 目标用户凭明文 token 接受邀请。
%% Opts：
%%   membership_hook :: fun((Conn, InvitationRow) -> ok | {error, {Code, Msg}})
%%     首次消费成功后同事务调用（membership adapter 挂点）；V1 默认不注入。
%% 返回 {ok, View}：首次消费 already_accepted => false；
%% 幂等重放（重复 accept）already_accepted => true，同一终态视图。
-spec accept(integer(), integer(), binary(), opts()) ->
    {ok, map()} | {error, {integer(), binary()}}.
accept(TargetUid, OrgId, Token, Opts) when
    is_integer(TargetUid),
    TargetUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_binary(Token),
    Token =/= <<>>
->
    Digest = organization_invitation:token_digest(Token),
    Hook = maps:get(membership_hook, Opts, undefined),
    Tx = fun(Conn) -> accept_tx(Conn, TargetUid, OrgId, Digest, Hook, 2) end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_invitation_accept_failed, OrgId, TargetUid, Reason]),
            {error, {500, <<"接受邀请失败，请稍后重试"/utf8>>}}
    end;
accept(_, _, _, _) ->
    {error, {400, <<"token 与用户标识必须是有效值"/utf8>>}}.

accept_tx(Conn, TargetUid, OrgId, Digest, Hook, RetriesLeft) ->
    lock_accept_org_tx(Conn, OrgId),
    %% 1) lazy expire sweep（本 Org），使过期分支裁决基于行内真实 status
    case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, Reason0} -> throw({abort_tx, {internal, Reason0}})
    end,
    %% 2) (org, target, digest) 同语句裁决：非目标用户 / 跨 Org / 错 token 均命中不了行
    Found =
        case organization_invitation_pg:find_by_digest_tx(Conn, OrgId, TargetUid, Digest) of
            {ok, FoundRow} -> {ok, FoundRow};
            {error, not_found} -> {error, not_found};
            {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
        end,
    case Found of
        {error, not_found} ->
            abort(404, <<"邀请不存在或已失效"/utf8>>);
        {ok, Row} ->
            case organization_invitation:classify_accept(os:system_time(second), Row) of
                ok ->
                    consume_accept(
                        Conn,
                        Row,
                        Hook,
                        RetriesLeft,
                        fun(N) -> accept_tx(Conn, TargetUid, OrgId, Digest, Hook, N) end
                    );
                replay_accepted ->
                    {ok, (view(Row))#{already_accepted => true}};
                expired ->
                    abort(409, <<"邀请已过期"/utf8>>);
                rejected ->
                    abort(409, <<"邀请已被拒绝"/utf8>>);
                revoked ->
                    abort(409, <<"邀请已被撤销"/utf8>>)
            end
    end.

%% @doc 免口令接受（P0 定向邀请直达）：target 身份（JWT）即凭据。
%% 语义与 accept/4 完全同构（target-only / 一次性 / 幂等 / hook 同事务），
%% 仅行定位不同：digest 定位换成 (org, target) 最新 pending/accepted 行。
%% 定向邀请（target_user_id 必填，C11 V1）下这是安全的：行作用域仍是
%% target-only 同语句裁决，非目标用户命中不了行。
-spec accept_targeted(integer(), integer(), opts()) ->
    {ok, map()} | {error, {integer(), binary()}}.
accept_targeted(TargetUid, OrgId, Opts) when
    is_integer(TargetUid),
    TargetUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_map(Opts)
->
    Hook = maps:get(membership_hook, Opts, undefined),
    Tx = fun(Conn) -> accept_targeted_tx(Conn, TargetUid, OrgId, Hook, 2) end,
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_invitation_accept_targeted_failed, OrgId, TargetUid, Reason]),
            {error, {500, <<"接受邀请失败，请稍后重试"/utf8>>}}
    end;
accept_targeted(_, _, _) ->
    {error, {400, <<"用户标识必须是有效值"/utf8>>}}.

accept_targeted_tx(Conn, TargetUid, OrgId, Hook, RetriesLeft) ->
    lock_accept_org_tx(Conn, OrgId),
    %% 1) lazy expire sweep（同 accept_tx：过期裁决基于行内真实 status）
    case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, Reason0} -> throw({abort_tx, {internal, Reason0}})
    end,
    case organization_invitation_pg:find_latest_active_for_target_tx(Conn, OrgId, TargetUid) of
        {error, not_found} ->
            abort(404, <<"邀请不存在或已失效"/utf8>>);
        {error, Reason1} ->
            throw({abort_tx, {internal, Reason1}});
        {ok, Row} ->
            case organization_invitation:classify_accept(os:system_time(second), Row) of
                ok ->
                    consume_accept(
                        Conn,
                        Row,
                        Hook,
                        RetriesLeft,
                        fun(N) -> accept_targeted_tx(Conn, TargetUid, OrgId, Hook, N) end
                    );
                replay_accepted ->
                    {ok, (view(Row))#{already_accepted => true}};
                expired ->
                    abort(409, <<"邀请已过期"/utf8>>);
                rejected ->
                    abort(409, <<"邀请已被拒绝"/utf8>>);
                revoked ->
                    abort(409, <<"邀请已被撤销"/utf8>>)
            end
    end.

%% Retry：并发对手在本事务读后提交消费时的重入续延（幂等收敛到终态）。
consume_accept(Conn, Row, Hook, RetriesLeft, Retry) ->
    InvitationId = maps:get(<<"id">>, Row),
    case organization_invitation_pg:consume_pending_tx(Conn, InvitationId, accept) of
        {ok, Consumed} ->
            %% membership adapter 挂点：同事务、消费成功之后；失败即整体回滚
            case run_hook(Hook, Conn, Consumed) of
                ok ->
                    {ok, (view(Consumed))#{already_accepted => false}};
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    abort(Code, Msg);
                {error, Reason} ->
                    throw({abort_tx, {internal, {membership_hook, Reason}}})
            end;
        {error, not_pending} when RetriesLeft > 0 ->
            Retry(RetriesLeft - 1);
        {error, not_pending} ->
            abort(409, <<"邀请已被处理"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

run_hook(undefined, _Conn, _Row) -> ok;
run_hook(Hook, Conn, Row) when is_function(Hook, 2) -> Hook(Conn, Row).

%% ===================================================================
%% reject —— Reject command（target-only / 幂等）
%% ===================================================================

-spec reject(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
reject(TargetUid, OrgId, InvitationId) ->
    Tx = fun(Conn) -> decide_tx(Conn, TargetUid, OrgId, InvitationId, reject) end,
    command(Tx).

%% ===================================================================
%% revoke —— Revoke command（owner/admin / 幂等）
%% ===================================================================

-spec revoke(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
revoke(ActorUid, OrgId, InvitationId) ->
    Tx = fun(Conn) -> revoke_tx(Conn, ActorUid, OrgId, InvitationId) end,
    command(Tx).

revoke_tx(Conn, ActorUid, OrgId, InvitationId) ->
    %% 锁组织行 + 治理裁决
    case organization_invitation_pg:lock_organization_tx(Conn, OrgId) of
        {ok, #{<<"status">> := <<"active">>}} -> ok;
        {ok, _Archived} -> abort(409, <<"Organization 已归档"/utf8>>);
        {error, not_found} -> abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
    end,
    case organization_invitation_pg:member_tx(Conn, OrgId, ActorUid) of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} when
            Role =:= <<"owner">>; Role =:= <<"admin">>
        ->
            ok;
        _ ->
            abort(403, <<"仅组织 Owner 或 Admin 可撤销邀请"/utf8>>)
    end,
    decide_tx(Conn, 0, OrgId, InvitationId, revoke).

%% reject/revoke 共用：TargetUid=0 表示治理路径（不限 target），target-only 反之。
decide_tx(Conn, TargetUid, OrgId, InvitationId, Kind) ->
    case Kind of
        reject -> lock_accept_org_tx(Conn, OrgId);
        revoke -> ok
    end,
    case organization_invitation_pg:expire_due_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, Reason0} -> throw({abort_tx, {internal, Reason0}})
    end,
    case organization_invitation_pg:find_tx(Conn, OrgId, InvitationId, TargetUid) of
        {error, not_found} ->
            abort(404, <<"邀请不存在"/utf8>>);
        {error, Reason1} ->
            throw({abort_tx, {internal, Reason1}});
        {ok, Row} ->
            case maps:get(<<"status">>, Row) of
                <<"pending">> ->
                    case organization_invitation_pg:consume_pending_tx(Conn, InvitationId, Kind) of
                        {ok, Consumed} -> {ok, view(Consumed)};
                        {error, Reason2} -> throw({abort_tx, {internal, Reason2}})
                    end;
                <<"rejected">> when Kind =:= reject -> {ok, (view(Row))#{already_terminal => true}};
                <<"revoked">> when Kind =:= revoke -> {ok, (view(Row))#{already_terminal => true}};
                <<"accepted">> ->
                    abort(409, <<"邀请已被接受"/utf8>>);
                <<"expired">> ->
                    abort(409, <<"邀请已过期"/utf8>>);
                <<"revoked">> ->
                    abort(409, <<"邀请已被撤销"/utf8>>);
                <<"rejected">> ->
                    abort(409, <<"邀请已被拒绝"/utf8>>)
            end
    end.

%% ===================================================================
%% list —— target / org 两个视角（铁律 6：同语句作用域）
%% ===================================================================

-spec list_for_target(integer(), opts()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
list_for_target(TargetUid, Opts) when is_integer(TargetUid), TargetUid > 0, is_map(Opts) ->
    Status = maps:get(status, Opts, undefined),
    Limit = maps:get(limit, Opts, 20),
    Tx = fun(Conn) ->
        organization_invitation_pg:list_for_target_tx(Conn, TargetUid, Status, Limit)
    end,
    case elib_pg:with_tx(Tx) of
        {ok, Rows} ->
            {ok, [view(Row) || Row <- Rows]};
        {error, Reason} ->
            ?ERROR_LOG([organization_invitation_list_target_failed, TargetUid, Reason]),
            {error, {500, <<"获取邀请列表失败，请稍后重试"/utf8>>}}
    end;
list_for_target(_, _) ->
    {error, {400, <<"user_id 必须是正整数"/utf8>>}}.

-spec list_for_org(integer(), integer(), opts()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
list_for_org(ActorUid, OrgId, Opts) when
    is_integer(ActorUid),
    ActorUid > 0,
    is_integer(OrgId),
    OrgId > 0,
    is_map(Opts)
->
    Status = maps:get(status, Opts, undefined),
    Limit = maps:get(limit, Opts, 20),
    Tx = fun(Conn) -> list_org_tx(Conn, ActorUid, OrgId, Status, Limit) end,
    case elib_pg:with_tx(Tx) of
        {ok, Rows} ->
            {ok, [view(Row) || Row <- Rows]};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_invitation_list_org_failed, OrgId, Reason]),
            {error, {500, <<"获取邀请列表失败，请稍后重试"/utf8>>}}
    end;
list_for_org(_, _, _) ->
    {error, {400, <<"organization_id 与 user_id 必须是正整数"/utf8>>}}.

list_org_tx(Conn, ActorUid, OrgId, Status, Limit) ->
    case organization_invitation_pg:lock_organization_tx(Conn, OrgId) of
        {ok, #{<<"status">> := <<"active">>}} -> ok;
        {ok, _Archived} -> abort(409, <<"Organization 已归档"/utf8>>);
        {error, not_found} -> abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason1} -> throw({abort_tx, {internal, Reason1}})
    end,
    case organization_invitation_pg:member_tx(Conn, OrgId, ActorUid) of
        {ok, #{<<"status">> := <<"active">>, <<"role">> := Role}} when
            Role =:= <<"owner">>; Role =:= <<"admin">>
        ->
            ok;
        _ ->
            abort(403, <<"仅组织 Owner 或 Admin 可查看邀请列表"/utf8>>)
    end,
    organization_invitation_pg:list_for_org_tx(Conn, OrgId, Status, Limit).

%% ===================================================================
%% 内部
%% ===================================================================

%% Keep org -> invitation -> membership lock order, including accepted replays.
lock_accept_org_tx(Conn, OrgId) ->
    case organization_invitation_pg:lock_organization_for_share_tx(Conn, OrgId) of
        {ok, _} -> ok;
        {error, not_found} -> abort(404, <<"邀请不存在或已失效"/utf8>>);
        {error, Reason} -> throw({abort_tx, {internal, Reason}})
    end.

command(Tx) ->
    case elib_pg:with_tx(Tx) of
        {ok, View} ->
            {ok, View};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            ?ERROR_LOG([organization_invitation_command_failed, Reason]),
            {error, {500, <<"操作失败，请稍后重试"/utf8>>}}
    end.

-spec default_expires_at() -> integer().
default_expires_at() ->
    organization_invitation:expires_at_from_now(os:system_time(second)).

%% @doc 响应投影白名单：token_digest 永不出现；明文 token 只在 create 视图临时加入。
-spec view(map()) -> map().
view(Row) ->
    #{
        invitation_id => maps:get(<<"id">>, Row, maps:get(id, Row, undefined)),
        organization_id => maps:get(
            <<"organization_id">>,
            Row,
            maps:get(organization_id, Row, undefined)
        ),
        target_user_id => maps:get(
            <<"target_user_id">>,
            Row,
            maps:get(target_user_id, Row, undefined)
        ),
        invited_by => maps:get(<<"invited_by">>, Row, maps:get(invited_by, Row, undefined)),
        status => maps:get(<<"status">>, Row, maps:get(status, Row, undefined)),
        expires_at => maps:get(<<"expires_at">>, Row, maps:get(expires_at, Row, undefined)),
        responded_at => maps:get(
            <<"responded_at">>,
            Row,
            maps:get(responded_at, Row, undefined)
        ),
        created_at => maps:get(<<"created_at">>, Row, maps:get(created_at, Row, undefined))
    }.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
