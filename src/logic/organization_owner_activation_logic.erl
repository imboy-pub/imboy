-module(organization_owner_activation_logic).

%% 待激活 Owner 治理通道（GZAPP-06 / D11-D13 + §4.1）。
%%
%% 定位：owner_activation_invite 表（迁移 00000138）之上的平台侧治理与
%% 消费语义。与 organization_admin_logic 同层（平台通道，无租户 actor 校验；
%% 平台鉴权由 adm_acl 在 handler 层完成，操作者审计由 handler 层
%% adm_operation_log_ds 承担）。
%%
%% 语义冻结（任务卡 GZAPP-06 + 产品决策）：
%%   * D11 30 天 TTL：expires_at = now + 30d；到期不删除、不是状态值——
%%     过期由「expires_at =< now」谓词裁决，reactivate 刷新 TTL 回 pending；
%%   * D12 失败不回滚：短信发送尝试永远在事务提交之后（attempt_send/1）；
%%     失败只驱动 invite.status → sms_failed，企业/转移结果不动，可重发；
%%   * D13 换 Owner 恰一不变量：单事务「成员 upsert → 降旧 → 升新 →
%%     改 owner_id 投影 → 旧 invite superseded」，提交时由 00000126/00000127
%%     的 DEFERRABLE invariant 触发器做最终防线；事务失败整体回滚；
%%   * token 只存 sha256 digest（复用 organization_invitation:new_token/0 +
%%     token_digest/1 的同一 sha256 hex 模式）；明文只在 create/reactivate/
%%     resend/transfer 响应返回一次。**因为库里没有明文，resend/reactivate
%%     都以轮换 token 实现**（digest 原地替换，旧链接即时失效）；
%%   * 手机号 PII：mobile 明文只进 owner_activation_invite 表与 fake outbox；
%%     本模块所有日志/视图出站一律 imboy_mobile:mask/1（前3后4）。
%%
%% 错误码稳定口径：400 形状 / 404 不存在 / 409 状态裁决 / 500 兜底；
%% 不泄露内部 SQL/PII（错误消息不携带手机号原文）。

-export([
    %% 治理写
    admin_resend/2,
    admin_reactivate/2,
    admin_transfer_by_phone/4,
    %% 激活消费（合同级：单次消费 CAS）
    activate_by_token/1,
    %% 治理读
    admin_status/1,
    %% 事务后发送尝试（organization_admin_logic:admin_create_pending_owner 复用）
    attempt_send/1,
    record_send_result/2,
    %% 创建路径共享原语（organization_admin_logic:admin_create_pending_owner 复用）
    resolve_target_tx/3,
    new_invite_ctx/6,
    insert_invite_tx/2,
    live_invite_tx/2,
    %% 视图（出站白名单，供 handler 归一化）
    invite_view/1
]).

-include("log.hrl").

-define(TTL_SECONDS, 30 * 86400).

-define(INVITE_COLS,
    <<"id, organization_id, owner_user_id, mobile, status,",
        "       extract(epoch from expires_at)::bigint AS expires_at,",
        "       extract(epoch from last_sent_at)::bigint AS last_sent_at,", "       resend_count,",
        "       extract(epoch from consumed_at)::bigint AS consumed_at,",
        "       extract(epoch from created_at)::bigint AS created_at">>
).

-define(LIVE_INVITE_SQL,
    <<"SELECT ", ?INVITE_COLS/binary, " FROM owner_activation_invite",
        " WHERE organization_id = $1 AND status IN ('pending','sms_failed')",
        " ORDER BY id DESC LIMIT 1">>
).

%% ===================================================================
%% 写：重发激活短信（幂等可重入；token 轮换 + resend_count++）
%% D12：发送失败 → sms_failed，不回滚任何治理结果。
%% ===================================================================

-spec admin_resend(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_resend(_AdmUserId, OrgId) when is_integer(OrgId), OrgId > 0 ->
    Tx =
        fun(Conn) ->
            {ok, OrgRow} = lock_org_tx(Conn, OrgId),
            case live_invite_tx(Conn, OrgId) of
                {ok, Invite} ->
                    rotate_invite_tx(
                        Conn,
                        Invite#{org_name => maps:get(<<"name">>, OrgRow, <<>>)},
                        #{refresh_ttl => false}
                    );
                {error, not_found} ->
                    abort(404, <<"没有待处理的 Owner 激活邀请"/utf8>>);
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
        end,
    finish_send(Tx);
admin_resend(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ===================================================================
%% 写：重新激活（30 天 TTL 到期不删除；刷新 TTL 回 pending + token 轮换）
%% ===================================================================

-spec admin_reactivate(integer(), integer()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_reactivate(_AdmUserId, OrgId) when is_integer(OrgId), OrgId > 0 ->
    Tx =
        fun(Conn) ->
            {ok, OrgRow} = lock_org_tx(Conn, OrgId),
            case live_invite_tx(Conn, OrgId) of
                {ok, Invite} ->
                    rotate_invite_tx(
                        Conn,
                        Invite#{org_name => maps:get(<<"name">>, OrgRow, <<>>)},
                        #{refresh_ttl => true}
                    );
                {error, not_found} ->
                    abort(404, <<"没有可重新激活的 Owner 邀请（已激活或已作废）"/utf8>>);
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
        end,
    finish_send(Tx);
admin_reactivate(_, _) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% 事务（轮换）成功后：出站视图 + 事务外发送尝试（D12）。
finish_send(Tx) ->
    case elib_pg:with_tx(Tx) of
        {ok, RotateCtx} when is_map(RotateCtx) ->
            SendStatus = record_send_result(attempt_send(RotateCtx), RotateCtx),
            {ok, rotate_result_view(RotateCtx, SendStatus)};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {rollback, Reason} ->
            {error, internal_error(owner_activation_rotate_failed, Reason)};
        {error, Reason} ->
            {error, internal_error(owner_activation_rotate_failed, Reason)}
    end.

%% ===================================================================
%% 写：按手机号换 Owner（D13/§4.1）
%%   目标手机号 →
%%     已注册活跃 Human（status=1, account_type=0）→ 直接转移（不发邀请）；
%%     预创建待激活 Human（status=0）→ 复用：转移 + 新 pending invite；
%%     注销中/已删除 → 409；非 Human → 400；不存在 → 预创建后同 pending 路径。
%%   单事务：成员 upsert → 降旧 owner → 升新 owner → owner_id 投影 →
%%   旧 live invite superseded →（pending 路径）新 invite 行。
%%   事务失败整体回滚；短信失败不回滚转移（D12 同口径）。
%% ===================================================================

-spec admin_transfer_by_phone(integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_transfer_by_phone(_AdmUserId, OrgId, Mobile, PeerIP) when
    is_integer(OrgId), OrgId > 0, is_binary(Mobile)
->
    case imboy_mobile:normalize(Mobile) of
        {error, invalid} ->
            {error, {400, <<"owner_mobile 格式非法（5-20 位数字）"/utf8>>}};
        {ok, Mobile1} ->
            Tx = fun(Conn) -> transfer_tx(Conn, _AdmUserId, OrgId, Mobile1, PeerIP) end,
            case elib_pg:with_tx(Tx) of
                {ok, Result} when is_map(Result) ->
                    ok = ?INFO_LOG([
                        organization_owner_transferred_by_phone,
                        OrgId,
                        maps:get(<<"owner_user_id">>, Result),
                        imboy_mobile:mask(Mobile1)
                    ]),
                    finish_transfer_send(Result);
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    {error, {Code, Msg}};
                {rollback, Reason} ->
                    _ = ?ERROR_LOG([owner_transfer_by_phone_failed, OrgId, Reason]),
                    {error, {500, <<"更换 Owner 失败，请稍后重试"/utf8>>}};
                {error, Reason} ->
                    _ = ?ERROR_LOG([owner_transfer_by_phone_failed, OrgId, Reason]),
                    {error, {500, <<"更换 Owner 失败，请稍后重试"/utf8>>}}
            end
    end;
admin_transfer_by_phone(_, _, _, _) ->
    {error, {400, <<"organization_id 必须是正整数，owner_mobile 必填"/utf8>>}}.

transfer_tx(Conn, AdmUserId, OrgId, Mobile, PeerIP) ->
    case lock_org_tx(Conn, OrgId) of
        {ok, #{<<"owner_id">> := CurrentOwner} = OrgRow} ->
            OrgName = maps:get(<<"name">>, OrgRow, <<>>),
            case resolve_target_tx(Conn, Mobile, PeerIP) of
                {ok, TargetUid, TargetKind} ->
                    case TargetUid =:= CurrentOwner of
                        true ->
                            abort(400, <<"不能转移给当前 Owner（手机号归属未变）"/utf8>>);
                        false ->
                            do_transfer_tx(
                                Conn,
                                AdmUserId,
                                OrgId,
                                OrgName,
                                CurrentOwner,
                                TargetUid,
                                TargetKind,
                                Mobile
                            )
                    end;
                {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
                    abort(Code, Msg);
                {error, {500, Reason}} ->
                    throw({abort_tx, {internal, Reason}})
            end
    end.

do_transfer_tx(Conn, AdmUserId, OrgId, OrgName, CurrentOwner, TargetUid, TargetKind, Mobile) ->
    %% 1) 成员 upsert（目标行收敛为 member/active；组织行已锁，锁序一致）
    case upsert_member_tx(Conn, OrgId, TargetUid) of
        ok ->
            %% 2) 降旧 → 升新 → 投影（恰一 active owner 语句序，与
            %%    organization_admin_logic:do_transfer_locked 同构）
            ok = chain_owner_transfer_tx(Conn, OrgId, CurrentOwner, TargetUid),
            %% 3) 旧 live invite → superseded（D13：旧邀请即时作废）
            {ok, _} = supersede_live_tx(Conn, OrgId),
            %% 4) pending 路径：新 invite（registered 目标已可登录，不发邀请）
            transfer_result_tx(
                Conn, AdmUserId, OrgId, OrgName, CurrentOwner, TargetUid, TargetKind, Mobile
            )
    end.

chain_owner_transfer_tx(Conn, OrgId, CurrentOwner, TargetUid) ->
    case organization_owner_store:demote_previous_owner_tx(Conn, OrgId, CurrentOwner) of
        ok ->
            case organization_owner_store:promote_target_tx(Conn, OrgId, TargetUid) of
                ok ->
                    case
                        organization_owner_store:update_owner_projection_tx(
                            Conn, OrgId, TargetUid
                        )
                    of
                        {ok, _} -> ok;
                        {error, Reason} -> throw({abort_tx, {internal, Reason}})
                    end;
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end;
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

transfer_result_tx(Conn, AdmUserId, OrgId, OrgName, CurrentOwner, TargetUid, TargetKind, Mobile) ->
    Base = #{
        <<"organization_id">> => OrgId,
        <<"owner_user_id">> => TargetUid,
        <<"previous_owner_id">> => CurrentOwner,
        <<"org_name">> => OrgName
    },
    case TargetKind of
        registered ->
            {ok, Base#{
                <<"mode">> => <<"direct_transfer">>,
                <<"invite">> => null
            }};
        pending ->
            Token = organization_invitation:new_token(),
            InviteCtx = new_invite_ctx(OrgId, TargetUid, Mobile, AdmUserId, Token, OrgName),
            case insert_invite_tx(Conn, InviteCtx) of
                ok ->
                    {ok, Base#{
                        <<"mode">> => <<"pending_transfer">>,
                        <<"invite">> => InviteCtx
                    }};
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
    end.

%% 转移提交后：pending 路径做一次发送尝试（D12：失败只置 sms_failed，
%% 转移本身不回滚）；registered 路径无短信。
finish_transfer_send(#{<<"mode">> := <<"pending_transfer">>} = Result) ->
    InviteCtx = maps:get(<<"invite">>, Result),
    SendStatus = record_send_result(attempt_send(InviteCtx), InviteCtx),
    {ok, transfer_result_view(Result, SendStatus)};
finish_transfer_send(Result) ->
    {ok, transfer_result_view(Result, none)}.

%% ===================================================================
%% 激活消费（合同级；单次消费 CAS）
%%   token（明文，一次有效）→ digest 定位 → pending|sms_failed 且未过期 →
%%   CAS {status→activated, consumed_at} 恰 1 行 + user 激活（status 0→1）。
%%   重复消费 → 409；过期 → 409（走 reactivate）；digest 无命中 → 404。
%%   落地页/短信链接载体不在本卡；本函数 + adm consume 端点即 API 合同。
%% ===================================================================

-spec activate_by_token(binary()) -> {ok, map()} | {error, {integer(), binary()}}.
activate_by_token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    Digest = organization_invitation:token_digest(Token),
    Now = os:system_time(second),
    Tx =
        fun(Conn) ->
            case find_by_digest_tx(Conn, Digest) of
                {ok, #{<<"id">> := InviteId, <<"status">> := Status} = Row} ->
                    case Status of
                        <<"activated">> ->
                            abort(409, <<"激活链接已被使用"/utf8>>);
                        <<"superseded">> ->
                            abort(409, <<"激活链接已作废（Owner 已更换）"/utf8>>);
                        Live when Live =:= <<"pending">>; Live =:= <<"sms_failed">> ->
                            maybe_consume_tx(Conn, InviteId, Row, Now)
                    end;
                {error, not_found} ->
                    abort(404, <<"激活链接无效"/utf8>>);
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
        end,
    case elib_pg:with_tx(Tx) of
        {ok, Result} ->
            ok = ?INFO_LOG([
                owner_activation_consumed,
                maps:get(<<"invite_id">>, Result),
                maps:get(<<"organization_id">>, Result)
            ]),
            {ok, Result};
        {error, {Code, Msg}} when is_integer(Code), is_binary(Msg) ->
            {error, {Code, Msg}};
        {rollback, Reason} ->
            _ = ?ERROR_LOG([owner_activation_consume_failed, Reason]),
            {error, {500, <<"激活失败，请稍后重试"/utf8>>}};
        {error, Reason} ->
            %% R3-4：内部失败（with_tx 把 throw({abort_tx, {internal, R}}) 归一为
            %% {error, {internal, R}}，Code 是原子）落不进上面的整数守卫——
            %% 没有本子句会以 case_clause 崩掉请求进程，而不是返回 500 兜底。
            _ = ?ERROR_LOG([owner_activation_consume_failed, Reason]),
            {error, {500, <<"激活失败，请稍后重试"/utf8>>}}
    end;
activate_by_token(_) ->
    {error, {400, <<"token 必填"/utf8>>}}.

maybe_consume_tx(Conn, InviteId, Row, Now) ->
    ExpiresAt = maps:get(<<"expires_at">>, Row, Now),
    case ExpiresAt =< Now of
        true ->
            abort(409, <<"激活链接已过期，请联系管理员重新激活"/utf8>>);
        false ->
            case consume_invite_tx(Conn, InviteId) of
                {ok, 1} ->
                    OwnerUid = maps:get(<<"owner_user_id">>, Row),
                    case activate_user_tx(Conn, OwnerUid) of
                        {ok, _} ->
                            {ok, #{
                                <<"invite_id">> => InviteId,
                                <<"organization_id">> => maps:get(<<"organization_id">>, Row),
                                <<"owner_user_id">> => OwnerUid
                            }};
                        {error, Reason} ->
                            throw({abort_tx, {internal, Reason}})
                    end;
                {ok, 0} ->
                    %% 并发重放：另一请求先消费成功
                    abort(409, <<"激活链接已被使用"/utf8>>);
                {error, Reason} ->
                    throw({abort_tx, {internal, Reason}})
            end
    end.

%% 单次消费 CAS：consumed_at 恰写一次（ck_owner_activation_invite_consume_shape 兜底）
consume_invite_tx(Conn, InviteId) ->
    Sql =
        <<"UPDATE owner_activation_invite SET status = 'activated', consumed_at = now(),",
            " updated_at = now()"
            " WHERE id = $1 AND consumed_at IS NULL AND status IN ('pending','sms_failed')">>,
    elib_pg:execute(Conn, Sql, [InviteId]).

%% user 激活：status 0→1 CAS；已是 1（如用户已在别处真实注册）幂等放行。
activate_user_tx(Conn, OwnerUid) ->
    Sql = <<"UPDATE \"user\" SET status = 1 WHERE id = $1 AND status = 0">>,
    elib_pg:execute(Conn, Sql, [OwnerUid]).

%% ===================================================================
%% 读：待激活 Owner 状态（治理面板）
%% ===================================================================

-spec admin_status(integer()) -> {ok, map()} | {error, {integer(), binary()}}.
admin_status(OrgId) when is_integer(OrgId), OrgId > 0 ->
    Sql =
        <<"SELECT o.id, o.name, o.owner_id, o.status AS org_status,",
            " u.status AS owner_user_status, u.account_type AS owner_account_type",
            " FROM organization o LEFT JOIN \"user\" u ON u.id = o.owner_id", " WHERE o.id = $1">>,
    case elib_pg:one(Sql, [OrgId], undefined) of
        {ok, Row} when is_map(Row), map_size(Row) > 0 ->
            OwnerUserStatus = maps:get(<<"owner_user_status">>, Row),
            Now = os:system_time(second),
            InviteView =
                case live_invite(OrgId) of
                    {ok, Invite} -> invite_view_with_ttl(Invite, Now);
                    _ -> latest_invite_view(OrgId)
                end,
            {ok, #{
                <<"organization_id">> => OrgId,
                <<"organization_name">> => maps:get(<<"name">>, Row),
                <<"organization_status">> => maps:get(<<"org_status">>, Row),
                <<"owner_user_id">> => maps:get(<<"owner_id">>, Row),
                <<"owner_activated">> => OwnerUserStatus =:= 1,
                <<"invite">> => InviteView
            }};
        {ok, _} ->
            {error, {404, <<"Organization 不存在"/utf8>>}};
        {error, Reason} ->
            _ = ?ERROR_LOG([owner_activation_status_failed, OrgId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end;
admin_status(_) ->
    {error, {400, <<"organization_id 必须是正整数"/utf8>>}}.

%% ===================================================================
%% 事务后发送尝试（D12）+ 状态回写（事务外单语句；失败不抛错——
%% 治理结果已提交，回写失败只损失审计精度，不阻断响应）
%% ===================================================================

%% InviteCtx（tx 内构建，atom 键）：#{invite_id, organization_id, owner_user_id,
%% mobile, token, expires_at, resend_count, org_name}——token 明文只在内存/响应一次。
-spec attempt_send(map()) -> ok | {error, binary()}.
attempt_send(#{mobile := Mobile, token := Token, org_name := OrgName}) ->
    imboy_sms_provider:send_activation(Mobile, Token, OrgName);
attempt_send(_) ->
    {error, invalid_ctx}.

-spec record_send_result(ok | {error, binary()} | term(), map()) -> binary().
record_send_result(ok, InviteCtx) ->
    _ = mark_last_sent(maps:get(invite_id, InviteCtx, undefined)),
    <<"sent">>;
record_send_result({error, Reason}, InviteCtx) ->
    InviteId = maps:get(invite_id, InviteCtx, undefined),
    _ = mark_sms_failed(InviteId),
    _ = ?ERROR_LOG([
        owner_activation_sms_failed,
        InviteId,
        {reason, Reason},
        {mobile_masked, imboy_mobile:mask(maps:get(mobile, InviteCtx, <<>>))}
    ]),
    <<"sms_failed">>;
record_send_result(_, InviteCtx) ->
    _ = mark_sms_failed(maps:get(invite_id, InviteCtx, undefined)),
    <<"sms_failed">>.

mark_last_sent(InviteId) when is_integer(InviteId) ->
    Sql =
        <<
            "UPDATE owner_activation_invite SET last_sent_at = now(), updated_at = now()"
            " WHERE id = $1 AND status IN ('pending','sms_failed')"
        >>,
    _ = elib_pg:execute(Sql, [InviteId]),
    ok;
mark_last_sent(_) ->
    ok.

mark_sms_failed(InviteId) when is_integer(InviteId) ->
    Sql =
        <<"UPDATE owner_activation_invite SET status = 'sms_failed', last_sent_at = now(),",
            " updated_at = now() WHERE id = $1 AND status = 'pending'">>,
    _ = elib_pg:execute(Sql, [InviteId]),
    ok;
mark_sms_failed(_) ->
    ok.

%% ===================================================================
%% 事务原语（本模块 + organization_admin_logic:admin_create_pending_owner 复用；
%% new_invite_ctx/insert_invite_ctx/attempt_send 为跨模块共享面）
%% ===================================================================

%% 锁组织行 + archived 门禁（404/409 直接 throw abort_tx）。
lock_org_tx(Conn, OrgId) ->
    case organization_owner_store:lock_organization_tx(Conn, OrgId) of
        {ok, #{<<"status">> := <<"active">>} = Row} ->
            case org_name_tx(Conn, OrgId) of
                {ok, Name} -> {ok, Row#{<<"name">> => Name}};
                {error, _} -> {ok, Row#{<<"name">> => <<>>}}
            end;
        {ok, _Archived} ->
            abort(409, <<"Organization 已归档，Owner 治理被拒绝"/utf8>>);
        {error, not_found} ->
            abort(404, <<"Organization 不存在"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

org_name_tx(Conn, OrgId) ->
    case elib_pg:query(Conn, <<"SELECT name FROM organization WHERE id = $1">>, [OrgId]) of
        {ok, [#{<<"name">> := Name}]} -> {ok, Name};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% 目标手机号 → {ok, Uid, registered|pending} | 业务拒绝。
%% 手机号唯一（uk_mobile）保证至多一行；多状态裁决 fail-closed。
resolve_target_tx(Conn, Mobile, PeerIP) ->
    Sql = <<"SELECT id, status, account_type FROM \"user\" WHERE mobile = $1">>,
    case elib_pg:query(Conn, Sql, [Mobile]) of
        {ok, []} ->
            create_pending_user_tx(Conn, Mobile, PeerIP);
        {ok, [#{<<"id">> := Uid, <<"status">> := 1, <<"account_type">> := 0}]} ->
            {ok, Uid, registered};
        {ok, [#{<<"status">> := 1, <<"account_type">> := _}]} ->
            {error, {400, <<"该手机号归属非人类账号，不能作为 Owner"/utf8>>}};
        {ok, [#{<<"id">> := Uid, <<"status">> := 0, <<"account_type">> := 0}]} ->
            %% 既有预创建待激活 Human（可能来自其他企业的 pending 创建）：复用
            {ok, Uid, pending};
        {ok, [#{<<"status">> := 0, <<"account_type">> := _}]} ->
            {error, {400, <<"该手机号归属非人类账号，不能作为 Owner"/utf8>>}};
        {ok, [#{<<"status">> := _}]} ->
            {error, {409, <<"该手机号归属账号不可用（注销中/已删除）"/utf8>>}};
        {ok, _OtherShape} ->
            {error, {500, owner_target_row_shape}};
        {error, Reason} ->
            {error, {500, Reason}}
    end.

%% 预创建待激活 Human（D11：真实但不可登录——status=0 复用 user 表既有
%% 「禁用」语义，account_type=0 Human，不造 Bot/占位类型）：
%%   * password：随机 32 字节盐渍哈希——任何人（含平台）都不知道明文，
%%     激活前不可登录；激活走 token 消费链，不依赖该口令；
%%   * account：gzpo_<TSID> 唯一占位（uk_account）；
%%   * mobile：唯一键即定位键（与既有真实注册流共用一个 user 域）。
create_pending_user_tx(Conn, Mobile, PeerIP) ->
    Uid = elib_tsid:generate(user),
    Account = <<"gzpo_", (integer_to_binary(Uid))/binary>>,
    Password = elib_password:generate(crypto:strong_rand_bytes(32)),
    Nickname = <<"待激活 Owner"/utf8>>,
    Sql =
        <<"INSERT INTO \"user\" (id, password, account, mobile, nickname, status,",
            " account_type, reg_ip, reg_cosv, source)",
            " VALUES ($1, $2, $3, $4, $5, 0, 0, $6, 'adm_pending_owner', 'adm_pending_owner')">>,
    case elib_pg:execute(Conn, Sql, [Uid, Password, Account, Mobile, Nickname, PeerIP]) of
        {ok, 1} ->
            {ok, Uid, pending};
        {ok, _} ->
            {error, {500, pending_user_affected}};
        {error, Reason} ->
            {error, {500, Reason}}
    end.

upsert_member_tx(Conn, OrgId, TargetUid) ->
    Sql =
        <<"INSERT INTO organization_member (organization_id, user_id, role, status, joined_at)",
            " VALUES ($1, $2, 'member', 'active', now())",
            " ON CONFLICT (organization_id, user_id)"
            " DO UPDATE SET role = 'member', status = 'active', updated_at = now()">>,
    case elib_pg:execute(Conn, Sql, [OrgId, TargetUid]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

supersede_live_tx(Conn, OrgId) ->
    Sql =
        <<
            "UPDATE owner_activation_invite SET status = 'superseded', updated_at = now()"
            " WHERE organization_id = $1 AND status IN ('pending','sms_failed')"
        >>,
    elib_pg:execute(Conn, Sql, [OrgId]).

%% resend/reactivate 共用：token 轮换（库里无明文 → 原地替换 digest，
%% 旧链接即时失效）；refresh_ttl 决定是否重置 30d；resend_count 恒 +1。
rotate_invite_tx(Conn, #{<<"id">> := InviteId} = Invite, Opts) ->
    Token = organization_invitation:new_token(),
    Digest = organization_invitation:token_digest(Token),
    Now = os:system_time(second),
    ExpiresAt =
        case maps:get(refresh_ttl, Opts, false) of
            true -> Now + ?TTL_SECONDS;
            false -> maps:get(<<"expires_at">>, Invite, Now)
        end,
    ResendCount = maps:get(<<"resend_count">>, Invite, 0) + 1,
    Sql =
        <<
            "UPDATE owner_activation_invite SET token_digest = $2, status = 'pending',"
            " expires_at = to_timestamp($3), resend_count = $4, updated_at = now()"
            " WHERE id = $1 AND status IN ('pending','sms_failed')"
        >>,
    case elib_pg:execute(Conn, Sql, [InviteId, Digest, ExpiresAt, ResendCount]) of
        {ok, 1} ->
            {ok, #{
                invite_id => InviteId,
                organization_id => maps:get(<<"organization_id">>, Invite),
                owner_user_id => maps:get(<<"owner_user_id">>, Invite),
                mobile => maps:get(<<"mobile">>, Invite),
                token => Token,
                expires_at => ExpiresAt,
                resend_count => ResendCount,
                org_name => maps:get(org_name, Invite, <<>>)
            }};
        {ok, _} ->
            abort(409, <<"邀请状态已变化，请刷新后重试"/utf8>>);
        {error, Reason} ->
            throw({abort_tx, {internal, Reason}})
    end.

%% 创建路径的 invite 上下文（organization_admin_logic:admin_create_pending_owner
%% 与本模块 transfer 共用形状；token 明文只随响应出站一次）。
new_invite_ctx(OrgId, OwnerUid, Mobile, AdmUserId, Token, OrgName) ->
    #{
        invite_id => elib_tsid:generate(owner_activation_invite),
        organization_id => OrgId,
        owner_user_id => OwnerUid,
        mobile => Mobile,
        token => Token,
        expires_at => os:system_time(second) + ?TTL_SECONDS,
        created_by => AdmUserId,
        org_name => OrgName
    }.

insert_invite_tx(
    Conn,
    #{
        invite_id := Id,
        organization_id := OrgId,
        owner_user_id := OwnerUid,
        mobile := Mobile,
        token := Token,
        expires_at := ExpiresAt,
        created_by := AdmUserId
    }
) ->
    Digest = organization_invitation:token_digest(Token),
    Sql =
        <<
            "INSERT INTO owner_activation_invite"
            " (id, organization_id, owner_user_id, mobile, status, token_digest,"
            "  expires_at, resend_count, created_by, created_at, updated_at)"
            " VALUES ($1, $2, $3, $4, 'pending', $5, to_timestamp($6), 0, $7, now(), now())"
        >>,
    case elib_pg:execute(Conn, Sql, [Id, OrgId, OwnerUid, Mobile, Digest, ExpiresAt, AdmUserId]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, invite_affected};
        {error, Reason} -> {error, Reason}
    end.

live_invite_tx(Conn, OrgId) ->
    case elib_pg:query(Conn, ?LIVE_INVITE_SQL, [OrgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

live_invite(OrgId) ->
    case elib_pg:query(?LIVE_INVITE_SQL, [OrgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

latest_invite_view(OrgId) ->
    Sql =
        <<"SELECT ", ?INVITE_COLS/binary, " FROM owner_activation_invite",
            " WHERE organization_id = $1 ORDER BY id DESC LIMIT 1">>,
    case elib_pg:query(Sql, [OrgId]) of
        {ok, [Row | _]} -> invite_view(Row);
        _ -> null
    end.

find_by_digest_tx(Conn, Digest) ->
    Sql =
        <<"SELECT ", ?INVITE_COLS/binary,
            " FROM owner_activation_invite"
            " WHERE token_digest = $1">>,
    case elib_pg:query(Conn, Sql, [Digest]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% 出站视图（白名单：mobile 明文永不出站；TSID int 由 handler 归一 string）
%% ===================================================================

invite_view(Row) ->
    #{
        <<"invite_id">> => maps:get(<<"id">>, Row),
        <<"organization_id">> => maps:get(<<"organization_id">>, Row),
        <<"owner_user_id">> => maps:get(<<"owner_user_id">>, Row),
        <<"status">> => maps:get(<<"status">>, Row),
        <<"mobile_masked">> => imboy_mobile:mask(maps:get(<<"mobile">>, Row, <<>>)),
        <<"expires_at">> => maps:get(<<"expires_at">>, Row),
        <<"last_sent_at">> => maps:get(<<"last_sent_at">>, Row),
        <<"resend_count">> => maps:get(<<"resend_count">>, Row),
        <<"consumed_at">> => maps:get(<<"consumed_at">>, Row),
        <<"created_at">> => maps:get(<<"created_at">>, Row)
    }.

invite_view_with_ttl(Row, Now) ->
    ExpiresAt = maps:get(<<"expires_at">>, Row, Now),
    (invite_view(Row))#{
        <<"ttl_remaining_seconds">> => ExpiresAt - Now,
        <<"expired">> => ExpiresAt =< Now
    }.

%% resend/reactivate 出站：invite 视图 + 新 token（只此一次）+ 发送结果。
rotate_result_view(RotateCtx, SendStatus) ->
    #{
        <<"invite">> => #{
            <<"invite_id">> => maps:get(invite_id, RotateCtx),
            <<"organization_id">> => maps:get(organization_id, RotateCtx),
            <<"owner_user_id">> => maps:get(owner_user_id, RotateCtx),
            <<"status">> =>
                case SendStatus of
                    <<"sent">> -> <<"pending">>;
                    _ -> <<"sms_failed">>
                end,
            <<"mobile_masked">> => imboy_mobile:mask(maps:get(mobile, RotateCtx, <<>>)),
            <<"expires_at">> => maps:get(expires_at, RotateCtx),
            <<"resend_count">> => maps:get(resend_count, RotateCtx),
            <<"activation_token">> => maps:get(token, RotateCtx)
        },
        <<"sms_sent">> => SendStatus =:= <<"sent">>
    }.

transfer_result_view(Result, SendStatus) ->
    Invite =
        case maps:get(<<"invite">>, Result) of
            null ->
                null;
            Ctx ->
                #{
                    <<"invite_id">> => maps:get(invite_id, Ctx),
                    <<"organization_id">> => maps:get(organization_id, Ctx),
                    <<"owner_user_id">> => maps:get(owner_user_id, Ctx),
                    <<"status">> =>
                        case SendStatus of
                            <<"sent">> -> <<"pending">>;
                            none -> <<"pending">>;
                            _ -> <<"sms_failed">>
                        end,
                    <<"mobile_masked">> => imboy_mobile:mask(maps:get(mobile, Ctx, <<>>)),
                    <<"expires_at">> => maps:get(expires_at, Ctx),
                    <<"resend_count">> => 0,
                    <<"activation_token">> => maps:get(token, Ctx)
                }
        end,
    #{
        <<"organization_id">> => maps:get(<<"organization_id">>, Result),
        <<"owner_user_id">> => maps:get(<<"owner_user_id">>, Result),
        <<"previous_owner_id">> => maps:get(<<"previous_owner_id">>, Result),
        <<"mode">> => maps:get(<<"mode">>, Result),
        <<"invite">> => Invite,
        <<"sms_sent">> => SendStatus =:= <<"sent">>
    }.

internal_error(Tag, Reason) ->
    _ = ?ERROR_LOG([Tag, Reason]),
    {500, <<"操作失败，请稍后重试"/utf8>>}.

-spec abort(integer(), binary()) -> no_return().
abort(Code, Msg) ->
    throw({abort_tx, {Code, Msg}}).
