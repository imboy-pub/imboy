%%% @doc EB-11-A06：ACK 不删真源 / 提交失败 fail-closed / 到期前与 hold 中不删 / 到期后
%%% **精确 purge** 及附件同生命周期——**均有 DB 证据**（test-only）。
%%%
%%% 依据：plan §2.1 #16/#17/#18、EB-11-A06、`agents/a5/EB-11/ORDER.md` §1/§4（BUG-01 口径）。
%%%
%%% ## 边界声明
%%%
%%%   * **BUG-01** 已由迁移 123 修复：device_id 为 NULL 时重复 ACK 必须仍恰好一行。
%%%   * **fail-closed 的注入点**：`canonical_tx` 端口覆盖是 application 层显式支持的参数
%%%     （`eb_e2e_tx_probe`）⇒ 该负例的层次是「application + 真 PG」，不是 HTTP 层。
%%%   * **对象字节**：FND-4 修复后，purge 必须先回收对象字节再删 DB 元数据；测试不得
%%%     手工清理来制造零残留。
%%%
%%% ## 时间设计（为什么消息都用**已到期**的 accepted_at）
%%%
%%% schema 的 purge guard 对**未到期**行一律 23514 拒绝物理删除（且无 GUC 旁路），所以：
%%%   * 本 run 的消息（A01 + A06 的 M1/M2 + 重试）一律注入 `accepted_at = now-1095d-1h`
%%%     ⇒ retain_until ≈ now-1h（**已到期**，可被 bounded purge 物理清理）；
%%%   * 只保留**一条**未到期的 FU 消息作为「到期前 purge=0」的被测对象 —— 它的残留
%%%     （1 条消息 + 被其 RESTRICT FK 钉住的会话/客户/渠道标识）是 A05 residual 的
%%%     **声明项**，不是遗漏。
-module(eb_e2e_a06).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    Owner1 = maps:get(owner1, Scope),
    B = maps:get(b_user, Ctx),
    IdentityB = maps:get(identity_b, Ctx),
    AcceptedAt = maps:get(accepted_at, Ctx),
    BTok = maps:get(b, maps:get(tokens, Scope)),
    ConvId = maps:get(conversation_id, Ctx),
    ContactId = maps:get(contact_id, Ctx),
    OutMsgId = maps:get(out_message_id, Ctx),
    KeyRef = maps:get(key_ref, Ctx),
    io:format("~n== EB-11-A06 ACK / 保留期 / purge / fail-closed（DB 证据）==~n"),

    %% ---------------------------------------------------------------
    %% 1) ACK 只改 delivery：canonical 真源不变、不删除
    %% ---------------------------------------------------------------
    Before = canonical_snapshot(Org1, OutMsgId),
    AckPath = eb_e2e_lib:tenant_path(
        Org1,
        <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages/",
            (integer_to_binary(OutMsgId))/binary, "/ack">>,
        Ws1
    ),
    AckBody = #{<<"recipient_ref">> => <<"contact:", (integer_to_binary(ContactId))/binary>>},
    Ack1 = eb_e2e_lib:post(BTok, AckPath, AckBody),
    Delivery1 = delivery_count(Org1, OutMsgId),
    Ack2 = eb_e2e_lib:post(BTok, AckPath, AckBody),
    Delivery2 = delivery_count(Org1, OutMsgId),
    After = canonical_snapshot(Org1, OutMsgId),
    eb_e2e_lib:assert(
        <<"EB-11-A06.1">>,
        io_lib:format(
            "ACK 前后 canonical 真源 hash 逐字不变（含密文/Org/Ws/sender/actor/retain_until）：~ts → ~ts",
            [binary:part(Before, 0, 16), binary:part(After, 0, 16)]
        ),
        Before =:= After andalso canonical_count(Org1, OutMsgId) =:= 1
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A06.2">>,
        io_lib:format(
            "ACK 只写 delivery（status=~p；第一次后行数=~p，第二次后行数=~p；HTTP=~p/~p）",
            [
                delivery_status(Org1, OutMsgId),
                Delivery1,
                Delivery2,
                eb_e2e_lib:status(Ack1),
                eb_e2e_lib:status(Ack2)
            ]
        ),
        eb_e2e_lib:status(Ack1) =:= 200 andalso eb_e2e_lib:status(Ack2) =:= 200 andalso
            delivery_status(Org1, OutMsgId) =:= <<"delivered">>
    ),
    %% BUG-01 已闭合：迁移 123 的 NULLS NOT DISTINCT 使 NULL-device ACK 保持幂等。
    eb_e2e_lib:assert(
        <<"EB-11-A06.3">>,
        io_lib:format(
            "device_id=NULL 的 ACK 重放保持幂等：第一次/第二次后 delivery 行数均为 1（实测 ~p/~p）",
            [Delivery2, Delivery1]
        ),
        Delivery1 =:= 1 andalso Delivery2 =:= 1
    ),
    eb_e2e_lib:evidence(
        "a06-ack-null-device-idempotency.txt",
        "message_id=~p recipient_ref=contact:~p device_id=NULL delivery_rows_after_1st=~p after_2nd=~p"
        " canonical_hash_before=~ts after=~ts canonical_rows=~p",
        [OutMsgId, ContactId, Delivery1, Delivery2, Before, After, canonical_count(Org1, OutMsgId)]
    ),

    %% ---------------------------------------------------------------
    %% 2) 提交失败 fail-closed（application + 真 PG；注入点 = canonical_tx 端口）
    %% ---------------------------------------------------------------
    ok = eb_e2e_tx_probe:reset(),
    ok = eb_e2e_tx_probe:set_mode(fail),
    AuditBefore = accept_audit_count(Org1),
    RowsBefore = message_count(Org1, Ws1),
    NotifyRef = make_ref(),
    Failed = enterprise_business_facade:append_message(Org1, #{
        workspace_id => Ws1,
        conversation_id => ConvId,
        client_msg_id => eb_e2e_lib:canary(<<"CMID-FAIL">>),
        sender_type => <<"business_identity">>,
        body => eb_e2e_lib:canary(<<"MSGFail">>),
        identity_id => IdentityB,
        actor_user_id => B,
        accepted_at => AcceptedAt,
        key_ref => KeyRef,
        canonical_tx => eb_e2e_tx_probe,
        notify => fun(Notification) ->
            persistent_term:put({eb_e2e_a06, notification, NotifyRef}, Notification),
            ok
        end
    }),
    Notified = persistent_term:get({eb_e2e_a06, notification, NotifyRef}, none),
    eb_e2e_lib:assert(
        <<"EB-11-A06.4">>,
        io_lib:format(
            "提交失败 fail-closed：不返回 accepted（实测 ~p）、零新增行（~p→~p）、不发 realtime resource id（~p）、无接受审计",
            [short(Failed), RowsBefore, message_count(Org1, Ws1), Notified]
        ),
        element(1, Failed) =:= error andalso RowsBefore =:= message_count(Org1, Ws1) andalso
            Notified =:= none andalso accept_audit_count(Org1) =:= AuditBefore
    ),
    eb_e2e_lib:evidence(
        "a06-fail-closed.txt",
        "fail_mode_attempt=~p rows_before=~p rows_after=~p accept_audit_before=~p after=~p "
        "notification=~p tx_probe_calls=~p",
        [
            Failed,
            RowsBefore,
            message_count(Org1, Ws1),
            AuditBefore,
            accept_audit_count(Org1),
            Notified,
            eb_e2e_tx_probe:calls(accept)
        ]
    ),
    %% 恢复：同一幂等键重试 ⇒ 恰好一条消息、恰好一条接受审计（真实端口）
    ok = eb_e2e_tx_probe:set_mode(ok),
    Retry = enterprise_business_facade:append_message(Org1, #{
        workspace_id => Ws1,
        conversation_id => ConvId,
        client_msg_id => eb_e2e_lib:canary(<<"CMID-FAIL">>),
        sender_type => <<"business_identity">>,
        body => eb_e2e_lib:canary(<<"MSGFail">>),
        identity_id => IdentityB,
        actor_user_id => B,
        accepted_at => AcceptedAt,
        key_ref => KeyRef
    }),
    RowsAdded = message_count(Org1, Ws1) - RowsBefore,
    eb_e2e_lib:assert(
        <<"EB-11-A06.5">>,
        io_lib:format(
            "恢复后同一幂等键重试恰好一条消息 + 一条接受审计（新增行=~p，accept 审计增量=~p）",
            [RowsAdded, accept_audit_count(Org1) - AuditBefore]
        ),
        element(1, Retry) =:= ok andalso RowsAdded =:= 1 andalso
            accept_audit_count(Org1) - AuditBefore =:= 1
    ),

    %% ---------------------------------------------------------------
    %% 3) 注入时钟边界：FU（未到期，保留）× M1/M2（已到期）
    %% ---------------------------------------------------------------
    FU = append_message_as_b(Ctx, <<"CMID-FU">>, default, <<"MSGFU">>),
    M1 = append_message_as_b(Ctx, <<"CMID-EXPIRED-1">>, AcceptedAt, <<"MSGEXP1">>),
    M2 = append_message_as_b(Ctx, <<"CMID-EXPIRED-2">>, AcceptedAt, <<"MSGEXP2">>),
    M1Asset = attach_asset(Ctx, M1),
    NowReal = eb_e2e_lib:now_sec(),
    RetainFU = retain_until_sec(Org1, FU),
    RetainM1 = retain_until_sec(Org1, M1),
    TotalBefore = message_count(Org1, Ws1),
    eb_e2e_lib:assert(
        <<"EB-11-A06.6">>,
        io_lib:format(
            "注入时钟构造到期边界：FU retain_until=~p（未到期，now=~p）、M1 retain_until=~p（已到期）；"
            "M1 附件=~p",
            [RetainFU, NowReal, RetainM1, maps:get(asset_id, M1Asset, undefined)]
        ),
        is_integer(RetainFU) andalso is_integer(RetainM1) andalso
            RetainFU > NowReal + 1000 andalso RetainM1 < NowReal andalso
            is_integer(maps:get(asset_id, M1Asset, undefined))
    ),
    P1 = purge(Org1, Ws1, NowReal - 86400),
    eb_e2e_lib:assert(
        <<"EB-11-A06.7">>,
        io_lib:format(
            "到期前 purge 精确为 0（注入 now=now-1d ⇒ deleted=~p；行数 ~p→~p；附件=~p）",
            [deleted(P1), TotalBefore, message_count(Org1, Ws1), asset_count(Org1, Ws1)]
        ),
        deleted(P1) =:= 0 andalso message_count(Org1, Ws1) =:= TotalBefore
    ),

    %% ---------------------------------------------------------------
    %% 4) active hold 阻断（合成 hold；真实 hold 属人工 Gate）
    %% ---------------------------------------------------------------
    Hold = enterprise_business_facade:create_hold(Org1, #{
        workspace_id => Ws1,
        scope => <<"conversation">>,
        scope_conversation_id => ConvId,
        reason_code => <<"eb11-synthetic-hold">>,
        synthetic => true,
        actor_user_id => Owner1
    }),
    HoldId =
        case Hold of
            {ok, View} -> maps:get(hold_id, View, undefined);
            _ -> undefined
        end,
    eb_e2e_lib:assert(
        <<"EB-11-A06.8">>,
        io_lib:format(
            "合成 hold 落库且 active（hold_id=~p；scope=conversation；synthetic 必须为 true）",
            [HoldId]
        ),
        is_integer(HoldId) andalso hold_active(Org1, HoldId)
    ),
    P2 = purge(Org1, Ws1, NowReal + 86400),
    eb_e2e_lib:assert(
        <<"EB-11-A06.9">>,
        io_lib:format(
            "active hold 阻止到期 purge（deleted=~p skipped=~p 条；行数不变=~p）",
            [
                deleted(P2),
                length(maps:get(skipped, P2, [])),
                message_count(Org1, Ws1) =:= TotalBefore
            ]
        ),
        deleted(P2) =:= 0 andalso message_count(Org1, Ws1) =:= TotalBefore andalso
            length(maps:get(skipped, P2, [])) >= 1
    ),
    Shrink = enterprise_business_facade:open_retention_policy(Org1, #{
        workspace_id => Ws1,
        data_class => <<"enterprise_message">>,
        retention_days => 30
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A06.10">>,
        io_lib:format("保留策略只允许延长：缩短到 30d 被拒（实测 ~p）", [Shrink]),
        element(1, Shrink) =:= error
    ),

    %% ---------------------------------------------------------------
    %% 5) 释放 hold 后到期**精确 purge** + 附件同生命周期
    %% ---------------------------------------------------------------
    Release = enterprise_business_facade:release_hold(Org1, #{
        workspace_id => Ws1,
        hold_id => HoldId,
        actor_user_id => Owner1,
        synthetic => true
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A06.11">>,
        io_lib:format(
            "hold 释放（一次性 released_at）：~p；释放后 hold 不再是 active（~p）",
            [short(Release), hold_active(Org1, HoldId)]
        ),
        element(1, Release) =:= ok andalso not hold_active(Org1, HoldId)
    ),
    MinId = min_message_id(Org1, Ws1),
    MinObjectKey = object_key_of_message(Org1, MinId),
    M1ObjectKey = maps:get(object_key, M1Asset, undefined),
    P3 = purge(Org1, Ws1, NowReal + 86400, 1),
    DeletedIds = maps:get(purged, P3, []),
    eb_e2e_lib:assert(
        <<"EB-11-A06.12">>,
        io_lib:format(
            "到期后精确 purge（batch_limit=1）：恰删最早期到目标 ~p（期望 [~p]）；剩余消息=~p；purge 审计=~p",
            [DeletedIds, MinId, message_count(Org1, Ws1), purge_audit_count(Org1)]
        ),
        deleted(P3) =:= 1 andalso DeletedIds =:= [MinId] andalso
            message_count(Org1, Ws1) =:= TotalBefore - 1 andalso purge_audit_count(Org1) =:= 1
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A06.13">>,
        io_lib:format(
            "附件元数据与所属消息同生命周期：被删消息 asset=~p；M1 尚未 purge，asset=~p",
            [assets_of_message(Org1, MinId), assets_of_message(Org1, M1)]
        ),
        assets_of_message(Org1, MinId) =:= 0 andalso assets_of_message(Org1, M1) =:= 1
    ),
    ObjectsAfterP3 = bucket_keys(),
    eb_e2e_lib:assert(
        <<"EB-11-A06.14">>,
        io_lib:format(
            "P3 对象生命周期：被删消息对象已不存在（~p）；M1 对象仍存在（~p）；桶键=~p",
            [
                object_state(Org1, Ws1, MinObjectKey),
                object_state(Org1, Ws1, M1ObjectKey),
                ObjectsAfterP3
            ]
        ),
        object_state(Org1, Ws1, MinObjectKey) =:= absent andalso
            object_state(Org1, Ws1, M1ObjectKey) =:= present andalso
            lists:member(M1ObjectKey, ObjectsAfterP3)
    ),
    P4 = purge(Org1, Ws1, NowReal + 86400),
    eb_e2e_lib:assert(
        <<"EB-11-A06.15">>,
        io_lib:format(
            "第二批 purge 清空**全部**到期行（deleted=~p）；未到期的 FU 是唯一幸存者（剩余=~p，仅 FU=~p）；"
            "其它 Workspace=~p；purge 审计=~p",
            [
                deleted(P4),
                message_count(Org1, Ws1),
                survive_only_fu(Org1, Ws1, FU),
                message_count(Org1, maps:get(ws1b, Scope)),
                purge_audit_count(Org1)
            ]
        ),
        deleted(P4) =:= TotalBefore - 2 andalso message_count(Org1, Ws1) =:= 1 andalso
            survive_only_fu(Org1, Ws1, FU) andalso
            message_count(Org1, maps:get(ws1b, Scope)) =:= 0 andalso purge_audit_count(Org1) =:= 2
    ),
    ObjectsAfterP4 = bucket_keys(),
    eb_e2e_lib:assert(
        <<"EB-11-A06.16">>,
        io_lib:format(
            "P4 后全部已到期附件元数据与对象字节均删除：asset_rows=~p M1_object=~p bucket_keys=~p",
            [asset_count(Org1, Ws1), object_state(Org1, Ws1, M1ObjectKey), ObjectsAfterP4]
        ),
        asset_count(Org1, Ws1) =:= 0 andalso
            object_state(Org1, Ws1, M1ObjectKey) =:= absent andalso ObjectsAfterP4 =:= []
    ),
    eb_e2e_lib:evidence(
        "a06-purge.txt",
        "FU=~p retain=~p(unexpired) | M1=~p retain=~p | M2=~p | min_id=~p | total_before=~p | "
        "P1(now-1d)=~p | P2(hold)=~p | P3(limit=1)=~p | P4=~p | purge_audit=~p | "
        "asset_rows_after=~p | bucket_keys_after_p3=~p | bucket_keys_after_p4=~p",
        [
            FU,
            RetainFU,
            M1,
            RetainM1,
            M2,
            MinId,
            TotalBefore,
            summary(P1),
            summary(P2),
            summary(P3),
            summary(P4),
            purge_audit_count(Org1),
            asset_count(Org1, Ws1),
            ObjectsAfterP3,
            ObjectsAfterP4
        ]
    ),
    Ctx#{fu_message_id => FU}.

%% ===================================================================
%% 辅助
%% ===================================================================

%% 以 B（active 的 sales 成员）的身份追加消息；`Accepted` 为 `default` 时用缺省时钟。
append_message_as_b(Ctx, ClientMsgId, Accepted, BodyKind) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Base = #{
        workspace_id => maps:get(ws1, Scope),
        conversation_id => maps:get(conversation_id, Ctx),
        client_msg_id => eb_e2e_lib:canary(ClientMsgId),
        sender_type => <<"business_identity">>,
        body => eb_e2e_lib:canary(BodyKind),
        identity_id => maps:get(identity_b, Ctx),
        actor_user_id => maps:get(b_user, Ctx),
        key_ref => maps:get(key_ref, Ctx)
    },
    Params =
        case Accepted of
            default -> Base;
            Ts -> Base#{accepted_at => Ts}
        end,
    case enterprise_business_facade:append_message(Org1, Params) of
        {ok, View} -> maps:get(message_id, View);
        _Other -> undefined
    end.

%% 给指定消息挂一个已确认附件（facade + 测试侧 key_ref；F6）。
attach_asset(Ctx, MessageId) ->
    Scope = maps:get(scope, Ctx),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    KeyRef = maps:get(key_ref, Ctx),
    Body = eb_e2e_lib:canary(<<"ASSET-EXPIRED">>),
    Hash = eb_asset_content:sha256_hex(Body),
    case
        enterprise_business_facade:request_presign(Org1, #{
            workspace_id => Ws1,
            conversation_id => maps:get(conversation_id, Ctx),
            mime => <<"text/plain">>,
            size_bytes => byte_size(Body),
            object_hash => Hash,
            message_id => MessageId,
            business_identity_id => maps:get(identity_b, Ctx),
            actor_user_id => maps:get(b_user, Ctx),
            key_ref => KeyRef
        })
    of
        {ok, View} ->
            AssetId = maps:get(asset_id, View),
            Ref = maps:get(upload_ref, View),
            Put = eb_asset_app:put_object(Org1, #{
                workspace_id => Ws1,
                upload_ref => Ref,
                payload => Body,
                actor_user_id => maps:get(b_user, Ctx),
                key_ref => KeyRef
            }),
            Confirm = enterprise_business_facade:confirm_asset(Org1, #{
                workspace_id => Ws1,
                upload_ref => Ref,
                actor_user_id => maps:get(b_user, Ctx),
                key_ref => KeyRef
            }),
            Row =
                case
                    eb_e2e_lib:rows(
                        <<"SELECT id, object_key FROM enterprise_asset WHERE organization_id=$1 AND id=$2">>,
                        [Org1, AssetId]
                    )
                of
                    [R | _] -> R;
                    [] -> #{}
                end,
            #{
                asset_id => AssetId,
                object_key => maps:get(<<"object_key">>, Row, undefined),
                put => short(Put),
                confirm => short(Confirm)
            };
        Other ->
            #{asset_id => undefined, error => Other}
    end.

purge(OrgId, Ws, Now) ->
    purge(OrgId, Ws, Now, 100).

purge(OrgId, Ws, Now, Limit) ->
    case
        eb_retention_app:purge_batch(OrgId, #{
            workspace_id => Ws, now => Now, batch_limit => Limit
        })
    of
        {ok, Summary} -> Summary;
        {error, Reason} -> #{error => Reason, deleted => -1, purged => [], skipped => []}
    end.

deleted(Summary) ->
    maps:get(deleted, Summary, -1).

summary(Summary) ->
    maps:with([deleted, purged, skipped], Summary).

short({Class, Reason, _Stack}) when is_atom(Class) -> io_lib:format("~p:~p", [Class, Reason]);
short({ok, View}) when is_map(View) ->
    maps:with([hold_id, status, case_id, asset_id, message_id], View);
short({error, Reason}) ->
    io_lib:format("error:~p", [Reason]);
short(Other) ->
    Other.

%% canonical 真源的**全字段指纹**（含密文、Org/Ws、sender、actor、retain_until）。
canonical_snapshot(OrgId, MsgId) ->
    Row = hd(
        eb_e2e_lib:rows(
            <<
                "SELECT id, organization_id, workspace_id, conversation_id, sender_type,"
                " sender_contact_id, sender_business_identity_id, actor_user_id, body_cipher,"
                " content_hash, policy_id, policy_version, retention_days,"
                " extract(epoch from retain_until)::bigint AS retain_epoch, visibility, version"
                " FROM enterprise_message WHERE organization_id=$1 AND id=$2"
            >>,
            [OrgId, MsgId]
        )
    ),
    eb_e2e_lib:stable_hash(Row).

canonical_count(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_message WHERE organization_id=$1 AND id=$2">>,
        [OrgId, MsgId],
        0
    ).

message_count(OrgId, Ws) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>,
        [OrgId, Ws],
        0
    ).

asset_count(OrgId, Ws) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>,
        [OrgId, Ws],
        0
    ).

assets_of_message(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_asset WHERE organization_id=$1 AND message_id=$2">>,
        [OrgId, MsgId],
        0
    ).

object_key_of_message(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT object_key AS v FROM enterprise_asset"
            " WHERE organization_id=$1 AND message_id=$2 ORDER BY id LIMIT 1"
        >>,
        [OrgId, MsgId],
        undefined
    ).

object_state(_OrgId, _Ws, undefined) ->
    absent;
object_state(OrgId, Ws, Key) ->
    case eb_asset_object_stub:get(Key, eb_asset_object_stub:key_prefix(OrgId, Ws)) of
        {ok, _} -> present;
        {error, not_found} -> absent;
        {error, Reason} -> {error, Reason}
    end.

min_message_id(OrgId, Ws) ->
    eb_e2e_lib:scalar(
        <<"SELECT min(id) AS v FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>,
        [OrgId, Ws],
        undefined
    ).

%% 幸存者必须恰是未到期的 FU（既证明 purge 精确，也证明「未到期不删」）。
survive_only_fu(OrgId, Ws, FU) ->
    Ids = [
        maps:get(<<"id">>, R)
     || R <- eb_e2e_lib:rows(
            <<"SELECT id FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>,
            [OrgId, Ws]
        )
    ],
    Ids =:= [FU].

delivery_count(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_message_delivery WHERE organization_id=$1 AND message_id=$2">>,
        [OrgId, MsgId],
        0
    ).

delivery_status(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT status FROM enterprise_message_delivery WHERE organization_id=$1 AND message_id=$2 LIMIT 1">>,
        [OrgId, MsgId],
        absent
    ).

accept_audit_count(OrgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1 AND action='message.accept'">>,
        [OrgId],
        0
    ).

purge_audit_count(OrgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1 AND action='message.purge'">>,
        [OrgId],
        0
    ).

hold_active(OrgId, HoldId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT (released_at IS NULL) AS active FROM enterprise_retention_hold"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, HoldId],
        false
    ) =:= true.

retain_until_sec(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT extract(epoch from retain_until)::bigint AS v FROM enterprise_message"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, MsgId],
        undefined
    ).

%% 本地替身桶里的全部键（只读）。用 element/2 显式取出，避免生成器里的元组模式
%% 在某些编译路径下不匹配（本次实撞后改为显式取元）。
bucket_keys() ->
    [
        element(2, K)
     || {K, _V} <- persistent_term:get(),
        is_tuple(K),
        tuple_size(K) =:= 2,
        is_tuple(element(1, K)),
        tuple_size(element(1, K)) =:= 2,
        element(1, element(1, K)) =:= eb_asset_object_stub,
        element(2, element(1, K)) =:= object,
        is_binary(element(2, K))
    ].
