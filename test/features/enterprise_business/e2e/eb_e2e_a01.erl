%%% @doc EB-11-A01：owner→A 建数据→suspend→B 继承**全链**（test-only）。
%%%
%%% 依据：plan §2.1 #1..#8、EB-11-A01、`agents/a5/EB-11/ORDER.md` §1/§4。
%%%
%%% ## 层次口径（必须逐条声明，不得混为一谈）
%%%
%%%   * **HTTP 层（真）**：真中间件（含 `auth_ds:verify_token` 校验真 JWT）→ 真
%%%     handler → 真 facade → 真 application → 真 PG。用于授权/隔离/治理/交接/取流。
%%%   * **facade 层（真，但由测试侧注入 `key_ref`）**：`create_contact` /`append_note` /
%%%     `append_message` / `request_presign` 等**加密侧写端点**需要企业托管主密钥引用，
%%%     而动作表白名单里没有 `key_ref`（客户端提供即被忽略），生产装配也没有密钥提供者
%%%     ⇒ HTTP 层**无法**携带（F6）。本模块在这些步骤把 `key_ref` 注入 facade 调用，
%%%     其余参数、路径、租户键与 HTTP 层同形；报告标注为「非真实密钥管理路径」。
%%%   * **测试侧事实补足（F1/F2）**：见 `eb_e2e_facts_probe`；本模块同时用**生产装配**
%%%     复现 F1/F2 的 403，使缺口是「可观测事实」而不是被掩盖的假设。
-module(eb_e2e_a01).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Tok = maps:get(tokens, Scope),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    A = maps:get(a_user, Scope),
    B = maps:get(b_user, Scope),
    Owner1 = maps:get(owner1, Scope),
    O1 = maps:get(owner1, Tok),
    ATok = maps:get(a, Tok),
    BTok = maps:get(b, Tok),
    io:format("~n== EB-11-A01 owner→A 建数据→suspend→B 继承全链 ==~n"),

    %% ---------------------------------------------------------------
    %% 0) 默认 Workspace 解析（真只读事实 Port；两 Org 各自解析）
    %% ---------------------------------------------------------------
    DefaultWs1 = default_workspace(Org1, Owner1),
    DefaultWs2 = default_workspace(maps:get(org2, Scope), maps:get(owner2, Scope)),
    eb_e2e_lib:assert_eq(
        <<"EB-11-A01.1">>,
        "Org1 默认 Workspace 由 eb_member_fact_pg:default_workspace/2 解析（成员 active + workspace active + id 最小）",
        Ws1,
        DefaultWs1
    ),
    eb_e2e_lib:assert_eq(
        <<"EB-11-A01.2">>,
        "Org2 默认 Workspace 同样可解析（两 Org 各自默认 Workspace）",
        maps:get(ws2, Scope),
        DefaultWs2
    ),
    eb_e2e_lib:evidence(
        "a01-default-workspace.txt",
        "org1=~p default_workspace=~p | org2=~p default_workspace=~p | ws1b(同 Org 第二个 workspace)=~p",
        [Org1, DefaultWs1, maps:get(org2, Scope), DefaultWs2, maps:get(ws1b, Scope)]
    ),

    %% ---------------------------------------------------------------
    %% 1) 业务身份：自举夹具 + HTTP 创建第二个身份 + 真实装配下的 403（F1/F2 证据）
    %% ---------------------------------------------------------------
    {Identity1, Assignment1} = eb_e2e_fixture:bootstrap_identity(Org1, A, Owner1, <<"sales">>),
    eb_e2e_lib:assert(
        <<"EB-11-A01.3">>,
        "自举夹具建立 Org1 第一个 sales 身份 + A 的 active 经办（自举死锁，登记为 EB-11-FND-1）",
        assignment_active(Org1, Identity1, A)
    ),
    %% F1 证据：**生产装配**（auth_facts = eb_pg_auth_facts）下，A 的写动作恒 403 permission_missing。
    eb_e2e_lib:set_facts_mode(real),
    RealWrite = eb_e2e_lib:post(
        ATok,
        eb_e2e_lib:tenant_path(Org1, <<"/business-identities">>, Ws1),
        #{<<"function_key">> => <<"customer_service">>, <<"display_name">> => <<"eb11-real-mode">>}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.4">>,
        io_lib:format(
            "生产装配（无测试侧权限补足）下 A 的写端点 403 permission_missing（F1 可复现）；实测 status=~p msg=~ts",
            [eb_e2e_lib:status(RealWrite), eb_e2e_lib:msg(RealWrite)]
        ),
        eb_e2e_lib:status(RealWrite) =:= 403
    ),
    eb_e2e_lib:evidence(
        "a01-F1-real-assembly-403.txt",
        "POST /business-identities (A, 生产装配) status=~p body=~ts",
        [eb_e2e_lib:status(RealWrite), eb_e2e_lib:body(RealWrite)]
    ),
    eb_e2e_lib:set_facts_mode(probe),

    %% 测试侧权限补足（F1）：A 持有 sales 经办；补 org.manage + conversation.write 等。
    ok = eb_e2e_facts_probe:grant([
        <<"org.manage">>,
        <<"contact.read">>,
        <<"conversation.write">>,
        <<"conversation.read">>,
        <<"message.write">>,
        <<"asset.read">>,
        <<"asset.write">>,
        <<"note.write">>
    ]),
    %% FND-1（RULING-2026-09-15 §五）：身份创建/绑定是**治理动作**（governance auth）。
    %% owner（无任何经办关系）直接创建 = 空 Org 自举解锁的正例；
    %% member（有 sales 经办但无治理角色）= 403 governance_insufficient 负例。
    Create = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(Org1, <<"/business-identities">>, Ws1),
        #{<<"function_key">> => <<"customer_service">>, <<"display_name">> => <<"eb11-service">>}
    ),
    Identity2 = eb_e2e_lib:tsid(eb_e2e_lib:pget(eb_e2e_lib:payload(Create), <<"id">>)),
    eb_e2e_lib:assert(
        <<"EB-11-A01.5">>,
        io_lib:format(
            "HTTP 创建业务身份（owner 治理路径，FND-1）：status=~p code=~p id=~p（TSID 以 JSON string 传输）",
            [eb_e2e_lib:status(Create), eb_e2e_lib:code(Create), Identity2]
        ),
        eb_e2e_lib:status(Create) =:= 200 andalso is_integer(Identity2) andalso
            is_binary(eb_e2e_lib:pget(eb_e2e_lib:payload(Create), <<"id">>))
    ),
    MemberCreate = eb_e2e_lib:post(
        ATok,
        eb_e2e_lib:tenant_path(Org1, <<"/business-identities">>, Ws1),
        #{
            <<"function_key">> => <<"customer_service">>,
            <<"display_name">> => <<"eb11-member-denied">>
        }
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.6">>,
        io_lib:format(
            "member 调身份管理端点被拒（经办身份不自动获得治理权 ⇒ governance_insufficient）；实测 status=~p msg=~ts",
            [eb_e2e_lib:status(MemberCreate), eb_e2e_lib:msg(MemberCreate)]
        ),
        eb_e2e_lib:status(MemberCreate) =:= 403 andalso
            binary:match(eb_e2e_lib:msg(MemberCreate), <<"governance_insufficient">>) =/= nomatch
    ),
    %% 绑定 B 到 Identity2（同 Org 成员），并验证「同 identity 第二个 active」被拒。
    Bind = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/business-identities/", (integer_to_binary(Identity2))/binary, "/assign">>,
            Ws1
        ),
        #{<<"user_id">> => integer_to_binary(B)}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.7">>,
        io_lib:format("HTTP 绑定 B 到业务身份：status=~p", [eb_e2e_lib:status(Bind)]),
        eb_e2e_lib:status(Bind) =:= 200 andalso assignment_active(Org1, Identity2, B)
    ),
    Duplicate = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/business-identities/", (integer_to_binary(Identity2))/binary, "/assign">>,
            Ws1
        ),
        #{<<"user_id">> => integer_to_binary(B)}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.8">>,
        io_lib:format(
            "同一 (Org,user,function) 第二条 active 经办被拒（409）；实测 status=~p msg=~ts",
            [eb_e2e_lib:status(Duplicate), eb_e2e_lib:msg(Duplicate)]
        ),
        eb_e2e_lib:status(Duplicate) =:= 409
    ),

    %% ---------------------------------------------------------------
    %% 2) A 建数据（facade 层 + 测试侧 key_ref 注入；F6）
    %% ---------------------------------------------------------------
    KeyRef = eb_e2e_lib:key_ref(),
    Subject = eb_e2e_lib:canary(<<"SUBJECT">>),
    Profile = eb_e2e_lib:canary(<<"PROFILE">>),
    Note = eb_e2e_lib:canary(<<"NOTE">>),
    MsgOut = eb_e2e_lib:canary(<<"MSGOUT">>),
    MsgIn = eb_e2e_lib:canary(<<"MSGIN">>),
    AssetBody = eb_e2e_lib:canary(<<"ASSET">>),
    %% 本 run 的消息一律用**已到期**的 accepted_at（now-1095d-1h）构造：这样
    %% retain_until = accepted_at + 1095d ≈ now-1h（已到期），A06 的 bounded purge 才能
    %% 物理清理本 run 的合成行（未到期的 canonical 行按 schema 禁止删除，会变成不可清残留）。
    A01Accepted = eb_e2e_lib:now_sec() - 1095 * 86400 - 3600,
    %% 1095 天保留策略（合成）：先建策略，再写消息（无策略 fail-closed）。
    Policy = enterprise_business_facade:open_retention_policy(Org1, #{
        workspace_id => Ws1,
        data_class => <<"enterprise_message">>,
        retention_days => 1095,
        actor_user_id => Owner1,
        key_ref => KeyRef
    }),
    AssetPolicy = enterprise_business_facade:open_retention_policy(Org1, #{
        workspace_id => Ws1,
        data_class => <<"enterprise_asset">>,
        retention_days => 1095,
        actor_user_id => Owner1
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A01.9">>,
        io_lib:format(
            "合成 1095d 保留策略落库（message=~p asset=~p）",
            [element(1, Policy), element(1, AssetPolicy)]
        ),
        element(1, Policy) =:= ok andalso element(1, AssetPolicy) =:= ok
    ),
    Contact = enterprise_business_facade:create_contact(Org1, #{
        workspace_id => Ws1,
        channel => <<"imboy">>,
        subject => Subject,
        key_ref => KeyRef,
        display_name => <<"eb11-contact">>,
        created_by_business_identity_id => Identity1,
        actor_user_id => A
    }),
    ContactId = contact_id(Org1),
    eb_e2e_lib:assert(
        <<"EB-11-A01.10">>,
        io_lib:format("A 创建企业客户（客户归 Org；channel subject 只落 HMAC）；contact_id=~p", [ContactId]),
        element(1, Contact) =:= ok andalso contact_owner_is_org(Org1, ContactId)
    ),
    ContactIdentityHmac = subject_hmac(Org1),
    eb_e2e_lib:assert(
        <<"EB-11-A01.11">>,
        "渠道标识只落 64 位 hex HMAC，明文 subject 不入库",
        is_binary(ContactIdentityHmac) andalso byte_size(ContactIdentityHmac) =:= 64 andalso
            subject_absent(Subject)
    ),
    ProfileUpdate = enterprise_business_facade:update_contact(Org1, #{
        workspace_id => Ws1,
        contact_id => ContactId,
        profile_plaintext => Profile,
        key_ref => KeyRef,
        actor_user_id => A
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A01.12">>,
        "客户资料经企业托管加密落库（profile_cipher 非空、key_version 非空）",
        element(1, ProfileUpdate) =:= ok andalso
            cipher_present(<<"enterprise_contact">>, <<"profile_cipher">>, Org1, ContactId)
    ),
    %% FND-3 已闭合：facade 接受业务明文，application 经服务端 provider 加密后落库。
    FacadeNote = enterprise_business_facade:append_note(Org1, #{
        workspace_id => Ws1,
        contact_id => ContactId,
        body_plaintext => Note,
        key_ref => KeyRef,
        actor_user_id => A
    }),
    eb_e2e_lib:assert(
        <<"EB-11-A01.13">>,
        io_lib:format(
            "FND-3 闭合：facade 明文备注路径成功（实测 ~p）",
            [FacadeNote]
        ),
        element(1, FacadeNote) =:= ok
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.14">>,
        "备注经企业托管加密落库（body_cipher 非空、明文不入库）",
        cipher_present(<<"enterprise_note">>, <<"body_cipher">>, Org1, undefined) andalso
            note_plaintext_absent(Org1, Note)
    ),

    %% ---------------------------------------------------------------
    %% 3) HTTP：A 建立会话（合成 consent + 默认 Workspace）
    %% ---------------------------------------------------------------
    OpenConv = eb_e2e_lib:post(
        ATok,
        eb_e2e_lib:tenant_path(Org1, <<"/conversations">>, Ws1),
        #{
            <<"contact_id">> => integer_to_binary(ContactId),
            <<"business_identity_id">> => integer_to_binary(Identity1)
        }
    ),
    ConvId = eb_e2e_lib:tsid(eb_e2e_lib:pget(eb_e2e_lib:payload(OpenConv), <<"conversation_id">>)),
    Consent = consent_row(Org1, ConvId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.15">>,
        io_lib:format(
            "HTTP 建立企业会话（workspace_id 必须等于服务端解析的默认 Workspace）：status=~p conv=~p",
            [eb_e2e_lib:status(OpenConv), ConvId]
        ),
        eb_e2e_lib:status(OpenConv) =:= 200 andalso is_integer(ConvId) andalso
            maps:get(<<"organization_id">>, Consent, undefined) =:= Org1 andalso
            maps:get(<<"workspace_id">>, Consent, undefined) =:= Ws1
    ),
    %% A01.16：合成 consent 与证据类别在同一条 conversation INSERT 中固化。
    ConsentKind = maps:get(<<"consent_evidence_kind">>, Consent, undefined),
    eb_e2e_lib:assert(
        <<"EB-11-A01.16">>,
        io_lib:format(
            "合成 consent 已固化：consent_at=~p notice_version=~p consent_evidence_kind=~p",
            [
                maps:get(<<"consent_at">>, Consent, undefined),
                maps:get(<<"notice_version">>, Consent, undefined),
                ConsentKind
            ]
        ),
        maps:get(<<"consent_at">>, Consent, undefined) =/= null andalso
            maps:get(<<"notice_version">>, Consent, undefined) =/= null andalso
            ConsentKind =:= <<"synthetic">>
    ),
    eb_e2e_lib:evidence(
        "a01-consent-evidence-kind.txt",
        "consent_at=~p notice_version=~p consent_evidence_kind=~p",
        [
            maps:get(<<"consent_at">>, Consent, undefined),
            maps:get(<<"notice_version">>, Consent, undefined),
            ConsentKind
        ]
    ),
    %% 跨 Workspace：同 Org 的另一个 Workspace 读会话历史必须为空（不得串租户范围）
    CrossWsRead = eb_e2e_lib:get(
        ATok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages">>,
            maps:get(ws1b, Scope)
        )
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.17">>,
        io_lib:format(
            "同 Org 跨 Workspace 读会话历史不返回任何消息（status=~p payload=~p）",
            [eb_e2e_lib:status(CrossWsRead), eb_e2e_lib:payload(CrossWsRead)]
        ),
        erlang:length(payload_list(CrossWsRead)) =:= 0
    ),

    %% ---------------------------------------------------------------
    %% 4) 显式 sender + 消息密文 + policy snapshot（facade 层；F6）
    %% ---------------------------------------------------------------
    Out = enterprise_business_facade:append_message(Org1, #{
        workspace_id => Ws1,
        conversation_id => ConvId,
        client_msg_id => eb_e2e_lib:canary(<<"CMID-OUT">>),
        sender_type => <<"business_identity">>,
        body => MsgOut,
        identity_id => Identity1,
        actor_user_id => A,
        accepted_at => A01Accepted,
        key_ref => KeyRef,
        notify => fun(Notification) ->
            persistent_term:put({eb_e2e_a01, notification}, Notification),
            ok
        end
    }),
    OutMsgId = result_msg_id(Out),
    eb_e2e_lib:assert(
        <<"EB-11-A01.20">>,
        io_lib:format(
            "显式 sender（business_identity）写消息：accepted=~p message_id=~p",
            [maps:get(accepted, element(2, Out), undefined), OutMsgId]
        ),
        element(1, Out) =:= ok andalso is_integer(OutMsgId)
    ),
    OutRow = message_row(Org1, OutMsgId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.18">>,
        "canonical 消息落库：密文非空、明文金丝雀不在库、sender XOR/FK 与 actor 正确",
        maps:get(<<"body_cipher">>, OutRow, undefined) =/= null andalso
            maps:get(<<"sender_business_identity_id">>, OutRow, undefined) =:= Identity1 andalso
            maps:get(<<"sender_contact_id">>, OutRow, undefined) =:= null andalso
            maps:get(<<"actor_user_id">>, OutRow, undefined) =:= A andalso
            message_plaintext_absent(MsgOut)
    ),
    In = enterprise_business_facade:append_message(Org1, #{
        workspace_id => Ws1,
        conversation_id => ConvId,
        client_msg_id => eb_e2e_lib:canary(<<"CMID-IN">>),
        sender_type => <<"contact">>,
        body => MsgIn,
        contact_id => ContactId,
        accepted_at => A01Accepted,
        key_ref => KeyRef
    }),
    InMsgId = result_msg_id(In),
    InRow = message_row(Org1, InMsgId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.19">>,
        "入站消息 sender=contact（无 actor）落库，且 ACK/hide 前 canonical 行可追溯",
        element(1, In) =:= ok andalso
            maps:get(<<"sender_contact_id">>, InRow, undefined) =:= ContactId andalso
            maps:get(<<"sender_business_identity_id">>, InRow, undefined) =:= null andalso
            maps:get(<<"actor_user_id">>, InRow, undefined) =:= null
    ),
    RetainDelta = retain_delta_sec(Org1, OutMsgId, A01Accepted),
    %% created_at 是 DB 的 CURRENT_TIMESTAMP（含小数秒），accepted_at 是整秒 ⇒ 允许 <1s 容差。
    eb_e2e_lib:assert(
        <<"EB-11-A01.21">>,
        io_lib:format(
            "合成 1095d 算法：retain_until = accepted_at + 1095*86400 秒（实测差值 ~p）",
            [RetainDelta]
        ),
        RetainDelta =:= 1095 * 86400
    ),
    Notification = persistent_term:get({eb_e2e_a01, notification}, undefined),
    eb_e2e_lib:assert(
        <<"EB-11-A01.22">>,
        io_lib:format(
            "提交后通知只含资源 id（键集=~p），不含消息明文",
            [notification_keys(Notification)]
        ),
        is_map(Notification) andalso
            lists:all(
                fun(K) -> lists:member(K, eb_message_app:notification_keys()) end,
                maps:keys(Notification)
            ) andalso
            length(eb_e2e_lib:contains_any(Notification, [MsgOut])) =:= 0
    ),

    %% ---------------------------------------------------------------
    %% 5) 附件闭环（facade 层；F6） + HTTP 代理下载
    %% ---------------------------------------------------------------
    Hash = eb_asset_content:sha256_hex(AssetBody),
    Presign = enterprise_business_facade:request_presign(Org1, #{
        workspace_id => Ws1,
        conversation_id => ConvId,
        mime => <<"text/plain">>,
        size_bytes => byte_size(AssetBody),
        object_hash => Hash,
        message_id => OutMsgId,
        business_identity_id => Identity1,
        actor_user_id => A,
        key_ref => KeyRef
    }),
    {ok, PresignView} = Presign,
    AssetId = maps:get(asset_id, PresignView),
    UploadRef = maps:get(upload_ref, PresignView),
    Put = eb_asset_app:put_object(Org1, #{
        workspace_id => Ws1,
        upload_ref => UploadRef,
        payload => AssetBody,
        actor_user_id => A,
        key_ref => KeyRef
    }),
    Confirm = enterprise_business_facade:confirm_asset(Org1, #{
        workspace_id => Ws1,
        upload_ref => UploadRef,
        actor_user_id => A,
        key_ref => KeyRef
    }),
    AssetRow = asset_row(Org1, AssetId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.23">>,
        io_lib:format(
            "企业附件 presign→PUT→confirm 全链：status=~p object_key 前缀=~ts（私有测试对象前缀 enterprise/<Org>/<Ws>/）",
            [
                maps:get(status, AssetRow, undefined),
                binary:part(
                    maps:get(<<"object_key">>, AssetRow, <<>>),
                    0,
                    min(24, byte_size(maps:get(<<"object_key">>, AssetRow, <<>>)))
                )
            ]
        ),
        maps:get(<<"status">>, AssetRow, undefined) =:= <<"active">> andalso
            element(1, Put) =:= ok andalso
            element(1, Confirm) =:= ok andalso
            eb_e2e_lib:binary_prefix(
                eb_asset_object_stub:key_prefix(Org1, Ws1),
                maps:get(<<"object_key">>, AssetRow, <<>>)
            )
    ),
    AssetRetain = asset_retain_sec(Org1, AssetId),
    MsgRetain = retain_until_sec(Org1, OutMsgId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.24">>,
        io_lib:format(
            "附件保留期不短于所属消息（asset=~p message=~p）",
            [AssetRetain, MsgRetain]
        ),
        is_integer(AssetRetain) andalso is_integer(MsgRetain) andalso AssetRetain >= MsgRetain
    ),
    Content = eb_e2e_lib:get(
        ATok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            Ws1
        )
    ),
    ContentHeaders = eb_e2e_lib:headers(Content),
    eb_e2e_lib:assert(
        <<"EB-11-A01.25">>,
        io_lib:format(
            "HTTP 代理下载返回原始字节（status=~p content-length=~p x-asset-sha256=~ts）且无存储侧引用",
            [
                eb_e2e_lib:status(Content),
                maps:get(<<"content-length">>, ContentHeaders, undefined),
                maps:get(<<"x-asset-sha256">>, ContentHeaders, undefined)
            ]
        ),
        eb_e2e_lib:status(Content) =:= 200 andalso
            eb_e2e_lib:body(Content) =:= AssetBody andalso
            maps:get(<<"x-asset-sha256">>, ContentHeaders, undefined) =:= Hash andalso
            length(eb_e2e_lib:leak_scan(ContentHeaders)) =:= 0
    ),

    %% ---------------------------------------------------------------
    %% 交接前的资源指纹（ID/owner/hash/count），交接后再取一次比对
    FingerprintBefore = resource_fingerprint(Org1, Ws1, Identity1),

    %% 6) 治理动作与离职交接（plan §2.1 #4/#5/#6）：全程走真实 HTTP 路径。
    SuspendHttp = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/members/", (integer_to_binary(A))/binary, "/suspend">>,
            Ws1
        ),
        #{<<"reason">> => <<"eb11-synthetic-suspend">>}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.26">>,
        io_lib:format(
            "FND-2 闭合：owner 经 HTTP suspend 成功并立即撤销企业授权（status=~p member=~p）",
            [eb_e2e_lib:status(SuspendHttp), member_status(Org1, A)]
        ),
        eb_e2e_lib:status(SuspendHttp) =:= 200 andalso
            member_status(Org1, A) =:= <<"suspended">>
    ),
    eb_e2e_lib:evidence(
        "a01-FND-2-governance-http-green.txt",
        "POST /members/:uid/suspend status=~p body=~ts | member.status=~p",
        [
            eb_e2e_lib:status(SuspendHttp),
            eb_e2e_lib:body(SuspendHttp),
            member_status(Org1, A)
        ]
    ),

    %% 6a) S1：已 suspend 的成员经 HTTP 打开并冻结 case，不重复撤权。
    OffboardHttp = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(Org1, <<"/offboarding">>, Ws1),
        #{
            <<"leaver_user_id">> => integer_to_binary(A),
            <<"successor_user_id">> => integer_to_binary(B),
            <<"reason">> => <<"eb11-synthetic-handover">>
        }
    ),
    OpenView = eb_e2e_lib:payload(OffboardHttp),
    CaseId = eb_e2e_lib:tsid(eb_e2e_lib:pget(OpenView, case_id)),
    SnapshotHash = eb_e2e_lib:pget(OpenView, snapshot_hash),
    CaseVersion = eb_e2e_lib:pget(OpenView, version),
    eb_e2e_lib:assert(
        <<"EB-11-A01.27">>,
        io_lib:format(
            "S1 打开并冻结交接 case（快照 active 经办 + **撤权** + CAS draft→frozen）："
            "case=~p status=~p items(success/total)=~p member=~p",
            [
                CaseId,
                eb_e2e_lib:pget(OpenView, status),
                io_lib:format("~p/~p", [
                    eb_e2e_lib:pget(OpenView, item_success), eb_e2e_lib:pget(OpenView, items_total)
                ]),
                member_status(Org1, A)
            ]
        ),
        eb_e2e_lib:status(OffboardHttp) =:= 200 andalso is_integer(CaseId) andalso
            eb_e2e_lib:pget(OpenView, items_total) =:= 1 andalso
            eb_e2e_lib:pget(OpenView, status) =:= <<"frozen">> andalso
            is_binary(SnapshotHash) andalso member_status(Org1, A) =:= <<"suspended">>
    ),
    eb_e2e_lib:evidence(
        "a01-S1-open-frozen.txt",
        "case_id=~p status=~p version=~p items_total=~p snapshot_hash=~ts member_suspended=~p "
        "（调用层次：facade → eb_offboarding_app → Core organization_member_logic:suspend/3）",
        [
            CaseId,
            eb_e2e_lib:pget(OpenView, status),
            CaseVersion,
            eb_e2e_lib:pget(OpenView, items_total),
            SnapshotHash,
            eb_e2e_lib:pget(OpenView, member_suspended)
        ]
    ),

    %% 6b) §2.1 #4：suspend 后旧 JWT 与新签发 token 同时失效；个人能力不受影响
    OldJwtRead = eb_e2e_lib:get(ATok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    eb_e2e_lib:assert(
        <<"EB-11-A01.28">>,
        io_lib:format(
            "suspend 后**旧 JWT** 访问企业业务 API 立即失败（逐请求重取事实，不等 token 过期）：status=~p msg=~ts",
            [eb_e2e_lib:status(OldJwtRead), eb_e2e_lib:msg(OldJwtRead)]
        ),
        eb_e2e_lib:status(OldJwtRead) =:= 403
    ),
    FreshATok = token_ds:encrypt_token(A),
    FreshRead = eb_e2e_lib:get(FreshATok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    eb_e2e_lib:assert(
        <<"EB-11-A01.29">>,
        "suspend 后**新签发** token 同样被拒（判据是逐请求事实，而非 token 年龄）",
        eb_e2e_lib:status(FreshRead) =:= 403
    ),
    Personal = personal_probe(A, ATok),
    eb_e2e_lib:assert(
        <<"EB-11-A01.30">>,
        io_lib:format(
            "suspend 不动个人 IM 能力：个人端点仍返回 200（path=~ts code=~p）",
            [maps:get(path, Personal), maps:get(code, Personal)]
        ),
        maps:get(status, Personal) =:= 200
    ),

    %% 6c) S2：交接 execute（CAS）——identity 的 active 经办 A→B，owner/资源不变
    Execute = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1, <<"/offboarding/", (integer_to_binary(CaseId))/binary, "/execute">>, Ws1
        ),
        #{<<"expected_version">> => CaseVersion}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.31">>,
        io_lib:format(
            "S2 交接 execute（CAS 推进 transferring）：实测 ~p；identity 的 active 经办人=~p（应为 [~p]）",
            [eb_e2e_lib:status(Execute), active_assignees(Org1, Identity1), B]
        ),
        eb_e2e_lib:status(Execute) =:= 200 andalso assignment_active(Org1, Identity1, B) andalso
            not assignment_active(Org1, Identity1, A)
    ),
    FingerprintAfter = resource_fingerprint(Org1, Ws1, Identity1),
    eb_e2e_lib:assert(
        <<"EB-11-A01.32">>,
        io_lib:format(
            "交接后资源 ID/owner/hash/count 不变（指纹 ~ts → ~ts）",
            [binary:part(FingerprintBefore, 0, 12), binary:part(FingerprintAfter, 0, 12)]
        ),
        FingerprintBefore =:= FingerprintAfter
    ),
    %% 6d) S3：verify（残留/ID/Org/hash/count 复核）→ finalize（未 verify 不得 removed）
    Verify = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1, <<"/offboarding/", (integer_to_binary(CaseId))/binary, "/verify">>, Ws1
        ),
        #{}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.33">>,
        io_lib:format("S3 verify（HTTP）：status=~p", [eb_e2e_lib:status(Verify)]),
        eb_e2e_lib:status(Verify) =:= 200
    ),
    Finalize = eb_e2e_lib:post(
        O1,
        eb_e2e_lib:tenant_path(
            Org1, <<"/offboarding/", (integer_to_binary(CaseId))/binary, "/finalize">>, Ws1
        ),
        #{}
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.34">>,
        io_lib:format(
            "S3 finalize（未 verify 不得 removed；DB guard 同点复核 active 经办）：实测 ~p；成员状态=~p",
            [eb_e2e_lib:status(Finalize), member_status(Org1, A)]
        ),
        eb_e2e_lib:status(Finalize) =:= 200 andalso member_status(Org1, A) =/= <<"active">> andalso
            member_status(Org1, A) =/= <<"suspended">>
    ),
    ItemStatuses = item_statuses(Org1, CaseId),
    eb_e2e_lib:assert(
        <<"EB-11-A01.35">>,
        io_lib:format(
            "FND-6 闭合：case/项/审计可查询且状态与真实计数收敛："
            "case=~p 项状态=~p execute 审计=~p item_success/total=~p",
            [
                case_status(Org1, CaseId),
                ItemStatuses,
                audit_count(Org1, <<"offboarding.execute">>),
                case_counts(Org1, CaseId)
            ]
        ),
        case_status(Org1, CaseId) =:= <<"completed">> andalso
            ItemStatuses =:= [<<"success">>] andalso
            audit_count(Org1, <<"offboarding.execute">>) =:= 1 andalso
            case_counts(Org1, CaseId) =:= {1, 1}
    ),
    eb_e2e_lib:evidence(
        "a01-FND-6-case-counters.txt",
        "case=~p item_statuses=~p case_item_total/item_success=~p | "
        "case 行计数与 item 事实逐字一致",
        [case_status(Org1, CaseId), ItemStatuses, case_counts(Org1, CaseId)]
    ),
    eb_e2e_lib:evidence(
        "a01-S2S3-transfer-verify-finalize.txt",
        "execute=~p | verify=~p | finalize=~p | case=~p items(success/total)=~p | "
        "member=~p | active_assignees(identity1)=~p | execute_audit=~p",
        [
            eb_e2e_lib:status(Execute),
            eb_e2e_lib:status(Verify),
            eb_e2e_lib:status(Finalize),
            case_status(Org1, CaseId),
            case_counts(Org1, CaseId),
            member_status(Org1, A),
            active_assignees(Org1, Identity1),
            audit_count(Org1, <<"offboarding.execute">>)
        ]
    ),

    %% 7) §2.1 #6：B 用**同一 business_identity_id**读到原客户/完整会话/消息/附件
    BContacts = eb_e2e_lib:get(BTok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    BDetail = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(Org1, <<"/contacts/", (integer_to_binary(ContactId))/binary>>, Ws1)
    ),
    BMessages = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages">>,
            Ws1
        )
    ),
    BContent = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            Ws1
        )
    ),
    BMessagesList = payload_list(BMessages),
    eb_e2e_lib:assert(
        <<"EB-11-A01.36">>,
        io_lib:format(
            "B（经 offboarding 承接 Identity1）读到原客户/会话历史/附件：contacts=~p detail_code=~p "
            "messages=~p asset_bytes=~p",
            [
                lists:member(integer_to_binary(ContactId), contact_ids(BContacts)),
                eb_e2e_lib:code(BDetail),
                length(BMessagesList),
                byte_size(eb_e2e_lib:body(BContent))
            ]
        ),
        lists:member(integer_to_binary(ContactId), contact_ids(BContacts)) andalso
            eb_e2e_lib:code(BDetail) =:= 0 andalso
            length(BMessagesList) =:= 2 andalso
            eb_e2e_lib:body(BContent) =:= AssetBody
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.37">>,
        io_lib:format(
            "B 读到的附件 hash 与上传时逐字相同（object_hash=~ts）；会话当前经办 identity=~p（= Identity1）",
            [
                maps:get(<<"x-asset-sha256">>, eb_e2e_lib:headers(BContent), undefined),
                conversation_assignee(Org1, ConvId)
            ]
        ),
        maps:get(<<"x-asset-sha256">>, eb_e2e_lib:headers(BContent), undefined) =:= Hash andalso
            conversation_assignee(Org1, ConvId) =:= Identity1
    ),
    %% §2.1 #7：A 已 removed ⇒ 个人好友/个人会话/通用 private 附件/导出接口都读不到企业数据
    AAfter = eb_e2e_lib:get(ATok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    AAfterContent = eb_e2e_lib:get(
        ATok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            Ws1
        )
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A01.38">>,
        io_lib:format(
            "A（已 removed）的旧 JWT 读客户/下载附件均被拒（contacts=~p content=~p）",
            [eb_e2e_lib:status(AAfter), eb_e2e_lib:status(AAfterContent)]
        ),
        eb_e2e_lib:status(AAfter) =:= 403 andalso eb_e2e_lib:status(AAfterContent) =:= 403
    ),

    Concurrency = concurrency_scenario(Scope),

    Ctx#{
        identity1 => Identity1,
        identity_b => Identity1,
        b_user => B,
        assignment1 => Assignment1,
        identity2 => Identity2,
        contact_id => ContactId,
        conversation_id => ConvId,
        out_message_id => OutMsgId,
        in_message_id => InMsgId,
        asset_id => AssetId,
        asset_body => AssetBody,
        asset_hash => Hash,
        accepted_at => A01Accepted,
        offboarding => completed,
        fingerprint => FingerprintBefore,
        key_ref => KeyRef,
        canaries => [Subject, Profile, Note, MsgOut, MsgIn, AssetBody],
        concurrency => Concurrency
    }.

%% ===================================================================
%% 并发交接（plan §2.1 #8：并发执行仅一方成功）
%% ===================================================================

%% 并发交接（plan §2.1 #8）。由于 FND-2，治理动作走 facade 层：CAS 由数据库行锁裁决，
%% 该负例的语义（恰一方推进、落败方零写入零审计）与 HTTP 层完全一致。
concurrency_scenario(Scope) ->
    Org2 = maps:get(org2, Scope),
    Ws2 = maps:get(ws2, Scope),
    Owner2 = maps:get(owner2, Scope),
    Tok = maps:get(tokens, Scope),
    O2 = maps:get(owner2, Tok),
    X = maps:get(x_user, Scope),
    Y = maps:get(y_user, Scope),
    {Identity, _Asg} = eb_e2e_fixture:bootstrap_identity(Org2, X, Owner2, <<"sales">>),

    %% (a) offboarding 并发 execute（§2.1 #8 的原定目标；BLK-1 已由 A0 补播解决，真实可跑）
    {ok, ConcOpen} = enterprise_business_facade:open_offboarding(Org2, #{
        workspace_id => Ws2,
        leaver_user_id => X,
        successor_user_id => Y,
        reason => <<"eb11-concurrency">>,
        actor_user_id => Owner2
    }),
    ConcCaseId = maps:get(case_id, ConcOpen),
    ConcVersion = maps:get(version, ConcOpen),
    ConcResults = parallel(4, fun() ->
        enterprise_business_facade:execute_offboarding(Org2, #{
            workspace_id => Ws2,
            case_id => ConcCaseId,
            expected_version => ConcVersion,
            actor_user_id => Owner2
        })
    end),
    ConcWinners = [R || {ok, _} = R <- ConcResults],
    eb_e2e_lib:assert(
        <<"EB-11-A01.39">>,
        io_lib:format(
            "offboarding 并发 execute（4 路同 expected_version）：**恰一方推进**（胜=~p/4；结论集=~p）",
            [length(ConcWinners), [result_status(R) || R <- ConcResults]]
        ),
        length(ConcResults) =:= 4 andalso length(ConcWinners) =:= 1 andalso
            case_status(Org2, ConcCaseId) =:= <<"transferring">> andalso
            audit_count(Org2, <<"offboarding.execute">>) =:= 1 andalso
            active_assignees(Org2, Identity) =:= [Y]
    ),
    Blocked = {concurrency_offboarding, ConcCaseId, [result_status(R) || R <- ConcResults]},

    %% (b) 可用 CAS 路径的并发单赢面（替代取证，口径如实标注）：
    %%     1) 合成 hold 的**一次性释放**（append-only：append-only 事实只允许 released_at 一次写入）；
    %%     2) 同一 (客户, 经办 identity) 的**并发建会话**（DB 唯一约束裁决）。
    Hold = enterprise_business_facade:create_hold(Org2, #{
        workspace_id => Ws2,
        scope => <<"workspace">>,
        reason_code => <<"eb11-concurrency-hold">>,
        synthetic => true,
        actor_user_id => Owner2
    }),
    HoldId =
        case Hold of
            {ok, HV} -> maps:get(hold_id, HV);
            _Other -> undefined
        end,
    ReleaseResults = parallel(4, fun() ->
        enterprise_business_facade:release_hold(Org2, #{
            workspace_id => Ws2, hold_id => HoldId, actor_user_id => Owner2, synthetic => true
        })
    end),
    ReleaseWinners = [R || {ok, _} = R <- ReleaseResults],
    eb_e2e_lib:assert(
        <<"EB-11-A01.40">>,
        io_lib:format(
            "并发 CAS（合成 hold 一次性释放，4 路）：恰一方成功（胜=~p / 4；创建 hold=~p；结论集=~p）",
            [length(ReleaseWinners), short(Hold), [short(R) || R <- ReleaseResults]]
        ),
        length(ReleaseResults) =:= 4 andalso length(ReleaseWinners) =:= 1
    ),

    %% 说明（如实）：同一 (客户, 经办 identity) 的会话**没有**唯一约束（会话 id 是新 TSID），
    %% 因此「并发建会话」不是单赢面，本 run 不以它取证单赢；改以**真实 CAS** 取证。
    EndResults = parallel(4, fun() ->
        enterprise_business_facade:end_assignment(Org2, #{
            workspace_id => Ws2,
            %% offboarding 并发（上一步）已把该 identity 的 active 经办从 X 换到 Y，
            %% 故此处结束的必须是**当前**经办人 Y（换错人会得到 assignee_mismatch）。
            identity_id => Identity,
            user_id => Y,
            end_reason => <<"eb11-concurrency-end">>,
            actor_user_id => Owner2
        })
    end),
    EndWinners = [R || {ok, _} = R <- EndResults],
    eb_e2e_lib:assert(
        <<"EB-11-A01.41">>,
        io_lib:format(
            "并发 CAS（经办关系 active→ended，4 路；当前经办人=Y）：恰一方成功（胜=~p / 4；结论集=~p）",
            [length(EndWinners), [short(R) || R <- EndResults]]
        ),
        length(EndResults) =:= 4 andalso length(EndWinners) =:= 1 andalso
            assignment_active_count(Org2, Identity) =:= 0
    ),
    eb_e2e_lib:evidence(
        "a01-concurrency.txt",
        "offboarding_concurrent_execute=~p | hold_release: winners=~p/4 conclusions=~p | "
        "end_assignment: winners=~p/4 conclusions=~p",
        [
            short(Blocked),
            length(ReleaseWinners),
            [short(R) || R <- ReleaseResults],
            length(EndWinners),
            [short(R) || R <- EndResults]
        ]
    ),
    #{
        identity => Identity,
        hold_release_winners => length(ReleaseWinners),
        end_assignment_winners => length(EndWinners)
    }.

%% 4 路并发：全部进程就绪后同时放行（同一时刻冲同一 CAS/唯一约束）。
parallel(N, Fun) ->
    Self = self(),
    Ref = make_ref(),
    Wait = fun() ->
        receive
            {go, Ref} -> ok
        end
    end,
    Pids = [
        spawn(fun() ->
            Wait(),
            Result = (catch Fun()),
            Self ! {Ref, Result}
        end)
     || _ <- lists:seq(1, N)
    ],
    lists:foreach(fun(P) -> P ! {go, Ref} end, Pids),
    [
        receive
            {Ref, Result} -> Result
        end
     || _ <- Pids
    ].

%% ===================================================================
%% 只读取证辅助
%% ===================================================================

default_workspace(OrgId, UserId) ->
    case eb_member_fact_pg:default_workspace(OrgId, UserId) of
        {ok, Ws} -> Ws;
        {error, _} -> undefined
    end.

assignment_active(OrgId, IdentityId, UserId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT count(*) AS n FROM organization_business_identity_assignment"
            " WHERE organization_id=$1 AND business_identity_id=$2 AND user_id=$3 AND status='active'"
        >>,
        [OrgId, IdentityId, UserId],
        0
    ) =:= 1.

assignment_active_count(OrgId, IdentityId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT count(*) AS n FROM organization_business_identity_assignment"
            " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
        >>,
        [OrgId, IdentityId],
        0
    ).

active_assignees(OrgId, IdentityId) ->
    [
        maps:get(<<"user_id">>, R)
     || R <- eb_e2e_lib:rows(
            <<
                "SELECT user_id FROM organization_business_identity_assignment"
                " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
                " ORDER BY user_id"
            >>,
            [OrgId, IdentityId]
        )
    ].

member_status(OrgId, UserId) ->
    eb_e2e_lib:scalar(
        <<"SELECT status FROM organization_member WHERE organization_id=$1 AND user_id=$2">>,
        [OrgId, UserId],
        absent
    ).

audit_count(OrgId, Action) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_audit_event WHERE organization_id=$1 AND action=$2">>,
        [OrgId, Action],
        0
    ).

case_status(OrgId, CaseId) ->
    eb_e2e_lib:scalar(
        <<"SELECT status FROM enterprise_offboarding_case WHERE organization_id=$1 AND id=$2">>,
        [OrgId, CaseId],
        absent
    ).

%% 项级事实（case 级 counter 不更新，见 FND-6）
item_statuses(OrgId, CaseId) ->
    [
        maps:get(<<"status">>, R)
     || R <- eb_e2e_lib:rows(
            <<
                "SELECT status FROM enterprise_offboarding_item WHERE organization_id=$1 AND case_id=$2"
                " ORDER BY id"
            >>,
            [OrgId, CaseId]
        )
    ].

case_counts(OrgId, CaseId) ->
    case
        eb_e2e_lib:rows(
            <<
                "SELECT item_success, item_total FROM enterprise_offboarding_case"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [OrgId, CaseId]
        )
    of
        [Row | _] -> {maps:get(<<"item_success">>, Row), maps:get(<<"item_total">>, Row)};
        [] -> absent
    end.

contact_id(OrgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT id FROM enterprise_contact WHERE organization_id=$1 ORDER BY id LIMIT 1">>,
        [OrgId],
        undefined
    ).

contact_owner_is_org(OrgId, ContactId) ->
    eb_e2e_lib:scalar(
        <<"SELECT organization_id FROM enterprise_contact WHERE id=$1">>,
        [ContactId],
        undefined
    ) =:= OrgId.

subject_hmac(OrgId) ->
    eb_e2e_lib:scalar(
        <<"SELECT subject_hmac FROM enterprise_contact_identity WHERE organization_id=$1 LIMIT 1">>,
        [OrgId],
        undefined
    ).

subject_absent(Subject) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_contact_identity WHERE subject_hmac = $1">>,
        [Subject],
        0
    ) =:= 0.

cipher_present(Table, Column, OrgId, Id) ->
    Sql = iolist_to_binary([
        "SELECT count(*) AS n FROM ",
        Table,
        " WHERE organization_id=$1 AND ",
        Column,
        " IS NOT NULL",
        case Id of
            undefined -> <<>>;
            _ -> <<" AND id=$2">>
        end
    ]),
    Params =
        case Id of
            undefined -> [OrgId];
            _ -> [OrgId, Id]
        end,
    eb_e2e_lib:scalar(Sql, Params, 0) >= 1.

consent_row(OrgId, ConvId) ->
    case
        eb_e2e_lib:rows(
            <<"SELECT * FROM enterprise_conversation WHERE organization_id=$1 AND id=$2">>,
            [OrgId, ConvId]
        )
    of
        [Row | _] -> Row;
        [] -> #{}
    end.

message_row(OrgId, MsgId) ->
    case
        eb_e2e_lib:rows(
            <<"SELECT * FROM enterprise_message WHERE organization_id=$1 AND id=$2">>,
            [OrgId, MsgId]
        )
    of
        [Row | _] -> Row;
        [] -> #{}
    end.

asset_row(_OrgId, undefined) ->
    #{};
asset_row(OrgId, AssetId) ->
    case
        eb_e2e_lib:rows(
            <<"SELECT * FROM enterprise_asset WHERE organization_id=$1 AND id=$2">>,
            [OrgId, AssetId]
        )
    of
        [Row | _] -> Row;
        [] -> #{}
    end.

%% 仅用于证据打印：把崩溃/错误项压成一行（避免把整棵 stack 打进断言行）。
short({Class, Reason, _Stack}) -> io_lib:format("~p:~p", [Class, short(Reason)]);
short({error, Reason}) -> io_lib:format("error:~p", [short(Reason)]);
short(Other) when is_map(Other) -> maps:with([status, case_id, snapshot_hash], Other);
short(Other) -> Other.

conversation_assignee(OrgId, ConvId) ->
    eb_e2e_lib:scalar(
        <<"SELECT business_identity_id FROM enterprise_conversation WHERE organization_id=$1 AND id=$2">>,
        [OrgId, ConvId],
        undefined
    ).

note_plaintext_absent(OrgId, Plaintext) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT count(*) AS n FROM enterprise_note"
            " WHERE organization_id=$1 AND body_cipher=$2"
        >>,
        [OrgId, Plaintext],
        0
    ) =:= 0.

%% facade 结果的通用读取（ok/status/error 三种形状）。
result_status({ok, View}) when is_map(View) -> maps:get(status, View, ok);
result_status({ok, _Other}) -> ok;
result_status(Other) -> Other.

result_ok({ok, _}) -> true;
result_ok(_Other) -> false.

%% case_id 缺失时不做无意义调用（避免把 harness 崩溃伪装成业务失败）。
safe_case_call(_Fun, undefined) -> {error, case_not_opened};
safe_case_call(Fun, _Arg) -> Fun(undefined).

%% 保留期一律在 SQL 侧换算成 Unix 秒（不依赖 Erlang 侧的 timestamptz 表示）。
retain_until_sec(_OrgId, undefined) ->
    undefined;
retain_until_sec(OrgId, MsgId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT extract(epoch from retain_until)::bigint AS v FROM enterprise_message"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, MsgId],
        undefined
    ).

asset_retain_sec(_OrgId, undefined) ->
    undefined;
asset_retain_sec(OrgId, AssetId) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT extract(epoch from retain_until)::bigint AS v FROM enterprise_asset"
            " WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, AssetId],
        undefined
    ).

%% 算法口径：retain_until - 注入的 accepted_at（整秒）必须逐字等于 retention_days*86400。
retain_delta_sec(_OrgId, undefined, _AcceptedAt) ->
    undefined;
retain_delta_sec(OrgId, MsgId, AcceptedAt) ->
    eb_e2e_lib:scalar(
        <<
            "SELECT (extract(epoch from retain_until)::bigint - $3::bigint) AS v"
            " FROM enterprise_message WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, MsgId, AcceptedAt],
        undefined
    ).

message_plaintext_absent(MsgOut) ->
    eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_message WHERE body_cipher = $1">>,
        [MsgOut],
        0
    ) =:= 0.

out_msg_id(#{message_id := Id}) -> Id;
out_msg_id(#{id := Id}) -> Id;
out_msg_id(_Other) -> undefined.

payload_list(Resp) ->
    case eb_e2e_lib:payload(Resp) of
        List when is_list(List) -> List;
        _Other -> []
    end.

contact_ids(Resp) ->
    [
        eb_e2e_lib:pget(Item, <<"id">>)
     || Item <- payload_list(Resp)
    ].

notification_keys(Notification) when is_map(Notification) -> maps:keys(Notification);
notification_keys(_Other) -> [].

result_msg_id({ok, View}) -> out_msg_id(View);
result_msg_id(_Other) -> undefined.

%% 资源指纹口径（与 EB-08 的 A02 同规）：identity 行本身 + 挂在它上面的会话数/客户数
%% + 消息 ID/密文 hash 集合 + 附件 ID/object_hash 集合。assignee 历史**不在**指纹内。
resource_fingerprint(OrgId, Ws, IdentityId) ->
    Identity = eb_e2e_lib:rows(
        <<
            "SELECT id, organization_id, function_key, status, version"
            " FROM organization_business_identity WHERE organization_id=$1 AND id=$2"
        >>,
        [OrgId, IdentityId]
    ),
    %% 会话的**当前经办 identity**不在指纹内：交接本来就会改它（与 EB-08-A02 排除 assignee
    %% 的口径一致）；而「资源 ID/Org/Workspace/客户/状态」必须逐字不变。
    Conversations = eb_e2e_lib:rows(
        <<
            "SELECT id, organization_id, workspace_id, contact_id, status"
            " FROM enterprise_conversation WHERE organization_id=$1 AND workspace_id=$2"
            " ORDER BY id"
        >>,
        [OrgId, Ws]
    ),
    Messages = eb_e2e_lib:rows(
        <<
            "SELECT id, organization_id, workspace_id, sender_type, sender_contact_id,"
            " sender_business_identity_id, actor_user_id, content_hash, retain_until"
            " FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2 ORDER BY id"
        >>,
        [OrgId, Ws]
    ),
    Assets = eb_e2e_lib:rows(
        <<
            "SELECT id, organization_id, workspace_id, message_id, object_hash, status"
            " FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2 ORDER BY id"
        >>,
        [OrgId, Ws]
    ),
    Contacts = eb_e2e_lib:rows(
        <<
            "SELECT id, organization_id, created_by_business_identity_id, status"
            " FROM enterprise_contact WHERE organization_id=$1 ORDER BY id"
        >>,
        [OrgId]
    ),
    eb_e2e_lib:stable_hash({Identity, Contacts, Conversations, Messages, Assets}).

%% 个人端点探针：suspend 不得删除个人 IM 能力（返回 200 即证明未被企业撤权连带）
personal_probe(_UserId, Token) ->
    Candidates = [
        <<"/api/v1/conversation/mine">>,
        <<"/api/v1/user_collect/page">>,
        <<"/api/v1/user/deletion_status">>
    ],
    Results = [
        begin
            Resp = eb_e2e_lib:post(Token, Path, #{}),
            #{path => Path, status => eb_e2e_lib:status(Resp), code => eb_e2e_lib:code(Resp)}
        end
     || Path <- Candidates
    ],
    case [R || #{status := 200} = R <- Results] of
        [First | _] -> First;
        [] -> hd(Results)
    end.
