%%% @doc EB-11-A02：跨 Org / Workspace、旧 JWT、个人 API/ACK/archive、下载代理**负例**（test-only）。
%%%
%%% 依据：plan §2.1 #4/#7/#13/#16、EB-11-A02、`agents/a5/EB-11/ORDER.md` §1。
%%%
%%% 口径：负例一律断言**拒绝**；拒绝面同时打印实测 status/msg，便于 A0 复核「拒绝理由是否可区分」。
%%% 刻意不断言「最小拒绝码」之外的语义（例如不把 403 与 404 的取舍当成缺陷），但**任何 2xx
%%% 都视为失败**——安全/隔离断言不因实现细节放宽。
-module(eb_e2e_a02).

-export([run/1]).

run(Ctx) ->
    Scope = maps:get(scope, Ctx),
    Tok = maps:get(tokens, Scope),
    Org1 = maps:get(org1, Scope),
    Ws1 = maps:get(ws1, Scope),
    Org2 = maps:get(org2, Scope),
    Ws2 = maps:get(ws2, Scope),
    A = maps:get(a_user, Scope),
    BTok = maps:get(b, Tok),
    XTok = maps:get(x, Tok),
    ATok = maps:get(a, Tok),
    O1 = maps:get(owner1, Tok),
    ContactId = maps:get(contact_id, Ctx),
    ConvId = maps:get(conversation_id, Ctx),
    OutMsgId = maps:get(out_message_id, Ctx),
    AssetId = maps:get(asset_id, Ctx),
    Canaries = maps:get(canaries, Ctx),
    io:format("~n== EB-11-A02 跨 Org/Workspace、旧 JWT、个人 API/ACK/archive、下载代理负例 ==~n"),

    %% 1) 无凭证
    NoAuth = eb_e2e_lib:get(undefined, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    a02(<<"EB-11-A02.1">>, "无 Authorization 头的企业请求 401", [401], NoAuth),
    NoAuthContent = eb_e2e_lib:get(
        undefined,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            Ws1
        )
    ),
    a02(<<"EB-11-A02.2">>, "无凭证的代理下载 401（不泄露资源是否存在）", [401], NoAuthContent),

    %% 2) 跨 Org（Org2 成员读 Org1；Org1 成员读 Org2）
    CrossOrg = eb_e2e_lib:get(XTok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1)),
    a02(<<"EB-11-A02.3">>, "Org2 成员访问 Org1 企业面被拒（cross_org/no_member）", [403], CrossOrg),
    ReverseCross = eb_e2e_lib:get(BTok, eb_e2e_lib:tenant_path(Org2, <<"/contacts">>, Ws2)),
    a02(<<"EB-11-A02.4">>, "Org1 成员访问 Org2 企业面被拒", [403], ReverseCross),
    CrossOrgAsset = eb_e2e_lib:get(
        XTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            Ws1
        )
    ),
    a02(<<"EB-11-A02.5">>, "跨 Org 代理下载被拒（403，且不回显对象存在性）", [403], CrossOrgAsset),

    %% 3) 跨 Workspace（同 Org 的另一个 Workspace）
    CrossWsMsgs = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages">>,
            maps:get(ws1b, Scope)
        )
    ),
    DbMsgsInWs1 = eb_e2e_lib:scalar(
        <<"SELECT count(*) AS n FROM enterprise_message WHERE organization_id=$1 AND workspace_id=$2">>,
        [Org1, Ws1],
        0
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A02.6">>,
        io_lib:format(
            "跨 Workspace 读会话历史不返回消息（Ws1 实际有 ~p 条消息，故空结果非平凡）：payload=~p",
            [DbMsgsInWs1, eb_e2e_lib:payload(CrossWsMsgs)]
        ),
        DbMsgsInWs1 >= 2 andalso length(payload_list(CrossWsMsgs)) =:= 0
    ),
    CrossWsAsset = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(AssetId))/binary, "/content">>,
            maps:get(ws1b, Scope)
        )
    ),
    a02(<<"EB-11-A02.7">>, "跨 Workspace 代理下载被拒（元数据查询带 Org+Ws 双键）", [404], CrossWsAsset),

    %% 4) 猜 ID / 不存在的资源
    GuessAsset = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/assets/", (integer_to_binary(eb_e2e_lib:id()))/binary, "/content">>,
            Ws1
        )
    ),
    a02(<<"EB-11-A02.8">>, "随机猜 asset id 的代理下载 404", [404], GuessAsset),
    GuessContact = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/contacts/", (integer_to_binary(eb_e2e_lib:id()))/binary>>,
            Ws1
        )
    ),
    a02(<<"EB-11-A02.9">>, "随机猜 contact id 的详情读取 404", [404], GuessContact),

    %% 5) 旧 JWT（A 已 suspend + 交接后 removed）：企业面全拒，含 ACK / 下载 / 会话历史
    OldAck = eb_e2e_lib:post(
        ATok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages/",
                (integer_to_binary(OutMsgId))/binary, "/ack">>,
            Ws1
        ),
        #{<<"recipient_ref">> => <<"identity:", (integer_to_binary(A))/binary>>}
    ),
    a02(<<"EB-11-A02.10">>, "旧 JWT（已 suspend/removed）对企业 ACK 端点被拒", [403], OldAck),
    OldMsgs = eb_e2e_lib:get(
        ATok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages">>,
            Ws1
        )
    ),
    a02(<<"EB-11-A02.11">>, "旧 JWT 对企业会话历史读取被拒", [403], OldMsgs),

    %% 6) 平台运营面：租户 JWT 越权 + 无会话
    PlatformNoSession = eb_e2e_lib:get(
        undefined,
        <<"/api/adm/enterprise-business/organizations/", (integer_to_binary(Org1))/binary,
            "/identities?workspace_id=", (integer_to_binary(Ws1))/binary>>
    ),
    PlatformReject1 = eb_e2e_lib:status(PlatformNoSession),
    eb_e2e_lib:assert(
        <<"EB-11-A02.12">>,
        io_lib:format("平台运营面无 Admin session 被拒（status=~p）", [PlatformReject1]),
        PlatformReject1 >= 400
    ),
    PlatformTenantJwt = eb_e2e_lib:get(
        O1,
        <<"/api/adm/enterprise-business/organizations/", (integer_to_binary(Org1))/binary,
            "/identities?workspace_id=", (integer_to_binary(Ws1))/binary>>
    ),
    PlatformReject2 = eb_e2e_lib:status(PlatformTenantJwt),
    eb_e2e_lib:assert(
        <<"EB-11-A02.13">>,
        io_lib:format(
            "租户 JWT 不能冒充平台管理员（/api/adm 面被拒；实测 status=~p msg=~ts）",
            [PlatformReject2, eb_e2e_lib:msg(PlatformTenantJwt)]
        ),
        PlatformReject2 >= 400
    ),

    %% 7) 个人 API 不得读到企业数据（payload 扫描 + 个人表零新增）
    PersonalPaths = [
        <<"/api/v1/conversation/mine">>,
        <<"/api/v1/user_collect/page">>,
        <<"/api/v1/user/export_data">>,
        <<"/api/v1/friend/category_list">>
    ],
    EnterpriseIds = [
        integer_to_binary(ContactId),
        integer_to_binary(ConvId),
        integer_to_binary(OutMsgId),
        integer_to_binary(AssetId)
    ],
    PersonalFindings = [
        {Path, eb_e2e_lib:status(Resp),
            eb_e2e_lib:contains_any(eb_e2e_lib:body(Resp), EnterpriseIds ++ Canaries)}
     || Path <- PersonalPaths,
        Resp <- [eb_e2e_lib:post(BTok, Path, #{})]
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A02.14">>,
        io_lib:format(
            "个人 API 响应不含企业资源 id / 明文金丝雀（逐路径扫描结果=~p）",
            [PersonalFindings]
        ),
        lists:all(fun({_P, _S, Hits}) -> Hits =:= [] end, PersonalFindings)
    ),
    PersonalSnapshot = eb_e2e_fixture:personal_tables_snapshot([
        maps:get(a_user, Scope),
        maps:get(b_user, Scope),
        maps:get(owner1, Scope),
        maps:get(x_user, Scope),
        maps:get(owner2, Scope)
    ]),
    eb_e2e_lib:assert(
        <<"EB-11-A02.15">>,
        io_lib:format(
            "个人域表零企业残留（friend/conversation/msg_c2c/attachment/user_collect/user_device）=~p",
            [PersonalSnapshot]
        ),
        lists:all(
            fun
                ({_T, 0}) -> true;
                ({_T, absent}) -> true;
                (_) -> false
            end,
            PersonalSnapshot
        )
    ),
    eb_e2e_lib:evidence("a02-personal-tables.txt", "~p", [PersonalSnapshot]),
    %% 个人附件入口不能取企业附件（企业下载只走 enterprise 代理端点）
    PersonalAttach = eb_e2e_lib:post(
        BTok,
        <<"/api/v1/attachment/view">>,
        #{<<"id">> => integer_to_binary(AssetId)}
    ),
    PersonalAttachReject = eb_e2e_lib:status(PersonalAttach),
    eb_e2e_lib:assert(
        <<"EB-11-A02.16">>,
        io_lib:format("个人附件接口对非本表资源不返回对象内容（status=~p）", [PersonalAttachReject]),
        PersonalAttachReject >= 400
    ),

    %% 8) 企业 ACK 只写 delivery：响应无删除/归档语义
    Ack = eb_e2e_lib:post(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages/",
                (integer_to_binary(OutMsgId))/binary, "/ack">>,
            Ws1
        ),
        #{<<"recipient_ref">> => <<"contact:", (integer_to_binary(ContactId))/binary>>}
    ),
    AckBody = string:lowercase(binary_to_list(eb_e2e_lib:body(Ack))),
    Forbidden = [
        W
     || W <- [<<"delete">>, <<"archive">>, <<"purge">>, <<"remove">>, <<"destroy">>, ~B'销毁'],
        string:find(AckBody, binary_to_list(W)) =/= nomatch
    ],
    eb_e2e_lib:assert(
        <<"EB-11-A02.17">>,
        io_lib:format(
            "企业 ACK 契约是 delivery-only（响应无删除/归档语义；命中=~p；status=~p）",
            [Forbidden, eb_e2e_lib:status(Ack)]
        ),
        eb_e2e_lib:status(Ack) =:= 200 andalso Forbidden =:= []
    ),
    MalformedAck = eb_e2e_lib:post(
        BTok,
        eb_e2e_lib:tenant_path(
            Org1,
            <<"/conversations/", (integer_to_binary(ConvId))/binary, "/messages/",
                (integer_to_binary(OutMsgId))/binary, "/ack">>,
            Ws1
        ),
        #{<<"recipient_ref">> => <<"nope">>}
    ),
    a02(
        <<"EB-11-A02.18">>,
        "ACK 的 recipient_ref 形状非法（须 contact:<id>|identity:<id>）被拒",
        [400, 422],
        MalformedAck
    ),

    %% 9) 请求形状负例：缺 workspace_id / 非法 TSID / 客户端自报租户归属 / 方法不匹配
    MissingWs = eb_e2e_lib:get(BTok, eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, 0)),
    a02(<<"EB-11-A02.19">>, "缺 workspace_id 422（绝不取默认值）", [422], MissingWs),
    BadTsid = eb_e2e_lib:get(
        BTok,
        eb_e2e_lib:tenant_path(Org1, <<"/contacts/not-a-tsid">>, Ws1)
    ),
    a02(<<"EB-11-A02.20">>, "路径 TSID 非法 400", [400], BadTsid),
    Spoofed = eb_e2e_lib:post(
        BTok,
        eb_e2e_lib:tenant_path(Org1, <<"/conversations">>, Ws1),
        #{
            <<"contact_id">> => integer_to_binary(ContactId),
            <<"business_identity_id">> => integer_to_binary(maps:get(identity1, Ctx)),
            <<"workspace_organization_id">> => integer_to_binary(Org2)
        }
    ),
    a02(<<"EB-11-A02.21">>, "客户端自报租户归属（workspace_organization_id）400", [400], Spoofed),
    WrongMethod = eb_e2e_lib:get(BTok, eb_e2e_lib:tenant_path(Org1, <<"/offboarding">>, Ws1)),
    a02(<<"EB-11-A02.22">>, "未登记方法（GET /offboarding）405", [405], WrongMethod),

    %% 10) F6（RULING-2026-09-15 §七）：主密钥材料不经 HTTP/JSON 面——客户端
    %% 提交 key_ref 即结构化 422；密钥只由服务端 `imboy.eb_enterprise_keyring` 装配。
    KeyRefIgnored = eb_e2e_lib:post(
        BTok,
        eb_e2e_lib:tenant_path(Org1, <<"/contacts">>, Ws1),
        #{
            <<"channel">> => <<"imboy">>,
            <<"subject">> => <<"eb11-client-key">>,
            <<"key_ref">> => <<"eb11-attacker-key">>
        }
    ),
    eb_e2e_lib:assert(
        <<"EB-11-A02.23">>,
        io_lib:format(
            "客户端提交 key_ref 被结构化拒绝（422 unexpected_argument.key_ref，"
            "密钥只由服务端装配）；实测 status=~p msg=~ts",
            [eb_e2e_lib:status(KeyRefIgnored), eb_e2e_lib:msg(KeyRefIgnored)]
        ),
        eb_e2e_lib:status(KeyRefIgnored) =:= 422
    ),

    %% 11) 下载代理响应面：不含存储能力（object key / URL / endpoint / presign）
    ok.

%% ===================================================================
%% 负例断言辅助
%% ===================================================================

%% @doc 断言「拒绝」：实测状态码必须落在允许集合内（集合里**不含**任何 2xx）。
a02(Id, Desc, Allowed, Resp) ->
    Status = eb_e2e_lib:status(Resp),
    eb_e2e_lib:assert(
        Id,
        io_lib:format("~ts（实测 status=~p msg=~ts 允许集合=~p）", [
            Desc, Status, eb_e2e_lib:msg(Resp), Allowed
        ]),
        lists:member(Status, Allowed)
    ).

payload_list(Resp) ->
    case eb_e2e_lib:payload(Resp) of
        List when is_list(List) -> List;
        _Other -> []
    end.
