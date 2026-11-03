%%% @doc CS-BE-01B：坐席发送面协议补全验收套件（真 HTTP + 真 facade + 真 scratch PG）。
%%%
%%% 冻结契约（Web Seat 复用 widget 同款协议，经企业租户面一条路径发送）：
%%%
%%%   * `append_message`（POST conversations/:id/messages）：`asset_ids` =
%%%     TSID string 数组（list 传输形态）；空正文 + 有效 asset_ids 合法；
%%%     body 与 asset_ids **皆空 = 422 invalid_body**（确定错误码）；
%%%     同一 client_msg_id + 同一 asset_ids 重放幂等（replayed=true，同 message）；
%%%     响应含 CS-BE-01 的 assets 回显投影（五键白名单）。
%%%   * `presign`（POST /assets/presign）：成功视图投影 `upload.url`（指向本 API
%%%     的 presign **PUT** 端点；查询串携带 workspace_id/upload_ref，百分号编码；
%%%     短 TTL = 凭证 expires_at）+ `upload_ref`（不透明 token）。
%%%   * `presign`（PUT，同路径第二用例）：请求体 = 原始字节；凭证复核
%%%     （过期/篡改/同上传人/经办 ACL/hash/size/mime）全部在既有
%%%     `eb_asset_app:put_object`，本套件零复制断言。
%%%   * `confirm`（POST /assets/confirm {upload_ref}）：绑定资产（active）。
%%%   * 跨租户：凭证 AAD 绑定 (Org, Workspace)——跨 Org / 跨 Workspace 的 PUT
%%%     一律在 scope 门失败（404/403，无枚举面）——V3-C04 direct 化。
%%%
%%% 装配口径：`probe`（真事实 + 补 asset.write/conversation.write 权限，缺口
%%% 说明见 eb09_facts_probe）；密钥走 env keyring（F6 生产装配路径）。
-module(eb_seat_send_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).
-define(FIX, eb_pg_test_fixture).
-define(TIMEOUT_S, 60).

seat_send_protocol_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    %% V3-C04：正文走 direct s3_put 直传——本地 closeout Garage 容器凭据经
    %% 环境变量注入（不写死进仓）；receipt HMAC secret 测试内随机生成。
    case os:getenv("C04_GARAGE_ENDPOINT") of
        false ->
            ok;
        Ep ->
            ok = application:set_env(imboy, garage, #{
                endpoint => list_to_binary(Ep),
                bucket => list_to_binary(os:getenv("C04_GARAGE_BUCKET")),
                access_key => list_to_binary(os:getenv("C04_GARAGE_AK")),
                secret_key => list_to_binary(os:getenv("C04_GARAGE_SK")),
                region => <<"garage">>
            }),
            ok = application:set_env(
                imboy,
                attachment_receipt_secret,
                binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)
            )
    end,
    Result = setup_db(),
    case Result of
        {ok, _} ->
            Previous = application:get_env(imboy, eb_asset_object_store),
            ok = application:unset_env(imboy, eb_asset_object_store),
            try
                Registry = ?FIX:install_asset_registry(),
                {asset_disk, Previous, Registry, Result}
            catch
                Class:Reason:Stack ->
                    try
                        cleanup(Result)
                    after
                        ?FIX:restore_asset_store(Previous)
                    end,
                    erlang:raise(Class, Reason, Stack)
            end;
        _ ->
            Result
    end.

setup_db() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            try ?FIX:ensure_purge_role() of
                ok ->
                    {ok, Conn};
                {error, Reason} ->
                    eunit_runner:eunit_cleanup_db(Conn),
                    {error, Reason}
            catch
                Class:Reason:Stack ->
                    eunit_runner:eunit_cleanup_db(Conn),
                    erlang:raise(Class, Reason, Stack)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({asset_disk, Previous, Registry, Result}) ->
    try
        cleanup(Result)
    after
        try
            ?FIX:restore_asset_registry(Registry)
        after
            ?FIX:restore_asset_store(Previous)
        end
    end;
cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({asset_disk, _Previous, _Registry, Result}) ->
    cases(Result);
cases({ok, _Conn}) ->
    [
        {timeout, ?TIMEOUT_S, fun full_chain_presign_put_confirm_send_assets/0},
        {timeout, ?TIMEOUT_S, fun s3_put_descriptor_shape_independent_of_base_url/0},
        {timeout, ?TIMEOUT_S, fun append_message_body_and_assets_both_empty_422/0},
        {timeout, ?TIMEOUT_S, fun append_message_invalid_asset_element_400/0},
        {timeout, ?TIMEOUT_S, fun presign_rejected_cross_workspace_and_cross_org/0},
        {timeout, ?TIMEOUT_S, fun presign_cross_org_conversation_404/0},
        {timeout, ?TIMEOUT_S, fun presign_requires_seat_authorization/0}
    ];
cases(Other) ->
    erlang:error({csbe01b_seat_send_db_unavailable, Other}).

%% ===================================================================
%% 1. 全链：presign → PUT 字节 → confirm → append_message(asset_ids)
%% ===================================================================

full_chain_presign_put_confirm_send_assets() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>, <<"conversation.write">>]),
    ok = set_keyring(Scope),
    PreviousBase = application:get_env(imboy, base_url),
    ok = application:set_env(imboy, base_url, <<"https://synthetic-seat.invalid">>),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Identity = maps:get(sales_identity_id, Scope),
        Payload = <<"CSBE01B-SEND-PAYLOAD">>,
        MsgId = <<"csbe01b-send-", (integer_to_binary(?FIX:id()))/binary>>,

        Presign = presign_ok(Actor, Org, Ws, Conv, Payload, <<"seat-notes.txt">>),

        %% V3-C04 冻结形状：direct 会话键 + s3_put 直传 descriptor（存储 URL，
        %% 含 x-amz-checksum-sha256 绑定声明——声明即 expected 冻结）。
        ObjectKey = maps:get(<<"object_key">>, Presign),
        ?assert(is_binary(ObjectKey)),
        Upload = maps:get(<<"upload">>, Presign),
        ?assertEqual(<<"PUT">>, maps:get(<<"method">>, Upload)),
        ?assertEqual(<<"s3_put">>, maps:get(<<"transport">>, Upload)),
        ?assert(is_binary(maps:get(<<"url">>, Upload, undefined))),
        Headers = maps:get(<<"required_headers">>, Upload),
        ?assert(
            lists:any(
                fun(H) -> maps:get(<<"key">>, H) =:= <<"x-amz-checksum-sha256">> end,
                Headers
            )
        ),

        %% 正文直传 S3（真 Garage）：presigned PUT + 绑定头；BEAM 零正文。
        S3Resp = s3_put(
            maps:get(<<"url">>, Upload),
            Headers,
            Payload
        ),
        ?assertEqual(200, S3Resp),

        %% confirm：direct finalize + Worker 同字节校验 + active 绑定。
        ConfirmResp = confirm_key(Actor, Org, Ws, ObjectKey),
        case maps:get(status, ConfirmResp) of
            200 ->
                ok;
            Other ->
                erlang:error(
                    {confirm_failed, {Other, ?S:msg(ConfirmResp), ?S:payload(ConfirmResp)}}
                )
        end,
        ConfirmView = ?S:payload(ConfirmResp),
        ?assertEqual(<<"active">>, maps:get(<<"status">>, ConfirmView)),
        AssetIdBin = maps:get(<<"asset_id">>, ConfirmView),

        %% 发送附件消息：asset_ids（TSID string 数组）+ 空正文，出站坐席身份。
        SendResp = send_message(Actor, Org, Ws, Conv, MsgId, #{
            <<"asset_ids">> => [AssetIdBin],
            <<"identity_id">> => integer_to_binary(Identity)
        }),
        ?assertEqual(200, maps:get(status, SendResp)),
        ?assertEqual(0, ?S:code(SendResp)),
        SendView = ?S:payload(SendResp),
        ?assertEqual(true, maps:get(<<"accepted">>, SendView)),
        ?assertEqual(false, maps:get(<<"replayed">>, SendView)),
        Assets = maps:get(<<"assets">>, maps:get(<<"message">>, SendView), missing),
        %% CS-BE-01 冻结五键白名单 + 逐值核对（id 是 TSID string）。
        ?assertEqual([{AssetIdBin, <<"seat-notes.txt">>, <<"active">>}], [
            {maps:get(<<"id">>, A), maps:get(<<"file_name">>, A), maps:get(<<"status">>, A)}
         || A <- Assets
        ]),
        lists:foreach(
            fun(A) ->
                ?assertEqual(
                    [<<"file_name">>, <<"id">>, <<"mime">>, <<"size_bytes">>, <<"status">>],
                    lists:usort(maps:keys(A))
                ),
                ?assertEqual(<<"text/plain">>, maps:get(<<"mime">>, A)),
                ?assertEqual(byte_size(Payload), maps:get(<<"size_bytes">>, A))
            end,
            Assets
        ),

        %% 重放幂等：同一 client_msg_id + 同一 asset_ids → 同一 message（含 assets）。
        ReplayResp = send_message(Actor, Org, Ws, Conv, MsgId, #{
            <<"asset_ids">> => [AssetIdBin],
            <<"identity_id">> => integer_to_binary(Identity)
        }),
        ?assertEqual(200, maps:get(status, ReplayResp)),
        ReplayView = ?S:payload(ReplayResp),
        ?assertEqual(true, maps:get(<<"replayed">>, ReplayView)),
        ?assertEqual(
            maps:get(<<"message_id">>, SendView),
            maps:get(<<"message_id">>, ReplayView)
        ),
        ?assertEqual(Assets, maps:get(<<"assets">>, maps:get(<<"message">>, ReplayView)))
    after
        restore_base(PreviousBase),
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% V3-C04：s3_put descriptor 与业务 base_url 无关（存储签名 URL 恒下发，
%% 不再出现 api_byte 的「无 https 基址即省略 url」分支）。
s3_put_descriptor_shape_independent_of_base_url() ->
    Scope = eb_asset_it_lib:new_scope(),
    PreviousBase = application:get_env(imboy, base_url),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    ok = set_keyring(Scope),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        lists:foreach(
            fun(Base) ->
                restore_base(Base),
                Presign = presign_ok(Actor, Org, Ws, Conv, <<"synthetic">>, undefined),
                Upload = maps:get(<<"upload">>, Presign),
                ?assertEqual(<<"s3_put">>, maps:get(<<"transport">>, Upload)),
                Url = maps:get(<<"url">>, Upload),
                ?assert(is_binary(Url), {url_missing_for, Base}),
                ?assert(nomatch =/= binary:match(Url, <<"X-Amz-Signature">>)),
                ?assertNot(maps:is_key(<<"upload_ref">>, Presign)),
                ?assertNot(maps:is_key(<<"asset_id">>, Presign))
            end,
            [undefined, {ok, <<"http://synthetic-seat.invalid">>}]
        )
    after
        restore_base(PreviousBase),
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

restore_base({ok, Value}) -> application:set_env(imboy, base_url, Value);
restore_base(undefined) -> application:unset_env(imboy, base_url).

%% ===================================================================
%% 2. body 与 asset_ids 皆空：422 invalid_body（确定错误码）
%% ===================================================================

append_message_body_and_assets_both_empty_422() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"conversation.write">>]),
    ok = set_keyring(Scope),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Identity = maps:get(sales_identity_id, Scope),
        Resp = send_message(
            Actor,
            Org,
            Ws,
            Conv,
            <<"csbe01b-empty-", (integer_to_binary(?FIX:id()))/binary>>,
            #{<<"identity_id">> => integer_to_binary(Identity)}
        ),
        ?assertEqual(422, maps:get(status, Resp)),
        %% 稳定标签：{invalid_body, undefined} 的原子路径（undefined 是缺省
        %% 标记，非取值泄漏——body 未提供即 undefined）。
        ?assertEqual(<<"invalid_body.undefined">>, ?S:msg(Resp))
    after
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 3. asset_ids 元素非法（非 TSID）：400 invalid_param.asset_ids
%% ===================================================================

append_message_invalid_asset_element_400() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"conversation.write">>]),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Identity = maps:get(sales_identity_id, Scope),
        Resp = send_message(
            Actor,
            Org,
            Ws,
            Conv,
            <<"csbe01b-badids-", (integer_to_binary(?FIX:id()))/binary>>,
            #{
                <<"asset_ids">> => [<<"not-a-tsid">>],
                <<"identity_id">> => integer_to_binary(Identity)
            }
        ),
        ?assertEqual(400, maps:get(status, Resp)),
        ?assertEqual(<<"invalid_param.asset_ids">>, ?S:msg(Resp))
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 4. 跨租户 PUT：凭证 AAD 绑定 (Org, Workspace)——跨 Ws / 跨 Org 均 422
%% ===================================================================

presign_rejected_cross_workspace_and_cross_org() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    ok = set_keyring(Scope),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, _Ws} = eb_asset_it_lib:tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Payload = <<"CSBE01B-CROSS">>,

        %% V3-C04：direct 会话控制面逐门绑定 (org, workspace, conversation)——
        %% 跨 Workspace 的 presign 在 scope 门失败（conversation 不属该 ws）。
        CrossWsResp = ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
            ?S:request(
                Port,
                <<"POST">>,
                qs_ws(presign_path(Org), OtherWs),
                presign_body(Conv, Payload, undefined)
            )
        end),
        ?assert(lists:member(maps:get(status, CrossWsResp), [403, 404])),

        %% 跨 Org：actor 先补上 OtherOrg 的成员行 + customer_service 经办
        %% （授权门可过），再重放 Org1 的凭证 ⇒ 同一确定拒绝。
        OtherIdentity = ?FIX:id(),
        OtherAssignment = ?FIX:id(),
        ok = sql(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
                " VALUES ($1,$2,'customer_service',$3,'active',1,$4)"
            >>,
            [
                OtherIdentity,
                OtherOrg,
                name(<<"csbe01b-ident-">>, OtherIdentity),
                maps:get(
                    owner_user_id, Scope
                )
            ]
        ),
        ok = sql(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','active')"
            >>,
            [OtherOrg, Actor]
        ),
        ok = sql(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
                " VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [OtherAssignment, OtherOrg, OtherIdentity, Actor, maps:get(owner_user_id, Scope)]
        ),
        %% V3-C04：跨 Org 的 presign 在 scope 门失败（conversation 归属）。
        CrossOrgResp = ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
            ?S:request(
                Port,
                <<"POST">>,
                qs_ws(presign_path(OtherOrg), OtherWs),
                presign_body(Conv, Payload, undefined)
            )
        end),
        ?assert(lists:member(maps:get(status, CrossOrgResp), [403, 404]))
    after
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 5. presign 指向跨 Org 会话：404（不区分不存在与跨租户，避免枚举）
%% ===================================================================

presign_cross_org_conversation_404() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        ForeignConv = maps:get(conversation_id, Scope) + ?FIX:id(),
        Payload = <<"CSBE01B-FOREIGN">>,
        ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs_ws(presign_path(Org), Ws),
                #{
                    conversation_id => integer_to_binary(ForeignConv),
                    mime => <<"text/plain">>,
                    size_bytes => byte_size(Payload),
                    object_hash => eb_asset_content:sha256_hex(Payload)
                }
            ),
            ?assertEqual(404, maps:get(status, Resp)),
            ?assertEqual(<<"not_found">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 6. PUT 端点复走路由级坐席授权门：无凭证 401 / 非成员 403
%% ===================================================================

%% V3-C04：同源 PUT 字节端点移除；授权门断言改打 presign 控制面
%% （无凭证 401 / 非成员 403——控制认证与权限逐面保留）。
presign_requires_seat_authorization() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Body = presign_body(maps:get(conversation_id, Scope), <<"x">>, undefined),
        Resp0 = ?S:with_listener(tenant, presign, session(probe, 0), fun(Port) ->
            ?S:request(Port, <<"POST">>, qs_ws(presign_path(Org), Ws), Body)
        end),
        ?assertEqual(401, maps:get(status, Resp0)),
        %% 无成员关系的真实 uid：授权门 403（no_member 面）。
        Resp1 = ?S:with_listener(
            tenant,
            presign,
            session(probe, maps:get(peer_user_id, Scope)),
            fun(Port) ->
                ?S:request(Port, <<"POST">>, qs_ws(presign_path(Org), Ws), Body)
            end
        ),
        ?assertEqual(403, maps:get(status, Resp1))
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

set_keyring(Scope) ->
    #{key := Key, key_version := V} = eb_asset_it_lib:key_ref(Scope),
    application:set_env(imboy, eb_enterprise_keyring, #{
        active_version => V,
        keys => #{V => binary:encode_hex(Key, lowercase)}
    }).

session(probe, Uid) ->
    #{current_uid => Uid, auth_facts => ?S:facts({probe, []})}.

presign_path(Org) ->
    ?S:path(tenant, presign, #{org_id => Org}).

%% presign POST（成功即返回 payload map）。
presign_ok(Actor, Org, Ws, Conv, Payload, FileName) ->
    Resp = ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
        ?S:request(
            Port,
            <<"POST">>,
            qs_ws(presign_path(Org), Ws),
            presign_body(
                Conv, Payload, FileName
            )
        )
    end),
    ?assertEqual(200, maps:get(status, Resp)),
    ?S:payload(Resp).

presign_body(Conv, Payload, undefined) ->
    #{
        conversation_id => integer_to_binary(Conv),
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => eb_asset_content:sha256_hex(Payload)
    };
presign_body(Conv, Payload, FileName) ->
    (presign_body(Conv, Payload, undefined))#{file_name => FileName}.

%% V3-C04：confirm 用 direct 会话键（object_key）定位 finalize。
confirm_key(Actor, Org, Ws, ObjectKey) ->
    ?S:with_listener(tenant, confirm_asset, session(probe, Actor), fun(Port) ->
        ?S:request(
            Port,
            <<"POST">>,
            qs_ws(?S:path(tenant, confirm_asset, #{org_id => Org}), Ws),
            #{object_key => ObjectKey}
        )
    end).

%% 正文直传 S3（真 Garage）：按 descriptor 签发的 presigned PUT + 绑定头；
%% 返回 HTTP 状态码（httpc，inets 已由 eunit 启动链带起）。
%% V3-C04：正文直传 S3 用 gun（与生产 bounded control plane 同款 HTTP/1.1
%% 客户端；头与签名逐字一致，不引入任何规范化差异）。
s3_put(Url, RequiredHeaders, Payload) ->
    Map = uri_string:normalize(Url, [return_map]),
    #{host := Host, port := Port, path := Path, query := Query} = Map,
    Hdrs = [
        {binary_to_list(maps:get(<<"key">>, H)), binary_to_list(maps:get(<<"value">>, H))}
     || H <- RequiredHeaders
    ],
    {ok, Conn} = gun:open(binary_to_list(Host), Port, #{protocols => [http]}),
    try
        {ok, _} = gun:await_up(Conn, 10000),
        PathQuery = <<Path/binary, $?, Query/binary>>,
        Ref = gun:request(Conn, <<"PUT">>, PathQuery, Hdrs, Payload),
        case gun:await(Conn, Ref, 30000) of
            {response, fin, 200, _} ->
                200;
            {response, nofin, 200, _} ->
                {ok, _} = gun:await_body(Conn, Ref, 30000),
                200;
            {response, _, Status, RespHdrs} ->
                {ok, Body} = gun:await_body(Conn, Ref, 30000),
                erlang:error({s3_put_failed, {Status, RespHdrs, Body}});
            Other ->
                erlang:error({s3_put_failed, Other})
        end
    after
        gun:close(Conn)
    end.

send_message(Actor, Org, Ws, Conv, ClientMsgId, Extra) ->
    ?S:with_listener(tenant, conversation_messages, session(probe, Actor), fun(Port) ->
        ?S:request(
            Port,
            <<"POST">>,
            qs_ws(?S:path(tenant, conversation_messages, #{org_id => Org, id => Conv}), Ws),
            maps:merge(
                #{client_msg_id => ClientMsgId, sender_type => <<"business_identity">>},
                Extra
            )
        )
    end).

%% workspace_id 单参数查询串（十进制 TSID 无需编码）。
qs_ws(Path, Ws) ->
    <<Path/binary, "?workspace_id=", (integer_to_binary(Ws))/binary>>.

sql(Sql, Params) ->
    ok = ?FIX:exec(Sql, Params).

name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
