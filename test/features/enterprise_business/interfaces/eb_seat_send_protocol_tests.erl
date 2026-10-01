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
%%%     一律 422 invalid_upload_ref（确定错误码，无枚举面）。
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
    Previous = ?FIX:select_asset_stub(),
    {asset_stub, Previous, setup_db()}.

setup_db() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case ?FIX:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({asset_stub, Previous, Result}) ->
    try
        cleanup(Result)
    after
        ?FIX:restore_asset_store(Previous)
    end;
cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({asset_stub, _Previous, Result}) ->
    cases(Result);
cases({ok, _Conn}) ->
    [
        {timeout, ?TIMEOUT_S, fun full_chain_presign_put_confirm_send_assets/0},
        {timeout, ?TIMEOUT_S, fun presign_without_https_base_omits_upload_url/0},
        {timeout, ?TIMEOUT_S, fun append_message_body_and_assets_both_empty_422/0},
        {timeout, ?TIMEOUT_S, fun append_message_invalid_asset_element_400/0},
        {timeout, ?TIMEOUT_S, fun put_rejected_cross_workspace_and_cross_org/0},
        {timeout, ?TIMEOUT_S, fun presign_cross_org_conversation_404/0},
        {timeout, ?TIMEOUT_S, fun put_requires_seat_authorization/0}
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
        UploadRef = maps:get(<<"upload_ref">>, Presign),
        AssetIdBin = maps:get(<<"asset_id">>, Presign),

        %% upload.url 形状（CS-BE-01B 冻结）：绝对 https + 本 org 的 presign
        %% 路径 + 查询串 workspace_id/upload_ref；短 TTL 存在性（expires_at）。
        assert_upload_url_shape(Presign, Org, Ws, UploadRef),

        %% PUT 原始字节（按 upload.url 的 path+query 打到本监听器——权威基址
        %% 属部署面，测试以同一路径/查询驱动同一路由）。
        Url = maps:get(<<"url">>, maps:get(<<"upload">>, Presign)),
        PutResp = put_raw(Actor, url_path_query(Url), Payload),
        ?assertEqual(200, maps:get(status, PutResp)),
        ?assertEqual(0, ?S:code(PutResp)),
        ?assertEqual(<<"pending_confirm">>, maps:get(<<"status">>, ?S:payload(PutResp))),

        %% confirm：绑定资产 → active。
        ConfirmResp = confirm_ref(Actor, Org, Ws, UploadRef),
        ?assertEqual(200, maps:get(status, ConfirmResp)),
        ?assertEqual(<<"active">>, maps:get(<<"status">>, ?S:payload(ConfirmResp))),

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

presign_without_https_base_omits_upload_url() ->
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
                assert_upload_url_shape(Presign, Org, Ws, maps:get(<<"upload_ref">>, Presign)),
                ?assertNot(maps:is_key(<<"url">>, maps:get(<<"upload">>, Presign)))
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

put_rejected_cross_workspace_and_cross_org() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    ok = set_keyring(Scope),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Payload = <<"CSBE01B-CROSS">>,

        Presign = presign_ok(Actor, Org, Ws, Conv, Payload, undefined),
        UploadRef = maps:get(<<"upload_ref">>, Presign),

        %% 跨 Workspace（同 Org）：授权门照过（成员/职能/权限都在本 Org），
        %% 凭证 AAD 绑定 (Org, Ws) 不匹配 ⇒ 422 invalid_upload_ref。
        CrossWsResp = put_query(
            Actor,
            presign_path(Org),
            [
                {<<"workspace_id">>, OtherWs}, {<<"upload_ref">>, UploadRef}
            ],
            Payload
        ),
        ?assertEqual(422, maps:get(status, CrossWsResp)),
        ?assertEqual(<<"invalid_upload_ref">>, ?S:msg(CrossWsResp)),

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
        CrossOrgResp = put_query(
            Actor,
            presign_path(OtherOrg),
            [
                {<<"workspace_id">>, OtherWs}, {<<"upload_ref">>, UploadRef}
            ],
            Payload
        ),
        ?assertEqual(422, maps:get(status, CrossOrgResp)),
        ?assertEqual(<<"invalid_upload_ref">>, ?S:msg(CrossOrgResp))
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

put_requires_seat_authorization() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Resp0 = put_query(
            0,
            presign_path(Org),
            [
                {<<"workspace_id">>, Ws}, {<<"upload_ref">>, <<"forged-ref">>}
            ],
            <<"x">>
        ),
        ?assertEqual(401, maps:get(status, Resp0)),
        %% 无成员关系的真实 uid：授权门 403（no_member 面），字节不落桶。
        Resp1 = put_query(
            maps:get(peer_user_id, Scope),
            presign_path(Org),
            [
                {<<"workspace_id">>, Ws}, {<<"upload_ref">>, <<"forged-ref">>}
            ],
            <<"x">>
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

%% PUT 原始字节：PathQuery 已是完整 path?query（服务端 URL 原样）。
put_raw(Actor, PathQuery, Payload) ->
    with_presign_listener(Actor, fun(Port) ->
        ?S:request(Port, <<"PUT">>, PathQuery, Payload, #{
            <<"content-type">> => <<"application/octet-stream">>
        })
    end).

%% PUT 原始字节：查询串由参数拼装（**百分号编码**——upload_ref 的 base64
%% 字母表含 + / =，裸拼会被 cow_qs 当 form 编码破坏）。
put_query(Actor, Path, Qs, Payload) ->
    Query = uri_string:compose_query([{K, to_bin(V)} || {K, V} <- Qs]),
    put_raw(Actor, <<Path/binary, $?, Query/binary>>, Payload).

with_presign_listener(Actor, Fun) ->
    ?S:with_listener(tenant, presign, session(probe, Actor), Fun).

confirm_ref(Actor, Org, Ws, UploadRef) ->
    ?S:with_listener(tenant, confirm_asset, session(probe, Actor), fun(Port) ->
        ?S:request(
            Port,
            <<"POST">>,
            qs_ws(?S:path(tenant, confirm_asset, #{org_id => Org}), Ws),
            #{upload_ref => UploadRef}
        )
    end).

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

%% upload.url 形状断言：绝对 https、本 org 的 presign 路径、查询串（解码后）
%% 恰为 workspace_id/upload_ref、expires_at 短 TTL 存在（≤ now + 1h 上界）。
%% base_url 未配置（非 https）时 fail-closed：无 url 键（同形断言）。
assert_upload_url_shape(Presign, Org, Ws, UploadRef) ->
    Upload = maps:get(<<"upload">>, Presign),
    ?assertEqual(<<"PUT">>, maps:get(<<"method">>, Upload)),
    ExpiresAt = maps:get(<<"expires_at">>, Upload, undefined),
    ?assert(is_integer(ExpiresAt)),
    ?assert(ExpiresAt =< eb_asset_it_lib:now_sec() + 3600),
    case config_ds:env(base_url, <<>>) of
        <<"https://", _/binary>> = Base ->
            Url = maps:get(<<"url">>, Upload, undefined),
            ?assert(is_binary(Url)),
            Prefix = <<Base/binary, (presign_path(Org))/binary, "?">>,
            ?assertEqual(
                {match, 0},
                begin
                    Size = byte_size(Prefix),
                    case Url of
                        <<Prefix:Size/binary, _/binary>> -> {match, 0};
                        _ -> nomatch
                    end
                end
            ),
            Query = binary:part(Url, byte_size(Prefix), byte_size(Url) - byte_size(Prefix)),
            ?assertEqual(
                [{<<"upload_ref">>, UploadRef}, {<<"workspace_id">>, integer_to_binary(Ws)}],
                lists:sort(uri_string:dissect_query(Query))
            ),
            %% object key / 存储引用零出站（url 是本 API 代理路径）。
            ?assertEqual(nomatch, binary:match(Url, <<"object_key">>)),
            ?assertEqual(nomatch, binary:match(Url, <<"X-Amz-">>));
        _Unconfigured ->
            ?assert(not maps:is_key(<<"url">>, Upload))
    end.

%% 绝对 URL → path?query（打到本测试监听器；权威基址属部署面）。
url_path_query(<<"https://", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>) of
        [_Authority, PathQ] -> <<"/", PathQ/binary>>;
        [_Authority] -> <<"/">>
    end.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V).

sql(Sql, Params) ->
    ok = ?FIX:exec(Sql, Params).

name(Prefix, Id) ->
    <<Prefix/binary, (integer_to_binary(Id))/binary>>.
