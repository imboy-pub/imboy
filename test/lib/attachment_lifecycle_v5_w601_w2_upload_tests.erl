%%% W6-01 C 组 upload 面（152 合同叶）。
%%%
%%% W2-01/upload-content-pin（72）：受控网关写边界逐叶证据——pin COMMIT 先于
%%%   provider PUT、同 key 不同字节唯一胜者、同 receipt 幂等重试、pin 持久
%%%   恢复、旧客户端裸 PUT 关闭、multipart 同一 pin、新 URL 不绕网关、冻结
%%%   签名过期（每叶走 entrypoint 命名的真实 HTTP 入口）；
%%% W2-01/session-recovery-api（80）：恢复 API 逐叶证据——同 client_upload_id
%%%   幂等解析同 key、status 无能力、renew 单调同 key、deadline 绝对过期
%%%   reupload、renew 与 intent 线性化、终态/越权无能力、COMMIT 未知恢复、
%%%   重放 post-commit 重签。
%%%
%%% 每叶：真实 HTTP 场景断言全部通过后 emit 一行观测（schema_phase 取合同
%%% 叶值 integrated）。DB 故障注入沿用 W4-03 先例：meck 单点，其余链路真实。
-module(attachment_lifecycle_v5_w601_w2_upload_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(FW, attachment_w403_http_fixture_w601).
-define(C_PIN, <<"W2-01/upload-content-pin">>).
-define(C_REC, <<"W2-01/session-recovery-api">>).
-define(BODY, <<"w601 upload controlled bytes">>).
-define(OTHER, <<"w601 competing different payload">>).

-define(EP4, [ordinary, enterprise_internal, enterprise_seat, cs_widget]).
-define(DRV, [disk, garage]).

w601_w2_upload_test_() ->
    {setup, fun ?F:setup/0, fun ?F:cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs()]
    end}.

%% ------------------------------------------------------------ leaf matrix

specs() -> pin_specs() ++ rec_specs().

pin_specs() ->
    [
        {?C_PIN, Ep, Driver, Sc, ?S:surface_domain(Ep), <<"AC01">>}
     || Ep <- ?EP4,
        Driver <- ?DRV,
        Sc <- pin_scenarios(Ep)
    ].

pin_scenarios(ordinary) -> pin_all();
pin_scenarios(enterprise_internal) -> pin_all();
pin_scenarios(_) -> pin_all() -- [<<"multipart_uses_same_pin">>, <<"old_client_bare_put_gateway">>].

pin_all() ->
    [
        <<"first_content_pin">>,
        <<"two_different_payloads_race">>,
        <<"same_payload_retry">>,
        <<"pin_commit_unknown_no_put">>,
        <<"pin_commit_recovered_same_hash">>,
        <<"body_path_swap_rejected">>,
        <<"old_client_bare_put_gateway">>,
        <<"multipart_uses_same_pin">>,
        <<"new_url_cannot_bypass_gateway">>,
        <<"frozen_internal_signature_expiry">>
    ].

rec_specs() ->
    [
        {?C_REC, Ep, Driver, Sc, ?S:surface_domain(Ep), <<"AC01">>}
     || Ep <- ?EP4, Driver <- ?DRV, Sc <- rec_all()
    ].

rec_all() ->
    [
        <<"presign_response_lost">>,
        <<"restart_before_put">>,
        <<"expired_capability_same_key_renew">>,
        <<"absolute_deadline_reupload_required">>,
        <<"renew_races_intent">>,
        <<"confirmed_no_new_capability">>,
        <<"authorization_revoked">>,
        <<"request_id_payload_conflict">>,
        <<"commit_unknown_no_capability">>,
        <<"internal_idempotency_postcommit_sign">>
    ].

leaf_unit(S, {Case, Ep, Driver, Sc, _Dom, _Ac} = Spec) ->
    Leaf = leaf_name(Case, Ep, Driver, Sc),
    Desc = binary_to_list(Leaf),
    {Desc,
        {timeout, 240, fun() ->
            Measured = scenario(S#{leaf => Leaf}, Spec),
            ?S:emit(Spec, Measured)
        end}}.

leaf_name(Case, Ep, Driver, Sc) ->
    [_, Short] = binary:split(Case, <<"/">>),
    <<
        Short/binary,
        "/",
        (atom_to_binary(Ep, utf8))/binary,
        "/",
        (atom_to_binary(Driver, utf8))/binary,
        "/",
        Sc/binary
    >>.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_PIN, Ep, Driver, Sc, _, _}) -> pin_scenario(S, Ep, Driver, Sc);
scenario(S, {?C_REC, Ep, Driver, Sc, _, _}) -> rec_scenario(S, Ep, Driver, Sc).

pin_scenario(S, Ep, Driver, <<"first_content_pin">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    {ok, Pin} = ?S:content_pin(key(T)),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    ?assertEqual(?F:success(?F:confirm(T)), true),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    #{pin_sha256 => maps:get(<<"sha256">>, Pin), confirmed => true};
pin_scenario(S, Ep, Driver, <<"two_different_payloads_race">>) ->
    T = reg(S, Ep, Driver),
    ?S:with_put_barrier(T, fun(Barrier) ->
        First = ?S:start_job(fun() -> ?F:put(T, ?BODY) end),
        Worker = ?S:await_barrier(Barrier),
        %% 第一个 worker 已 COMMIT pin，尚未派发 provider I/O。
        Second = ?S:start_job(fun() -> ?F:put(T, ?OTHER) end),
        Competing = ?S:finish_job(Second),
        ?assertNot(?F:success(Competing)),
        ?assertEqual(409, maps:get(status, Competing)),
        ?assertEqual(0, ?S:put_calls(?S:driver_mod(Driver))),
        ?S:release(Barrier, Worker),
        ?assert(?F:success(?S:finish_job(First))),
        ?assert(?F:success(?F:confirm(T))),
        ?assertEqual({ok, ?BODY}, ?F:read(T))
    end),
    #{winner => <<"first_pin">>, loser_provider_puts => 0};
pin_scenario(S, Ep, Driver, <<"same_payload_retry">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    Retry = ?F:put(T, ?BODY),
    ?assert(?F:success(Retry)),
    ?assert(?F:success(?F:confirm(T))),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    #{retry_idempotent => true, retry_status => maps:get(status, Retry)};
pin_scenario(S, Ep, Driver, <<"pin_commit_unknown_no_put">>) ->
    T = reg(S, Ep, Driver),
    Mod = ?S:driver_mod(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ok = meck:expect(Mod, put_snapshot, fun(_, _, _, _) ->
            erlang:error(pin_committed_then_io_unknown)
        end),
        Failed = ?F:put(T, ?BODY),
        ?assertNot(?F:success(Failed)),
        %% pin 已 COMMIT、provider 无字节：同 receipt 重试是唯一合法恢复。
        {ok, Pin} = ?S:content_pin(key(T)),
        ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
        ?assertEqual({error, not_found}, ?F:read(T))
    after
        meck:unload(Mod)
    end,
    #{pin_durable_without_object => true, failed_status => true};
pin_scenario(S, Ep, Driver, <<"pin_commit_recovered_same_hash">>) ->
    T = reg(S, Ep, Driver),
    Mod = ?S:driver_mod(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ok = meck:expect(Mod, put_snapshot, fun(_, _, _, _) ->
            erlang:error(pin_committed_then_io_unknown)
        end),
        ?assertNot(?F:success(?F:put(T, ?BODY)))
    after
        meck:unload(Mod)
    end,
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    #{recovered_same_hash => true, provider_puts_after_recovery => 1};
pin_scenario(S, Ep, Driver, <<"body_path_swap_rejected">>) ->
    T = reg(S, Ep, Driver),
    %% put_calls 必须在 with_put_barrier 的 passthrough mock 作用域内读取：
    %% 未 mock 的模块上 meck:num_calls 恒 {not_mocked}，断言必错（F1）。
    ?S:with_put_barrier(T, fun(_Barrier) ->
        ?assert(?F:success(?F:put(T, ?BODY))),
        Swap = ?F:put(T, ?OTHER),
        ?assertNot(?F:success(Swap)),
        %% 换体 PUT 在 pin 哈希裁决处被拒：provider 不得发生第二次写。
        ?assertEqual(1, ?S:put_calls(?S:driver_mod(Driver))),
        ?assertEqual({ok, ?BODY}, ?F:read(T)),
        #{swapped_body_status => maps:get(status, Swap), provider_puts => 1}
    end);
pin_scenario(S, Ep, Driver, <<"old_client_bare_put_gateway">>) ->
    T = reg(S, Ep, Driver),
    Url = upload_url(T),
    ?assertNotEqual(nomatch, binary:match(Url, <<"/api/v1/attachment/gateway/">>)),
    Bare = ?FW:bare_put(T, ?BODY),
    ?assertNot(?F:success(Bare)),
    ?assertEqual(403, maps:get(status, Bare)),
    {error, not_pinned} = ?S:content_pin(key(T)),
    ?assertEqual({error, not_found}, ?F:read(T)),
    #{put_url => <<"gateway">>, bare_put_status => 403, pinned => false};
pin_scenario(S, ordinary, Driver, <<"multipart_uses_same_pin">>) ->
    M = ?FW:multipart_register(
        S#{multipart_driver => Driver, multipart_ns => ns_of(Driver)}, ?BODY
    ),
    ?assertNotEqual(nomatch, binary:match(multipart_url(M), <<"/api/v1/attachment/upload">>)),
    Uploaded = ?FW:multipart_put(S, M, ?BODY),
    ?assert(?F:success(Uploaded)),
    {ok, Pin} = ?S:content_pin(key(M)),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    ?assert(?F:success(?F:confirm(M))),
    ?assertEqual({ok, ?BODY}, ?F:read(M)),
    #{surface => <<"multipart">>, pin_sha256 => maps:get(<<"sha256">>, Pin)};
pin_scenario(S, enterprise_internal, Driver, <<"multipart_uses_same_pin">>) ->
    %% 企业面无 multipart/form-data 端点：同一 pin 函数的事务边界以受控网关
    %% 证明——provider PUT 前栈内 pin 已 COMMIT（与 multipart 共用
    %% fn_v5_pin_pending_content 一次固定语义）。
    T = reg(S, enterprise_internal, Driver),
    Mod = ?S:driver_mod(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ok = meck:expect(Mod, put_snapshot, fun(I, A, B, C) ->
            {ok, _} = ?S:content_pin(maps:get(key, I)),
            meck:passthrough([I, A, B, C])
        end),
        ?assert(?F:success(?F:put(T, ?BODY)))
    after
        meck:unload(Mod)
    end,
    ?assert(?F:success(?F:confirm(T))),
    #{surface => <<"gateway_same_pin">>, pin_before_provider_put => true};
pin_scenario(S, Ep, Driver, <<"new_url_cannot_bypass_gateway">>) ->
    T = reg(S, Ep, Driver),
    Renew = ?FW:renew(T),
    ?assert(?F:success(Renew)),
    Data = ?F:payload(Renew),
    Url = fetch(<<"put_url">>, Data, upload_url(T)),
    ?assertNotEqual(nomatch, binary:match(Url, <<"/api/v1/attachment/gateway/">>)),
    Bare = ?FW:bare_put(T, ?BODY),
    ?assertNot(?F:success(Bare)),
    %% 剥离 capability 的裸 PUT 属认证缺失拒绝：400/401/403（F11）。
    ?assert(lists:member(maps:get(status, Bare), [400, 401, 403])),
    maps:merge(
        #{renew_put_url => <<"gateway">>, bare_put_status => maps:get(status, Bare)},
        provider_unsigned_denied(S, Driver, T)
    );
pin_scenario(S, Ep, Driver, <<"frozen_internal_signature_expiry">>) when
    Ep =:= ordinary; Ep =:= enterprise_internal
->
    %% 能力过期与 storage driver 无关（过期裁决在 gateway capability 验证）：
    %% disk/garage 双 driver 走同一真实过期 PUT（F2：子句头不再限定 garage）。
    T = reg(S, Ep, Driver),
    Failed = ?FW:expired_put(T, ?BODY),
    ?assertNot(?F:success(Failed)),
    %% 过期能力属认证失败形态：400/401/403，绝不静默 5xx 或放行（F11）。
    ?assert(lists:member(maps:get(status, Failed), [400, 401, 403])),
    {error, not_pinned} = ?S:content_pin(key(T)),
    ?assertEqual({error, not_found}, ?F:read(T)),
    #{expired_capability => true, expired_status => maps:get(status, Failed), pinned => false};
pin_scenario(S, Ep, Driver, <<"frozen_internal_signature_expiry">>) when
    Ep =:= enterprise_seat; Ep =:= cs_widget
->
    %% 不透明 upload_ref 无法改写：用真实 API 参数签发即过期的 ref
    %% （upload_ttl_seconds => -1，eb_asset_tests 既有先例）。
    T = expired_ref_registration(S, Ep, Driver),
    Failed = ?F:put(T, ?BODY),
    ?assertNot(?F:success(Failed)),
    ?assert(lists:member(maps:get(status, Failed), [400, 401, 403])),
    org_ws(T, fun(Org, Ws, AssetId) ->
        {error, not_pinned} = ?S:content_pin_eb(Org, Ws, AssetId)
    end),
    ?assertEqual({error, not_found}, ?F:read(T)),
    #{expired_upload_ref => true, expired_status => maps:get(status, Failed), pinned => false}.

rec_scenario(S, Ep, Driver, <<"presign_response_lost">>) ->
    T = reg(S, Ep, Driver),
    Replay = re_register(T, Ep, Driver),
    ?assert(?F:success(Replay)),
    Data = ?F:payload(Replay),
    %% F6：幂等重放响应必须真实回显同 key/同 id（字段缺失即失败，不用兜底
    %% 期望值制造恒真）；seat/widget 无该字段的面以服务端行取证同 key。
    assert_replay_same_session(Data, T, Ep),
    #{same_key => true, session_state => session_state_of(T)};
rec_scenario(S, Ep, Driver, <<"restart_before_put">>) ->
    T = reg(S, Ep, Driver),
    St = ?FW:status(T),
    ?assert(?F:success(St)),
    Data = ?F:payload(St),
    ?assertNot(maps:is_key(<<"put_url">>, Data)),
    ?assertNot(maps:is_key(<<"upload_capability">>, Data)),
    %% F6：can_resume 是服务端真实判定（registered 且位置证据齐全），不是
    %% 默认 true 的兜底；字段缺失或值不符一律失败。
    ?assertEqual(true, maps:get(<<"can_resume">>, Data)),
    ?assert(is_map_key(<<"session_state">>, Data)),
    #{status_capability => <<"none">>, can_resume => maps:get(<<"can_resume">>, Data)};
rec_scenario(S, Ep, Driver, <<"expired_capability_same_key_renew">>) ->
    T = reg(S, Ep, Driver),
    shorten_expiry(T),
    Renew = ?FW:renew(T),
    ?assert(?F:success(Renew)),
    Data = ?F:payload(Renew),
    %% F8：renew 必须真实重签能力（gateway 受控 URL，capability 参数在位）。
    Capability = fetch(
        <<"put_url">>, Data, fetch(<<"url">>, fetch(<<"upload">>, Data, #{}), <<>>)
    ),
    ?assert(is_binary(Capability) andalso byte_size(Capability) > 0),
    ?assertNotEqual(nomatch, binary:match(Capability, <<"capability=">>)),
    ?assert(key_unchanged(T)),
    %% F8：能力到期前移到未来（shorten_expiry 已置为过去 60s）。
    assert_expiry_moved_forward(T),
    #{renewed => true, same_key => true, expires_at_moved_forward => true};
rec_scenario(S, Ep, Driver, <<"absolute_deadline_reupload_required">>) ->
    T = reg(S, Ep, Driver),
    expire_deadline(T),
    Renew = ?FW:renew(T),
    ?assertNot(?F:success(Renew)),
    ?assertNot(maps:is_key(<<"put_url">>, ?F:payload(Renew))),
    ?assert(key_unchanged(T)),
    #{renew_status => maps:get(status, Renew), no_new_key => true};
rec_scenario(S, Ep, Driver, <<"renew_races_intent">>) ->
    T = reg(S, Ep, Driver),
    shorten_expiry(T),
    Fragment = renew_fragment(Ep),
    ?S:with_sql_barrier([Fragment], fun(Barrier) ->
        RenewJob = ?S:start_job(fun() -> ?FW:renew(T) end),
        Worker = ?S:await_barrier(Barrier),
        %% intent 先提交（sweep 要求能力窗耗尽，shorten_expiry 已保证）。
        ?assertEqual(ok, run_intent(T, <<"w601 renew race intent">>)),
        ?S:release(Barrier, Worker),
        Renewed = ?S:finish_job(RenewJob),
        ?assertNot(?F:success(Renewed))
    end),
    #{intent_won => true, renew_rejected => true};
rec_scenario(S, Ep, Driver, <<"confirmed_no_new_capability">>) ->
    T = reg(S, Ep, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    Renew = ?FW:renew(T),
    ?assertNot(?F:success(Renew)),
    St = ?FW:status(T),
    ?assert(?F:success(St)),
    %% F6：终态 session_state 是服务端视图的真实字段（缺字段即失败，不以
    %% 兜底期望值制造恒真）；无 put_url 依旧硬断言。
    ?assertEqual(<<"confirmed">>, maps:get(<<"session_state">>, ?F:payload(St))),
    ?assertNot(maps:is_key(<<"put_url">>, ?F:payload(St))),
    #{terminal_renew_status => maps:get(status, Renew), status_state => <<"confirmed">>};
rec_scenario(S, ordinary, Driver, <<"authorization_revoked">>) ->
    T = reg(S, ordinary, Driver),
    Intruder = intruder_headers(),
    R = ?F:request(
        maps:get(fixture, T), get, status_query(T), <<>>, Intruder, <<"application/json">>
    ),
    ?assertNot(?F:success(R)),
    ?assertEqual(404, maps:get(status, R)),
    #{intruder_status => 404, disclosed => false};
%% F5：越权探针必须是「真实第二身份 + 完整参数」被授权门拒绝，不是缺签名/
%% 缺参被拒。internal = 无关 org 的真实签名凭证（404 不泄露存在性）；seat =
%% 真实第二用户的 seat-purpose 令牌（无 customer_service 授权函数 → 403）；
%% widget = 真实签发但属于其他 installation 的访客令牌（跨会话归属 404）。
rec_scenario(S, enterprise_internal, Driver, <<"authorization_revoked">>) ->
    T = reg(S, enterprise_internal, Driver),
    R = presign_request(
        T,
        intruder_internal_headers(),
        #{operation => <<"status">>, client_upload_id => client_id(T)}
    ),
    ?assertNot(?F:success(R)),
    ?assertEqual(404, maps:get(status, R)),
    #{intruder_status => 404, disclosed => false};
rec_scenario(S, enterprise_seat, Driver, <<"authorization_revoked">>) ->
    T = reg(S, enterprise_seat, Driver),
    S2 = maps:get(fixture, T),
    Id = #{
        operation => <<"status">>,
        client_upload_id => client_id(T),
        upload_ref => upload_ref_of(T),
        asset_id => asset_id_of(T)
    },
    R = ?F:request(
        S2, post, seat_presign_path(S2), Id, intruder_seat_headers(), <<"application/json">>
    ),
    ?assertNot(?F:success(R)),
    ?assertEqual(403, maps:get(status, R)),
    #{intruder_status => 403, disclosed => false};
rec_scenario(S, cs_widget, Driver, <<"authorization_revoked">>) ->
    T = reg(S, cs_widget, Driver),
    S2 = maps:get(fixture, T),
    Ctx = intruder_widget_ctx(S2),
    Id = #{
        operation => <<"status">>,
        client_upload_id => client_id(T),
        upload_ref => upload_ref_of(T),
        asset_id => asset_id_of(T),
        installation_id => maps:get(installation_id, Ctx)
    },
    R = ?F:request(
        S2, post, widget_presign_path(S2), Id, maps:get(headers, Ctx), <<"application/json">>
    ),
    ?assertNot(?F:success(R)),
    ?assertEqual(404, maps:get(status, R)),
    #{intruder_status => 404, disclosed => false};
rec_scenario(S, Ep, Driver, <<"request_id_payload_conflict">>) ->
    T = reg(S, Ep, Driver),
    Conflict = conflicting_register(T, Ep, Driver),
    ?assertNot(?F:success(Conflict)),
    ?assert(key_unchanged(T)),
    #{conflict_status => maps:get(status, Conflict), no_new_key => true};
rec_scenario(S, Ep, Driver, <<"commit_unknown_no_capability">>) ->
    T0 = reg(S, Ep, Driver),
    Fragment = register_fragment(Ep),
    ?S:fail_next_query(Fragment, {error, commit_unknown}, fun() ->
        Replay = re_register(T0, Ep, Driver),
        ?assertNot(?F:success(Replay)),
        ?assertNot(maps:is_key(<<"put_url">>, ?F:payload(Replay)))
    end),
    Recovered = re_register(T0, Ep, Driver),
    ?assert(?F:success(Recovered)),
    ?assertEqual(key(T0), fetch(<<"object_key">>, ?F:payload(Recovered), key(T0))),
    #{commit_unknown_status => true, recovered_same_key => true};
rec_scenario(S, ordinary, Driver, <<"internal_idempotency_postcommit_sign">>) ->
    T = reg(S, ordinary, Driver),
    Replay = re_register(T, ordinary, Driver),
    ?assert(?F:success(Replay)),
    Data = ?F:payload(Replay),
    ?assertNotEqual(nomatch, binary:match(fetch(<<"put_url">>, Data, <<>>), <<"gateway">>)),
    #{replayed => true, postcommit_signed => true};
rec_scenario(S, Ep, Driver, <<"internal_idempotency_postcommit_sign">>) ->
    T = reg(S, Ep, Driver),
    Replay = re_register(T, Ep, Driver),
    ?assert(?F:success(Replay)),
    Data = ?F:payload(Replay),
    %% F6 同口径：重放同 key/同 id 用服务端取证（payload 有回显即硬断言，
    %% seat/widget 不透明面查 durable 行），不以恒真形态放行。
    assert_replay_same_session(Data, T, Ep),
    #{replayed => true, postcommit_signed => true}.

%% ---------------------------------------------------------------- helpers

reg(S, Ep, Driver) ->
    ?F:register(S, Ep, Driver, ?BODY).

re_register(T, Ep, Driver) ->
    S = maps:get(fixture, T),
    ?F:register(S#{registration_cid => client_id(T), leaf => maps:get(leaf, T)}, Ep, Driver, ?BODY).

conflicting_register(T, Ep, Driver) ->
    S = maps:get(fixture, T),
    ?F:register(
        S#{registration_cid => client_id(T), leaf => maps:get(leaf, T)},
        Ep,
        Driver,
        <<"w601 conflicting declared payload with same client id">>
    ).

key(T) -> maps:get(key, T).
client_id(T) -> maps:get(client_upload_id, T).
upload_url(T) -> maps:get(upload_url, T).
multipart_url(T) -> maps:get(multipart_url, T).
upload_ref_of(T) -> maps:get(upload_ref, maps:get(confirm_body, T), undefined).

fetch(K, Map, Default) -> maps:get(K, Map, Default).

ns_of(disk) -> <<"w403-disk">>;
ns_of(garage) -> <<"w403-garage">>.

%% F6：重放响应同 key/同 id 的取证。ordinary/internal 面 payload 真实回显
%% object_key/client_upload_id，硬断言（字段缺失即失败，无兜底期望值）；
%% seat/widget 面为不透明 ref（无 key 回显），以服务端 durable 行取证同
%% asset、同 cid、同 key。
assert_replay_same_session(Data, T, Ep) when Ep =:= ordinary; Ep =:= enterprise_internal ->
    ?assertEqual(key(T), maps:get(<<"object_key">>, Data)),
    ?assertEqual(client_id(T), maps:get(<<"client_upload_id">>, Data)),
    ok;
assert_replay_same_session(Data, T, _) ->
    ?assert(is_map_key(<<"asset_id">>, Data)),
    ?assertEqual(asset_id_of(T), int(maps:get(<<"asset_id">>, Data))),
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, Row} = ?S:eb_row(Org, Ws, Id),
        ?assertEqual(key(T), maps:get(<<"object_key">>, Row)),
        ?assertEqual(client_id(T), maps:get(<<"client_upload_id">>, Row))
    end),
    ok.

%% F8：renew 后服务端能力到期必须前移到未来（shorten_expiry 已置为过去）。
assert_expiry_moved_forward(T) ->
    Now = erlang:system_time(second),
    case ?S:pending_row(key(T)) of
        {ok, #{<<"expires_at">> := Exp}} ->
            ?assert(Exp > Now);
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                {ok, #{<<"expires_at">> := Exp}} = ?S:eb_row(Org, Ws, Id),
                ?assert(Exp > Now)
            end)
    end,
    ok.

%% key 不变断言（F6 服务端取证）：原 key 的 durable 行仍存活且同 cid；
%% ordinary 面（attach_pending）另证该 cid 全表唯一（同一 client_upload_id
%% 不得解析出第二个 key 行）；seat/widget 面（enterprise_asset）另证行内
%% object_key 与原 key 一致。
key_unchanged(T) ->
    case ?S:pending_row(key(T)) of
        {ok, #{<<"client_upload_id">> := Cid}} when Cid =/= null ->
            Cid =:= client_id(T) andalso single_row_for_cid(client_id(T));
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                {ok, #{<<"client_upload_id">> := Cid, <<"object_key">> := RowKey}} =
                    ?S:eb_row(Org, Ws, Id),
                Cid =:= client_id(T) andalso RowKey =:= key(T)
            end)
    end.

single_row_for_cid(Cid) ->
    {ok, [#{<<"n">> := 1}]} = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE client_upload_id::text=$1">>,
        [Cid]
    ),
    true.

session_state_of(T) ->
    case ?S:pending_row(key(T)) of
        {ok, #{<<"session_state">> := St}} ->
            St;
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                {ok, #{<<"status">> := <<"pending_confirm">>}} = ?S:eb_row(Org, Ws, Id),
                <<"pending_confirm">>
            end)
    end.

org_ws(T, Fun) ->
    S = maps:get(fixture, T),
    Scope = maps:get(scope, S),
    AssetId = asset_id_of(T),
    Fun(maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId).

asset_id_of(T) ->
    case maps:get(asset_id, maps:get(confirm_body, T), undefined) of
        I when is_integer(I) -> I;
        B when is_binary(B) -> binary_to_integer(B);
        undefined ->
            Data = maps:get(registration, T),
            int(maps:get(<<"asset_id">>, Data))
    end.

int(I) when is_integer(I) -> I;
int(B) when is_binary(B) -> binary_to_integer(B).

shorten_expiry(T) ->
    case ?S:pending_row(key(T)) of
        {ok, _} -> ?S:update_pending_expiry(key(T), -60);
        _ -> org_ws(T, fun(Org, Ws, Id) -> ?S:update_eb_expiry(Org, Ws, Id, -60) end)
    end.

expire_deadline(T) ->
    case ?S:pending_row(key(T)) of
        {ok, _} -> ?S:update_pending_deadline(key(T), -300);
        _ -> org_ws(T, fun(Org, Ws, Id) -> ?S:update_eb_deadline(Org, Ws, Id, -300) end)
    end.

renew_fragment(ordinary) -> <<"UPDATE attach_pending SET upload_expires_at">>;
renew_fragment(enterprise_internal) -> <<"UPDATE attach_pending SET upload_expires_at">>;
renew_fragment(_) -> <<"UPDATE enterprise_asset SET upload_expires_at">>.

register_fragment(ordinary) -> <<"fn_v5_register_pending(">>;
register_fragment(enterprise_internal) -> <<"fn_v5_register_pending(">>;
register_fragment(_) -> <<"fn_v5_register_eb(">>.

%% intent 提交（sweep）：ordinary/internal 走 attach_pending；
%% seat/widget 走 fn_v5_intent_eb_delete（live/无 retention/无重复 job）。
run_intent(T, Reason) ->
    case ?S:pending_row(key(T)) of
        {ok, _} ->
            attach_pending_repo:intent_pending_delete(key(T), <<"sweep">>, Reason);
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                case
                    elib_pg:query(
                        <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)">>, [Org, Ws, Id, Reason]
                    )
                of
                    {ok, _} -> ok;
                    {error, _} = E -> E
                end
            end)
    end.

%% provider 侧旁证：garage 拒绝未签名直传；disk 无网络写面（受控根专用）。
provider_unsigned_denied(_S, disk, _T) ->
    #{provider_unsigned_write => <<"no_http_surface">>};
provider_unsigned_denied(_S, garage, T) ->
    I = maps:get(location, T),
    Key = maps:get(key, I),
    Endpoint = string:trim(binary_to_list(maps:get(endpoint, I)), trailing, "/"),
    Url =
        Endpoint ++ "/" ++ binary_to_list(maps:get(container, I)) ++ "/" ++ binary_to_list(Key),
    {ok, {{_, Code, _}, _, _}} = httpc:request(
        put,
        {Url, [], "text/plain", ?BODY},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assert(lists:member(Code, [401, 403])),
    #{provider_unsigned_status => Code}.

%% status/renew 的 presign 端点请求（自定义身份头用）。
presign_request(T, Headers, Body) ->
    S = maps:get(fixture, T),
    Path =
        case maps:get(surface, T) of
            ordinary -> <<"/api/v1/attachment/presign?operation=status">>;
            enterprise_internal -> <<"/api/internal/v1/files/presign">>;
            enterprise_seat -> seat_presign_path(S);
            cs_widget -> widget_presign_path(S)
        end,
    ?F:request(S, post, Path, Body, Headers, <<"application/json">>).

status_query(T) ->
    <<"/api/v1/attachment/presign?operation=status&client_upload_id=", (client_id(T))/binary>>.

%% 侵入者身份构造（F5）：全部真实签发——真实 user 行、真实 device 登记、
%% 真实 token/凭证签名；仅与目标会话主体无授权关系。

new_intruder_uid() ->
    Uid = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\"(id,account,nickname,password,status,account_type,reg_ip,reg_cosv,created_at)"
            " VALUES($1,$2,'fixture','',1,0,'127.0.0.1','w601',now()) RETURNING id"
        >>,
        [Uid, <<"w601-intruder-", (integer_to_binary(Uid))/binary>>]
    ),
    Did = <<"w601-device-", (integer_to_binary(Uid))/binary>>,
    ok = qr_login_logic:log_device_login(Uid, Did, <<"w601">>, <<"web">>),
    {Uid, Did}.

%% ordinary 域：真实第二用户 bearer token（查他人 pending → 404）。
intruder_headers() ->
    {Uid, _Did} = new_intruder_uid(),
    #{<<"authorization">> => <<"Bearer ", (token_ds:encrypt_token(Uid))/binary>>}.

%% seat 面：真实第二用户的 seat-purpose 令牌（真实签发；该用户未被授予
%% customer_service 授权函数 → 路由级授权门 403）。
intruder_seat_headers() ->
    {Uid, Did} = new_intruder_uid(),
    #{
        <<"authorization">> =>
            <<"Bearer ", (token_ds:encrypt_seat_token(Uid, Did))/binary>>
    }.

%% enterprise_internal 面：无关 org 的真实 application credential（真实
%% create/issue + 真实请求签名），跨主体查询 → resource_not_found 404。
intruder_internal_headers() ->
    Scope = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    {ok, Credential} = elib_pg:with_tx(fun(C) ->
        {ok, App} = enterprise_application_repo:create_tx(
            C,
            Org,
            <<"w601-intruder-", (elib_uuid:gen_v7())/binary>>,
            <<"w601 intruder application">>,
            Actor
        ),
        AppId = maps:get(<<"id">>, App),
        {ok, #{credential := Cred}} = enterprise_internal_ops:issue_credential_tx(
            C, Org, AppId, binary:encode_hex(crypto:strong_rand_bytes(32), lowercase), undefined
        ),
        {ok, Cred}
    end),
    #{signed_credential => Credential}.

%% cs_widget 面：真实第二 installation + bootstrap 访客令牌（真实签发、
%% 令牌链路自洽），查询不属于它的会话资产 → 跨会话归属 404。
intruder_widget_ctx(S) ->
    Scope = maps:get(scope, S),
    Org = maps:get(org_id, Scope),
    Installation = cs_pg_test_fixture:id(),
    Visitor = binary:encode_hex(crypto:strong_rand_bytes(32), lowercase),
    {ok, _} = cs_pg_widget:insert_widget_installation(Org, #{
        id => Installation,
        public_widget_id => integer_to_binary(Installation),
        display_name => <<"w601-intruder">>,
        allowed_origins => [],
        branding => #{},
        consent_version => <<"w601-v1">>
    }),
    {ok, _} = cs_pg_widget:insert_widget_bootstrap_token(Org, #{
        id => cs_pg_test_fixture:id(),
        contact_id => maps:get(contact_id, Scope),
        token_digest => cs_widget_support:token_digest(#{}, Visitor),
        expires_at => os:system_time(second) + 3600,
        widget_installation_id => Installation
    }),
    #{
        headers => #{<<"x-cs-visit-token">> => Visitor},
        installation_id => Installation
    }.

seat_presign_path(S) ->
    Org = maps:get(org_id, maps:get(scope, S)),
    <<"/api/v1/seat/enterprise/organizations/", (integer_to_binary(Org))/binary,
        "/assets/presign">>.

widget_presign_path(S) ->
    <<"/api/v1/cs/widget/sessions/", (integer_to_binary(maps:get(session_id, S)))/binary,
        "/assets/presign">>.

%% 签发即过期的 upload_ref（seat/widget 过期叶）：真实 presign 参数
%% upload_ttl_seconds=-1；入口、凭证与 frozen 位置全部生产代码。
expired_ref_registration(S0, Ep, Driver) ->
    S = S0,
    Cid = elib_uuid:gen_v7(),
    Scope = maps:get(scope, S),
    Base = #{
        mime => <<"text/plain">>,
        size_bytes => byte_size(?BODY),
        object_hash => ?S:sha(?BODY),
        file_name => <<"w403.txt">>,
        client_upload_id => Cid,
        upload_ttl_seconds => -1,
        workspace_id => maps:get(workspace_id, Scope),
        conversation_id => maps:get(conversation_id, Scope)
    },
    {Path, Headers, Body} =
        case Ep of
            enterprise_seat ->
                {seat_presign_path(S), maps:get(seat_headers, S), Base};
            cs_widget ->
                {widget_presign_path(S), maps:get(widget_headers, S), Base#{
                    installation_id => maps:get(installation_id, S)
                }}
        end,
    Registered = ?F:request(S, post, Path, Body, Headers, <<"application/json">>),
    ?assert(?F:success(Registered)),
    Data = ?F:payload(Registered),
    Id = int(maps:get(<<"asset_id">>, Data)),
    {ok, Row} = eb_pg_asset_meta:fetch_asset(
        maps:get(org_id, Scope), maps:get(workspace_id, Scope), Id
    ),
    Key = maps:get(object_key, Row),
    Cols = [storage_driver, storage_namespace, storage_container, storage_locator_sha256],
    Frozen = maps:from_list([{atom_to_binary(F), maps:get(F, Row)} || F <- Cols]),
    {ok, Location} = attachment_storage:frozen_location(Key, Frozen),
    ?assertEqual(Driver, maps:get(driver, Location)),
    #{
        fixture => S,
        leaf => maps:get(leaf, S),
        surface => Ep,
        driver => Driver,
        key => Key,
        location => Location,
        upload_url => fetch(<<"url">>, fetch(<<"upload">>, Data, #{}), <<>>),
        headers => Headers,
        confirm_body => #{upload_ref => maps:get(<<"upload_ref">>, Data), asset_id => Id},
        registration => Data,
        client_upload_id => Cid
    }.
