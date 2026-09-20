%%% @doc EB-07 企业附件闭环的 eunit 套件（plan §EB-07 的门 `make eunit t=eb_asset_tests`）。
%%%
%%% 结构：
%%%   * A01..A06 各一个 test，直接复用集成场景
%%%     （`test/features/enterprise_business/application/asset/eb_asset_it_scenarios.erl`），
%%%     失败时把「哪个子断言红了」以 `[ASSERT-FAIL ...]` 明细打出来；
%%%   * 另有若干**窄口径单元断言**：单位边界（retain_until 的 ms/秒 不对称）、
%%%     上传凭证的不透明性与防篡改、mime/size/hash 校验 fail-closed、
%%%     cleanup 的跳过分类 —— 它们把上面场景依赖的前提单独钉死。
%%%
%%% 边界（硬约束）：合成租户 + **本地替身**对象存储。对象存储侧结论口径固定为
%%% `adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN`。
-module(eb_asset_tests).

-include_lib("eunit/include/eunit.hrl").

%% 场景超时：A05/A06 有多条真实 PG 往返 + 加密凭证派生，默认 5s 不够。
-define(SCENARIO_TIMEOUT, 180).

asset_test_() ->
    {setup, fun setup/0, fun cleanup_db/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({ok, Conn}) ->
    _ = eb_asset_object_stub:reset(),
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({ok, _Conn}) ->
    [
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a01/0},
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a02/0},
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a03/0},
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a04/0},
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a05/0},
        {timeout, ?SCENARIO_TIMEOUT, fun acceptance_a06/0},
        {timeout, 60, fun unit_retain_until_seconds_boundary/0},
        {timeout, 60, fun unit_upload_ref_is_opaque_and_bound_to_scope/0},
        {timeout, 60, fun unit_upload_ref_expiry_is_enforced/0},
        {timeout, 60, fun unit_mime_size_hash_validation_is_fail_closed/0},
        {timeout, 60, fun unit_cleanup_classifies_skips/0},
        {timeout, 60, fun csb02s_d6_visitor_presign_put_confirm_positive/0},
        {timeout, 60, fun csb02s_d6_visitor_scope_negatives/0},
        %% BE-S01b：访客 content proxy 的 contact 分支 + 绑定作用域门
        {timeout, 60, fun bes01b_visitor_content_linked_positive/0},
        {timeout, 60, fun bes01b_visitor_content_binding_negatives/0}
    ];
cases(_Skipped) ->
    {skip, "asset suite requires the scratch database connection"}.

%% ===================================================================
%% A01..A06（复用集成场景；逐条把子断言明细打在失败信息里）
%% ===================================================================

acceptance_a01() -> run_acceptance(<<"EB-07-A01">>).
acceptance_a02() -> run_acceptance(<<"EB-07-A02">>).
acceptance_a03() -> run_acceptance(<<"EB-07-A03">>).
acceptance_a04() -> run_acceptance(<<"EB-07-A04">>).
acceptance_a05() -> run_acceptance(<<"EB-07-A05">>).
acceptance_a06() -> run_acceptance(<<"EB-07-A06">>).

run_acceptance(Id) ->
    case eb_asset_it_scenarios:run(Id) of
        {ok, Passed} ->
            ?assert(Passed > 0),
            ok;
        {error, {Failed, Passed, Failures}} ->
            ?assertEqual(
                {Id, 0, []},
                {Id, Failed, Failures}
            ),
            %% 子断言明细的兜底输出（上面断言必然失败，这里是给日志的可读来源）
            ?debugFmt("~s: ~p/~p 子断言失败: ~p", [Id, Failed, Passed + Failed, Failures]),
            ok
    end.

%% ===================================================================
%% CSB-02S D6：访客（visit token contact 主体）附件作用域分支
%% ===================================================================

%% @doc 访客 presign → PUT → confirm 全流水：actor = 令牌 contact
%%（组织域主体），企业面 member 校验不参与也不放宽。
csb02s_d6_visitor_presign_put_confirm_positive() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Payload = <<"d6-visitor-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        Params = #{
            workspace_id => Ws,
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload),
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope),
            upload_ttl_seconds => 900
        },
        {ok, Presign} = enterprise_business_facade:request_presign(Org, Params),
        Ref = maps:get(upload_ref, Presign),
        AssetId = maps:get(asset_id, Presign),
        %% PUT（访客主体；凭证 uploader 逐字比对 contact）。
        {ok, _} = eb_asset_app:put_object(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            upload_ref => Ref,
            payload => Payload,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        %% confirm（重新鉴权仍走访客分支）。
        {ok, Confirmed} = enterprise_business_facade:confirm_asset(Org, #{
            workspace_id => Ws,
            upload_ref => Ref,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        ?assertEqual(active, maps:get(status, Confirmed)),
        ?assert(eb_asset_it_lib:object_present(Org, Ws, AssetId))
    after
        _ = eb_asset_object_stub:reset()
    end.

%% @doc 访客作用域负例：contact 与会话不符（403 面）；member/contact 双主体
%% 同时出现（作用域形状不成立）。
csb02s_d6_visitor_scope_negatives() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Payload = <<"d6-neg-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        Base = #{
            workspace_id => Ws,
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload),
            key_ref => eb_asset_it_lib:key_ref(Scope),
            upload_ttl_seconds => 900
        },
        %% ① 冒名 contact：presign 直接拒（403 面，非 500）。
        {error, {forbidden, contact_scope_mismatch}} =
            enterprise_business_facade:request_presign(Org, Base#{actor_contact_id => 1}),
        %% ② 双主体同时出现 = 形状不成立（422）。
        ?assertMatch(
            {error, {invalid_argument, {presign_scope, _}}},
            enterprise_business_facade:request_presign(
                Org,
                Base#{actor_contact_id => maps:get(contact_id, Scope), actor_user_id => 42}
            )
        ),
        %% ③ 无任何主体 = 形状不成立（422，既有行为不回归）。
        ?assertMatch(
            {error, {invalid_argument, {presign_scope, _}}},
            enterprise_business_facade:request_presign(Org, Base)
        )
    after
        _ = eb_asset_object_stub:reset()
    end.

%% ===================================================================
%% BE-S01b：访客 content proxy（contact 分支 + conversation 绑定作用域门）
%% ===================================================================

%% @doc 正向：linked（绑消息）资产经「conversation 绑定门 + contact 会话
%% 归属门」读出——视图是白名单投影（字节 + 完整性摘要），零 URL/object key。
bes01b_visitor_content_linked_positive() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        MsgId = eb_asset_it_lib:seed_message(Scope, Contact),
        Payload = <<"s01b-linked-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        AssetId = bes01b_visitor_upload(Scope, Conv, Contact, MsgId, Payload),
        {ok, View} =
            enterprise_business_facade:content_stream(Org, #{
                workspace_id => Ws,
                asset_id => AssetId,
                %% 绑定作用域（生产由 CS 会话服务端派生）：必须 == 资产行。
                conversation_id => Conv,
                actor_contact_id => Contact
            }),
        ?assertEqual(Payload, maps:get(body, View)),
        ?assertEqual(<<"text/plain">>, maps:get(mime, View)),
        ?assertEqual(MsgId, maps:get(message_id, View)),
        ?assertEqual(active, maps:get(status, View)),
        %% 红线：视图零存储侧引用（key / URL 不在白名单投影里）。
        ?assertNot(maps:is_key(object_key, View)),
        ?assertEqual(
            eb_asset_it_lib:sha256_hex(Payload), maps:get(object_hash, View)
        )
    after
        _ = eb_asset_object_stub:reset()
    end.

%% @doc 负例（PG oracle）：
%%   ① confirmed_unbound（message_id NULL）不出访客内容面（not_found）；
%%   ② 声明 conversation 与资产行不符（跨会话）⇒ not_found（不枚举）；
%%   ③ 既有成员路径不传 conversation_id ⇒ 绑定门不生效（行为不回归）。
bes01b_visitor_content_binding_negatives() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Payload = <<"s01b-neg-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        UnboundId = bes01b_visitor_upload(Scope, Conv, Contact, undefined, Payload),
        MsgId = eb_asset_it_lib:seed_message(Scope, Contact),
        LinkedId = bes01b_visitor_upload(Scope, Conv, Contact, MsgId, Payload),
        %% ① unbound：访客 404（attachment-state-machine：linked 才出访客面）。
        {error, not_found} =
            enterprise_business_facade:content_stream(Org, #{
                workspace_id => Ws,
                asset_id => UnboundId,
                conversation_id => Conv,
                actor_contact_id => Contact
            }),
        %% ② 跨会话声明：逐字不等 ⇒ not_found（与不存在同答案，不枚举）。
        {error, not_found} =
            enterprise_business_facade:content_stream(Org, #{
                workspace_id => Ws,
                asset_id => LinkedId,
                conversation_id => Conv + 1,
                actor_contact_id => Contact
            }),
        %% ③ 成员路径（不传绑定键）：绑定门不生效——linked 资产照常读出
        %%（成员门 = active + 会话经办，既有 A05 e2e 已覆盖其授权面）。
        {ok, _} =
            enterprise_business_facade:content_stream(Org, #{
                workspace_id => Ws,
                asset_id => UnboundId,
                actor_user_id => maps:get(actor_user_id, Scope)
            })
    after
        _ = eb_asset_object_stub:reset()
    end.

%% 访客全流水上传（presign → PUT → confirm）；MsgId = undefined 时为 unbound。
bes01b_visitor_upload(Scope, Conv, Contact, MsgId, Payload) ->
    {Org, Ws} = eb_asset_it_lib:tenant(Scope),
    Base = #{
        workspace_id => Ws,
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => eb_asset_it_lib:sha256_hex(Payload),
        actor_contact_id => Contact,
        key_ref => eb_asset_it_lib:key_ref(Scope),
        upload_ttl_seconds => 900
    },
    Params =
        case MsgId of
            undefined -> Base;
            _ -> Base#{message_id => MsgId}
        end,
    {ok, Presign} = enterprise_business_facade:request_presign(Org, Params),
    Ref = maps:get(upload_ref, Presign),
    {ok, _} = eb_asset_app:put_object(Org, #{
        workspace_id => Ws,
        conversation_id => Conv,
        upload_ref => Ref,
        payload => Payload,
        actor_contact_id => Contact,
        key_ref => eb_asset_it_lib:key_ref(Scope)
    }),
    {ok, Confirmed} =
        enterprise_business_facade:confirm_asset(Org, #{
            workspace_id => Ws,
            upload_ref => Ref,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
    ?assertEqual(active, maps:get(status, Confirmed)),
    maps:get(asset_id, Confirmed).

%% ===================================================================
%% 窄口径单元断言
%% ===================================================================

%% @doc `retain_until` 的**单位不对称**必须被显式钉住：
%% 元数据写入参数是**毫秒**（`eb_pg_asset_meta` 的 `to_timestamp($n/1000)`），
%% 而 `fetch_asset` / `fetch_message` 回读是**秒**（`eb_pg_store_sql:to_unix/1`）。
%% 用例层的公共边界统一用**秒**（与消息侧一致），写入时按毫秒传。
unit_retain_until_seconds_boundary() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Id = eb_pg_test_fixture:id(),
        TargetSec = eb_asset_it_lib:now_sec() + 3600,
        {ok, _} = eb_asset_store:insert_asset(Org, Ws, #{
            id => Id,
            object_hash => eb_asset_it_lib:sha256_hex(<<"unit-boundary">>),
            mime => <<"text/plain">>,
            size_bytes => 12,
            retain_until => TargetSec * 1000
        }),
        {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
        %% 回读是秒：与写入的秒值一致（±2s 容差仅用于跨秒边界）
        ReadBack = maps:get(retain_until, Row),
        ?assert(is_integer(ReadBack)),
        ?assert(abs(ReadBack - TargetSec) =< 2),
        ?assert(ReadBack < TargetSec * 1000)
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% @doc 上传凭证的不透明性 + 作用域绑定 + 防篡改。
unit_upload_ref_is_opaque_and_bound_to_scope() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        KeyRef = eb_asset_it_lib:key_ref(Scope),
        Payload = <<"unit-upload-ref">>,
        {ok, Presign} = eb_asset_it_lib:facade_presign(Scope, Actor, #{
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload)
        }),
        Ref = maps:get(upload_ref, Presign),
        ?assert(is_binary(Ref)),
        %% 不透明：既不是 URL，也不含对象 key
        ?assertEqual(nomatch, binary:match(Ref, <<"://">>)),
        AssetId = maps:get(asset_id, Presign),
        ?assertEqual(
            nomatch,
            binary:match(Ref, eb_asset_it_lib:object_key(Org, Ws, AssetId))
        ),
        %% 篡改一个字节 → 必须打不开（fail-closed，不得静默放行）
        Tampered = tamper(Ref),
        ?assertNotEqual(Ref, Tampered),
        ?assertMatch(
            {error, _},
            eb_asset_app:put_object(Org, #{
                workspace_id => Ws,
                actor_user_id => Actor,
                conversation_id => Conv,
                upload_ref => Tampered,
                payload => Payload,
                key_ref => KeyRef
            })
        ),
        %% 换租户重放 → 必须失败（AAD 绑定 Org/Workspace/会话）
        ?assertMatch(
            {error, _},
            eb_asset_app:put_object(maps:get(other_org_id, Scope), #{
                workspace_id => maps:get(other_workspace_id, Scope),
                actor_user_id => Actor,
                conversation_id => Conv,
                upload_ref => Ref,
                payload => Payload,
                key_ref => KeyRef
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% @doc 凭证过期必须被拒（注入式时钟：直接构造一个已过期的凭证不足以复现，
%% 故用 `upload_ttl_seconds => -1` 让签发即过期）。
unit_upload_ref_expiry_is_enforced() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Payload = <<"unit-expired-ref">>,
        {ok, Presign} = eb_asset_it_lib:facade_presign(Scope, Actor, #{
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload),
            upload_ttl_seconds => -1
        }),
        ?assertMatch(
            {error, expired_upload_ref},
            eb_asset_app:put_object(Org, #{
                workspace_id => Ws,
                actor_user_id => Actor,
                conversation_id => Conv,
                upload_ref => maps:get(upload_ref, Presign),
                payload => Payload,
                key_ref => eb_asset_it_lib:key_ref(Scope)
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% @doc mime / size / hash 三条输入校验都必须 fail-closed。
unit_mime_size_hash_validation_is_fail_closed() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        Actor = maps:get(actor_user_id, Scope),
        Conv = maps:get(conversation_id, Scope),
        Base = #{conversation_id => Conv, object_hash => eb_asset_it_lib:sha256_hex(<<"x">>)},
        %% 不在白名单的 mime
        ?assertMatch(
            {error, {invalid_mime, _}},
            eb_asset_it_lib:facade_presign(Scope, Actor, Base#{
                mime => <<"application/x-evil">>, size_bytes => 1
            })
        ),
        %% size 为 0 / 负数
        ?assertMatch(
            {error, {invalid_size_bytes, _}},
            eb_asset_it_lib:facade_presign(Scope, Actor, Base#{
                mime => <<"text/plain">>, size_bytes => 0
            })
        ),
        %% 超过上限
        ?assertMatch(
            {error, {invalid_size_bytes, _}},
            eb_asset_it_lib:facade_presign(Scope, Actor, Base#{
                mime => <<"text/plain">>, size_bytes => 512 * 1024 * 1024
            })
        ),
        %% 哈希形态非法（不是 64 位小写 hex）
        ?assertMatch(
            {error, {invalid_object_hash, _}},
            eb_asset_it_lib:facade_presign(Scope, Actor, #{
                conversation_id => Conv,
                mime => <<"text/plain">>,
                size_bytes => 4,
                object_hash => <<"NOT-A-SHA256">>
            })
        ),
        %% 缺哈希
        ?assertMatch(
            {error, {invalid_object_hash, _}},
            eb_asset_it_lib:facade_presign(Scope, Actor, #{
                conversation_id => Conv, mime => <<"text/plain">>, size_bytes => 4
            })
        )
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% @doc cleanup 的跳过分类：未超时 / 已确认 / 取不到 各归各的原因（不靠数字总数）。
unit_cleanup_classifies_skips() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Fresh = eb_asset_it_lib:seed_pending(Scope, Actor, <<"unit-cleanup-fresh">>, fresh),
        {ok, Active, _} = eb_asset_it_lib:upload_and_confirm(
            Scope, Actor, <<"unit-cleanup-active">>
        ),
        Unknown = eb_pg_test_fixture:id(),
        {ok, Result} = eb_asset_app:cleanup_pending(Org, #{
            workspace_id => Ws,
            asset_ids => [Fresh, Active, Unknown],
            ttl_seconds => 3600
        }),
        ?assertEqual([], maps:get(deleted, Result)),
        Skipped = maps:get(skipped, Result),
        ?assertEqual(not_expired, element(2, lists:keyfind(Fresh, 1, Skipped))),
        ?assertEqual(not_pending, element(2, lists:keyfind(Active, 1, Skipped))),
        ?assertEqual(not_found, element(2, lists:keyfind(Unknown, 1, Skipped)))
    after
        eb_pg_test_fixture:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

%% 翻转**中部**一个 base64 字符，得到「同长度但内容不同」的伪造凭证。
%%
%% 为什么不是末位：token 末位常是 `=` 填充，而 base64 解码器会忽略末字符的
%% 非有效位 —— 首版测试翻末位时被判据「放行」过，即那条断言**不 load-bearing**。
%% 中部字符一定落在有效位里，翻转必然改变解码后的字节。
tamper(Bin) when byte_size(Bin) > 4 ->
    Pos = byte_size(Bin) div 2,
    <<Head:Pos/binary, Char, Tail/binary>> = Bin,
    Flip =
        case Char of
            $A -> $B;
            _ -> $A
        end,
    <<Head/binary, Flip, Tail/binary>>.
