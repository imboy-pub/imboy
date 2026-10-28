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
    Result = setup_db(),
    case Result of
        {ok, _} ->
            Previous = eb_pg_test_fixture:select_asset_stub(),
            try
                Registry = eb_pg_test_fixture:install_asset_registry(),
                {asset_stub, Previous, Registry, Result}
            catch
                Class:Reason:Stack ->
                    try
                        cleanup_db(Result)
                    after
                        eb_pg_test_fixture:restore_asset_store(Previous)
                    end,
                    erlang:raise(Class, Reason, Stack)
            end;
        _ ->
            Result
    end.

setup_db() ->
    eunit_runner:eunit_setup_with_db().

cleanup_db({asset_stub, Previous, Registry, Result}) ->
    try
        cleanup_db(Result)
    after
        try
            eb_pg_test_fixture:restore_asset_registry(Registry)
        after
            eb_pg_test_fixture:restore_asset_store(Previous)
        end
    end;
cleanup_db({ok, Conn}) ->
    _ = eb_asset_object_stub:reset(),
    eunit_runner:eunit_cleanup_db(Conn);
cleanup_db(_Other) ->
    ok.

cases({asset_stub, _Previous, _Registry, Result}) ->
    cases(Result);
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
        {timeout, 60, fun bes01b_visitor_content_binding_negatives/0},
        %% W2-02：confirm 幂等重放 + asset_id 恢复（不要求持久化 upload_ref）
        {timeout, 60, fun w202_confirm_replay_idempotent_active/0},
        {timeout, 60, fun w202_confirm_by_asset_id_recovery/0},
        %% V5.5 收尾（W2-02 confirmed-content-immutable EB 叶；real-Garage）：
        %% seat / visitor_session 主体各 5 场景；缺 W202_GARAGE_* 时逐用例 skip
        %%（描述符仍列出，计数可见、不空转）。
        {timeout, 120, fun eb_garage_seat_old_capability_put_b_case/0},
        {timeout, 120, fun eb_garage_seat_overwrite_between_stat_and_confirm_case/0},
        {timeout, 120, fun eb_garage_seat_inflight_same_content_after_confirm_case/0},
        {timeout, 120, fun eb_garage_seat_hash_mismatch_no_confirm_case/0},
        {timeout, 120, fun eb_garage_seat_crash_after_pin_before_put_resume_case/0},
        {timeout, 120, fun eb_garage_widget_old_capability_put_b_case/0},
        {timeout, 120, fun eb_garage_widget_overwrite_between_stat_and_confirm_case/0},
        {timeout, 120, fun eb_garage_widget_inflight_same_content_after_confirm_case/0},
        {timeout, 120, fun eb_garage_widget_hash_mismatch_no_confirm_case/0},
        {timeout, 120, fun eb_garage_widget_crash_after_pin_before_put_resume_case/0}
    ];
cases(_Skipped) ->
    %% 裸 {skip,_} 从 instantiator 返回是非法 descriptor（eunit 报
    %% "bad test descriptor" 并致 halt(2)），列表包裹才是合法跳过形态。
    [{skip, "asset suite requires the scratch database connection"}].

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

%% ===================================================================
%% W2-02：confirm 幂等重放 + asset_id 恢复（不要求持久化 upload_ref）
%% ===================================================================

%% @doc 重放：已 active 资产的 confirm 返回与首确认相同的视图（同一
%% asset id、status=active），不再视为 conflict；跃迁/审计只在首确认一次。
w202_confirm_replay_idempotent_active() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Payload = <<"w202-replay-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        Base = #{
            workspace_id => Ws,
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload),
            key_ref => eb_asset_it_lib:key_ref(Scope),
            upload_ttl_seconds => 900
        },
        {ok, Presign} = enterprise_business_facade:request_presign(
            Org, Base#{actor_contact_id => Contact}
        ),
        Ref = maps:get(upload_ref, Presign),
        AssetId = maps:get(asset_id, Presign),
        {ok, _} = eb_asset_app:put_object(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            upload_ref => Ref,
            payload => Payload,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        ConfirmParams = #{
            workspace_id => Ws,
            upload_ref => Ref,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        },
        {ok, First} = enterprise_business_facade:confirm_asset(Org, ConfirmParams),
        ?assertEqual(AssetId, maps:get(asset_id, First)),
        ?assertEqual(active, maps:get(status, First)),
        %% 100 次重放：同一 asset 视图（幂等成功，非 conflict）
        lists:foreach(
            fun(_) ->
                {ok, Replay} = enterprise_business_facade:confirm_asset(Org, ConfirmParams),
                ?assertEqual(
                    maps:get(asset_id, First), maps:get(asset_id, Replay)
                )
            end,
            lists:seq(1, 100)
        )
    after
        _ = eb_asset_object_stub:reset()
    end.

%% @doc 恢复：响应丢失后仅凭授权 asset_id（无 upload_ref）confirm 成功；
%% 字节完整性仍以行派生 claims + 真实内容校验为准。
w202_confirm_by_asset_id_recovery() ->
    Scope = eb_asset_it_lib:new_scope(),
    try
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Payload = <<"w202-recover-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
        Base = #{
            workspace_id => Ws,
            conversation_id => Conv,
            mime => <<"text/plain">>,
            size_bytes => byte_size(Payload),
            object_hash => eb_asset_it_lib:sha256_hex(Payload),
            key_ref => eb_asset_it_lib:key_ref(Scope),
            upload_ttl_seconds => 900
        },
        {ok, Presign} = enterprise_business_facade:request_presign(
            Org, Base#{actor_contact_id => Contact}
        ),
        Ref = maps:get(upload_ref, Presign),
        AssetId = maps:get(asset_id, Presign),
        {ok, _} = eb_asset_app:put_object(Org, #{
            workspace_id => Ws,
            conversation_id => Conv,
            upload_ref => Ref,
            payload => Payload,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        %% 不带 upload_ref：仅 workspace_id + asset_id + 主体
        {ok, Recovered} = enterprise_business_facade:confirm_asset(Org, #{
            workspace_id => Ws,
            asset_id => AssetId,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        ?assertEqual(active, maps:get(status, Recovered)),
        %% 恢复后再以原 ref 重放：同一视图（同 id 幂等）
        {ok, Replay} = enterprise_business_facade:confirm_asset(Org, #{
            workspace_id => Ws,
            upload_ref => Ref,
            actor_contact_id => Contact,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }),
        ?assertEqual(maps:get(asset_id, Recovered), maps:get(asset_id, Replay))
    after
        _ = eb_asset_object_stub:reset()
    end.

-define(EB_GARAGE_TIMEOUT, 120).

%% ===================================================================
%% V5.5 real-Garage named tests（生成器：环境门 + 超时预算）
%% ===================================================================

eb_garage_seat_old_capability_put_b_case() ->
    eb_garage_named(fun eb_garage_seat_old_capability_put_b/0).

eb_garage_seat_overwrite_between_stat_and_confirm_case() ->
    eb_garage_named(fun eb_garage_seat_overwrite_between_stat_and_confirm/0).

eb_garage_seat_inflight_same_content_after_confirm_case() ->
    eb_garage_named(fun eb_garage_seat_inflight_same_content_after_confirm/0).

eb_garage_seat_hash_mismatch_no_confirm_case() ->
    eb_garage_named(fun eb_garage_seat_hash_mismatch_no_confirm/0).

eb_garage_seat_crash_after_pin_before_put_resume_case() ->
    eb_garage_named(fun eb_garage_seat_crash_after_pin_before_put_resume/0).

eb_garage_widget_old_capability_put_b_case() ->
    eb_garage_named(fun eb_garage_widget_old_capability_put_b/0).

eb_garage_widget_overwrite_between_stat_and_confirm_case() ->
    eb_garage_named(fun eb_garage_widget_overwrite_between_stat_and_confirm/0).

eb_garage_widget_inflight_same_content_after_confirm_case() ->
    eb_garage_named(fun eb_garage_widget_inflight_same_content_after_confirm/0).

eb_garage_widget_hash_mismatch_no_confirm_case() ->
    eb_garage_named(fun eb_garage_widget_hash_mismatch_no_confirm/0).

eb_garage_widget_crash_after_pin_before_put_resume_case() ->
    eb_garage_named(fun eb_garage_widget_crash_after_pin_before_put_resume/0).

%% 环境门：W202_GARAGE_* 齐备 ⇒ 带超时的真实用例；缺 ⇒ 可见 skip（不静默）。
eb_garage_named(Fun) ->
    case garage_ready() of
        true ->
            {timeout, ?EB_GARAGE_TIMEOUT, Fun};
        false ->
            {skip,
                "W202_GARAGE_* not exported; real-garage EB confirmed-byte "
                "cases not run"}
    end.

garage_ready() ->
    case
        {
            os:getenv("W202_GARAGE_ENDPOINT"),
            os:getenv("W202_GARAGE_ACCESS_KEY"),
            os:getenv("W202_GARAGE_SECRET_KEY")
        }
    of
        {EP, AK, SK} when EP =/= false, AK =/= false, SK =/= false ->
            true;
        _ ->
            false
    end.

%% ===================================================================
%% 用例体：seat / widget 同构，主体种类由 Kind 参数化
%% ===================================================================

eb_garage_seat_old_capability_put_b() ->
    eb_garage_with_scope(seat, old_capability_put_b).

eb_garage_seat_overwrite_between_stat_and_confirm() ->
    eb_garage_with_scope(seat, overwrite_between_stat_and_confirm).

eb_garage_seat_inflight_same_content_after_confirm() ->
    eb_garage_with_scope(seat, inflight_same_content_after_confirm).

eb_garage_seat_hash_mismatch_no_confirm() ->
    eb_garage_with_scope(seat, hash_mismatch_no_confirm).

eb_garage_seat_crash_after_pin_before_put_resume() ->
    eb_garage_with_scope(seat, crash_after_pin_before_put_resume).

eb_garage_widget_old_capability_put_b() ->
    eb_garage_with_scope(widget, old_capability_put_b).

eb_garage_widget_overwrite_between_stat_and_confirm() ->
    eb_garage_with_scope(widget, overwrite_between_stat_and_confirm).

eb_garage_widget_inflight_same_content_after_confirm() ->
    eb_garage_with_scope(widget, inflight_same_content_after_confirm).

eb_garage_widget_hash_mismatch_no_confirm() ->
    eb_garage_with_scope(widget, hash_mismatch_no_confirm).

eb_garage_widget_crash_after_pin_before_put_resume() ->
    eb_garage_with_scope(widget, crash_after_pin_before_put_resume).

%% 每测试自建 fixture：合成租户 + garage 注入；after 中先恢复 app env 再清 DB。
eb_garage_with_scope(Kind, Scenario) ->
    Scope = eb_asset_it_lib:new_scope(),
    Saved = garage_begin(),
    try
        eb_garage_scenario(Kind, Scenario, Scope)
    after
        garage_end(Saved),
        _ = eb_pg_test_fixture:cleanup(Scope)
    end.

%% -------------------------------------------------------------------
%% 场景 1：old_capability_put_b —— confirm 后旧 capability PUT 不同字节 B
%% 被三道门拒绝，确认对象字节仍是 hash A。
%% -------------------------------------------------------------------
eb_garage_scenario(Kind, old_capability_put_b, Scope) ->
    PayloadA = eb_garage_payload(<<"oldcap-A">>),
    Sess = eb_garage_open_confirmed_session(Scope, Kind, PayloadA),
    #{
        org := Org,
        ws := Ws,
        asset_id := AssetId,
        ref := Ref,
        actor_params := AP,
        scope := Scope0,
        bucket := Bucket,
        key := Key,
        payload := PayloadA,
        sha := ShaA
    } = Sess,
    PayloadB = eb_garage_payload(<<"oldcap-B">>),
    ShaB = eb_asset_it_lib:sha256_hex(PayloadB),
    %% ① 产品门：旧凭证（claims hash=A）携带 B 字节 —— PUT 前复核拒绝，
    %%    任何存储写/收据 pin 都不发生。
    {error, {hash_mismatch, ShaA, ShaB}} =
        eb_asset_app:put_object(
            Org,
            AP#{
                workspace_id => Ws,
                upload_ref => Ref,
                payload => PayloadB,
                key_ref => eb_asset_it_lib:key_ref(Scope0)
            }
        ),
    %% ②③ 收据门（EB 链 meta 入口，迁移 00000196 fn_v5_eb_content_gate）：
    %%    B 收据写前复核拒绝（23510 → content_mismatch）；
    %%    active 态新预准入直接拒绝（55000 → session_not_renewable）。
    Identity = eb_garage_frozen_identity(Org, Ws, AssetId),
    DescB = eb_garage_put_descriptor(Sess, PayloadB),
    {error, content_mismatch} =
        eb_pg_asset_meta:recheck_registered_put(Org, Ws, DescB, Identity),
    {error, session_not_renewable} =
        eb_pg_asset_meta:reserve_registered_put(Org, Ws, DescB),
    %% ④ DB：confirm 幂等重放同一 asset id、状态仍 active；行 hash 未被改写；
    %%    attachment 表无行。
    {ok, Replay} = eb_garage_confirm(Sess),
    ?assertEqual(AssetId, maps:get(asset_id, Replay)),
    ?assertEqual(active, maps:get(status, Replay)),
    ?assertEqual(ShaA, eb_asset_it_lib:asset_hash(Org, Ws, AssetId)),
    ?assertEqual(0, eb_garage_attachment_count(Key)),
    %% ⑤ 字节裁决（elib_oss:object_sha256 GET 全量）：确认对象字节仍是 A。
    {ok, ShaNow, _} = elib_oss:object_sha256(Bucket, Key),
    ?assertEqual(ShaA, ShaNow);
%% -------------------------------------------------------------------
%% 场景 2：overwrite_between_stat_and_confirm —— stat 与 confirm 之间对象
%% 字节被覆盖 ⇒ confirm 读全量字节识破并拒绝；状态不推进、无行。
%% -------------------------------------------------------------------
eb_garage_scenario(Kind, overwrite_between_stat_and_confirm, Scope) ->
    PayloadA = eb_garage_payload(<<"ovw-A">>),
    Sess = eb_garage_open_session(Scope, Kind, PayloadA),
    #{
        org := Org,
        ws := Ws,
        asset_id := AssetId,
        bucket := Bucket,
        key := Key,
        sha := ShaA
    } = Sess,
    PayloadB = eb_garage_payload(<<"ovw-B">>),
    ShaB = eb_asset_it_lib:sha256_hex(PayloadB),
    %% stat：对象在场（HEAD 只证元数据——02 §4「HEAD 本身不充分」的靶子）。
    {ok, #{size := _}} = elib_oss:head_object(Bucket, Key),
    %% stat 与 confirm 之间对象字节被带外覆盖。
    ok = elib_oss:put_object(Bucket, Key, PayloadB, <<"text/plain">>),
    %% confirm 真实读流识破：拒绝确认（EB 面 = integrity_check_failed）。
    {error, {integrity_check_failed, ShaA, ShaB}} = eb_garage_confirm(Sess),
    %% DB：状态不推进、无 confirm 行；attachment 表无行。
    ?assertEqual(pending_confirm, eb_asset_it_lib:asset_status(Org, Ws, AssetId)),
    ?assertEqual(0, eb_garage_attachment_count(Key)),
    %% 字节裁决：拒绝后对象就是被换上的 B——confirm 的判定与真实字节一致。
    {ok, ShaNow, _} = elib_oss:object_sha256(Bucket, Key),
    ?assertEqual(ShaB, ShaNow);
%% -------------------------------------------------------------------
%% 场景 3：inflight_same_content_after_confirm —— confirm 后同内容在途
%% 重写不改变已确认字节，confirm 幂等回读同一 asset id。
%% -------------------------------------------------------------------
eb_garage_scenario(Kind, inflight_same_content_after_confirm, Scope) ->
    PayloadA = eb_garage_payload(<<"infl-A">>),
    Sess = eb_garage_open_confirmed_session(Scope, Kind, PayloadA),
    #{
        org := Org,
        ws := Ws,
        asset_id := AssetId,
        bucket := Bucket,
        key := Key,
        payload := PayloadA,
        sha := ShaA
    } = Sess,
    %% 确认后同字节在途重写（幂等内容，不构成新版本）。
    ok = elib_oss:put_object(Bucket, Key, PayloadA, <<"text/plain">>),
    {ok, Replay} = eb_garage_confirm(Sess),
    ?assertEqual(AssetId, maps:get(asset_id, Replay)),
    ?assertEqual(active, maps:get(status, Replay)),
    %% DB：行 hash 与状态不变；attachment 表无行。
    ?assertEqual(ShaA, eb_asset_it_lib:asset_hash(Org, Ws, AssetId)),
    ?assertEqual(active, eb_asset_it_lib:asset_status(Org, Ws, AssetId)),
    ?assertEqual(0, eb_garage_attachment_count(Key)),
    %% 字节裁决：仍是 A。
    {ok, ShaNow, _} = elib_oss:object_sha256(Bucket, Key),
    ?assertEqual(ShaA, ShaNow);
%% -------------------------------------------------------------------
%% 场景 4：hash_mismatch_no_confirm —— 声明(A)与内容(B) hash 不匹配 ⇒
%% PUT 准入即拒（不落存储、不提交 pin），confirm 稳定失败、无行。
%% （EB 链语义差异：mismatch 在准入门 fail-closed，见文件头差异 §3。）
%% -------------------------------------------------------------------
eb_garage_scenario(Kind, hash_mismatch_no_confirm, Scope) ->
    PayloadA = eb_garage_payload(<<"hm-A">>),
    Sess = eb_garage_open_session_unput(Scope, Kind, PayloadA),
    #{
        org := Org,
        ws := Ws,
        asset_id := AssetId,
        ref := Ref,
        actor_params := AP,
        scope := Scope0,
        bucket := Bucket,
        key := Key,
        sha := ShaA
    } = Sess,
    PayloadB = eb_garage_payload(<<"hm-B">>),
    ShaB = eb_asset_it_lib:sha256_hex(PayloadB),
    %% ① 声明与内容不符：put_verify 服务端重算 hash —— 准入即拒。
    {error, {hash_mismatch, ShaA, ShaB}} =
        eb_asset_app:put_object(
            Org,
            AP#{
                workspace_id => Ws,
                upload_ref => Ref,
                payload => PayloadB,
                key_ref => eb_asset_it_lib:key_ref(Scope0)
            }
        ),
    %% ② DB：无收据 pin（拒绝发生在 pin 之前；epgsql 的 NULL 即 null 原子）。
    ?assertEqual(null, eb_garage_pin(Org, Ws, AssetId)),
    %% ③ confirm：对象从未准入 ⇒ 字节校验读不到对象，稳定失败、不推进。
    {error, {object_unreadable, {object_store, not_found}}} =
        eb_garage_confirm(Sess),
    ?assertEqual(pending_confirm, eb_asset_it_lib:asset_status(Org, Ws, AssetId)),
    ?assertEqual(0, eb_garage_attachment_count(Key)),
    %% ④ 字节裁决：Garage 里从未有该对象。
    {error, not_found} = elib_oss:object_sha256(Bucket, Key);
%% -------------------------------------------------------------------
%% 场景 5：crash_after_pin_before_put_resume —— pin 已 COMMIT 但对象 PUT
%% 未发生 ⇒ confirm 稳定失败且无行推进；同 pin 幂等重放 + 真实 PUT 后
%% confirm 成功且 asset id 幂等。
%% -------------------------------------------------------------------
eb_garage_scenario(Kind, crash_after_pin_before_put_resume, Scope) ->
    PayloadA = eb_garage_payload(<<"crash-A">>),
    Sess = eb_garage_open_session_unput(Scope, Kind, PayloadA),
    #{
        org := Org,
        ws := Ws,
        asset_id := AssetId,
        bucket := Bucket,
        key := Key,
        payload := PayloadA,
        sha := ShaA,
        scope := Scope0,
        actor_params := AP
    } = Sess,
    %% 崩溃窗口：pin 已提交（fn_v5_pin_eb_content 预准入收据落库）、PUT 未发生。
    DescA = eb_garage_put_descriptor(Sess, PayloadA),
    {ok, _} = eb_pg_asset_meta:reserve_registered_put(Org, Ws, DescA),
    PinBin = eb_garage_pin(Org, Ws, AssetId),
    ?assert(is_binary(PinBin)),
    ?assert(nomatch =/= binary:match(PinBin, ShaA)),
    %% confirm：对象缺失 ⇒ 稳定失败（EB 面 = object_unreadable{not_found}）；
    %% 状态不推进、attachment 无行。
    {error, {object_unreadable, {object_store, not_found}}} =
        eb_garage_confirm(Sess),
    ?assertEqual(pending_confirm, eb_asset_it_lib:asset_status(Org, Ws, AssetId)),
    ?assertEqual(0, eb_garage_attachment_count(Key)),
    %% 恢复：同收据 pin 幂等重放（fn_v5_pin_eb_content 同收据放行）+ 真 PUT
    %%（fn_v5_recheck_eb_content 写前复核 + attachment_storage_garage 落盘）。
    {ok, _} =
        eb_asset_app:put_object(
            Org,
            AP#{
                workspace_id => Ws,
                upload_ref => maps:get(ref, Sess),
                payload => PayloadA,
                key_ref => eb_asset_it_lib:key_ref(Scope0)
            }
        ),
    {ok, Confirmed} = eb_garage_confirm(Sess),
    ?assertEqual(AssetId, maps:get(asset_id, Confirmed)),
    ?assertEqual(active, maps:get(status, Confirmed)),
    %% attachment/asset id 幂等：重放 confirm 返回同一主行 id。
    {ok, Replay} = eb_garage_confirm(Sess),
    ?assertEqual(AssetId, maps:get(asset_id, Replay)),
    %% 字节裁决：确认对象字节 = A。
    {ok, ShaNow, _} = elib_oss:object_sha256(Bucket, Key),
    ?assertEqual(ShaA, ShaNow).

%% ===================================================================
%% Garage 注入 helper（套件本地；不改 eb_pg_test_fixture，口径同
%% attachment_lifecycle_v5_confirm_pg_tests:garage_apply/0）
%% ===================================================================

%% 保存三键旧值 → 置 garage 对象存储 + 注入 imboy.garage / garage 存储根。
%% 返回值交 garage_end/1 原样恢复（undefined ⇒ unset）。
garage_begin() ->
    Saved = [
        {eb_asset_object_store, application:get_env(imboy, eb_asset_object_store)},
        {attachment_storage_registry, application:get_env(imboy, attachment_storage_registry)},
        {garage, application:get_env(imboy, garage)}
    ],
    ok = application:set_env(imboy, eb_asset_object_store, garage),
    try
        garage_apply_registry(),
        Saved
    catch
        Class:Reason:Stack ->
            garage_restore(Saved),
            erlang:raise(Class, Reason, Stack)
    end.

garage_end(Saved) ->
    garage_restore(Saved).

garage_restore(Saved) ->
    [
        case Value of
            undefined -> application:unset_env(imboy, Key);
            {ok, V} -> application:set_env(imboy, Key, V)
        end
     || {Key, Value} <- Saved
    ],
    ok.

%% W202_GARAGE_* → imboy.garage + 版本化 registry 追加 garage 根，
%% enterprise 上传 namespace 指向该根（EB registration_location 只查此键）。
%% endpoint/container 与 imboy.garage 同源，满足 frozen_location 的
%% garage 一致性复核（attachment_storage:snapshot/4）。
garage_apply_registry() ->
    EP = garage_env("W202_GARAGE_ENDPOINT"),
    Bucket = garage_env("W202_GARAGE_BUCKET"),
    ok = application:set_env(imboy, garage, #{
        endpoint => EP,
        bucket => Bucket,
        access_key => garage_env("W202_GARAGE_ACCESS_KEY"),
        secret_key => garage_env("W202_GARAGE_SECRET_KEY")
    }),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Namespace =
        <<"eb-garage-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots =>
            Roots#{
                Namespace => #{
                    driver => <<"garage">>,
                    namespace => Namespace,
                    endpoint => EP,
                    container => Bucket
                }
            },
        upload_namespaces => Uploads#{<<"enterprise">> => Namespace}
    }).

garage_env(Name) ->
    case os:getenv(Name) of
        false -> erlang:error({no_go, required_resource_missing, Name});
        "" -> erlang:error({no_go, required_resource_missing, Name});
        Value -> list_to_binary(Value)
    end.

%% ===================================================================
%% 会话 / 收据 fixture（走产品入口；seat=member、widget=contact/visitor_session）
%% ===================================================================

%% 主体种类 → 请求参数 + fn_v5_register_eb 登记快照 subject（与
%% eb_asset_app:registration_subject/3 逐字段同形；widget 的 session_id
%% 必须在 presign 参数携带，否则登记快照编码 undefined 会炸）。
eb_garage_actor_params(seat, Scope) ->
    {Org, _Ws} = eb_asset_it_lib:tenant(Scope),
    Actor = maps:get(actor_user_id, Scope),
    Params = #{actor_user_id => Actor},
    Subject = #{
        <<"kind">> => <<"seat">>,
        <<"principal">> => integer_to_binary(Actor),
        <<"org_id">> => integer_to_binary(Org)
    },
    {Params, Subject};
eb_garage_actor_params(widget, Scope) ->
    {Org, _Ws} = eb_asset_it_lib:tenant(Scope),
    Contact = maps:get(contact_id, Scope),
    VsId =
        <<"vs-w55-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>,
    Params = #{actor_contact_id => Contact, visitor_session_id => VsId},
    Subject = #{
        <<"kind">> => <<"visitor_session">>,
        <<"principal">> => integer_to_binary(Contact),
        <<"org_id">> => integer_to_binary(Org),
        <<"session_id">> => VsId
    },
    {Params, Subject}.

%% presign（带 client_upload_id ⇒ fn_v5_register_eb 登记；未 PUT）。
%% 返回会话上下文 map，供各场景续作。
eb_garage_open_session_unput(Scope, Kind, Payload) ->
    {Org, Ws} = eb_asset_it_lib:tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    {AP, Subject} = eb_garage_actor_params(Kind, Scope),
    Cid = elib_uuid:gen_v7(),
    {ok, Presign} =
        enterprise_business_facade:request_presign(
            Org,
            AP#{
                workspace_id => Ws,
                conversation_id => Conv,
                mime => <<"text/plain">>,
                size_bytes => byte_size(Payload),
                object_hash => eb_asset_it_lib:sha256_hex(Payload),
                client_upload_id => Cid,
                upload_ttl_seconds => 900,
                key_ref => eb_asset_it_lib:key_ref(Scope)
            }
        ),
    %% V5 会话已登记（fn_v5_register_eb 返回 registered 视图）。
    ?assertEqual(<<"registered">>, maps:get(session_state, Presign)),
    Ref = maps:get(upload_ref, Presign),
    AssetId = maps:get(asset_id, Presign),
    Bucket = elib_oss:get_bucket(<<"private">>),
    Key = eb_asset_it_lib:object_key(Org, Ws, AssetId),
    #{
        kind => Kind,
        scope => Scope,
        org => Org,
        ws => Ws,
        conv => Conv,
        actor_params => AP,
        subject => Subject,
        cid => Cid,
        asset_id => AssetId,
        ref => Ref,
        bucket => Bucket,
        key => Key,
        payload => Payload,
        sha => eb_asset_it_lib:sha256_hex(Payload)
    }.

%% presign + 真 Garage PUT（A 字节落盘）；状态停在 pending_confirm。
eb_garage_open_session(Scope, Kind, Payload) ->
    Sess = eb_garage_open_session_unput(Scope, Kind, Payload),
    #{
        org := Org,
        ws := Ws,
        ref := Ref,
        actor_params := AP,
        scope := Scope0,
        bucket := Bucket,
        key := Key,
        sha := ShaA
    } = Sess,
    {ok, _} =
        eb_asset_app:put_object(
            Org,
            AP#{
                workspace_id => Ws,
                upload_ref => Ref,
                payload => Payload,
                key_ref => eb_asset_it_lib:key_ref(Scope0)
            }
        ),
    %% 真 Garage 往返自证：字节确实落盘且 hash 一致。
    {ok, ShaA, _} = elib_oss:object_sha256(Bucket, Key),
    Sess.

%% presign + PUT + confirm（首确认成功，status=active）。
eb_garage_open_confirmed_session(Scope, Kind, Payload) ->
    Sess = eb_garage_open_session(Scope, Kind, Payload),
    {ok, Confirmed} = eb_garage_confirm(Sess),
    ?assertEqual(active, maps:get(status, Confirmed)),
    ?assertEqual(maps:get(asset_id, Sess), maps:get(asset_id, Confirmed)),
    Sess.

%% confirm（upload_ref 重鉴权入口；claims 自凭证、授权重走 scope 门）。
eb_garage_confirm(Sess) ->
    #{org := Org, ws := Ws, actor_params := AP, ref := Ref, scope := Scope} =
        Sess,
    enterprise_business_facade:confirm_asset(
        Org,
        AP#{
            workspace_id => Ws,
            upload_ref => Ref,
            key_ref => eb_asset_it_lib:key_ref(Scope)
        }
    ).

%% fn_v5_pin/recheck_eb_content 的 PUT 描述符（与 eb_asset_app:asset_put_private
%% 组装的 Descriptor 同形；registered_put_receipt 自 payload 现算收据）。
eb_garage_put_descriptor(Sess, Payload) ->
    #{org := _Org, subject := Subject, asset_id := AssetId, conv := Conv} =
        Sess,
    Now = erlang:system_time(second),
    #{
        id => AssetId,
        object_hash => eb_asset_it_lib:sha256_hex(Payload),
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        payload => Payload,
        upload_subject => Subject,
        signature_context => #{
            issued_at => calendar:system_time_to_universal_time(Now, second),
            expires_at =>
                calendar:system_time_to_universal_time(Now + 600, second)
        },
        conversation_id => Conv
    }.

%% 行冻结位置 → fn_v5_recheck_eb_content 的 identity JSON（键值与迁移
%% 00000196 fn_v5_eb_content_gate 的 v_identity 同形）。
eb_garage_frozen_identity(Org, Ws, AssetId) ->
    {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, AssetId),
    #{
        driver => maps:get(storage_driver, Row),
        namespace => maps:get(storage_namespace, Row),
        container => maps:get(storage_container, Row),
        key => maps:get(object_key, Row),
        locator_digest => maps:get(storage_locator_sha256, Row)
    }.

%% ===================================================================
%% 只读探针
%% ===================================================================

%% attachment 表行数（EB 链恒 0：confirmed 行在 enterprise_asset）。
eb_garage_attachment_count(Key) ->
    eb_pg_test_fixture:scalar(
        <<"SELECT COUNT(*)::int AS n FROM attachment WHERE path=$1">>, [Key]
    ).

%% enterprise_asset 的 upload_content 收据（NULL → null 原子；已 pin → jsonb 文本）。
eb_garage_pin(Org, Ws, AssetId) ->
    eb_pg_test_fixture:scalar(
        <<
            "SELECT upload_content FROM enterprise_asset"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, AssetId]
    ).

%% 场景载荷（marker + 合成 id，跨轮唯一）。
eb_garage_payload(Marker) ->
    <<Marker/binary, "-", (integer_to_binary(eb_pg_test_fixture:id()))/binary>>.
