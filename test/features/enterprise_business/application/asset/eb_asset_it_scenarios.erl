%%% @doc EB-07 企业附件闭环的**验收场景**：一个 Acceptance 一个函数（A01..A06）。
%%%
%%% 场景内部把判定拆成若干 `check/3` 子断言：通过打印 `[CHECK <子ID>]`，失败打印
%%% `[ASSERT-FAIL <子ID>] ... :: <原因>`，并记账。同一套场景因此有两种消费方式：
%%%
%%%   * `scripts/enterprise_business_asset_it.sh`（经 `eb_asset_it_runner`）逐条断言；
%%%   * eunit 套件 `eb_asset_tests`（一个 Acceptance 一个 test）。
%%%
%%% 为什么失败令牌不是 `[FAIL]`：`control/verify_evidence.py` 要求 `green.log`
%%% 「不含 `[FAIL]` 行 / `FAIL=[1-9]`」。子断言失败若逐字写成 `[FAIL]`，一次通过的
%%% GREEN 里就会混进两种语义不同的字符串，让「GREEN 是否含失败」无法机械判定。
%%% 故失败令牌固定为 `[ASSERT-FAIL ...]`，脚本级红只在**整门**失败时打印。
%%%
%%% 边界（硬约束）：只用合成租户与**本地替身**对象存储；不声明真实 Garage 验收通过。
-module(eb_asset_it_scenarios).

-export([scenarios/0, run/1, reset/0, summary/0, check/3]).

-define(L, eb_asset_it_lib).
-define(FAIL_TAG, "[ASSERT-FAIL").
-define(LEDGER, {?MODULE, ledger}).

%% ===================================================================
%% 账本 / 场景入口
%% ===================================================================

%% @doc 场景 ID 列表（与 `control/required-acceptance.tsv` 的 EB-07 行一一对应）。
-spec scenarios() -> [binary()].
scenarios() ->
    [
        <<"EB-07-A01">>,
        <<"EB-07-A02">>,
        <<"EB-07-A03">>,
        <<"EB-07-A04">>,
        <<"EB-07-A05">>,
        <<"EB-07-A06">>
    ].

-spec reset() -> ok.
reset() ->
    put(?LEDGER, {0, 0, []}),
    ok.

%% @doc `{Passed, Failed, Failures}`。
-spec summary() -> {non_neg_integer(), non_neg_integer(), list()}.
summary() ->
    case get(?LEDGER) of
        undefined -> {0, 0, []};
        {P, F, L} -> {P, F, lists:reverse(L)}
    end.

%% @doc 子断言：`Fun` 返回 `ok`/`true` 视为通过；`{error, _}`/`false` 视为失败。
-spec check(binary(), iodata(), fun(() -> term())) -> ok.
check(SubId, Desc, Fun) ->
    Outcome =
        try Fun() of
            ok -> ok;
            true -> ok;
            {error, _} = E -> E;
            Other -> {error, {unexpected_outcome, Other}}
        catch
            Class:Err:Stack -> {error, {Class, Err, lists:sublist(Stack, 3)}}
        end,
    %% 打印口径：描述与原因都是 **UTF-8 二进制**，故必须用 `~ts`（`~s` 会把 UTF-8
    %% 字节当 latin1 码点再编码一次，得到双重编码的乱码）；格式串里的中文字面量是
    %% 码点列表，用普通 `~s`/字面量即可。
    case Outcome of
        ok ->
            io:format("  [CHECK ~s] ~ts~n", [SubId, ?L:fmt(Desc)]),
            record(ok);
        {error, Reason} ->
            io:format(
                "  ~s ~s] ~ts :: ~ts~n",
                [?FAIL_TAG, SubId, ?L:fmt(Desc), ?L:reason(Reason)]
            ),
            record({SubId, ?L:fmt(Desc), ?L:reason(Reason)})
    end.

record(ok) ->
    {P, F, L} = summary(),
    put(?LEDGER, {P + 1, F, L}),
    ok;
record(Item) ->
    {P, F, L} = summary(),
    put(?LEDGER, {P, F + 1, [Item | L]}),
    ok.

%% @doc 跑一个场景；返回 `{ok, Passed}` 或 `{error, {Failed, Passed, Failures}}`。
-spec run(binary()) -> {ok, non_neg_integer()} | {error, term()}.
run(Id) ->
    reset(),
    Scope = ?L:new_scope(),
    try
        dispatch(Id, Scope)
    catch
        Class:Err:Stack ->
            check(Id, <<"场景执行未抛异常（兜底捕获）"/utf8>>, fun() ->
                {error, {scenario_abort, Class, Err, lists:sublist(Stack, 3)}}
            end)
    after
        _ = eb_pg_test_fixture:cleanup(Scope),
        _ = eb_asset_object_stub:reset()
    end,
    case summary() of
        {P, 0, _} -> {ok, P};
        {P, F, Failures} -> {error, {F, P, Failures}}
    end.

dispatch(<<"EB-07-A01">>, Scope) -> a01(Scope);
dispatch(<<"EB-07-A02">>, Scope) -> a02(Scope);
dispatch(<<"EB-07-A03">>, Scope) -> a03(Scope);
dispatch(<<"EB-07-A04">>, Scope) -> a04(Scope);
dispatch(<<"EB-07-A05">>, Scope) -> a05(Scope);
dispatch(<<"EB-07-A06">>, Scope) -> a06(Scope);
dispatch(Other, _Scope) -> {error, {unknown_scenario, Other}}.

%% ===================================================================
%% A01：A 上传后 Org 持有；content 响应不含 storage URL/endpoint/object key
%% ===================================================================

a01(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Payload = <<"EB07-A01-synthetic-payload-bytes">>,
    Hash = ?L:sha256_hex(Payload),
    Opts = #{
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => Hash
    },

    check(<<"EB-07-A01.1">>, <<"presign 走公共 facade 且返回不透明 upload_ref"/utf8>>, fun() ->
        case ?L:facade_presign(Scope, Actor, Opts) of
            {ok, Resp} ->
                case
                    {
                        is_binary(maps:get(upload_ref, Resp, undefined)),
                        is_integer(maps:get(asset_id, Resp, undefined))
                    }
                of
                    {true, true} -> ok;
                    _ -> {error, {presign_shape, maps:keys(Resp)}}
                end;
            Other ->
                {error, {presign_failed, Other}}
        end
    end),

    {ok, Presign} = ?L:facade_presign(Scope, Actor, Opts),
    AssetId = maps:get(asset_id, Presign),
    UploadRef = maps:get(upload_ref, Presign),

    check(<<"EB-07-A01.2">>, <<"presign 响应不含 storage URL/endpoint/object key"/utf8>>, fun() ->
        ?L:no_storage_reference(Presign, Org, Ws, AssetId)
    end),

    check(<<"EB-07-A01.3">>, <<"客户端按 presigned PUT 写入后元数据为 pending_confirm"/utf8>>, fun() ->
        case
            eb_asset_app:put_object(Org, #{
                workspace_id => Ws,
                actor_user_id => Actor,
                conversation_id => Conv,
                upload_ref => UploadRef,
                payload => Payload,
                key_ref => ?L:key_ref(Scope)
            })
        of
            {ok, Put} ->
                case maps:get(status, Put, undefined) of
                    pending_confirm -> ok;
                    Other -> {error, {unexpected_status, Other}}
                end;
            Other ->
                {error, {put_object_failed, Other}}
        end
    end),

    check(<<"EB-07-A01.4">>, <<"confirm 后 metadata 为 active 且归本 Org/Workspace 持有"/utf8>>, fun() ->
        case ?L:facade_confirm(Scope, Actor, UploadRef, Conv) of
            {ok, Confirmed} ->
                assert_active_and_org_owned(Scope, AssetId, Confirmed);
            Other ->
                {error, {confirm_failed, Other}}
        end
    end),

    check(
        <<"EB-07-A01.5">>, <<"content 端点返回原始字节且不含 storage URL/endpoint/object key"/utf8>>, fun() ->
            case ?L:facade_content(Scope, Actor, AssetId) of
                {ok, Stream} ->
                    case maps:get(body, Stream, undefined) of
                        Payload -> ?L:no_storage_reference(Stream, Org, Ws, AssetId);
                        Other -> {error, {body_mismatch, byte_size(Other)}}
                    end;
                Other ->
                    {error, {content_failed, Other}}
            end
        end
    ),

    check(<<"EB-07-A01.6">>, <<"content 返回的 object_hash 与上传字节的 SHA-256 一致"/utf8>>, fun() ->
        case ?L:facade_content(Scope, Actor, AssetId) of
            {ok, Stream} ->
                case maps:get(object_hash, Stream, undefined) of
                    Hash -> ok;
                    Other -> {error, {hash_mismatch, Other, Hash}}
                end;
            Other ->
                {error, {content_failed, Other}}
        end
    end),

    check(
        <<"EB-07-A01.7">>, <<"未登记为个人 private attachment（personal attachment 表零新增行）"/utf8>>, fun() ->
            ?L:personal_attachment_rows(Scope)
        end
    ).

assert_active_and_org_owned(Scope, AssetId, Confirmed) ->
    {Org, Ws} = ?L:tenant(Scope),
    case maps:get(status, Confirmed, undefined) of
        active ->
            case ?L:asset_row(Org, Ws, AssetId) of
                {ok, Row} ->
                    case {maps:get(organization_id, Row), maps:get(workspace_id, Row)} of
                        {Org, Ws} -> ok;
                        Other -> {error, {org_ownership_mismatch, Other}}
                    end;
                Other ->
                    {error, {asset_row_missing, Other}}
            end;
        Other ->
            {error, {unexpected_status, Other}}
    end.

%% ===================================================================
%% A02：suspended A 的旧 JWT 新下载请求立即拒绝；PUT 后 suspend 的 confirm 失败
%% ===================================================================

a02(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    Actor = maps:get(actor_user_id, Scope),

    {ok, AssetId, _Ref1} = ?L:upload_and_confirm(Scope, Actor, <<"EB07-A02-first">>),
    check(<<"EB-07-A02.1">>, <<"suspend 前该 actor 可经鉴权代理取流"/utf8>>, fun() ->
        case ?L:facade_content(Scope, Actor, AssetId) of
            {ok, _} -> ok;
            Other -> {error, {pre_suspend_content_failed, Other}}
        end
    end),

    Payload2 = <<"EB07-A02-second-after-suspend">>,
    {ok, Presign2} = ?L:facade_presign(Scope, Actor, #{
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload2),
        object_hash => ?L:sha256_hex(Payload2)
    }),
    AssetId2 = maps:get(asset_id, Presign2),
    UploadRef2 = maps:get(upload_ref, Presign2),
    {ok, _} = eb_asset_app:put_object(Org, #{
        workspace_id => Ws,
        actor_user_id => Actor,
        conversation_id => Conv,
        upload_ref => UploadRef2,
        payload => Payload2,
        key_ref => ?L:key_ref(Scope)
    }),
    ok = ?L:suspend_member(Org, Actor),

    check(<<"EB-07-A02.2">>, <<"suspend 后旧 JWT 的新下载请求立即被拒（逐请求重取事实）"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, {forbidden, {member_status, suspended}}},
            fun() -> ?L:facade_content(Scope, Actor, AssetId) end
        )
    end),

    check(<<"EB-07-A02.3">>, <<"suspend 后 confirm 失败（fail-closed）"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, {forbidden, {member_status, suspended}}},
            fun() -> ?L:facade_confirm(Scope, Actor, UploadRef2, Conv) end
        )
    end),

    check(
        <<"EB-07-A02.4">>, <<"失败的 confirm 未把 status 推进为 active（仍 pending_confirm）"/utf8>>, fun() ->
            case ?L:facade_confirm(Scope, Actor, UploadRef2, Conv) of
                {ok, _} ->
                    {error, confirm_should_have_failed};
                {error, _} ->
                    case ?L:asset_status(Org, Ws, AssetId2) of
                        pending_confirm -> ok;
                        Other -> {error, {status_advanced_by_failed_confirm, Other}}
                    end
            end
        end
    ),

    check(<<"EB-07-A02.5">>, <<"suspend 后 presign 也被拒（不再有新的上传能力）"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, {forbidden, {member_status, suspended}}},
            fun() ->
                ?L:facade_presign(Scope, Actor, #{
                    conversation_id => Conv,
                    mime => <<"text/plain">>,
                    size_bytes => 8,
                    object_hash => ?L:sha256_hex(<<"12345678">>)
                })
            end
        )
    end),

    check(<<"EB-07-A02.6">>, <<"suspend 不追回 suspend 前已落地的对象（测试不宣称收回已传输数据）"/utf8>>, fun() ->
        %% 口径：本卡只证明「suspend 前已落地的对象与已完成的下载不被追回」，
        %% **不**声明「收回 suspend 前已传输或已下载的数据」—— 那既做不到，
        %% 也不在契约里（plan §EB-07 原文的显式限定）。
        case {?L:asset_status(Org, Ws, AssetId), ?L:object_present(Org, Ws, AssetId)} of
            {active, true} -> ok;
            Other -> {error, {pre_suspend_asset_disturbed, Other}}
        end
    end).

%% ===================================================================
%% A03：B 接任后经代理读取同 asset/hash
%% ===================================================================

a03(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Successor = maps:get(peer_user_id, Scope),
    ServiceIdentity = maps:get(service_identity_id, Scope),

    {ok, AssetId, _Ref} = ?L:upload_and_confirm(Scope, Actor, <<"EB07-A03-handover">>),
    Hash = ?L:asset_hash(Org, Ws, AssetId),

    check(<<"EB-07-A03.1">>, <<"接任者 B 在接任前无权读取（经办 ACL 逐请求判定）"/utf8>>, fun() ->
        ok = ?L:add_member(Org, Successor),
        ?L:expect_error_match(
            {error, {forbidden, not_assignee}},
            fun() -> ?L:facade_content(Scope, Successor, AssetId) end
        )
    end),

    check(<<"EB-07-A03.2">>, <<"B 取得同一 identity 的 active 经办关系（成功接任）"/utf8>>, fun() ->
        ?L:assignment_for(Org, Ws, Successor, ServiceIdentity, <<"customer_service">>)
    end),

    check(<<"EB-07-A03.3">>, <<"会话经办交接给 B 后，B 经代理读到同 asset 同 hash"/utf8>>, fun() ->
        Store = ?L:store(),
        case Store:update_conversation_assignee(Org, Ws, Conv, ServiceIdentity) of
            {ok, _} ->
                case ?L:facade_content(Scope, Successor, AssetId) of
                    {ok, Stream} ->
                        case maps:get(object_hash, Stream, undefined) of
                            Hash -> ok;
                            Other -> {error, {successor_hash_mismatch, Other, Hash}}
                        end;
                    Other ->
                        {error, {successor_content_failed, Other}}
                end;
            Other ->
                {error, {handover_failed, Other}}
        end
    end),

    check(<<"EB-07-A03.4">>, <<"交接后原经办 A 不再是经办（同一 ACL 反向生效）"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, {forbidden, not_assignee}},
            fun() -> ?L:facade_content(Scope, Actor, AssetId) end
        )
    end).

%% ===================================================================
%% A04：跨 Org / Workspace / asset id / object key 猜测失败
%% ===================================================================

a04(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    OtherWs = maps:get(other_workspace_id, Scope),
    Actor = maps:get(actor_user_id, Scope),

    {ok, AssetId, _Ref} = ?L:upload_and_confirm(Scope, Actor, <<"EB07-A04-scope">>),
    Guessed = eb_pg_test_fixture:id(),

    check(<<"EB-07-A04.1">>, <<"跨 Org 读取（同 asset id / 同 actor）被拒"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, not_found},
            fun() -> ?L:facade_content(Scope, Actor, AssetId, OtherOrg, OtherWs) end
        )
    end),

    check(<<"EB-07-A04.2">>, <<"同 Org 跨 Workspace 读取被拒"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, not_found},
            fun() -> ?L:facade_content(Scope, Actor, AssetId, Org, OtherWs) end
        )
    end),

    check(<<"EB-07-A04.3">>, <<"跨 Org 取元数据被拒（跨租户同 id 不可见）"/utf8>>, fun() ->
        case ?L:asset_status(OtherOrg, OtherWs, AssetId) of
            {error, not_found} -> ok;
            Other -> {error, {cross_org_metadata_visible, Other}}
        end
    end),

    check(<<"EB-07-A04.4">>, <<"asset id 猜测（随机 TSID）经鉴权代理取流失败"/utf8>>, fun() ->
        ?L:expect_error_match(
            {error, not_found},
            fun() -> ?L:facade_content(Scope, Actor, Guessed) end
        )
    end),

    check(<<"EB-07-A04.5">>, <<"跨 Org 删除对象失败且 victim 的对象仍在（两道路径）"/utf8>>, fun() ->
        case ?L:facade_content(Scope, Actor, AssetId) of
            {ok, _} ->
                Prefix = ?L:key_prefix(Org, Ws),
                ForeignKey = ?L:object_key(OtherOrg, OtherWs, AssetId),
                case eb_asset_object_stub:get(ForeignKey, Prefix) of
                    {error, out_of_scope} ->
                        cross_org_delete_probe(Scope, Actor, AssetId, OtherOrg, OtherWs);
                    Other ->
                        {error, {stub_prefix_guard_missing, Other}}
                end;
            Other ->
                {error, {baseline_read_failed, Other}}
        end
    end),

    check(<<"EB-07-A04.6">>, <<"object key 不可猜：对外可见值里不出现对象 key"/utf8>>, fun() ->
        {ok, Stream} = ?L:facade_content(Scope, Actor, AssetId),
        {ok, Row} = ?L:asset_row(Org, Ws, AssetId),
        Key = maps:get(object_key, Row),
        Binary = iolist_to_binary(io_lib:format("~p", [Stream])),
        case binary:match(Binary, Key) of
            nomatch -> ?L:no_storage_reference(Stream, Org, Ws, AssetId);
            _ -> {error, {object_key_leaked, Key}}
        end
    end).

cross_org_delete_probe(Scope, Actor, AssetId, OtherOrg, OtherWs) ->
    case eb_asset_store:delete_private(OtherOrg, OtherWs, AssetId) of
        {error, not_found} ->
            case ?L:facade_content(Scope, Actor, AssetId) of
                {ok, _} -> ok;
                Other -> {error, {post_delete_read_failed, Other}}
            end;
        Other ->
            {error, {cross_org_delete_succeeded, Other}}
    end.

%% ===================================================================
%% A05：cleanup 只删本脚本创建的超时 pending 对象
%% ===================================================================

a05(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    Actor = maps:get(actor_user_id, Scope),
    OtherOrg = maps:get(other_org_id, Scope),
    OtherWs = maps:get(other_workspace_id, Scope),

    Expired = ?L:seed_pending(Scope, Actor, <<"EB07-A05-expired">>, stale),
    Fresh = ?L:seed_pending(Scope, Actor, <<"EB07-A05-fresh">>, fresh),
    {ok, Active, _} = ?L:upload_and_confirm(Scope, Actor, <<"EB07-A05-active">>),
    Foreign = ?L:seed_foreign_expired_pending(Scope, <<"EB07-A05-foreign-bytes">>),

    %% 无元数据的孤儿对象：属于同一租户前缀，但不是本脚本的资产 → 不得被清理
    OrphanKey = <<(?L:key_prefix(Org, Ws))/binary, "orphan-not-registered">>,
    ok = eb_asset_object_stub:put(OrphanKey, <<"EB07-A05-orphan">>, #{}),

    %% 故意**超量供给**候选集：把不该删的也塞进去，判定必须只删合格项
    Candidates = [Expired, Fresh, Active, Foreign, eb_pg_test_fixture:id()],

    check(<<"EB-07-A05.1">>, <<"cleanup_pending 只删「超时 pending」的对象（超量候选集）"/utf8>>, fun() ->
        case
            eb_asset_app:cleanup_pending(Org, #{
                workspace_id => Ws,
                asset_ids => Candidates,
                ttl_seconds => 3600
            })
        of
            {ok, Result} ->
                Deleted = maps:get(deleted, Result, []),
                Skipped = maps:get(skipped, Result, []),
                case {lists:member(Expired, Deleted), lists:keyfind(Expired, 1, Skipped)} of
                    {true, false} -> cleanup_effects(Scope, Expired, Fresh, Active, Foreign);
                    {false, _} -> {error, {expired_pending_not_cleaned, Result}};
                    {true, Found} -> {error, {expired_asset_also_skipped, Found}}
                end;
            Other ->
                {error, {cleanup_failed, Other}}
        end
    end),

    check(<<"EB-07-A05.2">>, <<"未超时 pending / 已确认 / 跨租户 / 无元数据孤儿对象全部幸存"/utf8>>, fun() ->
        Survivors = [
            {fresh_pending_object, ?L:object_present(Org, Ws, Fresh)},
            {active_object, ?L:object_present(Org, Ws, Active)},
            {foreign_pending_object, ?L:object_present(OtherOrg, OtherWs, Foreign)},
            {orphan_object, ?L:object_present_key(Org, Ws, OrphanKey)},
            {active_metadata, ?L:asset_status(Org, Ws, Active) =:= active}
        ],
        case [N || {N, V} <- Survivors, V =/= true] of
            [] -> ok;
            Bad -> {error, {survivor_missing, Bad}}
        end
    end),

    check(<<"EB-07-A05.3">>, <<"跨租户候选被拒（不因超量供给而越界删除）"/utf8>>, fun() ->
        {ok, Result} = eb_asset_app:cleanup_pending(Org, #{
            workspace_id => Ws,
            asset_ids => [Foreign],
            ttl_seconds => 3600
        }),
        case {maps:get(deleted, Result, []), maps:get(skipped, Result, [])} of
            {[], [{Foreign, _Reason} | _]} -> ok;
            Other -> {error, {cross_tenant_cleanup_not_skipped, Other}}
        end
    end).

cleanup_effects(Scope, Expired, Fresh, Active, Foreign) ->
    {Org, Ws} = ?L:tenant(Scope),
    Checks = [
        {expired_object_removed, ?L:object_present(Org, Ws, Expired) =:= false},
        {expired_metadata_deleted, ?L:asset_status(Org, Ws, Expired) =:= deleted},
        {fresh_object_alive, ?L:object_present(Org, Ws, Fresh) =:= true},
        {active_object_alive, ?L:object_present(Org, Ws, Active) =:= true},
        {foreign_object_alive,
            ?L:object_present(
                maps:get(other_org_id, Scope), maps:get(other_workspace_id, Scope), Foreign
            ) =:=
                true}
    ],
    case [N || {N, false} <- Checks] of
        [] -> ok;
        Failed -> {error, {cleanup_effects_wrong, Failed}}
    end.

%% ===================================================================
%% A06：confirmed asset 的 retain_until/hold 不短于所属 message；
%%      ACK / 隐藏 / offboarding 均不触发对象删除
%% ===================================================================

a06(Scope) ->
    {Org, Ws} = ?L:tenant(Scope),
    Conv = maps:get(conversation_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Contact = maps:get(contact_id, Scope),
    MsgId = ?L:seed_message(Scope, Contact),

    %% 客户端**故意**请求一个远短于消息的 retain_until：必须被提升到消息那一档
    RequestedRetain = ?L:now_sec() + 10,
    Payload = <<"EB07-A06-retention-bound">>,
    {ok, Presign} = ?L:facade_presign(Scope, Actor, #{
        conversation_id => Conv,
        message_id => MsgId,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => ?L:sha256_hex(Payload),
        retain_until => RequestedRetain
    }),
    AssetId = maps:get(asset_id, Presign),
    Ref = maps:get(upload_ref, Presign),
    {ok, _} = eb_asset_app:put_object(Org, #{
        workspace_id => Ws,
        actor_user_id => Actor,
        conversation_id => Conv,
        upload_ref => Ref,
        payload => Payload,
        key_ref => ?L:key_ref(Scope)
    }),
    {ok, Confirmed} = ?L:facade_confirm(Scope, Actor, Ref, Conv),

    check(
        <<"EB-07-A06.1">>,
        <<"confirmed asset 的 retain_until 不短于所属 message（缩短请求被提升）"/utf8>>,
        fun() ->
            AssetRetain = maps:get(retain_until, Confirmed, undefined),
            MsgRetain = ?L:message_retain_until(Org, Ws, MsgId),
            case {is_integer(AssetRetain), is_integer(MsgRetain), AssetRetain >= MsgRetain} of
                {true, true, true} ->
                    ok;
                _ ->
                    {error,
                        {retain_until_shorter_than_message, AssetRetain, MsgRetain,
                            RequestedRetain}}
            end
        end
    ),

    check(<<"EB-07-A06.2">>, <<"缩短 asset.retain_until 被 DB 守卫拒绝（23514）"/utf8>>, fun() ->
        Shorten = max(0, ?L:message_retain_until(Org, Ws, MsgId) - 86400),
        ?L:expect_db_error(
            <<"23514">>,
            <<"trg_enterprise_asset_retention_guard">>,
            <<
                "UPDATE enterprise_asset SET retain_until = to_timestamp($4)"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, AssetId, Shorten]
        )
    end),

    HoldId = ?L:seed_hold(Scope, MsgId),

    check(<<"EB-07-A06.3">>, <<"active hold 覆盖所属 message 时附件行不得物理删除（23514）"/utf8>>, fun() ->
        ?L:expect_db_error(
            <<"23514">>,
            <<"trg_enterprise_asset_purge_guard">>,
            <<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
            [Org, Ws, AssetId]
        )
    end),

    check(<<"EB-07-A06.4">>, <<"hold 释放后仍未到期 ⇒ 仍不得物理删除（23514）"/utf8>>, fun() ->
        case release_holds(?L:store(), Org, Ws, HoldId, Actor) of
            ok ->
                ?L:expect_db_error(
                    <<"23514">>,
                    <<"trg_enterprise_asset_purge_guard">>,
                    <<"DELETE FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
                    [Org, Ws, AssetId]
                );
            {error, _} = Err ->
                Err
        end
    end),

    check(<<"EB-07-A06.5">>, <<"投递 ACK 不触发对象删除，canonical 行亦不被改写"/utf8>>, fun() ->
        Store = ?L:store(),
        Ack = #{
            id => eb_pg_test_fixture:id(),
            message_id => MsgId,
            %% 形态受 ck_emd_recipient_ref 约束（00000116:205）：`contact:<id>|identity:<id>`
            recipient_ref =>
                <<"contact:", (integer_to_binary(maps:get(contact_id, Scope)))/binary>>,
            device_id => <<"eb07-device">>,
            acked_at => ?L:now_sec()
        },
        case Store:ack_delivery(Org, Ws, Ack) of
            {ok, _} -> asset_and_message_intact(Org, Ws, AssetId, MsgId, ack);
            Other -> {error, {ack_failed, Other}}
        end
    end),

    check(<<"EB-07-A06.6">>, <<"隐藏消息（visibility=hidden tombstone）不触发对象删除"/utf8>>, fun() ->
        ok = eb_pg_test_fixture:exec(
            <<
                "UPDATE enterprise_message SET visibility='hidden'"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Ws, MsgId]
        ),
        asset_and_message_intact(Org, Ws, AssetId, MsgId, hide)
    end),

    check(<<"EB-07-A06.7">>, <<"offboarding case/item 建档不触发对象删除"/utf8>>, fun() ->
        Store = ?L:store(),
        CaseId = eb_pg_test_fixture:id(),
        Leaver = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        case
            Store:insert_offboarding_case(Org, Ws, #{
                id => CaseId,
                leaver_user_id => Leaver,
                successor_user_id => Successor,
                created_by_user_id => maps:get(owner_user_id, Scope),
                reason => <<"eb07-offboarding-decoy">>
            })
        of
            {ok, _} ->
                _ = Store:insert_offboarding_item(Org, Ws, #{
                    id => eb_pg_test_fixture:id(),
                    case_id => CaseId,
                    business_identity_id => maps:get(sales_identity_id, Scope),
                    from_user_id => Leaver,
                    to_user_id => Successor,
                    idempotency_key => <<"eb07-idem-1">>
                }),
                asset_and_message_intact(Org, Ws, AssetId, MsgId, offboarding);
            Other ->
                {error, {offboarding_case_failed, Other}}
        end
    end),

    check(<<"EB-07-A06.8">>, <<"已确认附件仍可经鉴权代理读取（ACK/隐藏/离职均不撤权）"/utf8>>, fun() ->
        case ?L:facade_content(Scope, Actor, AssetId) of
            {ok, Stream} -> ?L:no_storage_reference(Stream, Org, Ws, AssetId);
            Other -> {error, {content_failed_after_decoy_ops, Other}}
        end
    end).

%% @doc 释放 hold 并确认已无 active hold（A06.4 的前置）。
release_holds(Store, Org, Ws, HoldId, Actor) ->
    case Store:release_hold(Org, Ws, HoldId, Actor) of
        ok ->
            case Store:list_active_holds(Org, Ws) of
                {ok, []} -> ok;
                {ok, Still} -> {error, {hold_not_released, Still}};
                Other -> {error, {list_active_holds_failed, Other}}
            end;
        Other ->
            {error, {release_hold_failed, Other}}
    end.

asset_and_message_intact(Org, Ws, AssetId, MsgId, Label) ->
    case
        {
            ?L:asset_status(Org, Ws, AssetId),
            ?L:object_present(Org, Ws, AssetId),
            ?L:message_row_count(Org, Ws, MsgId)
        }
    of
        {active, true, 1} -> ok;
        Other -> {error, {asset_or_message_disturbed, Label, Other}}
    end.
