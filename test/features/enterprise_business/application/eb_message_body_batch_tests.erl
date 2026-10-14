%%% @doc CSB-02R：批量摘要读面（`eb_message_app:fetch_message_bodies/2`）的
%%% 语义矩阵测试。零 DB：store 经 `Params.store` 注入鸭子型 fake；解密经
%%% meck `eb_managed_crypto`（真密钥链已在 CS 侧 PG 套件覆盖）。冻结合同：
%%%   * keyring 不可用 → 整批 undefined（不报错）；
%%%   * hidden / 空正文 / purge / 跨 (ws,conv) 元组 → undefined；
%%%   * 单条解密失败 → 该条 undefined + 结构化 warning（F-R5，零 PII）；
%%%   * items 形状非法 → invalid_argument；store 故障原样上抛。
-module(eb_message_body_batch_tests).

-include_lib("eunit/include/eunit.hrl").

%% logger handler 回调（with_batch_warning_capture 以本模块挂真 handler，
%% logger 从其管理进程回调——不导出则 add_handler 报 function_not_exported）。
-export([init/1, log/2]).

-define(ORG, 911000000000001).
-define(FAKE_STORE, eb_fake_preview_store).

%% -- 套件 -------------------------------------------------------------------

%% 生成器（双下划线 + 显式列表）：返回的 {timeout, fun} 描述符由 eunit 逐项
%% 执行。此前缺列表括号时函数体只返回最后一个描述符、其余矩阵项从未执行
%% （eunit 报 2 项空转通过）——2026-10-10 终选重绑扫描发现并修复。
fetch_message_bodies_contract_test_() ->
    [
        {timeout, 30, fun returns_bodies_for_requested_items/0},
        {timeout, 30, fun unavailable_keyring_degrades_all_to_undefined/0},
        {timeout, 30, fun hidden_row_undefined/0},
        {timeout, 30, fun cross_scope_tuple_undefined/0},
        {timeout, 30, fun purged_id_undefined/0},
        {timeout, 30, fun empty_body_undefined/0},
        {timeout, 30, fun decrypt_error_degrades_with_warning/0},
        {timeout, 30, fun decrypt_error_class_raise_degrades/0},
        {timeout, 30, fun invalid_items_rejected/0},
        {timeout, 30, fun store_error_propagates/0},
        {timeout, 30, fun facade_shape_guard/0},
        {timeout, 30, fun batch_sql_shape_gate_test/0}
    ].

%% -- 夹具 --------------------------------------------------------------------

row(Id, Ws, Conv) ->
    #{
        id => Id,
        organization_id => ?ORG,
        workspace_id => Ws,
        conversation_id => Conv,
        visibility => visible,
        body_cipher => <<"cipher-", (integer_to_binary(Id))/binary>>,
        key_version => 1,
        aad_hash => <<"aad">>
    }.

with_crypto_meck(Expect, F) ->
    meck:new(eb_managed_crypto, [no_link, non_strict]),
    meck:expect(eb_managed_crypto, open_message_body, Expect),
    try
        F()
    after
        catch meck:unload(eb_managed_crypto)
    end,
    ok.

call(Items) ->
    call(Items, #{}).

call(Items, Extra) ->
    eb_message_app:fetch_message_bodies(
        ?ORG,
        maps:merge(
            #{
                items => Items,
                store => ?FAKE_STORE,
                %% 显式 key_ref 走 passthrough（resolve_key_ref 对 map 原样返回），
                %% 与 VM 的 imboy env 无关，测试确定性成立。
                key_ref => #{active_version => 1, keys => #{1 => binary:copy(<<"k">>, 32)}}
            },
            Extra
        )
    ).

%% -- 用例 --------------------------------------------------------------------

returns_bodies_for_requested_items() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101), row(502, 11, 102)]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<"坐席可见明文"/utf8>>} end,
        fun() ->
            {ok, Bodies} = call([{11, 101, 501}, {11, 102, 502}]),
            ?assertEqual(
                #{501 => <<"坐席可见明文"/utf8>>, 502 => <<"坐席可见明文"/utf8>>}, Bodies
            )
        end
    ).

unavailable_keyring_degrades_all_to_undefined() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101)]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<"不应被解出"/utf8>>} end,
        fun() ->
            %% 不可用判定的确定性入口：**非 map** key_ref 走 env 解析
            %% （bare VM 无 imboy env → {error, keyring_unavailable} → undefined）。
            %% map 形 key_ref 是 F6 冻结的显式注入 passthrough（原样采信），
            %% 不在批量面二次校验。
            {ok, Bodies} = call([{11, 101, 501}], #{key_ref => not_assembled}),
            ?assertEqual(#{501 => undefined}, Bodies),
            ?assertEqual(0, meck:num_calls(eb_managed_crypto, open_message_body, 8))
        end
    ).

hidden_row_undefined() ->
    ?FAKE_STORE:init(),
    Hidden = (row(501, 11, 101))#{visibility => hidden},
    ?FAKE_STORE:set_rows([Hidden]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<"撤回内容不透出"/utf8>>} end,
        fun() ->
            {ok, Bodies} = call([{11, 101, 501}]),
            ?assertEqual(#{501 => undefined}, Bodies),
            ?assertEqual(0, meck:num_calls(eb_managed_crypto, open_message_body, 8))
        end
    ).

cross_scope_tuple_undefined() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101)]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<"不越 scope"/utf8>>} end,
        fun() ->
            %% 行存在但请求元组的 ws / conv 与行归属不符 → 「无行返回」。
            {ok, B1} = call([{99, 101, 501}]),
            ?assertEqual(#{501 => undefined}, B1),
            {ok, B2} = call([{11, 999, 501}]),
            ?assertEqual(#{501 => undefined}, B2)
        end
    ).

purged_id_undefined() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<"已消失"/utf8>>} end,
        fun() ->
            {ok, Bodies} = call([{11, 101, 404}]),
            ?assertEqual(#{404 => undefined}, Bodies)
        end
    ).

empty_body_undefined() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101)]),
    with_crypto_meck(
        %% 附件-only：解密成功但空正文 → undefined（不产出空 preview）。
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {ok, <<>>} end,
        fun() ->
            {ok, Bodies} = call([{11, 101, 501}]),
            ?assertEqual(#{501 => undefined}, Bodies)
        end
    ).

%% 解密失败（error 返回与 D5 的 error-class raise 两形态）→ 该条 undefined +
%% 恰一条结构化 warning（what=message_body_open_failed，含 message_id 与
%% reason，零密文/明文材料）；其余条目不受影响。
decrypt_error_degrades_with_warning() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101), row(502, 11, 102)]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) -> {error, aad_mismatch} end,
        fun() ->
            with_batch_warning_capture(fun() ->
                {ok, Bodies} = call([{11, 101, 501}, {11, 102, 502}]),
                ?assertEqual(#{501 => undefined, 502 => undefined}, Bodies),
                Warn = recv_batch_warning(),
                ?assertEqual(eb_message_body_open_failed, maps:get(what, Warn)),
                ?assert(is_map_key(message_id, Warn)),
                ?assertEqual(aad_mismatch, maps:get(reason, Warn))
            end)
        end
    ).

decrypt_error_class_raise_degrades() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_rows([row(501, 11, 101)]),
    with_crypto_meck(
        fun(_O, _W, _C, _M, _Cipher, _KV, _AAD, _KR) ->
            erlang:error({body_open_failed, 501, tampered})
        end,
        fun() ->
            {ok, Bodies} = call([{11, 101, 501}]),
            ?assertEqual(#{501 => undefined}, Bodies)
        end
    ).

invalid_items_rejected() ->
    ?FAKE_STORE:init(),
    ?assertEqual(
        {error, {invalid_argument, {items, [{11, 101, bad}]}}},
        call([{11, 101, bad}])
    ),
    ?assertEqual({error, {invalid_argument, items}}, call(not_a_list)),
    %% F5：数量上限 200（与消息页 limit 上限同口径）。
    TooMany = [{11, 101 + I, 1000 + I} || I <- lists:seq(0, 200)],
    ?assertEqual(
        {error, {invalid_argument, {items_too_many, 201}}},
        call(TooMany)
    ),
    %% F6：空请求集 → 空映射（合同声明）。
    ?assertEqual({ok, #{}}, call([])),
    ?assertEqual(
        {error, {invalid_argument, fetch_message_bodies}},
        eb_message_app:fetch_message_bodies(?ORG, #{})
    ),
    %% F1：app 面 org 形状守卫（与 facade is_integer 守卫同口径出参）。
    ?assertEqual(
        {error, {invalid_argument, {organization_id, bad}}},
        eb_message_app:fetch_message_bodies(bad, #{items => [{11, 101, 501}]})
    ),
    %% F4：同 Id 冲突 scope 元组 → fail-closed。
    ?assertEqual(
        {error, {invalid_argument, {items, {conflicting_scope, 501}}}},
        call([{11, 101, 501}, {11, 102, 501}])
    ).

store_error_propagates() ->
    ?FAKE_STORE:init(),
    ?FAKE_STORE:set_error({error, pg_down}),
    ?assertEqual({error, pg_down}, call([{11, 101, 501}])).

facade_shape_guard() ->
    %% facade 面参数形状守卫（跨 Feature 唯一入口的合同边界）。
    ?assertEqual(
        {error, {invalid_argument, fetch_message_bodies}},
        enterprise_business_facade:fetch_message_bodies(?ORG, #{})
    ),
    ?assertEqual(
        {error, {invalid_argument, {organization_id, bad}}},
        enterprise_business_facade:fetch_message_bodies(bad, #{items => [{11, 101, 501}]})
    ).

%% F3：批量摘要语句的专属形状门（不入 sql_statements/0 双租户键判据集，
%% 豁免理由见该函数注释；此处冻结 org 单钉 + ANY 数组 + 无 OFFSET + ws 不进
%% SQL 钉——归属复核在 application 层）。
batch_sql_shape_gate_test() ->
    Sql = eb_pg_message_ext:fetch_message_rows_by_ids_sql(),
    ?assert(is_binary(Sql)),
    ?assertMatch({match, _}, re:run(Sql, "organization_id = \\$1"), org_must_be_pinned),
    ?assertMatch(
        {match, _}, re:run(Sql, "id = ANY\\(\\$2::bigint\\[\\]\\)"), ids_must_be_array
    ),
    ?assertEqual(nomatch, re:run(Sql, <<"OFFSET">>, [caseless]), no_offset_allowed),
    ?assertEqual(nomatch, re:run(Sql, <<"workspace_id = \\\$">>), ws_must_not_be_sql_pinned).

%% -- F-R5 warning 捕获（真 logger handler；logger 是 sticky 内核不 meck）----

-define(BATCH_WARN_HANDLER, eb_batch_body_open_failed_capture).

with_batch_warning_capture(F) ->
    case logger:add_handler(?BATCH_WARN_HANDLER, ?MODULE, #{config => self()}) of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok = logger:remove_handler(?BATCH_WARN_HANDLER),
            ok = logger:add_handler(?BATCH_WARN_HANDLER, ?MODULE, #{config => self()})
    end,
    try
        F()
    after
        _ = logger:remove_handler(?BATCH_WARN_HANDLER)
    end,
    ok.

recv_batch_warning() ->
    receive
        {eb_batch_body_open_failed, Report} -> Report
    after 2000 -> erlang:error(batch_body_open_failed_warning_not_emitted)
    end.

%% logger handler 回调（必须导出——logger 从其管理进程回调）。
init(Config) ->
    {ok, #{config => maps:get(config, Config, self())}}.

log(#{msg := {report, Report}} = _Event, HandlerConfig) when is_map(Report) ->
    case maps:get(what, Report, undefined) of
        eb_message_body_open_failed ->
            TestPid = maps:get(config, HandlerConfig, undefined),
            case is_pid(TestPid) of
                true ->
                    TestPid ! {eb_batch_body_open_failed, Report},
                    ok;
                false ->
                    ok
            end;
        _Other ->
            ok
    end;
log(_Event, _HandlerConfig) ->
    ok.
