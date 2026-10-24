-module(user_collect_v1_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% E2EE 收藏 v1 服务端分支测试（迁移 227 / PLAN
%%% docs/design/2026-10-24-e2ee-collect §7.2/§7.3/§7.4/§9，v7.1）。
%%%
%%% 覆盖（PLAN §15 F1-F4 的服务端部分 + 校验分支）：
%%%   * F1 生命周期与单行模型：创建（触发器 +1）→ 墓碑（status=0、attach=''
%%%     释放引用）→ 复活（复用原行 CAS，恰一行）→ legacy add 撞 sealed
%%%     （5501 非 SQL 500）；
%%%   * F2 CAS 与幂等：expected 匹配更新 / 不匹配 5502 / 同请求字节重放
%%%     幂等成功且引用计数不动（零副作用）；
%%%   * F3 legacy 守卫：legacy remove sealed → 墓碑化（行存续）；legacy
%%%     remark sealed → 5501；legacy 行 add/remove/change 回归（现状行为）；
%%%   * F4 高水位门：推进到 2 后 v1 写被拒（5504）；删光高版本行后门不回落；
%%%   * 校验：attach 哈希不存在/无权 → 5503；参数非法 → 5505；
%%%     v1 覆盖 legacy 行 → 5506。
%%%
%%% 运行：make eunit-local EUNIT_CONFIG=config/sys.local.eb.config t=user_collect_v1_tests
%%% 前置：库已应用迁移 227。测试自建数据自清理。

%% ------------------------------------------------------------------
%% F1 生命周期 + 单行模型 + legacy add 守卫
%% ------------------------------------------------------------------

f1_lifecycle_single_row_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        H1 = rand_hash(),
        H2 = rand_hash(),
        KindId = <<"f1_", (integer_to_binary(Uid))/binary>>,
        try
            ok = seed_attachment(H1, Uid),
            ok = seed_attachment(H2, Uid),
            %% 创建：generation=1；触发器 INSERT 分支 +1（v1 不调 save）
            {ok, #{<<"generation">> := 1}} =
                user_collect_logic:add_v1(Uid, 2, KindId, <<"c1">>, 1, [H1, H2]),
            2 = referer_sum([H1, H2]),
            %% 墓碑：status=0 + generation+1 + attach=''（释放引用）
            {ok, _} = user_collect_logic:remove_dispatch(Uid, KindId),
            {ok, Row0} = user_collect_repo:find_any_by_uid_kind_id(Uid, KindId),
            0 = maps:get(<<"status">>, Row0),
            2 = maps:get(<<"generation">>, Row0),
            <<>> = maps:get(<<"attach_file_hash256">>, Row0),
            0 = referer_sum([H1, H2]),
            %% 复活：复用原行 UPDATE（客户端已重 seal），恰一行
            {ok, #{<<"generation">> := 3}} =
                user_collect_logic:add_v1(Uid, 2, KindId, <<"c3">>, 1, [H1, H2]),
            2 = referer_sum([H1, H2]),
            {ok, _} = user_collect_repo:find_any_by_uid_kind_id(Uid, KindId),
            1 = row_count(Uid, KindId),
            %% legacy add 撞 sealed（任意 status）：业务错误而非 SQL 500
            %% （墓碑场景，F1 失败用例复现：v6 语义下此处 INSERT 撞约束）
            {ok, _} = user_collect_logic:remove_dispatch(Uid, KindId),
            {error, 5501} =
                user_collect_logic:add(
                    Uid,
                    <<"2">>,
                    KindId,
                    #{<<"payload">> => #{}, <<"type">> => <<"t">>},
                    <<"src">>,
                    <<"">>
                )
        after
            cleanup(Uid, KindId)
        end
    end).

%% ------------------------------------------------------------------
%% F2 CAS 条件更新与幂等（同请求字节重放零副作用）
%% ------------------------------------------------------------------

f2_cas_and_idempotency_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        H1 = rand_hash(),
        KindId = <<"f2_", (integer_to_binary(Uid))/binary>>,
        try
            ok = seed_attachment(H1, Uid),
            {ok, _} = user_collect_logic:add_v1(Uid, 2, KindId, <<"c1">>, 1, [H1]),
            %% CAS 成功：expected=1 → 2
            {ok, #{<<"generation">> := 2}} =
                user_collect_logic:update_v1(Uid, KindId, 1, 2, <<"c2">>, 1, [H1]),
            %% 幂等重放：同 generation/密文/kv/引用 → 幂等成功，计数不动
            1 = referer_sum([H1]),
            {ok, #{<<"generation">> := 2, <<"idempotent">> := true}} =
                user_collect_logic:update_v1(Uid, KindId, 1, 2, <<"c2">>, 1, [H1]),
            1 = referer_sum([H1]),
            %% 编辑差集：{H1} → {}（无附件文本化）→ 触发器 −1
            {ok, #{<<"generation">> := 3}} =
                user_collect_logic:update_v1(Uid, KindId, 2, 3, <<"c3">>, 1, []),
            0 = referer_sum([H1]),
            %% expected 不匹配（陈旧基线）→ 5502
            {error, 5502} =
                user_collect_logic:update_v1(Uid, KindId, 1, 2, <<"c2x">>, 1, []),
            %% 修订号跳跃（NewGen ≠ Expected+1）→ 5505
            {error, 5505} =
                user_collect_logic:update_v1(Uid, KindId, 3, 7, <<"c7">>, 1, []),
            %% 行内容未被冲突请求改动
            {ok, Row} = user_collect_repo:find_any_by_uid_kind_id(Uid, KindId),
            <<"c3">> = maps:get(<<"info">>, Row)
        after
            cleanup(Uid, KindId)
        end
    end).

%% ------------------------------------------------------------------
%% F3 legacy 守卫：sealed 行由服务端代执行墓碑；legacy 行行为不变
%% ------------------------------------------------------------------

f3_legacy_guards_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        H1 = rand_hash(),
        SealedKind = <<"f3s_", (integer_to_binary(Uid))/binary>>,
        LegacyKind = <<"f3l_", (integer_to_binary(Uid))/binary>>,
        try
            ok = seed_attachment(H1, Uid),
            %% sealed 行：legacy remove → 墓碑（行存续、引用释放）
            {ok, _} = user_collect_logic:add_v1(Uid, 2, SealedKind, <<"c1">>, 1, [H1]),
            1 = referer_sum([H1]),
            {ok, _} = user_collect_logic:remove(Uid, SealedKind),
            {ok, Row} = user_collect_repo:find_any_by_uid_kind_id(Uid, SealedKind),
            0 = maps:get(<<"status">>, Row),
            0 = referer_sum([H1]),
            %% sealed 行：legacy remark → 5501
            {error, 5501} =
                user_collect_logic:change(
                    Uid,
                    <<"remark">>,
                    SealedKind,
                    #{<<"remark">> => <<"r">>}
                ),
            %% legacy 行：add → remove → remark 全链现状行为（回归组）
            ok = legacy_add(Uid, LegacyKind),
            1 = row_count(Uid, LegacyKind),
            ok = user_collect_logic:change(
                Uid,
                <<"remark">>,
                LegacyKind,
                #{<<"remark">> => <<"ok">>}
            ),
            {ok, 1} = user_collect_logic:remove_dispatch(Uid, LegacyKind),
            not_found = user_collect_repo:find_any_by_uid_kind_id(Uid, LegacyKind)
        after
            cleanup(Uid, SealedKind),
            cleanup(Uid, LegacyKind)
        end
    end).

%% ------------------------------------------------------------------
%% F4 用户级 key_version 高水位门（只升不降，与行存续无关）
%% ------------------------------------------------------------------

f4_key_state_gate_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        H1 = rand_hash(),
        K1 = <<"f4a_", (integer_to_binary(Uid))/binary>>,
        K2 = <<"f4b_", (integer_to_binary(Uid))/binary>>,
        try
            ok = seed_attachment(H1, Uid),
            %% 轮换：K1 以 v2 写入 → 高水位推进到 2
            {ok, _} = user_collect_logic:add_v1(Uid, 2, K1, <<"c1v2">>, 2, [H1]),
            %% 掉队设备以 v1 写 K2（另一记录）→ 5504（门对请求生效）
            {error, 5504} =
                user_collect_logic:add_v1(Uid, 2, K2, <<"c2v1">>, 1, [H1]),
            not_found = user_collect_repo:find_any_by_uid_kind_id(Uid, K2),
            %% 删光该用户高版本行（模拟物理清除）后，门不随行回落
            {ok, _} = elib_pg:execute(
                <<"DELETE FROM public.user_collect WHERE user_id = $1">>, [Uid]
            ),
            {error, 5504} =
                user_collect_logic:add_v1(Uid, 2, K2, <<"c2v1">>, 1, [H1]),
            {ok, [#{<<"max_key_version">> := 2}]} = elib_pg:query(
                <<"SELECT max_key_version FROM public.user_collect_key_state ",
                    "WHERE user_id = $1">>,
                [Uid]
            )
        after
            cleanup(Uid, K1),
            cleanup(Uid, K2)
        end
    end).

%% ------------------------------------------------------------------
%% §7.2 引用校验与 v1 参数校验
%% ------------------------------------------------------------------

validation_branches_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        Other = new_uid(),
        H2 = rand_hash(),
        KMiss = <<"vm_", (integer_to_binary(Uid))/binary>>,
        KOther = <<"vo_", (integer_to_binary(Uid))/binary>>,
        KLegacy = <<"vl_", (integer_to_binary(Uid))/binary>>,
        try
            %% 哈希不存在 → 5503
            {error, 5503} =
                user_collect_logic:add_v1(Uid, 2, KMiss, <<"c">>, 1, [<<"ffff">>]),
            %% 他人 private 附件（无权引用）→ 5503
            ok = seed_attachment(H2, Other),
            {error, 5503} =
                user_collect_logic:add_v1(Uid, 2, KOther, <<"c">>, 1, [H2]),
            %% 参数非法 → 5505（kind_id 超 40 / 密文空 / kv=0 / 引用超 10）
            {error, 5505} =
                user_collect_logic:add_v1(
                    Uid,
                    2,
                    <<"k-this-kind-id-is-definitely-longer-than-forty-bytes!">>,
                    <<"c">>,
                    1,
                    []
                ),
            {error, 5505} =
                user_collect_logic:add_v1(Uid, 2, <<"k">>, <<"">>, 1, []),
            {error, 5505} =
                user_collect_logic:add_v1(Uid, 2, <<"k">>, <<"c">>, 0, []),
            %% v1 覆盖 legacy 行 → 5506
            ok = legacy_add(Uid, KLegacy),
            {error, 5506} =
                user_collect_logic:add_v1(Uid, 2, KLegacy, <<"c">>, 1, [])
        after
            cleanup(Uid, KMiss),
            cleanup(Uid, KOther),
            cleanup(Uid, KLegacy)
        end
    end).

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------

seed_attachment(Hash, OwnerUid) ->
    {ok, _} = elib_pg:execute(
        <<
            "INSERT INTO public.attachment (id, file_hash256, scope, creator_user_id, path)",
            " VALUES ($1, $2, 'private', $3, $4)"
        >>,
        [new_id(), Hash, OwnerUid, <<"t/", Hash/binary>>]
    ),
    ok.

referer_sum(Hashes) ->
    lists:sum([referer_of(H) || H <- Hashes]).

referer_of(Hash) ->
    {ok, [#{<<"s">> := S}]} = elib_pg:query(
        <<"SELECT coalesce(sum(referer_time), 0) AS s FROM public.attachment ",
            "WHERE file_hash256 = $1">>,
        [Hash]
    ),
    %% sum(bigint) 经 epgsql 返回 numeric 的 binary 文本形态
    binary_to_integer(S).

row_count(Uid, KindId) ->
    {ok, [#{<<"c">> := C}]} = elib_pg:query(
        <<"SELECT count(*) AS c FROM public.user_collect ", "WHERE user_id = $1 AND kind_id = $2">>,
        [Uid, KindId]
    ),
    C.

legacy_add(Uid, KindId) ->
    Info = #{<<"type">> => <<"text">>, <<"payload">> => #{<<"text">> => <<"hi">>}},
    case user_collect_logic:add(Uid, <<"1">>, KindId, Info, <<"src">>, <<"">>) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({legacy_add_failed, Reason})
    end.

%% 清理收藏行/高水位行；attachment 行不物理删（185 起 v5 実体删除须走
%% 删除意图域，测试不越域），哈希每测试随机不会复用，行残留可接受。
cleanup(Uid, KindId) ->
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM public.user_collect WHERE user_id = $1 AND kind_id = $2">>,
        [Uid, KindId]
    ),
    {ok, _} = elib_pg:execute(
        <<"DELETE FROM public.user_collect_key_state WHERE user_id = $1">>, [Uid]
    ),
    ok.

%% 每测试唯一 64-hex（SHA-256 形态），避免跨测试 referer 计数串扰。
rand_hash() ->
    <<Bin:32/binary, _/binary>> = crypto:strong_rand_bytes(32),
    binary:encode_hex(Bin, lowercase).

new_uid() ->
    ok =
        case elib_tsid:readiness() of
            ready -> ok;
            not_initialized -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
        end,
    elib_tsid:generate(user).

new_id() ->
    elib_tsid:generate(attachment).
