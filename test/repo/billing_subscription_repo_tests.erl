-module(billing_subscription_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc billing_subscription Repo 选列回归测试（C0-CONTRACT-01）
%%%
%%% 背景（真 bug）：C0-BILL-01 通过迁移 00000050 给 billing_subscription 加了
%%% owner_uid 并接上 fail-closed 归属校验，但 repo 的 ?COLUMNS 没同步加该列 ——
%%% find_by_id/find_active_by_tenant/page 选出来的 map 里根本没有 owner_uid，
%%% billing_logic:assert_owner/2 与 billing_handler:sub_owned_by/2 读到默认 0，
%%% 于是**合法订阅人**的 renew/cancel/usage/quota/invoice 全部 403，
%%% GET /billing/subscription 恒返回 {}。
%%%
%%% 既有单测把 billing_subscription_ds:find_by_id/1 整个 mock 掉，
%%% 因此完全看不到这条 SQL 少选列 —— 用选列断言把契约锁在 repo 层。
%%% @end
%%%===================================================================

%% 归属校验与对外契约依赖的列，任何一个漏选都会造成线上静默故障
columns_cover_contract_fields_test_() ->
    ?TEST_SIMPLE(fun() ->
        Cols = billing_subscription_repo:columns(),
        Required = [
            <<"id">>,
            <<"tenant_id">>,
            <<"plan_id">>,
            %% 授权依据：缺列 = 所有归属校验 fail-closed 误杀
            <<"owner_uid">>,
            <<"status">>,
            <<"current_period_start">>,
            <<"current_period_end">>,
            <<"auto_renew">>
        ],
        [
            ?assertNotEqual(nomatch, binary:match(Cols, C))
         || C <- Required
        ],
        ok
    end).

%% 选列必须是逗号分隔的裸列名，不得退化成 SELECT *（新增敏感列会自动外泄）
columns_not_wildcard_test_() ->
    ?TEST_SIMPLE(fun() ->
        Cols = billing_subscription_repo:columns(),
        ?assertEqual(nomatch, binary:match(Cols, <<"*">>)),
        ?assert(byte_size(Cols) > 0)
    end).
