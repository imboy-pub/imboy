-module(ai_agent_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_ds EUnit 测试（Phase 1 T1.2）
%%% 覆盖：建号编排（建 user + 标 account_type=1 + 绑 ai_agent）、
%%%       边界校验、is_agent/1 路由判定、trigger_policy jsonb 编解码。
%%%===================================================================

%% ===================================================================
%% create/1 — 建号 + 绑定编排
%% ===================================================================

create_ok_promotes_user_and_binds_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [{'generate', 0, fun() -> 999 end}]},
            {user_repo, [
                {'create', 1, fun(_) -> ok end},
                {'update', 2, fun(_Uid, _Data) -> {ok, 1} end}
            ]},
            {ai_agent_repo, [
                {'upsert', 1, fun(#{user_id := Uid}) -> {ok, [#{<<"user_id">> => Uid}]} end}
            ]}
        ],
        fun() ->
            Cfg = #{
                <<"nickname">> => <<"客服助手"/utf8>>,
                <<"provider">> => <<"qianfan">>,
                <<"trigger_policy">> => #{<<"mention">> => true}
            },
            {ok, #{<<"user_id">> := Uid}} = ai_agent_ds:create(Cfg),
            ?assertEqual(999, Uid),
            %% account_type 被标记为 1（agent）
            ?assert(meck:called(user_repo, update, [Uid, #{account_type => 1}])),
            %% ai_agent 绑定被调用（provider 透传，trigger_policy 编码为 JSON binary）
            ?assert(meck:called(ai_agent_repo, upsert, '_'))
        end
    ).

create_rejects_empty_nickname_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create', 1, fun(_) -> ok end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"nickname 不能为空"/utf8>>},
                ai_agent_ds:create(#{<<"provider">> => <<"qianfan">>})
            ),
            %% 校验失败不应触碰建号
            ?assertNot(meck:called(user_repo, create, '_'))
        end
    ).

create_rejects_empty_provider_test_() ->
    ?WITH_MECKS(
        [{user_repo, [{'create', 1, fun(_) -> ok end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"provider 不能为空"/utf8>>},
                ai_agent_ds:create(#{<<"nickname">> => <<"bot">>})
            )
        end
    ).

update_rejects_empty_provider_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'upsert', 1, fun(_) -> {ok, []} end}]}],
        fun() ->
            ?assertEqual(
                {error, <<"provider 不能为空"/utf8>>},
                ai_agent_ds:update(123, #{<<"model">> => <<"m1">>})
            ),
            ?assertNot(meck:called(ai_agent_repo, upsert, '_'))
        end
    ).

%% ===================================================================
%% is_agent/1 — 消息路由判定
%% ===================================================================

is_agent_returns_config_for_active_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(42) ->
                    {ok, #{
                        <<"user_id">> => 42,
                        <<"provider">> => <<"qianfan">>,
                        <<"status">> => 1,
                        <<"trigger_policy">> => <<"{\"mention\":true}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            {true, Agent} = ai_agent_ds:is_agent(42),
            ?assertEqual(<<"qianfan">>, maps:get(<<"provider">>, Agent)),
            %% jsonb 解码为 map
            ?assertEqual(#{<<"mention">> => true}, maps:get(<<"trigger_policy">>, Agent))
        end
    ).

is_agent_false_for_disabled_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'find', 1, fun(_) -> {ok, #{<<"status">> => 0}} end}]}],
        fun() ->
            ?assertEqual(false, ai_agent_ds:is_agent(42))
        end
    ).

is_agent_false_for_notfound_test_() ->
    ?WITH_MECKS(
        [{ai_agent_repo, [{'find', 1, fun(_) -> {error, notfound} end}]}],
        fun() ->
            ?assertEqual(false, ai_agent_ds:is_agent(99))
        end
    ).

%% ===================================================================
%% get/1 — trigger_policy jsonb 解码
%% ===================================================================

get_decodes_trigger_policy_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'find', 1, fun(7) ->
                    {ok, #{
                        <<"user_id">> => 7,
                        <<"trigger_policy">> => <<"{\"keywords\":[\"help\"]}">>
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Agent} = ai_agent_ds:get(7),
            ?assertEqual(
                #{<<"keywords">> => [<<"help">>]},
                maps:get(<<"trigger_policy">>, Agent)
            )
        end
    ).
