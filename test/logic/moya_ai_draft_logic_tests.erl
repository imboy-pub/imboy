%% moya_ai_draft_logic_tests
%% AI-01 / AI-03（provider 侧）— 墨芽书法 AI 回课 provider 包装测试（Step 11）。
%%
%% 纯 logic 用例：全部外呼（registry/provider chat）与配置均 meck，零真实网络、
%% 零真实模型密钥（api_key 一律占位值）。覆盖：
%%   AI-03 三形降级 —— provider 名未配置 / registry 未命中 / vision=false /
%%                     api_key 空 → {error, provider_unavailable} 且 chat 0 次调用
%%   AI-01 provider 路径 —— 成功（白名单重建）/ 超时 / provider 错误 / provider 崩溃 /
%%                     非法 JSON / 合法 JSON 但 Schema 失败 / 空 content
%%   validate_result 直测 —— 思维链结构性丢弃 / confidence 越界丢弃 / 各字段边界
%%
%% repo 落库路径（requeue/failed/重试上限/附件删除）在
%% test/repo/moya_ai_worker_tests.erl（真库 4323 scratch）。

-module(moya_ai_draft_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(PROVIDER, <<"moya-fake">>).
-define(MODEL, <<"glm-test-model">>).
%% fake provider 用真实存在的 qianfan 模块（meck 全量替换 chat/3 与
%% capabilities/0，零真实网络/零真实密钥；不能 meck 不存在的模块）
-define(FAKE_MOD, imboy_llm_qianfan).

%%%===================================================================
%%% 夹具：合法结构化结果（含应被丢弃的思维链键与越界 confidence 变体）
%%%===================================================================

valid_result() ->
    #{
        <<"positive_point">> => <<"执笔姿势稳定，横画起收笔干净"/utf8>>,
        <<"focus_problem">> => <<"竖画整体右倾，重心不稳"/utf8>>,
        <<"practice_action">> => <<"每天三行悬针竖，对格线书写"/utf8>>,
        <<"evidence_moments">> => [1.5, 8.2, 20.0],
        <<"script_outline">> => [<<"先肯定坐姿与执笔"/utf8>>, <<"再演示竖画回正"/utf8>>],
        <<"needs_human_check">> => false,
        <<"confidence">> => 0.87,
        %% 思维链/额外键：validate 必须结构性丢弃，不落库（AI-02 白名单）
        <<"reasoning">> => <<"思维链片段：先看整体再逐帧…"/utf8>>,
        <<"raw_transcript">> => <<"must not leak"/utf8>>
    }.

whitelist_keys() ->
    [
        <<"char_reviews">>,
        <<"confidence">>,
        <<"evidence_moments">>,
        <<"focus_problem">>,
        <<"needs_human_check">>,
        <<"positive_point">>,
        <<"practice_action">>,
        <<"script_outline">>
    ].

%% 基础 mock 组：provider 可用（vision=true + key 占位值），chat 默认返回合法 JSON。
%% chat/3 返回值可经进程字典 fake_chat 覆盖（默认 undefined → 合法结果）。
provider_mocks() ->
    [
        {config_ds, [
            {'env', 2, fun
                (teaching_ai_llm_provider, _) ->
                    case get(fake_provider_name) of
                        undefined -> ?PROVIDER;
                        V -> V
                    end;
                (_, Default) ->
                    Default
            end}
        ]},
        {imboy_llm_registry, [
            {'lookup', 1, fun(_) ->
                case get(fake_lookup) of
                    undefined ->
                        {ok, #{
                            module => ?FAKE_MOD,
                            opts => #{api_key => <<"test-key-placeholder">>, model => ?MODEL}
                        }};
                    V ->
                        V
                end
            end}
        ]},
        {?FAKE_MOD, [
            {'capabilities', 0, fun() -> #{vision => true} end},
            {'chat', 3, fun(_Uid, Messages, _Opts) ->
                put(captured_messages, Messages),
                case get(fake_chat) of
                    undefined ->
                        {ok, #{<<"content">> => jsone:encode(valid_result())}};
                    {json, Map} ->
                        {ok, #{<<"content">> => jsone:encode(Map)}};
                    R ->
                        R
                end
            end}
        ]}
    ].

%%%===================================================================
%%% AI-03：三形降级 → {error, provider_unavailable}，零 provider 调用
%%%===================================================================

%% 形一：teaching_ai_llm_provider 未配置（默认 undefined）→ 不触 registry
ai03_provider_name_unconfigured_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> undefined;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> {ok, #{module => ?FAKE_MOD, opts => #{}}} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unavailable},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            ),
            %% 未配置名：连 registry 查询都不应发生
            ?assertEqual(0, meck:num_calls(imboy_llm_registry, lookup, 1))
        end
    ).

%% 形二：registry 未命中该 provider 名 → 降级（chat 0 次 = 零外呼）
ai03_registry_miss_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> ?PROVIDER;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) -> undefined end}
            ]},
            {?FAKE_MOD, [
                {'capabilities', 0, fun() -> #{vision => true} end},
                {'chat', 3, fun(_, _, _) -> {error, must_not_be_called} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unavailable},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            ),
            ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
        end
    ).

%% 形三：provider capabilities vision=false → 降级（chat 0 次 = 零外呼）
ai03_vision_false_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> ?PROVIDER;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) ->
                    {ok, #{module => ?FAKE_MOD, opts => #{api_key => <<"test-key-placeholder">>}}}
                end}
            ]},
            {?FAKE_MOD, [
                {'capabilities', 0, fun() -> #{vision => false} end},
                {'chat', 3, fun(_, _, _) -> {error, must_not_be_called} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unavailable},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            ),
            ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
        end
    ).

%% 形四：api_key 空（含未配置 key）→ 降级（chat 0 次 = 零外呼）
ai03_empty_api_key_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> ?PROVIDER;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) ->
                    {ok, #{module => ?FAKE_MOD, opts => #{api_key => <<>>}}}
                end}
            ]},
            {?FAKE_MOD, [
                {'capabilities', 0, fun() -> #{vision => true} end},
                {'chat', 3, fun(_, _, _) -> {error, must_not_be_called} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_unavailable},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            ),
            ?assertEqual(0, meck:num_calls(?FAKE_MOD, chat, 3))
        end
    ).

%%%===================================================================
%%% AI-01：成功 / 超时 / provider 错误 / 崩溃 / 非法 JSON
%%%===================================================================

%% 成功：白名单重建（思维链丢弃、confidence 保留）
ai01_success_whitelist_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        {ok, Result} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        ?assertEqual(whitelist_keys(), lists:sort(maps:keys(Result))),
        ?assertEqual(0.87, maps:get(<<"confidence">>, Result)),
        ?assertEqual(false, maps:get(<<"needs_human_check">>, Result)),
        ?assertEqual([1.5, 8.2, 20.0], maps:get(<<"evidence_moments">>, Result)),
        %% 思维链/原文键绝不透传（AI-02）
        ?assertEqual(false, maps:is_key(<<"reasoning">>, Result)),
        ?assertEqual(false, maps:is_key(<<"raw_transcript">>, Result))
    end).

ai01_success_returns_resolved_model_profile_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        {ok, Result, ModelProfile} =
            moya_ai_draft_logic:analyze_video_with_profile(meta(), attachment()),
        ?assertEqual(?MODEL, ModelProfile),
        ?assertEqual(whitelist_keys(), lists:sort(maps:keys(Result)))
    end).

ai01_model_profile_falls_back_to_provider_name_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        try
            lists:foreach(
                fun(Opts) ->
                    _ = put(fake_lookup, {ok, #{module => ?FAKE_MOD, opts => Opts}}),
                    {ok, _Result, ModelProfile} =
                        moya_ai_draft_logic:analyze_video_with_profile(meta(), attachment()),
                    ?assertEqual(?PROVIDER, ModelProfile)
                end,
                [
                    #{api_key => <<"test-key-placeholder">>},
                    #{api_key => <<"test-key-placeholder">>, model => <<>>}
                ]
            )
        after
            erase(fake_lookup)
        end
    end).

%% 超时：{error, timeout}（Worker 侧属瞬时错误可重试）
ai01_timeout_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {error, timeout}),
        try
            ?assertEqual(
                {error, timeout},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% provider 返回错误：收敛为 {error, provider_error}（瞬时）
ai01_provider_error_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {error, rate_limited}),
        try
            ?assertEqual(
                {error, provider_error},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% provider 崩溃（throw/exit）：被 catch 收敛为 provider_error，不扩散
%% （独立完整 mock 组：同一模块不可在一次 setup 中出现两次，否则后者卸掉前者）
ai01_provider_crash_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> ?PROVIDER;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) ->
                    {ok, #{module => ?FAKE_MOD, opts => #{api_key => <<"test-key-placeholder">>}}}
                end}
            ]},
            {?FAKE_MOD, [
                {'capabilities', 0, fun() -> #{vision => true} end},
                {'chat', 3, fun(_, _, _) -> throw(boom) end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, provider_error},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        end
    ).

%% 非法 JSON：content 非 JSON → bad_output（不重试）
ai01_bad_json_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {ok, #{<<"content">> => <<"好的，我来分析这段视频：首先…"/utf8>>}}),
        try
            ?assertEqual(
                {error, bad_output},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% 合法 JSON 但 Schema 失败（缺必填键）→ bad_output
ai01_valid_json_bad_schema_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {json, #{<<"positive_point">> => <<"只有一项"/utf8>>}}),
        try
            ?assertEqual(
                {error, bad_output},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% 空 content / 无 content 无 result → bad_output
ai01_empty_content_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {ok, #{<<"content">> => <<>>}}),
        R1 = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        _ = put(fake_chat, {ok, #{}}),
        R2 = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        try
            ?assertEqual({error, bad_output}, R1),
            ?assertEqual({error, bad_output}, R2)
        after
            erase(fake_chat)
        end
    end).

%%%===================================================================
%%% 模型输出规范化：各模型用不同包装裹 JSON，脱壳后必须可用
%%%===================================================================

%% 思维链块 + 裸 JSON（部分推理模型会内联 <think>）
ai01_think_block_wrapped_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"<think>先看整体，再看逐帧笔锋</think>\n"/utf8, (jsone:encode(valid_result()))/binary>>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            {ok, W} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            ?assertEqual(whitelist_keys(), lists:sort(maps:keys(W)))
        after
            erase(fake_chat)
        end
    end).

%% 结果框标记包裹（智谱 <|begin_of_box|>…<|end_of_box|>）
ai01_box_marker_wrapped_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"<|begin_of_box|>", (jsone:encode(valid_result()))/binary, "<|end_of_box|>">>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            ?assertMatch({ok, _}, moya_ai_draft_logic:analyze_video(meta(), attachment()))
        after
            erase(fake_chat)
        end
    end).

%% Markdown 代码围栏（模型输出 ```json … ```）
ai01_fenced_json_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"```json\n", (jsone:encode(valid_result()))/binary, "\n```">>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            ?assertMatch({ok, _}, moya_ai_draft_logic:analyze_video(meta(), attachment()))
        after
            erase(fake_chat)
        end
    end).

%% 前置解释文字 + JSON：取最外层 {...}
ai01_prose_prefixed_json_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"分析如下：\n"/utf8, (jsone:encode(valid_result()))/binary>>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            ?assertMatch({ok, _}, moya_ai_draft_logic:analyze_video(meta(), attachment()))
        after
            erase(fake_chat)
        end
    end).

%% 脱壳只去包装、不修补内容：think 被 max_tokens 截断（无 JSON）→ 仍 bad_output
ai01_think_truncated_no_json_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {ok, #{<<"content">> => <<"<think>用户现在需要分析这段视频，先看"/utf8>>}}),
        try
            ?assertEqual(
                {error, bad_output},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% 脱壳不救 Schema：包装里是合法 JSON 但缺必填键 → 仍 bad_output
ai01_wrapped_bad_schema_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Inner = jsone:encode(#{<<"positive_point">> => <<"只有一项"/utf8>>}),
        _ = put(fake_chat, {ok, #{<<"content">> => <<"<think>想完了</think>", Inner/binary>>}}),
        try
            ?assertEqual(
                {error, bad_output},
                moya_ai_draft_logic:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%%%===================================================================
%%% Phase B：AI 逐字点评字卡（char_reviews 识别制）
%%%===================================================================

%% 合法字卡透传：只保留契约四字段（额外键被剥离）
ai01_char_reviews_passthrough_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(
            fake_chat,
            {json, (valid_result())#{
                <<"char_reviews">> => [
                    #{
                        <<"index">> => 0,
                        <<"char">> => <<"人"/utf8>>,
                        <<"grade">> => <<"good">>,
                        <<"comment">> => <<"起笔藏锋到位"/utf8>>,
                        <<"bbox">> => [1, 2, 3, 4]
                    },
                    #{
                        <<"index">> => 1,
                        <<"char">> => <<"大"/utf8>>,
                        <<"grade">> => <<"fair">>,
                        <<"comment">> => <<"撇画略短"/utf8>>
                    }
                ]
            }}
        ),
        try
            {ok, W} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            Items = maps:get(<<"char_reviews">>, W),
            ?assertEqual(2, length(Items)),
            ?assertEqual(
                [<<"char">>, <<"comment">>, <<"grade">>, <<"index">>],
                lists:sort(maps:keys(hd(Items)))
            )
        after
            erase(fake_chat)
        end
    end).

%% 越界项逐项丢弃（index 负 / grade 非枚举 / char 超 8 字节 / comment 超 300 /
%% 非 map 项），合法项照常保留
ai01_char_reviews_drop_invalid_items_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(
            fake_chat,
            {json, (valid_result())#{
                <<"char_reviews">> => [
                    #{
                        <<"index">> => -1,
                        <<"char">> => <<"甲"/utf8>>,
                        <<"grade">> => <<"good">>,
                        <<"comment">> => <<>>
                    },
                    #{
                        <<"index">> => 0,
                        <<"char">> => <<"乙"/utf8>>,
                        <<"grade">> => <<"great">>,
                        <<"comment">> => <<>>
                    },
                    #{
                        <<"index">> => 1,
                        <<"char">> => binary:copy(<<"字"/utf8>>, 3),
                        <<"grade">> => <<"good">>,
                        <<"comment">> => <<>>
                    },
                    #{
                        <<"index">> => 2,
                        <<"char">> => <<"丙"/utf8>>,
                        <<"grade">> => <<"good">>,
                        <<"comment">> => binary:copy(<<"长"/utf8>>, 101)
                    },
                    #{
                        <<"index">> => 3,
                        <<"char">> => <<"丁"/utf8>>,
                        <<"grade">> => <<"poor">>,
                        <<"comment">> => <<"结构松散"/utf8>>
                    },
                    <<"not a map">>
                ]
            }}
        ),
        try
            {ok, W} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            Items = maps:get(<<"char_reviews">>, W),
            ?assertEqual(1, length(Items)),
            ?assertEqual(3, maps:get(<<"index">>, hd(Items))),
            ?assertEqual(<<"poor">>, maps:get(<<"grade">>, hd(Items)))
        after
            erase(fake_chat)
        end
    end).

%% 上限 50 项：超出截断（与 Phase A 的 ?CHAR_REVIEWS_MAX 同源）
ai01_char_reviews_cap_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Items = [
            #{
                <<"index">> => I,
                <<"char">> => <<"字"/utf8>>,
                <<"grade">> => <<"good">>,
                <<"comment">> => <<>>
            }
         || I <- lists:seq(0, 59)
        ],
        _ = put(fake_chat, {json, (valid_result())#{<<"char_reviews">> => Items}}),
        try
            {ok, W} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            ?assertEqual(50, length(maps:get(<<"char_reviews">>, W)))
        after
            erase(fake_chat)
        end
    end).

%% 非数组（模型把数组写成字符串）→ 字卡降级 null，不判整个点评失败
ai01_char_reviews_malformed_degrades_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {json, (valid_result())#{<<"char_reviews">> => <<"人 good"/utf8>>}}),
        try
            {ok, W} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            ?assertEqual(null, maps:get(<<"char_reviews">>, W)),
            %% 降级只作用于字卡：三段文本仍完整
            ?assertNotEqual(<<>>, maps:get(<<"positive_point">>, W))
        after
            erase(fake_chat)
        end
    end).

%% 缺失 / 空数组 → 显式 null（键恒存在，形状稳定）
ai01_char_reviews_absent_or_empty_is_null_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {json, valid_result()}),
        R1 = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        _ = put(fake_chat, {json, (valid_result())#{<<"char_reviews">> => []}}),
        R2 = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        try
            {ok, W1} = R1,
            ?assert(maps:is_key(<<"char_reviews">>, W1)),
            ?assertEqual(null, maps:get(<<"char_reviews">>, W1)),
            {ok, W2} = R2,
            ?assertEqual(null, maps:get(<<"char_reviews">>, W2))
        after
            erase(fake_chat)
        end
    end).

%% prompt/rubric 版本兜底：draft 行的版本列是空串且键恒存在，必须回落到当前版本
%% （否则模型收到的版本恒为空、prompt 演进无从回溯）；显式给定版本时不得被覆盖
version_blank_falls_back_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Blank = (meta())#{prompt_version => <<>>, rubric_version => <<>>},
        _ = put(fake_chat, {json, valid_result()}),
        try
            {ok, _} = moya_ai_draft_logic:analyze_video(Blank, attachment()),
            [_, #{<<"content">> := Content}] = get(captured_messages),
            {ok, _} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            [_, #{<<"content">> := Content2}] = get(captured_messages),
            ?assertNotEqual(nomatch, binary:match(Content, <<"p-2026-09-12.1">>)),
            ?assertNotEqual(nomatch, binary:match(Content, <<"r-hardpen-1">>)),
            ?assertNotEqual(nomatch, binary:match(Content2, <<"p-2026-09-09.1">>))
        after
            erase(fake_chat)
        end
    end).

%%%===================================================================
%%% validate_result 直测（Schema 边界，无 meck）
%%%===================================================================

validate_ok_whitelist_test() ->
    {ok, W} = moya_ai_draft_logic:validate_result(valid_result()),
    ?assertEqual(whitelist_keys(), lists:sort(maps:keys(W))).

validate_confidence_out_of_range_test() ->
    Base = valid_result(),
    Input = Base#{<<"confidence">> => 1.5},
    {ok, W} = moya_ai_draft_logic:validate_result(Input),
    %% 越界 confidence 丢弃，其余白名单保留（含 Phase B 的 char_reviews）
    ?assertEqual(false, maps:is_key(<<"confidence">>, W)),
    ?assertEqual(7, maps:size(W)).

validate_moments_bounds_test() ->
    with_strict(fun() ->
        Base = valid_result(),
        %% 空 / 超过 5 项 / 负值 / 非数值 → bad_output
        [
            begin
                BadInput = Base#{<<"evidence_moments">> => Bad},
                ?assertEqual(
                    {error, bad_output},
                    moya_ai_draft_logic:validate_result(BadInput)
                )
            end
         || Bad <- [[], [1, 2, 3, 4, 5, 6], [-0.1, 2], [<<"1.5">>]]
        ],
        %% 恰 1 项 / 恰 5 项合法
        One = Base#{<<"evidence_moments">> => [0]},
        {ok, _} = moya_ai_draft_logic:validate_result(One),
        Five = Base#{<<"evidence_moments">> => [1, 2, 3, 4, 5]},
        {ok, _} = moya_ai_draft_logic:validate_result(Five)
    end).

validate_outline_bounds_test() ->
    with_strict(fun() ->
        Base = valid_result(),
        %% 超过 3 项 / 单项超 200 字节 / 非二进制 → bad_output
        Long = binary:copy(<<"a">>, 201),
        [
            begin
                BadInput = Base#{<<"script_outline">> => Bad},
                ?assertEqual(
                    {error, bad_output},
                    moya_ai_draft_logic:validate_result(BadInput)
                )
            end
         || Bad <- [[<<"a">>, <<"b">>, <<"c">>, <<"d">>], [Long], [1, 2]]
        ]
    end).

validate_text_fields_test() ->
    with_strict(fun() ->
        Base = valid_result(),
        %% 必填文本：缺失 / 空 / 非二进制 / 超 300 字节 → bad_output
        Long = binary:copy(<<"字"/utf8>>, 151),
        [
            begin
                BadInput = Base#{<<"positive_point">> => Bad},
                ?assertEqual(
                    {error, bad_output},
                    moya_ai_draft_logic:validate_result(BadInput)
                )
            end
         || Bad <- [undefined, <<>>, 123, Long]
        ]
    end).

validate_needs_human_check_test() ->
    with_strict(fun() ->
        Base = valid_result(),
        BadInput = Base#{<<"needs_human_check">> => <<"yes">>},
        ?assertEqual(
            {error, bad_output},
            moya_ai_draft_logic:validate_result(BadInput)
        )
    end).

validate_non_map_test() ->
    ?assertEqual({error, bad_output}, moya_ai_draft_logic:validate_result([valid_result()])),
    ?assertEqual({error, bad_output}, moya_ai_draft_logic:validate_result(<<"json string">>)).

%%%===================================================================
%%% 宽松校验 relaxed_schema（仅非生产环境生效）
%%%===================================================================

%% 素材不合格（如测试视频是桌面场景）时模型会**诚实拒答**：
%% positive_point:null + needs_human_check:true + 空 evidence_moments。
%% 严格校验下这必然 bad_output，开发环境因此永远看不到终态。
%% relaxed 让草稿落地并保留「需人工复核」标记——流程可跑通，不假装质量达标。
%% 整体包 with_env(<<"local">>)：imboy_env:current() 在 IMBOYENV 未设时
%% fail-safe 默认 <<"prod">>，不显式钉住环境的话，裸 shell / CI 跑 eunit
%% 会让宽松守卫恒为 false，本用例假红成 {error, bad_output}。
relaxed_accepts_honest_refusal_test() ->
    Refusal = (valid_result())#{
        <<"positive_point">> => null,
        <<"needs_human_check">> => true,
        <<"evidence_moments">> => []
    },
    with_env(<<"local">>, fun() ->
        %% 基线：不开开关时仍严格判负（默认行为不变）。
        %% with_strict 显式摘掉开关：sys.local.config 可能带 true（逐机漂移），
        %% 不钉住的话「基线」实际在宽松口径下跑，假红成 {ok, _}。
        with_strict(fun() ->
            ?assertEqual({error, bad_output}, moya_ai_draft_logic:validate_result(Refusal))
        end),
        with_relaxed(fun() ->
            {ok, W} = moya_ai_draft_logic:validate_result(Refusal),
            ?assertEqual(true, maps:get(<<"needs_human_check">>, W)),
            ?assertEqual([], maps:get(<<"evidence_moments">>, W)),
            %% 占位文本而非空串：老师侧要能看出「这项 AI 没给出来」
            ?assert(byte_size(maps:get(<<"positive_point">>, W)) > 0)
        end)
    end).

%% 缺 / 畸形的 needs_human_check 在 relaxed 下保守取 true（要人工看），
%% 绝不能默认成 false 让不合格草稿被当成可信产出。
relaxed_missing_check_flag_is_conservative_test() ->
    Base = valid_result(),
    with_env(<<"local">>, fun() ->
        [
            begin
                Input = maps:remove(<<"needs_human_check">>, Base#{Key => Bad}),
                with_relaxed(fun() ->
                    {ok, W} = moya_ai_draft_logic:validate_result(Input),
                    ?assertEqual(true, maps:get(<<"needs_human_check">>, W))
                end)
            end
         || {Key, Bad} <- [{<<"positive_point">>, null}, {<<"evidence_moments">>, []}]
        ]
    end).

%% 最重要的守卫：配置被误带到生产时，prod 环境恒走严格校验。
relaxed_never_applies_in_prod_test() ->
    Refusal = (valid_result())#{
        <<"positive_point">> => null,
        <<"evidence_moments">> => []
    },
    with_relaxed(fun() ->
        with_env(<<"prod">>, fun() ->
            ?assertEqual(
                {error, bad_output},
                moya_ai_draft_logic:validate_result(Refusal)
            )
        end)
    end),
    %% 反向自证：非 prod 下同一份输入确实被放宽（否则上面那条可能是假绿）
    with_relaxed(fun() ->
        with_env(<<"local">>, fun() ->
            ?assertMatch({ok, _}, moya_ai_draft_logic:validate_result(Refusal))
        end)
    end).

validate_test_() ->
    [
        {"validate rebuilds whitelist dropping chain-of-thought", fun validate_ok_whitelist_test/0},
        {"validate drops out-of-range confidence", fun validate_confidence_out_of_range_test/0},
        {"validate evidence_moments bounds", fun validate_moments_bounds_test/0},
        {"validate script_outline bounds", fun validate_outline_bounds_test/0},
        {"validate text field bounds", fun validate_text_fields_test/0},
        {"validate needs_human_check must be boolean", fun validate_needs_human_check_test/0},
        {"validate rejects non-map", fun validate_non_map_test/0},
        {"relaxed accepts honest refusal with placeholder",
            fun relaxed_accepts_honest_refusal_test/0},
        {"relaxed missing check flag is conservative",
            fun relaxed_missing_check_flag_is_conservative_test/0},
        {"relaxed never applies in prod", fun relaxed_never_applies_in_prod_test/0}
    ].

%%%===================================================================
%%% Internal
%%%===================================================================

with_relaxed(Fun) ->
    Old = application:get_env(imboy, teaching_ai_relaxed_schema),
    application:set_env(imboy, teaching_ai_relaxed_schema, true),
    try
        Fun()
    after
        case Old of
            undefined -> application:unset_env(imboy, teaching_ai_relaxed_schema);
            {ok, V} -> application:set_env(imboy, teaching_ai_relaxed_schema, V)
        end
    end.

%% 严格用例的对称保护：sys.local.config（gitignored 逐机文件）可能带
%% {teaching_ai_relaxed_schema, true}，eunit-local 加载后宽松口径会污染
%% 「坏输入必须判负」的基线断言（2026-09-19 实证 5 例假红）。显式摘掉开关。
with_strict(Fun) ->
    Old = application:get_env(imboy, teaching_ai_relaxed_schema),
    application:unset_env(imboy, teaching_ai_relaxed_schema),
    try
        Fun()
    after
        case Old of
            undefined -> application:unset_env(imboy, teaching_ai_relaxed_schema);
            {ok, V} -> application:set_env(imboy, teaching_ai_relaxed_schema, V)
        end
    end.

%% imboy_env:current/0 优先读 OS env IMBOYENV，故两侧都要设，
%% 否则「设了 app env 却不生效」会让 prod 守卫的测试变成假绿。
with_env(Bin, Fun) when is_binary(Bin) ->
    OldOs = os:getenv("IMBOYENV"),
    OldApp = application:get_env(imboy, env),
    os:putenv("IMBOYENV", binary_to_list(Bin)),
    application:set_env(imboy, env, Bin),
    try
        Fun()
    after
        case OldOs of
            false -> os:unsetenv("IMBOYENV");
            V -> os:putenv("IMBOYENV", V)
        end,
        case OldApp of
            undefined -> application:unset_env(imboy, env);
            {ok, A} -> application:set_env(imboy, env, A)
        end
    end.

meta() ->
    #{
        submission_id => 970001,
        prompt_version => <<"p-2026-09-09.1">>,
        rubric_version => <<"r-hardpen-1">>
    }.

attachment() ->
    #{
        <<"id">> => 978001,
        <<"path">> => <<"p/978001">>,
        <<"mime_type">> => <<"video/mp4">>,
        <<"size">> => 1024
    }.

%%%===================================================================
%%% OpenAI 兼容多模态 provider 接入
%%%===================================================================

%% provider 条目显式 vision=true 可越过模块级 capabilities=false
%% （imboy_llm_openai 同模块多 provider，模块级能力声明无法区分端点）
opts_vision_override_test_() ->
    ?WITH_MECKS(
        [
            {config_ds, [
                {'env', 2, fun
                    (teaching_ai_llm_provider, _) -> ?PROVIDER;
                    (_, Default) -> Default
                end}
            ]},
            {imboy_llm_registry, [
                {'lookup', 1, fun(_) ->
                    {ok, #{
                        module => ?FAKE_MOD,
                        opts => #{api_key => <<"k">>, vision => true}
                    }}
                end}
            ]},
            {?FAKE_MOD, [
                {'capabilities', 0, fun() -> #{vision => false} end},
                {'chat', 3, fun(_Uid, _Messages, _Opts) ->
                    {ok, #{<<"content">> => jsone:encode(valid_result())}}
                end}
            ]}
        ],
        fun() ->
            {ok, Result} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
            ?assertEqual(whitelist_keys(), lists:sort(maps:keys(Result)))
        end
    ).

%% 附件带可达 url → user content 升级为视频段数组（video_url + text）
video_url_messages_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Url = <<"https://cdn.bigmodel.cn/agent-demos/lark/113123.mov">>,
        Attachment = attachment(),
        {ok, _} = moya_ai_draft_logic:analyze_video(meta(), Attachment#{<<"url">> => Url}),
        Messages = get(captured_messages),
        [#{<<"role">> := <<"system">>}, #{<<"role">> := <<"user">>, <<"content">> := Segments}] =
            Messages,
        ?assert(is_list(Segments)),
        ?assertMatch(
            [#{<<"type">> := <<"video_url">>}, #{<<"type">> := <<"text">>}],
            Segments
        ),
        [VideoSeg, _] = Segments,
        #{<<"video_url">> := #{<<"url">> := GotUrl}} = VideoSeg,
        ?assertEqual(Url, GotUrl)
    end).

%% 附件无 url（骨架路径）→ user content 维持纯文本，行为不变
no_url_keeps_text_messages_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        {ok, _} = moya_ai_draft_logic:analyze_video(meta(), attachment()),
        [_, #{<<"role">> := <<"user">>, <<"content">> := Content}] = get(captured_messages),
        ?assert(is_binary(Content))
    end).
