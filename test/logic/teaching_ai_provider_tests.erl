%% teaching_ai_provider_tests
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
%% test/repo/teaching_ai_worker_tests.erl（真库 4323 scratch）。

-module(teaching_ai_provider_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(PROVIDER, <<"moya-fake">>).
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
                            module => ?FAKE_MOD, opts => #{api_key => <<"test-key-placeholder">>}
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
        {ok, Result} = teaching_ai_provider:analyze_video(meta(), attachment()),
        ?assertEqual(whitelist_keys(), lists:sort(maps:keys(Result))),
        ?assertEqual(0.87, maps:get(<<"confidence">>, Result)),
        ?assertEqual(false, maps:get(<<"needs_human_check">>, Result)),
        ?assertEqual([1.5, 8.2, 20.0], maps:get(<<"evidence_moments">>, Result)),
        %% 思维链/原文键绝不透传（AI-02）
        ?assertEqual(false, maps:is_key(<<"reasoning">>, Result)),
        ?assertEqual(false, maps:is_key(<<"raw_transcript">>, Result))
    end).

%% 超时：{error, timeout}（Worker 侧属瞬时错误可重试）
ai01_timeout_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {error, timeout}),
        try
            ?assertEqual(
                {error, timeout},
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%% 空 content / 无 content 无 result → bad_output
ai01_empty_content_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        _ = put(fake_chat, {ok, #{<<"content">> => <<>>}}),
        R1 = teaching_ai_provider:analyze_video(meta(), attachment()),
        _ = put(fake_chat, {ok, #{}}),
        R2 = teaching_ai_provider:analyze_video(meta(), attachment()),
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

%% 思维链块 + 裸 JSON（glm-4.1v-thinking-flash 内联 <think>）
ai01_think_block_wrapped_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"<think>先看整体，再看逐帧笔锋</think>\n"/utf8, (jsone:encode(valid_result()))/binary>>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            {ok, W} = teaching_ai_provider:analyze_video(meta(), attachment()),
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
            ?assertMatch({ok, _}, teaching_ai_provider:analyze_video(meta(), attachment()))
        after
            erase(fake_chat)
        end
    end).

%% Markdown 代码围栏（glm-4.6v-flashx 输出 ```json … ```）
ai01_fenced_json_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Content = <<"```json\n", (jsone:encode(valid_result()))/binary, "\n```">>,
        _ = put(fake_chat, {ok, #{<<"content">> => Content}}),
        try
            ?assertMatch({ok, _}, teaching_ai_provider:analyze_video(meta(), attachment()))
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
            ?assertMatch({ok, _}, teaching_ai_provider:analyze_video(meta(), attachment()))
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
                teaching_ai_provider:analyze_video(meta(), attachment())
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
                teaching_ai_provider:analyze_video(meta(), attachment())
            )
        after
            erase(fake_chat)
        end
    end).

%%%===================================================================
%%% validate_result 直测（Schema 边界，无 meck）
%%%===================================================================

validate_ok_whitelist_test() ->
    {ok, W} = teaching_ai_provider:validate_result(valid_result()),
    ?assertEqual(whitelist_keys(), lists:sort(maps:keys(W))).

validate_confidence_out_of_range_test() ->
    Base = valid_result(),
    Input = Base#{<<"confidence">> => 1.5},
    {ok, W} = teaching_ai_provider:validate_result(Input),
    %% 越界 confidence 丢弃，其余白名单保留
    ?assertEqual(false, maps:is_key(<<"confidence">>, W)),
    ?assertEqual(6, maps:size(W)).

validate_moments_bounds_test() ->
    Base = valid_result(),
    %% 空 / 超过 5 项 / 负值 / 非数值 → bad_output
    [
        begin
            BadInput = Base#{<<"evidence_moments">> => Bad},
            ?assertEqual(
                {error, bad_output},
                teaching_ai_provider:validate_result(BadInput)
            )
        end
     || Bad <- [[], [1, 2, 3, 4, 5, 6], [-0.1, 2], [<<"1.5">>]]
    ],
    %% 恰 1 项 / 恰 5 项合法
    One = Base#{<<"evidence_moments">> => [0]},
    {ok, _} = teaching_ai_provider:validate_result(One),
    Five = Base#{<<"evidence_moments">> => [1, 2, 3, 4, 5]},
    {ok, _} = teaching_ai_provider:validate_result(Five).

validate_outline_bounds_test() ->
    Base = valid_result(),
    %% 超过 3 项 / 单项超 200 字节 / 非二进制 → bad_output
    Long = binary:copy(<<"a">>, 201),
    [
        begin
            BadInput = Base#{<<"script_outline">> => Bad},
            ?assertEqual(
                {error, bad_output},
                teaching_ai_provider:validate_result(BadInput)
            )
        end
     || Bad <- [[<<"a">>, <<"b">>, <<"c">>, <<"d">>], [Long], [1, 2]]
    ].

validate_text_fields_test() ->
    Base = valid_result(),
    %% 必填文本：缺失 / 空 / 非二进制 / 超 300 字节 → bad_output
    Long = binary:copy(<<"字"/utf8>>, 151),
    [
        begin
            BadInput = Base#{<<"positive_point">> => Bad},
            ?assertEqual(
                {error, bad_output},
                teaching_ai_provider:validate_result(BadInput)
            )
        end
     || Bad <- [undefined, <<>>, 123, Long]
    ].

validate_needs_human_check_test() ->
    Base = valid_result(),
    BadInput = Base#{<<"needs_human_check">> => <<"yes">>},
    ?assertEqual(
        {error, bad_output},
        teaching_ai_provider:validate_result(BadInput)
    ).

validate_non_map_test() ->
    ?assertEqual({error, bad_output}, teaching_ai_provider:validate_result([valid_result()])),
    ?assertEqual({error, bad_output}, teaching_ai_provider:validate_result(<<"json string">>)).

validate_test_() ->
    [
        {"validate rebuilds whitelist dropping chain-of-thought", fun validate_ok_whitelist_test/0},
        {"validate drops out-of-range confidence", fun validate_confidence_out_of_range_test/0},
        {"validate evidence_moments bounds", fun validate_moments_bounds_test/0},
        {"validate script_outline bounds", fun validate_outline_bounds_test/0},
        {"validate text field bounds", fun validate_text_fields_test/0},
        {"validate needs_human_check must be boolean", fun validate_needs_human_check_test/0},
        {"validate rejects non-map", fun validate_non_map_test/0}
    ].

%%%===================================================================
%%% Internal
%%%===================================================================

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
%%% Vision provider 接入（GLM-4.6V-Flash 等 OpenAI 兼容多模态端点）
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
            {ok, Result} = teaching_ai_provider:analyze_video(meta(), attachment()),
            ?assertEqual(whitelist_keys(), lists:sort(maps:keys(Result)))
        end
    ).

%% 附件带可达 url → user content 升级为视频段数组（video_url + text）
video_url_messages_test_() ->
    ?WITH_MECKS(provider_mocks(), fun() ->
        Url = <<"https://cdn.bigmodel.cn/agent-demos/lark/113123.mov">>,
        Attachment = attachment(),
        {ok, _} = teaching_ai_provider:analyze_video(meta(), Attachment#{<<"url">> => Url}),
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
        {ok, _} = teaching_ai_provider:analyze_video(meta(), attachment()),
        [_, #{<<"role">> := <<"user">>, <<"content">> := Content}] = get(captured_messages),
        ?assert(is_binary(Content))
    end).
