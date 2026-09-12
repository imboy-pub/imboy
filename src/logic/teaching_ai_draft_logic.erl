-module(teaching_ai_draft_logic).
%%%
% 墨芽 AI 回课「点评草稿」产出（Step 11）
% Produces the AI review draft for a calligraphy submission
%
% 职责（本模块 = 领域契约 + 用例编排；外部厂商调用在 imboy_llm_* 那一层）：
%   1) 领域契约：output_schema 白名单重建、思维链/包装脱壳、char_reviews 校验
%   2) 用例编排：取 provider → 组装消息 → 调用 → 校验 → 降级
%   3) 防腐：按名字查 registry、能力/密钥判定，把外部失败折叠成稳定错误
% 命名说明（2026-09-13）：原名 teaching_ai_provider 与本仓「provider = 厂商端点
%   抽象」（llm_providers 配置 / imboy_llm_registry / imboy_llm_openai 等实现体）
%   撞词——本模块是**消费** provider 的一方，且配置键 teaching_ai_llm_provider
%   已占用「用哪个 provider」之语义。改为领域名词「草稿」（对应表
%   calligraphy_review_draft）+ 层后缀，与 teaching_X_logic 家族一致。
%
% 降级口径（AI-03）：
%   本模块把「provider 名未配置 / registry 未命中 / vision 声明缺失 / api_key
%   为空」统一折叠为 {error, provider_unavailable}：Worker 收到即 status=failed
%   + 明确降级老师人工队列（闭环不破）。换 provider / 换模型无需改动 Worker。
%   （原文写「vision 全 false 致多模态被 BLOCKED_EXTERNAL 阻塞」——已于 2026-09-11
%   接入视觉 provider 后失效；2026-09-12 模型换 glm-5.3-flash。）
%
% 输出契约（STEP-04 AiDraftResult，无思维链）：
%   仅白名单键落库：positive_point / focus_problem / evidence_moments /
%   practice_action / script_outline / needs_human_check / char_reviews
%   [/ confidence]
%   —— 模型返回的任何其他键（含思维链片段）在 validate 时被结构性丢弃。
%   char_reviews = Phase B 识别制逐字点评；与三段文本不同，整体畸形时降级 null
%   而非判整个点评失败（字卡是增量补充）。
%%%

-export([analyze_video/2, validate_result/1]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 视频回课分析入口（Worker 调用）
%% 入参：DraftMeta = #{submission_id, prompt_version, rubric_version}
%%       Attachment = #{id, path, mime_type, size}
%% 出参：{ok, WhitelistedResultMap} | {error, Reason}
%% Reason：provider_unavailable（无配置/无key/vision=false——AI-03 主降级路径）
%%         | timeout | provider_error | bad_output（非法 JSON/Schema 失败）
-spec analyze_video(map(), map()) -> {ok, map()} | {error, atom()}.
analyze_video(DraftMeta, Attachment) ->
    ProviderName = config_ds:env(teaching_ai_llm_provider, undefined),
    case resolve_provider(ProviderName) of
        {error, provider_unavailable} = E ->
            E;
        {ok, Mod, Opts} ->
            call_provider(Mod, Opts, DraftMeta, Attachment)
    end.

%% @doc 结构化结果校验 + 白名单重建（半成品/思维链不落库）
-spec validate_result(term()) -> {ok, map()} | {error, bad_output}.
validate_result(Result) when is_map(Result) ->
    try
        Positive = req_text(Result, <<"positive_point">>, 300),
        Focus = req_text(Result, <<"focus_problem">>, 300),
        Practice = req_text(Result, <<"practice_action">>, 300),
        Moments = req_moments(Result),
        Outline = req_outline(Result),
        NeedsCheck =
            case maps:get(<<"needs_human_check">>, Result) of
                B when is_boolean(B) -> B;
                _ -> throw(bad)
            end,
        Whitelist = #{
            <<"positive_point">> => Positive,
            <<"focus_problem">> => Focus,
            <<"practice_action">> => Practice,
            <<"evidence_moments">> => Moments,
            <<"script_outline">> => Outline,
            <<"needs_human_check">> => NeedsCheck,
            <<"char_reviews">> => char_reviews(Result)
        },
        {ok, maybe_confidence(Result, Whitelist)}
    catch
        _:_ -> {error, bad_output}
    end;
validate_result(_) ->
    {error, bad_output}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec resolve_provider(undefined | binary()) ->
    {ok, module(), map()} | {error, provider_unavailable}.
resolve_provider(undefined) ->
    {error, provider_unavailable};
resolve_provider(Name) when is_binary(Name) ->
    case imboy_llm_registry:lookup(Name) of
        {ok, #{module := Mod, opts := Opts}} ->
            ApiKey = maps:get(api_key, Opts, <<>>),
            Capabilities =
                try
                    Mod:capabilities()
                catch
                    _:_ -> undefined
                end,
            VisionOK =
                case Capabilities of
                    #{vision := true} -> true;
                    %% provider 条目可显式声明 vision（如 OpenAI 兼容多模态端点
                    %% glm-4.6v-flash；模块级 capabilities 无法区分同模块 provider）
                    _ -> maps:get(vision, Opts, false) =:= true
                end,
            HasKey = is_binary(ApiKey) andalso ApiKey =/= <<>>,
            case {VisionOK, HasKey} of
                {true, true} ->
                    {ok, Mod, Opts};
                _ ->
                    %% vision=false / 无 key：统一明确降级（不重试——AI-03）
                    ?INFO_LOG(["teaching_ai_draft_logic unavailable: vision/key missing"]),
                    {error, provider_unavailable}
            end;
        undefined ->
            {error, provider_unavailable}
    end.

-spec call_provider(module(), map(), map(), map()) -> {ok, map()} | {error, atom()}.
call_provider(Mod, Opts, DraftMeta, Attachment) ->
    Messages = build_messages(DraftMeta, Attachment),
    WorkerUid = 0,
    ChatRes =
        try
            Mod:chat(WorkerUid, Messages, Opts)
        catch
            _:CrashReason -> {'EXIT', CrashReason}
        end,
    case ChatRes of
        {ok, Resp} ->
            decode_response(Resp);
        {error, timeout} ->
            {error, timeout};
        {error, Reason} ->
            ?LOG_WARNING("teaching_ai_draft_logic chat error ~p", [Reason]),
            {error, provider_error};
        {'EXIT', _} ->
            {error, provider_error};
        _ ->
            {error, provider_error}
    end.

%% 提取 content 并解析 JSON。模型输出先经 json_body/1 脱壳（思维链块 / 结果框标记 /
%% Markdown 围栏 / 前置说明文字），再交 validate_result 白名单重建。
-spec decode_response(map()) -> {ok, map()} | {error, atom()}.
decode_response(Resp) ->
    Content =
        case maps:get(<<"content">>, Resp, undefined) of
            C when is_binary(C) -> C;
            _ -> maps:get(<<"result">>, Resp, undefined)
        end,
    case Content of
        Bin when is_binary(Bin), Bin =/= <<>> ->
            case
                try
                    jsone:decode(json_body(Bin), [{object_format, map}])
                catch
                    _:_ -> bad_json
                end
            of
                Json when is_map(Json) ->
                    validate_result(Json);
                _ ->
                    {error, bad_output}
            end;
        _ ->
            {error, bad_output}
    end.

%% ---- 模型输出规范化 ----
%% 各模型用自己的包装把 JSON 裹起来，解析前先脱壳：思维链块（推理型模型内联输出）、
%% 智谱结果框标记、Markdown 代码围栏。脱壳后仍非裸对象时，再退一步取最外层 {...}
%%（前置一句「分析如下：」之类）。只脱壳、不修补内容——内容本身畸形仍交解码器判失败。

-spec json_body(binary()) -> binary().
json_body(Bin) ->
    Trimmed = string:trim(unwrap(Bin)),
    case is_object(Trimmed) of
        true -> Trimmed;
        false -> outermost_object(Trimmed)
    end.

-spec unwrap(binary()) -> binary().
unwrap(Bin) ->
    Unfenced = strip_fence(strip_think(Bin)),
    binary:replace(
        binary:replace(Unfenced, <<"<|begin_of_box|>">>, <<>>, [global]),
        <<"<|end_of_box|>">>,
        <<>>,
        [global]
    ).

%% 思维链：有闭合标签时只保留最后一个 </think> 之后（可能多段）；无闭合说明被
%% max_tokens 截断，此时 think 之后的内容一并丢弃。
-spec strip_think(binary()) -> binary().
strip_think(Bin) ->
    case binary:split(Bin, <<"</think>">>, [global]) of
        [_] ->
            case binary:split(Bin, <<"<think>">>) of
                [Before, _Rest] -> Before;
                [_] -> Bin
            end;
        Parts ->
            lists:last(Parts)
    end.

-spec strip_fence(binary()) -> binary().
strip_fence(Bin) ->
    Trimmed = string:trim(Bin),
    case Trimmed of
        <<"```", Rest/binary>> ->
            case binary:split(Rest, <<"\n">>) of
                [_Lang, Body] -> strip_fence(Body);
                _ -> Trimmed
            end;
        _ ->
            case binary:matches(Trimmed, <<"```">>) of
                [] ->
                    Trimmed;
                Matches ->
                    {Pos, _} = lists:last(Matches),
                    binary:part(Trimmed, 0, Pos)
            end
    end.

-spec is_object(binary()) -> boolean().
is_object(<<"{", Rest/binary>>) ->
    case binary:last(Rest) of
        $} -> true;
        _ -> false
    end;
is_object(_) ->
    false.

-spec outermost_object(binary()) -> binary().
outermost_object(Bin) ->
    case {binary:match(Bin, <<"{">>), binary:matches(Bin, <<"}">>)} of
        {{Start, _}, [_ | _] = Matches} ->
            {End, _} = lists:last(Matches),
            case End > Start of
                true -> binary:part(Bin, Start, End - Start + 1);
                false -> Bin
            end;
        _ ->
            Bin
    end.

-spec build_messages(map(), map()) -> [map()].
build_messages(DraftMeta, Attachment) ->
    %% 版本兜底：draft 行入队时这两列写的是空串
    %% （teaching_submission_repo:enqueue_ai_draft_tx），键恒存在 → maps:get/3 的
    %% 默认值取不到。显式把空串回落到当前版本，否则模型收到的版本恒为空、
    %% prompt 演进无从回溯。
    %% p-2026-09-12.1：output_schema 增 char_reviews（Phase B 识别制逐字点评）。
    Rubric = version_or(maps:get(rubric_version, DraftMeta, <<>>), <<"r-hardpen-1">>),
    Prompt = version_or(maps:get(prompt_version, DraftMeta, <<>>), <<"p-2026-09-12.1">>),
    Task = jsone:encode(#{
        <<"task">> => <<"calligraphy_video_review"/utf8>>,
        <<"rubric_version">> => Rubric,
        <<"prompt_version">> => Prompt,
        <<"attachment">> => #{
            <<"object_key">> => maps:get(<<"path">>, Attachment, <<>>),
            <<"mime_type">> => maps:get(<<"mime_type">>, Attachment, <<>>)
        },
        <<"output_schema">> => <<
            "positive_point/focus_problem/evidence_moments/"
            "practice_action/script_outline/needs_human_check/confidence/"
            "char_reviews"/utf8
        >>
    }),
    System = #{
        <<"role">> => <<"system">>,
        <<"content">> => <<"你是书法老师的教学助手。只输出 JSON，不输出推理过程。"/utf8>>
    },
    %% 视频段走 video_url 通道（智谱等 OpenAI 兼容多模态端点）：要求 Attachment
    %% 带「模型侧可抓取」的 url（presign/公网直链）。
    %% ⚠️ 现状（2026-09-12 实查）：调用方 teaching_ai_worker:load_attachment/2 返回的
    %% map 只有 id/path/mime_type/size，**没有 url** → 生产路径恒走下面的纯文本分支，
    %% 模型拿不到视频（内容盲）。provider 侧已就绪，缺的是调用方接线；
    %% 详见记忆 teaching-ai-review-inert-two-gaps-2026-09-12。
    User =
        case maps:get(<<"url">>, Attachment, <<>>) of
            Url when is_binary(Url), byte_size(Url) > 0 ->
                Instruction = <<
                    "请观看视频中的书写过程，依据上述 output_schema 输出 JSON 点评；"
                    "evidence_moments 为视频内秒级时间点（0-5 个）。"
                    "char_reviews 为逐字点评数组（识别制）：从画面识别出所写的每个字，"
                    "每项 {\"index\":序号(从0起),\"char\":\"单字\",\"grade\":\"good|fair|poor\","
                    "\"comment\":\"逐字点评\"}，最多 50 项；识别不出逐字内容时给 []。"/utf8
                >>,
                #{
                    <<"role">> => <<"user">>,
                    <<"content">> => [
                        #{<<"type">> => <<"video_url">>, <<"video_url">> => #{<<"url">> => Url}},
                        #{
                            <<"type">> => <<"text">>,
                            <<"text">> => <<Task/binary, "\n"/utf8, Instruction/binary>>
                        }
                    ]
                };
            _ ->
                #{<<"role">> => <<"user">>, <<"content">> => Task}
        end,
    [System, User].

%% ---- Schema 校验小工具 ----

-spec req_text(map(), binary(), integer()) -> binary().
req_text(Result, Key, Max) ->
    case maps:get(Key, Result) of
        B when is_binary(B), byte_size(B) > 0, byte_size(B) =< Max -> B;
        _ -> throw(bad)
    end.

-spec req_moments(map()) -> [number()].
req_moments(Result) ->
    case maps:get(<<"evidence_moments">>, Result) of
        L when is_list(L), length(L) >= 1, length(L) =< 5 ->
            lists:foreach(
                fun(N) ->
                    case is_number(N) andalso N >= 0 of
                        true -> ok;
                        false -> throw(bad)
                    end
                end,
                L
            ),
            L;
        _ ->
            throw(bad)
    end.

-spec req_outline(map()) -> [binary()].
req_outline(Result) ->
    case maps:get(<<"script_outline">>, Result) of
        L when is_list(L), length(L) =< 3 ->
            lists:foreach(
                fun(B) ->
                    case is_binary(B) andalso byte_size(B) =< 200 of
                        true -> ok;
                        false -> throw(bad)
                    end
                end,
                L
            ),
            L;
        _ ->
            throw(bad)
    end.

-spec maybe_confidence(map(), map()) -> map().
maybe_confidence(Result, Acc) ->
    case maps:get(<<"confidence">>, Result, undefined) of
        N when is_number(N), N >= 0, N =< 1 ->
            Acc#{<<"confidence">> => N};
        _ ->
            Acc
    end.

%% 逐字点评字卡（Phase B 识别制）：AI 从画面识别所写字并逐字点评。
%% 校验规则与老师保存草稿同源（teaching_review_logic:parse_char_reviews/1），
%% 避免两套白名单漂移（单项越界丢弃、空数组归一 null、上限 50 项）。
%% 字卡是增量补充，故与三段文本处置不同：整体畸形时降级为 null，而不是判整个
%% 点评失败——模型偶尔把数组写成字符串，不该让一次回课白跑（三段文本缺失才是
%% 真的没法用，那条仍走 req_text throw）。键恒存在，无逐字数据时显式为 null。
-spec char_reviews(map()) -> [map()] | null.
char_reviews(Result) ->
    try teaching_review_logic:parse_char_reviews(Result) of
        {ok, CharReviews} -> CharReviews;
        {error, _} -> null
    catch
        _:_ -> null
    end.

-spec version_or(term(), binary()) -> binary().
version_or(<<>>, Default) ->
    Default;
version_or(V, _Default) when is_binary(V) ->
    V;
version_or(_, Default) ->
    Default.
