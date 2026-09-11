-module(teaching_ai_provider).
%%%
% 墨芽书法 AI 回课 provider 包装（Step 11）
% Wraps imboy_llm registry for teaching video review
%
% 现实约束（AI-03 / BLOCKED_EXTERNAL）：
%   imboy_llm 现有 provider capabilities().vision 全部 = false —— 真实多模态
%   调用被外部条件阻塞。本模块把「无 provider / 无 key / vision=false」统一折叠为
%   {error, provider_unavailable}：Worker 收到即 status=failed + 明确降级老师人工
%   队列（闭环不破）。真实 vision provider 接入后无需改动 Worker。
%
% 输出契约（STEP-04 AiDraftResult，无思维链）：
%   仅白名单键落库：positive_point / focus_problem / evidence_moments /
%   practice_action / script_outline / needs_human_check [/ confidence]
%   —— 模型返回的任何其他键（含思维链片段）在 validate 时被结构性丢弃。
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
            <<"needs_human_check">> => NeedsCheck
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
                    ?INFO_LOG(["teaching_ai_provider unavailable: vision/key missing"]),
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
            ?LOG_WARNING("teaching_ai_provider chat error ~p", [Reason]),
            {error, provider_error};
        {'EXIT', _} ->
            {error, provider_error};
        _ ->
            {error, provider_error}
    end.

%% 提取 content 并解析 JSON（模型以文本返回 JSON；多模态帧引用 BLOCKED_EXTERNAL 留待
%% vision provider 接入，骨架阶段 prompt 只携带业务元数据与附件 object_key 引用）
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
                    jsone:decode(Bin, [{object_format, map}])
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

-spec build_messages(map(), map()) -> [map()].
build_messages(DraftMeta, Attachment) ->
    Rubric = maps:get(rubric_version, DraftMeta, <<"r-hardpen-1">>),
    Prompt = maps:get(prompt_version, DraftMeta, <<"p-2026-09-09.1">>),
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
            "practice_action/script_outline/needs_human_check/confidence"/utf8
        >>
    }),
    System = #{
        <<"role">> => <<"system">>,
        <<"content">> => <<"你是书法老师的教学助手。只输出 JSON，不输出推理过程。"/utf8>>
    },
    %% 视频可达 URL（presign/公网直链）：多模态段 + 任务文本（GLM-4.6V 等
    %% video_url 通道）。仅 object_key 引用（骨架路径）维持纯文本，行为不变。
    User =
        case maps:get(<<"url">>, Attachment, <<>>) of
            Url when is_binary(Url), byte_size(Url) > 0 ->
                Instruction = <<
                    "请观看视频中的书写过程，依据上述 output_schema 输出 JSON 点评；"
                    "evidence_moments 为视频内秒级时间点（0-5 个）。"/utf8
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
