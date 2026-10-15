%%% @doc 智能客服的 LLM 消息构建（纯函数：不调模型、不读配置以外的事实）。
%%%
%%% 三件事，全部可单测：
%%%   * **多轮上下文**：把会话历史裁成模型可用的消息序列（轮数 + 字节双上限，
%%%     防止长会话把 token 预算吃光——这是"多轮对话"在生产里唯一会炸的地方）；
%%%   * **意图判定 prompt**：要求**严格 JSON**（`cs_ai_intent:parse_llm/1` 是它的
%%%     解析侧，两者共用值域）；
%%%   * **回复 prompt**：把 FAQ 检索结果作为**受控证据**注入（明确告知模型只能
%%%     依据证据作答、无依据时必须声明未解决），这是"幻觉"与"该转人工"的分界。
%%%
%%% 与 legacy `ai_agent_prompt` 的差别：那边是"通识助手"，本模块是"客服坐席"——
%%% 语气、边界（不承诺赔付、不发散）与 JSON 契约都不同，故不复用而是各司其职。
-module(cs_ai_prompt).

-moduledoc "智能客服 LLM 消息构建（纯函数）：多轮裁剪 + 意图 JSON + 受控证据注入。".

-export([
    build_classify_messages/3,
    build_reply_messages/4,
    trim_history/2,
    default_history_limit/0,
    default_history_bytes/0
]).

%% 历史裁剪：最多 8 轮（16 条消息）、最多 4000 字节 —— 双上限取先到者。
-define(HISTORY_LIMIT, 8).
-define(HISTORY_BYTES, 4000).

-type message() :: #{binary() => binary()}.
-export_type([message/0]).

-spec default_history_limit() -> pos_integer().
default_history_limit() -> ?HISTORY_LIMIT.

-spec default_history_bytes() -> pos_integer().
default_history_bytes() -> ?HISTORY_BYTES.

%% ===================================================================
%% 多轮上下文裁剪
%% ===================================================================

%% @doc 从会话历史（**时间升序**的 `#{role, content}`）取最近若干轮。
%%
%% 两条上限同时生效：先按轮数截断，再从**最新**往旧的方向累积字节，超限即停。
%% 返回仍是升序（模型按时间顺序读）。空正文的历史条目被丢弃——空消息会让部分
%% provider 直接 400。
-spec trim_history([message()], map()) -> [message()].
trim_history(History, Opts) when is_list(History) ->
    Limit = pos_int(maps:get(history_limit, Opts, ?HISTORY_LIMIT), ?HISTORY_LIMIT),
    ByteCap = pos_int(maps:get(history_bytes, Opts, ?HISTORY_BYTES), ?HISTORY_BYTES),
    Clean = lists:filtermap(fun clean_entry/1, History),
    ByCount = lists:nthtail(max(0, length(Clean) - Limit * 2), Clean),
    %% 从最新往旧累积字节，超限即停；结果仍按时间升序（模型按时间顺序读）
    fit_bytes(lists:reverse(ByCount), ByteCap, 0, []);
trim_history(_, _) ->
    [].

%% 空正文的历史条目丢弃——空消息会让部分 provider 直接 400。
clean_entry(M) when is_map(M) ->
    case maps:get(<<"content">>, M, <<>>) of
        C when is_binary(C), C =/= <<>> ->
            {true, #{
                <<"role">> => norm_role(maps:get(<<"role">>, M, <<"user">>)),
                <<"content">> => C
            }};
        _ ->
            false
    end;
clean_entry(_) ->
    false.

fit_bytes([], _Cap, _Acc, Kept) ->
    Kept;
fit_bytes([#{<<"content">> := C} = M | Rest], Cap, Acc, Kept) ->
    Size = byte_size(C),
    case Acc + Size > Cap andalso Kept =/= [] of
        true -> Kept;
        false -> fit_bytes(Rest, Cap, Acc + Size, [M | Kept])
    end.

norm_role(<<"assistant">>) -> <<"assistant">>;
norm_role(<<"system">>) -> <<"system">>;
norm_role(_) -> <<"user">>.

%% ===================================================================
%% 意图判定：要求严格 JSON
%% ===================================================================

%% @doc 意图分类消息。
%%
%% 值域与 `cs_ai_intent:parse_llm/1` **同源**（本函数只把它写进 prompt 文本，
%% 避免两处各写一份枚举而漂移）。
-spec build_classify_messages(binary(), [message()], binary()) -> [message()].
build_classify_messages(SystemPrompt, History, Text) ->
    System = iolist_to_binary([
        <<"你是客服工单分类器。只输出一个 JSON 对象，不要输出任何解释或代码围栏。\n"/utf8>>,
        <<"字段：intent（必须取自 refund|order|tech|other）、"/utf8>>,
        <<"confidence（0 到 1 的小数）、resolved（布尔：仅凭你能确定的信息能否解决客户问题）。\n"/utf8>>,
        <<"intent 含义：refund=退款退货、order=订单物流、tech=技术故障、other=其他。\n"/utf8>>,
        case SystemPrompt of
            <<>> -> <<>>;
            _ -> <<"补充业务背景："/utf8, SystemPrompt/binary, "\n">>
        end
    ]),
    [#{<<"role">> => <<"system">>, <<"content">> => System}] ++
        History ++
        [#{<<"role">> => <<"user">>, <<"content">> => Text}].

%% ===================================================================
%% 回复生成：受控证据注入
%% ===================================================================

%% @doc 回复生成消息。
%%
%% `Docs` = FAQ 命中条目（`#{question, answer}`，已按融合分降序）。它们是
%% **受控证据**：prompt 明确要求"只能依据以下资料作答，资料不足必须直说"。
%% 无资料时（`Docs = []`）**不注入空块**，而是显式告知"没有可用资料"——否则
%% 模型会自行脑补，且我们无法在事后区分"有据答复"与"编的"。
-spec build_reply_messages(binary(), [message()], binary(), [map()]) -> [message()].
build_reply_messages(SystemPrompt, History, Text, Docs) ->
    System = iolist_to_binary([
        <<"你是客服坐席，用简洁、礼貌、口语化的中文回答客户。\n"/utf8>>,
        <<"只能依据「可用资料」作答；资料不足以回答时必须直接说明需要人工核实，"/utf8>>,
        <<"不要编造政策、金额、时效或承诺。不要输出 JSON。\n"/utf8>>,
        case SystemPrompt of
            <<>> -> <<>>;
            _ -> <<"业务背景："/utf8, SystemPrompt/binary, "\n">>
        end,
        evidence_block(Docs)
    ]),
    [#{<<"role">> => <<"system">>, <<"content">> => System}] ++
        History ++
        [#{<<"role">> => <<"user">>, <<"content">> => Text}].

evidence_block([]) ->
    <<"可用资料：（无）—— 请直接说明暂无对应资料，需要人工帮你核实。\n"/utf8>>;
evidence_block(Docs) ->
    Rendered = [
        iolist_to_binary([
            integer_to_binary(N),
            <<". 问："/utf8>>,
            maps:get(question, D, <<>>),
            <<" 答："/utf8>>,
            maps:get(answer, D, <<>>),
            <<"\n">>
        ])
     || {N, D} <- lists:zip(lists:seq(1, length(Docs)), Docs)
    ],
    iolist_to_binary([<<"可用资料（只能依据这些内容作答）：\n"/utf8>>, Rendered]).

%% ===================================================================
%% 数值工具
%% ===================================================================

pos_int(V, _Default) when is_integer(V), V > 0 -> V;
pos_int(_, Default) -> Default.
