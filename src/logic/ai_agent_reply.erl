-module(ai_agent_reply).

%%%
% Agent C2C 回复分派 / Agent C2C reply dispatch（Phase 1 T1.4）
%
% 复用 msg_c2s_logic:c2s_to_external 的「异步调 LLM → 投递响应」骨架思路，
% 但作为 msg_c2c_logic:c2c/3 的旁路：人给 agent 发私聊 → 触发 LLM → 回投 agent→human。
% 核心通道只在 allow 分支尾部 fire-and-forget 调用 maybe_dispatch/3，不改变原返回值。
%
% E2EE 红线：服务端绝不解密 ciphertext 做 AI。E2EE 消息一律跳过（不触发）。
%%%

-export([maybe_dispatch/3]).
%% 供内部 async 闭包与测试调用
-export([run_and_reply/5]).

-include("log.hrl").

%% @doc C2C 旁路：若 ToId 是启用中的 agent 且消息为非 E2EE 文本，异步触发 LLM 回复。
%% 对普通 C2C（非 agent / E2EE / 无文本）无副作用，恒返回 ok（fire-and-forget）。
-spec maybe_dispatch(integer(), integer(), map()) -> ok.
maybe_dispatch(FromUid, ToId, Data) ->
    %% 故障隔离：agent 回复旁路的任何异常绝不能拖垮正常 C2C 投递。
    try
        do_maybe_dispatch(FromUid, ToId, Data)
    catch
        Class:Reason ->
            ok = ?ERROR_LOG("[AGENT_REPLY_GUARD] to=~p ~p:~p~n", [ToId, Class, Reason]),
            ok
    end.

-spec do_maybe_dispatch(integer(), integer(), map()) -> ok.
do_maybe_dispatch(FromUid, ToId, Data) ->
    case is_e2ee(Data) of
        true ->
            % E2EE 红线：不解密、不做 AI
            ok;
        false ->
            case extract_text(Data) of
                <<>> ->
                    % 非文本（图片/文件/位置等）不触发
                    ok;
                Text ->
                    case ai_agent_ds:is_agent(ToId) of
                        {true, Agent} ->
                            dispatch(FromUid, ToId, Agent, Text);
                        false ->
                            ok
                    end
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec dispatch(integer(), integer(), map(), binary()) -> ok.
dispatch(FromUid, ToId, Agent, Text) ->
    Provider = maps:get(<<"provider">>, Agent, <<>>),
    case imboy_llm_registry:lookup(Provider) of
        {ok, #{module := Mod, opts := Opts0}} ->
            Opts = merge_model(Opts0, Agent),
            Messages = build_messages(Agent, Text),
            % 异步：不阻塞发送者的 C2C 处理
            elib_async:async(fun() ->
                ?MODULE:run_and_reply(Mod, Opts, FromUid, ToId, Messages)
            end),
            ok;
        undefined ->
            ok = ?ERROR_LOG("[AGENT_REPLY] provider not found: ~p~n", [Provider]),
            ok
    end.

%% @doc 调 LLM 并把结果作为 C2C 从 agent 回投给 human（async 闭包体，独立导出便于测试）
-spec run_and_reply(module(), map(), integer(), integer(), [map()]) -> ok.
run_and_reply(Mod, Opts, FromUid, ToId, Messages) ->
    case Mod:chat(ToId, Messages, Opts) of
        {ok, RespMap} when is_map(RespMap) ->
            Result = maps:get(<<"result">>, RespMap, <<>>),
            deliver_reply(FromUid, ToId, Result);
        {error, Reason} ->
            ok = ?ERROR_LOG("[AGENT_REPLY_FAILED] to=~p reason=~p~n", [ToId, Reason]),
            ok
    end.

%% 构建 C2C 回复消息（agent→human）并经既有投递骨架发送
-spec deliver_reply(integer(), integer(), binary()) -> ok.
deliver_reply(_FromUid, _ToId, <<>>) ->
    ok;
deliver_reply(FromUid, ToId, Result) ->
    ReplyMsgId = ec_cnv:to_binary(elib_tsid:generate()),
    Content = elib_str:replace_single_quote(Result),
    Msg = #{
        <<"id">> => ReplyMsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => ec_cnv:to_binary(ToId),
        <<"to">> => ec_cnv:to_binary(FromUid),
        <<"msg_type">> => <<"text">>,
        %% text/content 双键：兼容不同渲染消费者
        <<"payload">> => #{<<"text">> => Content, <<"content">> => Content},
        <<"created_at">> => elib_dt:millisecond()
    },
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"c2c">>),
    message_ds:send_next(FromUid, ReplyMsgId, MsgJson, MsLi),
    ok.

%% OpenAI 兼容 messages：可选 system_prompt 开场 + 用户消息
-spec build_messages(map(), binary()) -> [map()].
build_messages(Agent, Text) ->
    User = #{<<"role">> => <<"user">>, <<"content">> => Text},
    case maps:get(<<"system_prompt">>, Agent, <<>>) of
        SP when is_binary(SP), SP =/= <<>> ->
            [#{<<"role">> => <<"system">>, <<"content">> => SP}, User];
        _ ->
            [User]
    end.

%% agent.model 非空则覆盖 provider 配置的 model
-spec merge_model(map(), map()) -> map().
merge_model(Opts, Agent) ->
    case maps:get(<<"model">>, Agent, <<>>) of
        M when is_binary(M), M =/= <<>> -> Opts#{model => M};
        _ -> Opts
    end.

%% 消息是否 E2EE（顶层 e2ee map 非 null，或 msg_type=e2ee）
-spec is_e2ee(map()) -> boolean().
is_e2ee(Data) ->
    E2EE = maps:get(<<"e2ee">>, Data, null),
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    (is_map(E2EE) andalso map_size(E2EE) > 0) orelse MsgType =:= <<"e2ee">>.

%% 从 C2C payload 提取文本（content 优先，text 兜底）
-spec extract_text(map()) -> binary().
extract_text(Data) ->
    Payload = maps:get(<<"payload">>, Data, #{}),
    case Payload of
        P when is_map(P) ->
            first_nonempty([
                maps:get(<<"content">>, P, <<>>),
                maps:get(<<"text">>, P, <<>>)
            ]);
        _ ->
            <<>>
    end.

-spec first_nonempty([term()]) -> binary().
first_nonempty([B | _]) when is_binary(B), B =/= <<>> -> B;
first_nonempty([_ | Rest]) -> first_nonempty(Rest);
first_nonempty([]) -> <<>>.
