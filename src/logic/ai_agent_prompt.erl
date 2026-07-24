-module(ai_agent_prompt).

%%%
%%% Agent LLM 消息构建公共骨架 / Shared prompt-building seam（P0-3 A3-3）
%%%
%%% 收口 ai_agent_reply / ai_agent_group_reply / ai_agent_proactive 三处重复的
%%% helper（build_messages / merge_model / is_e2ee / extract_text / first_nonempty），
%%% 消除逐字复制的私有副本。msg_c2c:80 与 msg_c2g:421 两处 dispatch 经各自的
%%% maybe_dispatch 仍保留差异逻辑（C2C 单查 vs C2G mentions 多查 / 支付旁路 /
%%% 触发策略 / 防环 / 投递目标），但「消息如何构建」收敛到本模块单一入口。
%%%
%%% A3-2 知识库注入落点：build_messages 在此读 ai_agent_kb_logic:kb_text() 追加到
%%% system content——改一处即让 C2C/C2G/欢迎三路同时带上知识库上下文。
%%%

-export([
    build_messages/2,
    build_messages_with_user/2,
    merge_model/2,
    is_e2ee/1,
    extract_text/1,
    first_nonempty/1
]).

%% @doc OpenAI 兼容 messages：可选 system_prompt（+ 知识库）开场 + 用户文本。
%% A3-2：当 ai_agent_kb_logic:kb_text() 非空时，追加到 system content。
-spec build_messages(map(), binary()) -> [map()].
build_messages(Agent, Text) ->
    User = #{<<"role">> => <<"user">>, <<"content">> => Text},
    build_messages_with_user(Agent, User).

%% @doc 同 build_messages/2，但 user 段由调用方提供（proactive 欢迎指令场景）
-spec build_messages_with_user(map(), map()) -> [map()].
build_messages_with_user(Agent, #{<<"role">> := <<"user">>} = User) ->
    SystemContent = system_content(Agent),
    case SystemContent of
        <<>> ->
            [User];
        _ ->
            [#{<<"role">> => <<"system">>, <<"content">> => SystemContent}, User]
    end.

%% 拼装 system content：agent.system_prompt + 知识库文本（A3-2 注入点）
-spec system_content(map()) -> binary().
system_content(Agent) ->
    SP = maps:get(<<"system_prompt">>, Agent, <<>>),
    %% A3-2: 知识库上下文注入（kb_text 内部做 enabled 门控 + 故障安全）
    case ai_agent_kb_logic:kb_text() of
        <<>> ->
            SP;
        KB when is_binary(SP), SP =/= <<>> ->
            <<SP/binary, "\n\n"/utf8, KB/binary>>;
        KB ->
            KB
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

%% 从 payload 提取文本（content 优先，text 兜底）
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
