-module(ai_agent_proactive).

%%%
% Agent 主动消息原语 / Agent proactive message primitives（AI 冷启动 M1）
%
% 与 ai_agent_reply（被动旁路）互补：本模块提供「agent 主动开口」原语，
% 当前唯一调用方是 user_onboarding_logic 的新手欢迎。投递骨架复用
% ai_agent_reply:deliver_reply 同款 message_ds:send_next 链。
%
% 成本红线：LLM 路必须过 agent_rate_limiter 闸门；deny/失败回退模板文案
% （欢迎是产品功能须可达，成本闸门只挡 LLM 调用、不挡投递本身）。
%
% E2EE 红线：主动消息由服务端以 agent 身份明文发出，agent 无设备私钥、
% 永远无法加密。因此在 e2ee_mode=required/compliance 的部署下必须**拒发**，
% 而不是把明文写进 msg_c2c——见 send_text/3 的部署级明文门。
%%%

-export([send_text/3]).
-export([send_welcome/4]).

-include("log.hrl").

%% 默认欢迎文案（运营文案以 config 表 onboarding.welcome_template 为准，此为兜底）
-define(DEFAULT_TEMPLATE, <<"嗨 {{nickname}}，我是你的 AI 助手🤖 有什么想聊的都可以找我～"/utf8>>).
%% 空昵称兜底称呼
-define(DEFAULT_NICKNAME, <<"新朋友"/utf8>>).
-define(NICKNAME_PLACEHOLDER, <<"{{nickname}}">>).
%% LLM 欢迎指令（工程常量，非运营文案；运营文案走 welcome_template）
-define(WELCOME_INSTRUCTION,
    <<"请用一句话向新用户打招呼，亲切自然，明确表明你是 AI 助手，50 字以内。用户昵称："/utf8>>
).

%% @doc 以 agent 身份向用户投递一条 C2C 文本（空文本短路，恒 ok）
-spec send_text(integer(), integer(), binary()) -> ok.
send_text(AgentUid, ToUid, Text) when
    is_integer(AgentUid), is_integer(ToUid), is_binary(Text), Text =/= <<>>
->
    Content = elib_str:replace_single_quote(Text),
    %% text/content 双键：兼容不同渲染消费者
    PayloadMap = #{<<"text">> => Content, <<"content">> => Content},
    PayloadJson = jsone:encode(PayloadMap, [native_utf8]),
    %% 部署级 E2EE fail-closed 门：本模块直写 msg_store_ds:stage/enqueue，
    %% 不经 msg_c2c_logic:stage_and_send_c2c，必须自带同款门，否则 required 部署下
    %% agent 明文会从这条旁路灌进 msg_c2c（E2EE 恒 null → 判定必为明文）。
    case imboy_policy:validate_message_write(<<"C2C">>, <<"text">>, <<>>, null, PayloadJson) of
        ok ->
            do_send_text(AgentUid, ToUid, PayloadMap, PayloadJson);
        {error, Reason} ->
            %% 拒发而非降级：agent 无设备私钥，没有"加密后再发"这条路。
            ?WARN_LOG("[AGENT_PROACTIVE_BLOCKED] to=~p reason=~p~n", [ToUid, Reason]),
            ok
    end;
send_text(_, _, _) ->
    ok.

%% @private 过门后的实际投递
-spec do_send_text(integer(), integer(), map(), binary()) -> ok.
do_send_text(AgentUid, ToUid, PayloadMap, PayloadJson) ->
    MsgId = ec_cnv:to_binary(elib_tsid:generate()),
    %% 【离线可达】持久化到 msg_c2c（离线 pull 源），与标准 c2c 同链
    %% （msg_c2c_logic:stage_and_send_c2c）：先 stage 写 staging 表（同步安全），
    %% 再 enqueue 触发 msg_store_worker claim staging → 写 msg_c2c。send_next 只推
    %% imboy_syn 在线设备，新用户注册时通常无 WS 会话，缺此步则离线欢迎消息永久丢失。
    NowTs = elib_dt:now(),
    _ = msg_store_ds:stage(
        <<"c2c">>, MsgId, <<"text">>, <<>>, null, PayloadJson, AgentUid, ToUid, NowTs, NowTs
    ),
    msg_store_ds:enqueue(<<"c2c">>, MsgId, #{
        payload => PayloadMap,
        from_id => AgentUid,
        to_id => ToUid,
        created_at => NowTs,
        server_ts => NowTs
    }),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => ec_cnv:to_binary(AgentUid),
        <<"to">> => ec_cnv:to_binary(ToUid),
        <<"msg_type">> => <<"text">>,
        <<"payload">> => PayloadMap,
        <<"created_at">> => elib_dt:millisecond()
    },
    MsLi = elib_retry_config:intervals(<<"c2c">>),
    message_ds:send_next(ToUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.

%% @doc 发送新手欢迎消息。默认模板路（零 LLM 成本）；Cfg.welcome_llm_enabled=true
%% 时走 LLM 个性化（过 rate limiter），失败/限流一律回退模板保证可达。
-spec send_welcome(integer(), integer(), binary(), map()) -> ok.
send_welcome(AgentUid, ToUid, Nickname, Cfg) ->
    case proactive_allowed(AgentUid) of
        false ->
            ok;
        true ->
            Template = template_of(Cfg),
            case maps:get(welcome_llm_enabled, Cfg, false) of
                true ->
                    case llm_welcome(AgentUid, ToUid, Nickname) of
                        {ok, Text} when is_binary(Text), Text =/= <<>> ->
                            send_text(AgentUid, ToUid, Text);
                        _ ->
                            send_text(AgentUid, ToUid, render(Template, Nickname))
                    end;
                _ ->
                    send_text(AgentUid, ToUid, render(Template, Nickname))
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% LLM 个性化欢迎：限流闸门 → agent 元数据 → registry → chat
-spec llm_welcome(integer(), integer(), binary()) -> {ok, binary()} | {error, term()}.
llm_welcome(AgentUid, ToUid, Nickname) ->
    case agent_rate_limiter:allow(AgentUid, ToUid) of
        allow ->
            case ai_agent_ds:is_agent(AgentUid) of
                {true, Agent} ->
                    do_llm_welcome(Agent, AgentUid, Nickname);
                false ->
                    {error, not_agent}
            end;
        {deny, Reason} ->
            ?WARN_LOG("[AGENT_WELCOME] rate limited agent=~p reason=~p~n", [AgentUid, Reason]),
            {error, {rate_limited, Reason}}
    end.

-spec do_llm_welcome(map(), integer(), binary()) -> {ok, binary()} | {error, term()}.
do_llm_welcome(Agent, AgentUid, Nickname) ->
    Provider = maps:get(<<"provider">>, Agent, <<>>),
    case imboy_llm_registry:lookup(Provider) of
        {ok, #{module := Mod, opts := Opts0}} ->
            Opts = ai_agent_prompt:merge_model(Opts0, Agent),
            User = #{
                <<"role">> => <<"user">>,
                <<"content">> => <<?WELCOME_INSTRUCTION/binary, Nickname/binary>>
            },
            Messages = ai_agent_prompt:build_messages_with_user(Agent, User),
            case Mod:chat(AgentUid, Messages, Opts) of
                {ok, RespMap} when is_map(RespMap) ->
                    {ok, maps:get(<<"result">>, RespMap, <<>>)};
                {error, Reason} ->
                    ?ERROR_LOG("[AGENT_WELCOME] llm failed agent=~p reason=~p~n", [
                        AgentUid, Reason
                    ]),
                    {error, Reason}
            end;
        undefined ->
            ?ERROR_LOG("[AGENT_WELCOME] provider not found: ~p~n", [Provider]),
            {error, provider_not_found}
    end.

%% 模板渲染：{{nickname}} 占位替换；空昵称用兜底称呼
-spec render(binary(), binary()) -> binary().
render(Template, Nickname) ->
    Name =
        case Nickname of
            <<>> -> ?DEFAULT_NICKNAME;
            N -> N
        end,
    binary:replace(Template, ?NICKNAME_PLACEHOLDER, Name, [global]).

-spec template_of(map()) -> binary().
template_of(Cfg) ->
    case maps:get(welcome_template, Cfg, ?DEFAULT_TEMPLATE) of
        T when is_binary(T), T =/= <<>> -> T;
        _ -> ?DEFAULT_TEMPLATE
    end.

-spec proactive_allowed(integer()) -> boolean().
proactive_allowed(AgentUid) ->
    try ai_agent_ds:is_agent(AgentUid) of
        {true, Agent} ->
            ai_agent_policy:allows(Agent, <<"proactive">>);
        false ->
            false
    catch
        _:_ ->
            %% 兼容旧部署/单测：无法读取角色时不改变原欢迎消息语义。
            true
    end.
