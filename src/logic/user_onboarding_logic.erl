-module(user_onboarding_logic).

%%%
% 新手引导编排 / New-user onboarding orchestration（AI 冷启动 M1）
%
% 注册成功后由 passport_logic 经 elib_async fire-and-forget 触发，三件套：
%   ① 默认好友：新用户 ↔ 欢迎 agent 双边写入（source=system_onboarding）
%   ② 默认订阅：逐个订阅配置频道（单频道失败不阻断其余）
%   ③ 欢迎消息：ai_agent_proactive:send_welcome（模板默认 / LLM 可选）
%
% 设计约束：
%   - 配置驱动（config 表 onboarding.* 键，管理后台可配）；enabled≠true 或
%     welcome_agent_uid<=0 时整体 no-op——私有化买家可整个关掉。
%   - 幂等：is_friend(Uid, AgentUid)=true 视为已引导过，整体跳过。
%   - 故障隔离：任何一步异常不拖垮其余步骤；本模块任何异常绝不向上抛
%     （注册链路零影响），恒返回 ok。
%%%

-export([after_signup/2]).
%% 管理后台 onboarding 配置读写（adm_ai_agent_handler:onboarding_config 调用）
-export([get_config/0, put_config/1]).

-include("log.hrl").

%% config 表键（管理后台 /api/adm/ai_agent/onboarding_config 可配）
-define(CFG_ENABLED, <<"onboarding.enabled">>).
-define(CFG_AGENT_UID, <<"onboarding.welcome_agent_uid">>).
-define(CFG_CHANNELS, <<"onboarding.default_channels">>).
-define(CFG_TEMPLATE, <<"onboarding.welcome_template">>).
-define(CFG_LLM, <<"onboarding.welcome_llm_enabled">>).

%% 系统默认好友的来源标记（写入 user_friend.setting）
-define(SOURCE, <<"system_onboarding">>).

%% 配置写入约束
-define(MAX_CHANNELS, 20).
-define(MAX_TEMPLATE_BYTES, 2000).

%% @doc 注册后引导入口（fire-and-forget 语义，恒 ok，绝不向上抛）
-spec after_signup(integer(), binary()) -> ok.
after_signup(Uid, Nickname) when is_integer(Uid), Uid > 0 ->
    try
        do_after_signup(Uid, Nickname)
    catch
        Class:Reason ->
            ?ERROR_LOG("[ONBOARDING] uid=~p ~p:~p~n", [Uid, Class, Reason]),
            ok
    end;
after_signup(_, _) ->
    ok.

%% ===================================================================
%% Admin 配置读写（白名单键 + 类型校验；半量更新；未知键忽略）
%% ===================================================================

%% @doc 读取 onboarding 全量配置（admin 视图，binary 键）
-spec get_config() -> map().
get_config() ->
    #{
        <<"enabled">> => config_ds:get(?CFG_ENABLED, false),
        <<"welcome_agent_uid">> => config_ds:get(?CFG_AGENT_UID, 0),
        <<"default_channels">> => config_ds:get(?CFG_CHANNELS, []),
        <<"welcome_template">> => config_ds:get(?CFG_TEMPLATE, <<>>),
        <<"welcome_llm_enabled">> => config_ds:get(?CFG_LLM, false)
    }.

%% @doc 半量写入 onboarding 配置：白名单键逐个校验，任一非法则整体拒绝
%% （零写入）；未知键忽略。成功返回 {ok, Patch}。
-spec put_config(map()) -> {ok, map()} | {error, binary()}.
put_config(Patch) when is_map(Patch) ->
    case validate_patch(maps:to_list(Patch)) of
        ok ->
            maps:foreach(fun write_key/2, Patch),
            {ok, Patch};
        {error, _} = Err ->
            Err
    end.

-spec validate_patch([{binary(), term()}]) -> ok | {error, binary()}.
validate_patch([]) ->
    ok;
validate_patch([{K, V} | Rest]) ->
    case validate_key(K, V) of
        ok -> validate_patch(Rest);
        {error, _} = Err -> Err
    end.

-spec validate_key(binary(), term()) -> ok | {error, binary()}.
validate_key(<<"enabled">>, V) when is_boolean(V) ->
    ok;
validate_key(<<"welcome_llm_enabled">>, V) when is_boolean(V) ->
    ok;
validate_key(<<"welcome_agent_uid">>, V) when is_integer(V), V >= 0 ->
    ok;
%% 前端 TSID 经 JSON 序列化为字符串（safeParseBigIntJson），接受数字 binary
validate_key(<<"welcome_agent_uid">>, V) when is_binary(V) ->
    try binary_to_integer(V) of
        N when N >= 0 -> ok;
        _ -> {error, <<"welcome_agent_uid 必须为非负整数"/utf8>>}
    catch
        _:_ -> {error, <<"welcome_agent_uid 必须为非负整数"/utf8>>}
    end;
validate_key(<<"default_channels">>, V) when is_list(V), length(V) =< ?MAX_CHANNELS ->
    case lists:all(fun(Ch) -> is_binary(Ch) andalso Ch =/= <<>> end, V) of
        true -> ok;
        false -> {error, <<"default_channels 元素必须为非空字符串"/utf8>>}
    end;
validate_key(<<"welcome_template">>, V) when
    is_binary(V), V =/= <<>>, byte_size(V) =< ?MAX_TEMPLATE_BYTES
->
    ok;
validate_key(K, _V) ->
    case cfg_key(K) of
        undefined -> ok;
        _ -> {error, <<K/binary, " 值非法"/utf8>>}
    end.

-spec write_key(binary(), term()) -> ok.
%% welcome_agent_uid 归一化为 integer 存储（前端可能传数字 binary 字符串）
write_key(<<"welcome_agent_uid">> = K, V) ->
    config_ds:set(cfg_key(K), to_int(V));
write_key(K, V) ->
    case cfg_key(K) of
        undefined -> ok;
        CfgKey -> config_ds:set(CfgKey, V)
    end.

%% admin 视图键 → config 表键映射（白名单）
-spec cfg_key(binary()) -> binary() | undefined.
cfg_key(<<"enabled">>) -> ?CFG_ENABLED;
cfg_key(<<"welcome_agent_uid">>) -> ?CFG_AGENT_UID;
cfg_key(<<"default_channels">>) -> ?CFG_CHANNELS;
cfg_key(<<"welcome_template">>) -> ?CFG_TEMPLATE;
cfg_key(<<"welcome_llm_enabled">>) -> ?CFG_LLM;
cfg_key(_) -> undefined.

%% ===================================================================
%% Internal
%% ===================================================================

-spec do_after_signup(integer(), binary()) -> ok.
do_after_signup(Uid, Nickname) ->
    case config_ds:get(?CFG_ENABLED, false) of
        true ->
            case to_int(config_ds:get(?CFG_AGENT_UID, 0)) of
                AgentUid when AgentUid > 0 ->
                    onboard(Uid, Nickname, AgentUid);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

-spec onboard(integer(), binary(), integer()) -> ok.
onboard(Uid, Nickname, AgentUid) ->
    %% 幂等守卫：已是好友 = 已引导过（防重试/重复路径产生重复欢迎）
    case friend_ds:is_friend(Uid, AgentUid) of
        true ->
            ok;
        false ->
            safe(add_default_friend, fun() -> add_default_friend(Uid, AgentUid) end),
            safe(subscribe_channels, fun() -> subscribe_channels(Uid) end),
            safe(send_welcome, fun() -> send_welcome(AgentUid, Uid, Nickname) end),
            ok
    end.

%% 单步故障隔离：记录后继续，不中断其余步骤
-spec safe(atom(), fun(() -> term())) -> ok.
safe(Step, Fun) ->
    try
        Fun(),
        ok
    catch
        Class:Reason ->
            ?ERROR_LOG("[ONBOARDING] step ~p failed ~p:~p~n", [Step, Class, Reason]),
            ok
    end.

%% 默认好友双边写入（agent 为关系发起方：is_from=1 标记在 agent→user 行）
-spec add_default_friend(integer(), integer()) -> ok.
add_default_friend(Uid, AgentUid) ->
    NowTs = elib_dt:now(),
    friend_ds:confirm_friend(
        false, AgentUid, Uid, <<>>, #{<<"is_from">> => 1, <<"source">> => ?SOURCE}, <<>>, NowTs
    ),
    friend_ds:confirm_friend(
        false, Uid, AgentUid, <<>>, #{<<"source">> => ?SOURCE}, <<>>, NowTs
    ),
    ok.

%% 默认订阅：单频道失败 warn 后继续（不吞错、不阻断）
-spec subscribe_channels(integer()) -> ok.
subscribe_channels(Uid) ->
    Channels = config_ds:get(?CFG_CHANNELS, []),
    lists:foreach(
        fun(Ch) ->
            case channel_logic:subscribe(Uid, Ch) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?WARN_LOG("[ONBOARDING] subscribe uid=~p ch=~p error ~p~n", [
                        Uid, Ch, Reason
                    ])
            end
        end,
        Channels
    ).

-spec send_welcome(integer(), integer(), binary()) -> ok.
send_welcome(AgentUid, Uid, Nickname) ->
    Cfg = #{
        welcome_template => config_ds:get(?CFG_TEMPLATE, <<>>),
        welcome_llm_enabled => config_ds:get(?CFG_LLM, false)
    },
    ai_agent_proactive:send_welcome(AgentUid, Uid, Nickname, Cfg).

%% config 值经 jsone:decode 返回，数字键防御 float/binary 形态
-spec to_int(term()) -> integer().
to_int(I) when is_integer(I) ->
    I;
to_int(F) when is_float(F) ->
    trunc(F);
to_int(B) when is_binary(B) ->
    try
        binary_to_integer(B)
    catch
        _:_ -> 0
    end;
to_int(_) ->
    0.
