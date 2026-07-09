-module(ai_agent_runtime).
-behaviour(gen_server).

%%%
% AI Agent 常驻在线 runtime / Agent presence runtime（Phase 1 T1.3）
%
% presence-only MVP：把启用中的 agent 注册进 syn 在线态（自身 Pid 作目标），
% 使 user_logic:is_online(agent)=true、客户端显示 agent 在线、可被 C2C 路由到。
% 回复由 T1.4 的 ai_agent_reply（msg_c2c 旁路）负责，本进程不消费帧回复，避免双回复。
%
% ponytail: 投递到 agent 的消息帧在本进程被丢弃（无 client ACK）→ 走 QoS 重试后
%   转离线存储（对 agent 账号即历史留存），是 presence-only 的已知开销。真需 agent 作
%   一等 WS 参与者（proper ACK / 持 token / 断线重连）再扩展，那是 review 后的下一步。
% ponytail: 靠周期 refresh 兜底新增/状态变更（不从 ds 反向调用，守 DS→上层单向依赖），
%   新建 agent 最长 ?REFRESH_MS 后上线；需即时上线再引显式通知。
%%%

-export([start_link/0, online_ids/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("log.hrl").

-define(DTYPE, <<"ai">>).
-define(DID, <<"ai_runtime">>).
% 5 分钟兜底重扫
-define(REFRESH_MS, 300000).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 当前已注册在线的 agent uid 列表（诊断/测试用）
-spec online_ids() -> [integer()].
online_ids() ->
    gen_server:call(?MODULE, online_ids).

%% ===================================================================
%% gen_server
%% ===================================================================

init([]) ->
    %% 立即尝试一次上线（DB 未就绪则 refresh 内部优雅跳过，靠定时重试）
    self() ! refresh,
    {ok, #{ids => sets:new()}}.

handle_call(online_ids, _From, #{ids := Ids} = State) ->
    {reply, sets:to_list(Ids), State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    State2 = refresh(State),
    erlang:send_after(?REFRESH_MS, self(), refresh),
    {noreply, State2};
handle_info(_Info, State) ->
    %% 丢弃投递到 agent 的帧（reply 由 T1.4 负责，见模块头 ponytail 注释）
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec refresh(map()) -> map().
refresh(State) ->
    case ai_agent_repo:active_ids() of
        {ok, Ids} ->
            lists:foldl(fun join_one/2, State, Ids);
        {error, Reason} ->
            _ = ?WARN_LOG({ai_agent_runtime_refresh_failed, Reason}),
            State
    end.

-spec join_one(integer(), map()) -> map().
join_one(Uid, #{ids := Ids} = State) ->
    _ = imboy_syn:join(Uid, ?DTYPE, self(), ?DID),
    State#{ids => sets:add_element(Uid, Ids)}.
