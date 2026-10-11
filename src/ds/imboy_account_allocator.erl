%%%-------------------------------------------------------------------
%%% @doc 账号 ID 批量预分配的专用分配进程（G-01 修复，收口 C06）
%%%
%%% 原计划 G-01 验收："若保留批量预分配，必须通过专用分配进程或等价
%%% 机制保证每个 ID 只领取一次"。
%%%
%%% 历史缺陷（W0 取证 + C06 复核）：account_ds:allocate/0 的
%%% `get_wait → 取头 → set Rest` 读改写中，depcache get_wait 仅在
%%% miss 时锁条目，命中路径为无锁 ETS 读——两进程同时命中同一批
%%% [Id|Rest] 会取走同一个头元素，把同一 account 号分配给两个账号
%%% （多节点下该批不广播，风险限单节点，但注册路径活跃，竞态真实）。
%%%
%%% 本进程以 gen_server 串行化领取（节点内唯一），批量来源保持
%%% PG Sequence（`nextval × 10` 随机序，跨节点全局唯一由 PG 保证）：
%%% - 节点内：本进程状态串行，每个 ID 恰好领取一次；
%%% - 跨节点：各节点独立批量取号，sequence 全局唯一；
%%% - 进程崩溃：池状态丢失即废弃（未领取号自然跳过，sequence 单调，
%%%   已领取的号不会被重复分配）；
%%% - PG 不可用：明确返回 {error, no_ids}，不虚假成功。
%%% @end
%%%-------------------------------------------------------------------
-module(imboy_account_allocator).
-moduledoc "账号 ID 专用分配进程：批量 Sequence 预取 + 串行领取。".
-behaviour(gen_server).

-export([start_link/0]).
-export([allocate/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 批量大小与随机化保持 account_ds 既有语义（nextval × 10 → 随机序）
-define(BATCH, 10).

%% @doc 启动（幂等：进程已注册时 {error, {already_started, Pid}}）。
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 领取一个账号 ID（调用方入口；进程不存在时惰性拉起）。
-spec allocate() -> non_neg_integer() | {error, no_ids | term()}.
allocate() ->
    ensure_started(),
    try
        gen_server:call(?MODULE, allocate, 30000)
    catch
        _:_ -> {error, allocator_unavailable}
    end.

%% 并发首调 race-safe：注册竞争的失败方直接复用胜者。
-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            _ = start_in_sup(),
            ok;
        _ ->
            ok
    end.

%% 挂入 imboy_sup（one_for_one worker）；sup 不可用（极早期启动窗口）
%% 时退化为独立进程兜底（注册成功即服务，进程崩溃后下次调用重建）。
-spec start_in_sup() -> {ok, pid()} | {error, term()}.
start_in_sup() ->
    ChildSpec = #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => transient,
        shutdown => 1000,
        type => worker
    },
    case supervisor:start_child(imboy_sup, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, already_present} ->
            %% spec 残留但进程死：先删 spec 再重启
            _ = supervisor:delete_child(imboy_sup, ?MODULE),
            supervisor:start_child(imboy_sup, ChildSpec);
        {error, _} = Err ->
            case start_link() of
                {ok, Pid} -> {ok, Pid};
                {error, {already_started, Pid}} -> {ok, Pid};
                _ -> Err
            end
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-spec init([]) -> {ok, []}.
init([]) ->
    {ok, []}.

%% 池为列表状态：串行 call 即原子领取；空池时批量取号（PG sequence）。
-spec handle_call(allocate, {pid(), term()}, [non_neg_integer()]) ->
    {reply, non_neg_integer() | {error, no_ids}, [non_neg_integer()]}.
handle_call(allocate, _From, [Id | Rest]) ->
    {reply, Id, Rest};
handle_call(allocate, _From, []) ->
    case fetch_batch() of
        [] ->
            {reply, {error, no_ids}, []};
        [Id | Rest] ->
            {reply, Id, Rest}
    end.

-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), term()) -> {noreply, term()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Internal
%% ===================================================================

%% 批量取号（随机序保持既有安全语义；PG 异常返回 [] → no_ids）。
-spec fetch_batch() -> [non_neg_integer()].
fetch_batch() ->
    Q = <<"SELECT nextval('public.imboy_account_id_seq') FROM generate_series(1, 10);">>,
    try elib_pg:query(Q, []) of
        {ok, Res} ->
            [X || {_, X} <- lists:sort([{rand:uniform(), N} || #{<<"nextval">> := N} <- Res])];
        _ ->
            []
    catch
        _:_ ->
            []
    end.
