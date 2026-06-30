%%% @doc 领域事件总线 / Domain Event Bus
%%%
%%% DDD 充血改造地基（Phase 0 / T0.1）：基于 OTP gen_event 的进程内事件总线，
%%% 用于解耦聚合根产生的领域事件与其副作用（通知投递、推送、审计等）。
%%%
%%% 充血范式：聚合根（*_agg）为纯函数，返回 {ok, NewState, [event()]}；
%%% logic 外壳在事务提交后调用 publish/1 派发事件，由订阅者（handler）执行副作用。
%%% 这样核心业务流程可独立于通知基础设施被 eunit 测试。
%%%
%%% Domain event bus built on OTP gen_event. Aggregates stay pure and return
%%% events; the imperative shell publishes them after commit, and subscribers
%%% perform side effects. Keeps the domain core testable without I/O.
-module(imboy_domain_event).
-compile([nowarn_deprecated_catch]).

-export([start_link/0]).
-export([publish/1, publish/2]).
-export([subscribe/2, unsubscribe/2, which_subscribers/0]).

-export_type([event/0]).

%% 领域事件统一为带类型标签的 tuple，例如：
%%   {member_added, GroupId :: binary(), UserId :: binary()}
%%   {owner_transferred, GroupId :: binary(), From :: binary(), To :: binary()}
-type event() :: tuple().

%% gen_event 管理进程注册名
-define(MGR, ?MODULE).

%% @doc 启动领域事件总线（由 imboy_sup 挂载）。
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?MGR}).

%% @doc 派发一批领域事件（聚合根产出的事件列表）。
%% 单条事件可用 publish/2 或传入单元素列表。
-spec publish([event()]) -> ok.
publish(Events) when is_list(Events) ->
    %% best-effort：通知为副作用，总线未启动/不可达时静默（catch），
    %% 不让 gen_event:notify 的 badarg 阻断聚合外壳的核心业务流程（如 join/leave）。
    lists:foreach(fun(E) -> catch gen_event:notify(?MGR, E) end, Events),
    ok.

%% @doc 派发单条事件（语法糖）。
-spec publish(atom(), [term()]) -> ok.
publish(Tag, Args) when is_atom(Tag), is_list(Args) ->
    publish([list_to_tuple([Tag | Args])]).

%% @doc 订阅领域事件。Handler 为实现 gen_event behaviour 的模块。
-spec subscribe(module() | {module(), term()}, term()) -> ok | {error, term()}.
subscribe(Handler, Args) ->
    gen_event:add_handler(?MGR, Handler, Args).

%% @doc 取消订阅。
-spec unsubscribe(module() | {module(), term()}, term()) -> term().
unsubscribe(Handler, Args) ->
    gen_event:delete_handler(?MGR, Handler, Args).

%% @doc 当前订阅者列表（运维/测试用）。
-spec which_subscribers() -> [module() | {module(), term()}].
which_subscribers() ->
    gen_event:which_handlers(?MGR).
