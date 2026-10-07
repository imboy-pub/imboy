%%% @doc 群组领域事件订阅者 / Group Domain Event Subscriber
%%%
%%% DDD 充血改造 Phase 2 / T2.0h：补齐 T0.1 声称却未落地的「默认 handler」。
%%% 订阅领域事件总线，将 group_agg 产出的成员变更事件桥接为既有 S2C 通知
%%% 投递（msg_s2c_ds:send），使 logic 退化外壳（T2.3）能把直调通知替换为
%%% publish 领域事件，而通知行为保持等价。
%%%
%%% gen_event subscriber bridging domain events to S2C notifications.
%%% 通知语义镜像 group_member_logic 的 join/leave 现状（SOURCE 权威）。
%%%
%%% 渐进策略（NOT Building）：本 task 仅新增订阅者并挂载；现有 join/leave
%%% 仍走直调通知（handler 暂空载，行为零变化），T2.3 再切 publish + 删直调。
-module(group_event_handler).
-moduledoc "群组领域事件订阅者（DDD Phase 2 T2.0h）—— 领域事件的默认 handler。".
-behaviour(gen_event).

-export([attach/0, detach/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% @doc 挂载到领域事件总线（由 imboy_app 启动时、总线就绪后调用）。
-spec attach() -> ok | {error, term()}.
attach() ->
    imboy_domain_event:subscribe(?MODULE, []).

%% @doc 从总线摘除（运维/测试用）。
-spec detach() -> term().
detach() ->
    imboy_domain_event:unsubscribe(?MODULE, []).

%% ===================================================================
%% gen_event 回调
%% ===================================================================

init(_Args) ->
    {ok, #{}}.

%% 成员加入 → group_member_join 通知（迁自 group_member_logic:group_member_join_notice/3）。
handle_event({member_added, Gid, Uid}, State) ->
    notify_member_join(Gid, Uid),
    {ok, State};
%% 成员移除 → group_member_leave 通知（迁自 group_member_logic:leave/3）。
handle_event({member_removed, Gid, Uid}, State) ->
    notify_member_leave(Gid, Uid),
    {ok, State};
%% 群主转让 → group_transfer 通知（Q05/F07：补齐四场景中唯一"零通知"缺口）。
handle_event({owner_transferred, Gid, FromUid, ToUid}, State) ->
    notify_owner_transferred(Gid, FromUid, ToUid),
    {ok, State};
%% 其余领域事件（group_dissolved 等）暂不投递：解散保持既有直发形态，
%% 不迁事件总线（主计划 §8：已有直发有效时不机械追加双轨）。
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Req, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal — 通知重建（与 group_member_logic 现状语义等价）
%% ===================================================================

%% @doc 群成员加入通知（nosave）。
-spec notify_member_join(integer(), integer()) -> ok.
notify_member_join(Gid, Uid) ->
    ToUidLi = group_ds:member_uids(Gid),
    User = user_ds:find_by_id(Uid, <<"account,avatar,nickname">>),
    Payload = #{
        <<"gid">> => Gid,
        <<"nickname">> => maps:get(<<"nickname">>, User, <<>>),
        <<"avatar">> => maps:get(<<"avatar">>, User, <<>>),
        <<"account">> => maps:get(<<"account">>, User, <<>>)
    },
    _ = msg_s2c_ds:send(Uid, ToUidLi, <<"group_member_join">>, <<>>, null, Payload, nosave),
    ok.

%% @doc 群成员离开通知（save）。
%% Q05/F07：被移除者本人追加进接收列表（去重）——被踢端即时/离线均可感知
%% （此前缓存先行删除导致本人永远不在 member_uids 里），主动退群场景同时
%% 兼顾多端同步；App 端 memberLeave 已有 leave_uid==self 分支。
-spec notify_member_leave(integer(), integer()) -> ok.
notify_member_leave(Gid, Uid) ->
    Members = group_ds:member_uids(Gid),
    ToUidLi =
        case lists:member(Uid, Members) of
            true ->
                %% 缓存竞态（member_uids 仍含被移除者）：不重复追加
                Members;
            false ->
                Members ++ [Uid]
        end,
    Payload = #{
        <<"gid">> => Gid,
        <<"leave_uid">> => Uid
    },
    _ = msg_s2c_ds:send(Uid, ToUidLi, <<"group_member_leave">>, <<>>, null, Payload, save),
    ok.

%% @doc 群主转让通知（save，离线可补）。
%% 事件 {owner_transferred, Gid, From, To} 由 group_logic:do_transfer 在
%% 事务提交成功后 publish；From 为旧群主（S2C from 字段），To 为新群主。
-spec notify_owner_transferred(integer(), integer(), integer()) -> ok.
notify_owner_transferred(Gid, FromUid, ToUid) ->
    ToUidLi = group_ds:member_uids(Gid),
    Payload = #{
        <<"gid">> => Gid,
        <<"from_owner_uid">> => FromUid,
        <<"to_owner_uid">> => ToUid
    },
    _ = msg_s2c_ds:send(FromUid, ToUidLi, <<"group_transfer">>, <<>>, null, Payload, save),
    ok.
