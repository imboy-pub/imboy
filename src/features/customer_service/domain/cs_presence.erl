%%% @doc 客服坐席 presence 运行态派生的领域纯函数（CS-BE-05）。
%%%
%%% 冻结决策 CS-DEC-02：运行态（online/away/busy/offline）是**派生值**——
%%% 心跳新鲜度 + 会话负载 + 手动覆盖三输入派生，不在 cs_seat 上新增治理列，
%%% 也不落库（本模块零 I/O、零库读，时钟由调用方显式注入 NowSec）。
%%%
%%% 派生规则（优先级从高到低）：
%%%   1. 手动 `away` 覆盖一切自动派生（手动 away 优先于自动 online）；
%%%   2. 心跳缺失（无 lease 行 / NULL 时间戳）或距今 > ?HEARTBEAT_TTL_SEC
%%%      → `offline`（90 秒无 heartbeat → offline）；
%%%   3. 活跃会话 ≥ max_concurrent → `busy`（容量满派生 busy）；
%%%   4. 否则 `online`（心跳新鲜且有余量）。
%%%
%%% 派单接入：`select_seat`（cs_dispatch）只考虑 `derived_status =:= online`
%%% 的坐席；`away/busy/offline` 都不参与**自动**派单（busy 由容量条件兜底、
%%% away 是人为主观不可用、offline 心跳已死）。显式 claim（坐席主动接单）
%%% 是在线事实本身，不做 presence 过滤（enabled + 容量门照旧）。
%%%
%%% 纯净性（铁律 4）：本模块无 I/O、无库读；presence 行快照由调用方显式传入。
-module(cs_presence).

-export([
    derive/2,
    annotate/3,
    dispatchable/1
]).

-type status() :: online | away | busy | offline.

-export_type([status/0]).

%% 心跳 TTL（秒）：90 秒无 heartbeat → offline（CS-DEC-02 冻结值）。
-define(HEARTBEAT_TTL_SEC, 90).

%% @doc 单坐席运行态派生。
%%
%% Presence 行快照（store 读出的投影；无行传 `undefined` 或空 map）：
%%   #{last_heartbeat_at => epoch 秒 | undefined,
%%     manual_status      => <<"away">> | undefined}
%%
%% Seat 快照（容量判定输入）：#{max_concurrent => pos_integer(),
%%                              active_count  => non_neg_integer()}
-spec derive(map() | undefined, map()) -> status().
derive(Presence, Seat) when is_map(Seat) ->
    Now = maps:get(now_sec, Seat, undefined),
    derive(Presence, Now, Seat);
derive(_Presence, _NotSeatMap) ->
    offline.

%% @doc 显式时钟入口（测试注入 NowSec；生产由调用方传服务端当前秒）。
-spec derive(map() | undefined, integer(), map()) -> status().
derive(Presence, NowSec, Seat) when is_integer(NowSec), is_map(Seat) ->
    case manual_status(Presence) of
        away ->
            away;
        none ->
            case heartbeat_fresh(Presence, NowSec) of
                false ->
                    offline;
                true ->
                    AtCapacity =
                        maps:get(active_count, Seat, 0) >= maps:get(max_concurrent, Seat, 0),
                    case AtCapacity of
                        true -> busy;
                        false -> online
                    end
            end
    end;
derive(_Presence, _Now, _Seat) ->
    offline.

%% @doc 给派单快照逐行注入 `derived_status`（annotate-then-select 两段式）。
%%
%% `Seats` 是 `cs_store_port:list_dispatchable_seats/1` 快照（含 active_count）；
%% `Presences` 是 org 级 presence 行投影列表（键 organization_id /
%% business_identity_id / last_heartbeat_at / manual_status）。行缺失的坐席
%% 视为从未上报心跳 → offline（从严：不上报 = 不参与自动派单）。
%%
%% NowSec 缺省时不做新鲜度裁决（只放手动 away 覆盖、其余 online）——仅
%% 供无需 TTL 语义的旧调用方过渡；生产派单链必须显式传 NowSec。
-spec annotate([map()], [map()], integer() | undefined) -> [map()].
annotate(Seats, Presences, NowSec) when is_list(Seats), is_list(Presences) ->
    ByIdentity = maps:from_list([
        {{maps:get(organization_id, P), maps:get(business_identity_id, P)}, P}
     || P <- Presences,
        is_map(P),
        is_map_key(organization_id, P),
        is_map_key(business_identity_id, P)
    ]),
    [
        case
            seat_status(
                S,
                maps:get(
                    {maps:get(organization_id, S, 0), maps:get(business_identity_id, S, 0)},
                    ByIdentity,
                    undefined
                ),
                NowSec
            )
        of
            undefined -> S;
            Derived -> S#{derived_status => Derived}
        end
     || S <- Seats, is_map(S)
    ];
annotate(_Seats, _Presences, _NowSec) ->
    [].

%% @doc 该派生状态是否参与自动派单：仅 `online`。
-spec dispatchable(status()) -> boolean().
dispatchable(online) -> true;
dispatchable(_Other) -> false.

%% ===================================================================
%% 内部
%% ===================================================================

seat_status(Seat, Presence, NowSec) when is_map(Seat) ->
    case manual_status(Presence) of
        away ->
            away;
        none when is_integer(NowSec) ->
            derive(Presence, NowSec, Seat);
        %% 无注入时钟的过渡形态：不做 TTL 裁决（见 annotate/3 文档）。
        none ->
            online;
        _ ->
            offline
    end;
seat_status(_NotSeat, _Presence, _NowSec) ->
    undefined.

manual_status(Presence) when is_map(Presence) ->
    case maps:get(manual_status, Presence, undefined) of
        <<"away">> -> away;
        _ -> none
    end;
manual_status(_None) ->
    none.

heartbeat_fresh(Presence, NowSec) when is_map(Presence) ->
    case maps:get(last_heartbeat_at, Presence, undefined) of
        Last when is_integer(Last) ->
            Gap = NowSec - Last,
            Gap >= 0 andalso Gap =< ?HEARTBEAT_TTL_SEC;
        _ ->
            false
    end;
heartbeat_fresh(_None, _NowSec) ->
    false.
