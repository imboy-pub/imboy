-module(user_dnd_rule_ds).
-moduledoc "免打扰规则领域服务 / user DND rule domain service。".
%%%
% user_dnd_rule_ds 是 user_dnd_rule domain service 缩写
% 用户免打扰(DND)规则领域服务，封装缓存与时段判断逻辑
% 调用链：Logic -> user_dnd_rule_ds -> user_dnd_rule_repo -> PostgreSQL
%
%% NOTE: 已接线（Q09/G1，2026-10-07）——push_notification_logic 的四个离线
%% 推送入口在离线判定后经 is_dnd_at/2 判定免打扰，命中即静默。
%% DND 规则写入 API（handler）尚未提供：规则面目前由运维/后续 API 落库。
%%%

-include("log.hrl").

-export([find_by_uid/1]).
-export([save/1]).
-export([delete/1]).
-export([is_dnd_at/2]).
-export([is_dnd_now/1]).
-export([is_dnd_now_at/2]).
-export([local_minute_of_day/1]).
-export([local_minute_of_day/2]).

%% 缓存键格式 {Table, Id}，TTL 1 小时（秒）
-define(CACHE_TTL, 3600).
-define(CACHE_KEY(Uid), {user_dnd_rule, Uid}).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 按用户ID查询免打扰规则（带缓存）
%% @param Uid 用户ID（支持 binary 或 integer）
%% @return map() 规则 map，未找到返回空 map
% user_dnd_rule_ds:find_by_uid(1).
-spec find_by_uid(binary() | integer()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(ec_cnv:to_integer(Uid));
find_by_uid(Uid) when is_integer(Uid) ->
    Key = ?CACHE_KEY(Uid),
    Fenced = imboy_cache_policy:is_fenced(Key),
    Fun = fun() ->
        case user_dnd_rule_repo:find_by_uid(Uid) of
            Rule when is_map(Rule), map_size(Rule) > 0, Fenced -> Rule;
            Rule when is_map(Rule) -> imboy_cache:uncached(Rule)
        end
    end,
    MemoKey =
        case Fenced of
            true -> Key;
            false -> {user_dnd_rule, Uid, uncached_v1}
        end,
    imboy_cache:memo(Fun, MemoKey, ?CACHE_TTL).

%% @doc 保存（upsert）免打扰规则并失效缓存
%% @param Data 含 <<"user_id">> 的规则 map（可选 <<"time_zone">>，D02）
%% @return ok | {error, invalid_zone} | {error, term()}（非法 zone 不写库不清缓存）
-spec save(map()) -> ok | {error, term()}.
save(#{<<"user_id">> := Uid0} = Data) ->
    case user_dnd_rule_repo:upsert(Data) of
        ok ->
            imboy_cache:invalidate(?CACHE_KEY(ec_cnv:to_integer(Uid0))),
            ok;
        {error, _} = Err ->
            Err
    end.

%% @doc 删除免打扰规则并失效缓存
%% @param Uid 用户ID（支持 binary 或 integer）
%% @return ok
-spec delete(binary() | integer()) -> ok.
delete(Uid0) ->
    Uid = ec_cnv:to_integer(Uid0),
    ok = user_dnd_rule_repo:delete_by_uid(Uid),
    imboy_cache:invalidate(?CACHE_KEY(Uid)),
    ok.

%% @doc 判断给定时刻（自 0 点起的分钟数，0-1439）是否落在用户免打扰时段
%%   仅当规则 status=1（启用）时生效；支持跨午夜区间（start_min > end_min）
%% @param Uid 用户ID
%% @param NowMin 当前时刻分钟数（0-1439）
%% @return boolean()
% user_dnd_rule_ds:is_dnd_at(1, 1380).  %% 23:00
-spec is_dnd_at(binary() | integer(), non_neg_integer()) -> boolean().
is_dnd_at(Uid, NowMin) ->
    case find_by_uid(Uid) of
        #{<<"status">> := 1, <<"start_min">> := S, <<"end_min">> := E} ->
            in_window(NowMin, S, E);
        _ ->
            false
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% 起止相等视为未设置时段
in_window(_Now, S, E) when S =:= E -> false;
%% 同日区间
in_window(Now, S, E) when S < E -> Now >= S andalso Now < E;
%% 跨午夜区间（如 23:00 -> 08:00）
in_window(Now, S, E) -> Now >= S orelse Now < E.

%% ===================================================================
%% D02（IMBOY-TIMEZONE-DECISIONS-V1，2026-10-27 冻结）：账号级 IANA 时区
%% ===================================================================

%% @doc 判定当前时刻是否落在用户 DND 时段（生产入口）。
%% 规则有 time_zone → 该 zone 当地时分；无/NULL → 部署 fallback zone
%% （配置 dnd_fallback_zone，本部署 = Asia/Shanghai，用户确认）。
%% 时分由 PG `NOW() AT TIME ZONE` 换算（instant→墙钟单向映射，DST 合同
%% 4a：重复小时两次都静默、缺失小时无 instant 命中），复用 in_window/3
%% 跨午夜算法；不修改全局 TZ，不从 offset 推断 IANA。
-spec is_dnd_now(integer()) -> boolean().
is_dnd_now(Uid) ->
    is_dnd_now_at(Uid, undefined).

%% @doc 同 is_dnd_now/1，可注入参考瞬间（RFC3339 binary 或 epoch 毫秒）。
%% 测试确定性入口（R15 AC 用其钉住 DST/跨区用例）；生产传 undefined=NOW()。
%% 判定失败（DB 瞬态故障）fail-open：不静默、照常推送，记错误日志。
-spec is_dnd_now_at(integer(), undefined | binary() | integer()) -> boolean().
is_dnd_now_at(Uid, At) ->
    case find_by_uid(Uid) of
        #{<<"status">> := 1, <<"start_min">> := S, <<"end_min">> := E} = Rule ->
            Zone = resolve_zone(Rule),
            case local_minute_of_day(Zone, At) of
                {ok, Min} ->
                    in_window(Min, S, E);
                {error, Reason} ->
                    ?ERROR_LOG([dnd_local_minute_failed, Uid, Zone, Reason]),
                    false
            end;
        _ ->
            false
    end.

%% @private 规则 zone 解析：显式 time_zone 优先；缺失/空/NULL → fallback。
%% 旧行（迁移前）无列值即走 fallback——R15-4：旧行为按部署 zone 解释，
%% 静默时段不因升级突然移动。
-spec resolve_zone(map()) -> binary().
resolve_zone(Rule) ->
    case maps:get(<<"time_zone">>, Rule, undefined) of
        Zone when is_binary(Zone), Zone =/= <<>> ->
            Zone;
        _ ->
            fallback_zone()
    end.

%% @private 部署 fallback zone（用户 2026-10-27 确认本部署 = Asia/Shanghai）。
-spec fallback_zone() -> binary().
fallback_zone() ->
    config_ds:env(dnd_fallback_zone, <<"Asia/Shanghai">>).

%% @doc 指定 IANA zone 的当地当日分钟数（0..1439），基准 PG NOW()。
%% 生产判定入口；确定性断言用 local_minute_of_day/2 注入参考瞬间。
-spec local_minute_of_day(binary()) -> {ok, non_neg_integer()} | {error, term()}.
local_minute_of_day(Zone) ->
    local_minute_of_day(Zone, undefined).

%% @doc 同 local_minute_of_day/1，At 为 RFC3339 binary 或 epoch 毫秒整数；
%% undefined 时用 PG NOW()。时区换算完全在 PG 侧（tzdata 权威），
%% instant→墙钟单向映射，DST 缺失小时天然无命中、重复小时两次同值。
-spec local_minute_of_day(binary(), undefined | binary() | integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
local_minute_of_day(Zone, undefined) when is_binary(Zone), Zone =/= <<>> ->
    query_minute(Zone, <<"NOW()">>, []);
local_minute_of_day(Zone, At) when is_binary(Zone), Zone =/= <<>>, is_binary(At) ->
    %% RFC3339 binary（epgsql rfc3339 codec）→ timestamptz
    query_minute(Zone, <<"$2::timestamptz">>, [At]);
local_minute_of_day(Zone, At) when is_binary(Zone), Zone =/= <<>>, is_integer(At) ->
    %% epoch 毫秒 → timestamptz（to_timestamp 取秒，double 精度足够分钟粒度）
    query_minute(Zone, <<"to_timestamp($2::bigint / 1000.0)">>, [At]).

%% @private InstantExpr 为基准瞬间表达式（NOW() 或 $2 派生），仅 $1=zone +
%% 可选 $2=注入瞬间；AT TIME ZONE 单参作用于 timestamptz → 得到当地墙钟
%% timestamp（无时区），EXTRACT 语义即当地时分。
query_minute(Zone, InstantExpr, AtParams) ->
    Sql = <<
        "SELECT (EXTRACT(HOUR FROM t)::int * 60 + EXTRACT(MINUTE FROM t)::int) AS min_of_day ",
        "FROM (SELECT (",
        InstantExpr/binary,
        " AT TIME ZONE $1) AS t) s"
    >>,
    case elib_pg:one(Sql, [Zone | AtParams]) of
        {ok, #{<<"min_of_day">> := Min}} -> {ok, Min};
        {error, Reason} -> {error, Reason}
    end.
