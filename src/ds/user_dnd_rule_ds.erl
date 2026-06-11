-module(user_dnd_rule_ds).
%%%
% user_dnd_rule_ds 是 user_dnd_rule domain service 缩写
% 用户免打扰(DND)规则领域服务，封装缓存与时段判断逻辑
% 调用链：Logic -> user_dnd_rule_ds -> user_dnd_rule_repo -> PostgreSQL
%
%% NOTE: 未接线——等待 DND handler/logic 落地（见根 CLAUDE.md 功能表）
%%%

-include("log.hrl").

-export([find_by_uid/1]).
-export([save/1]).
-export([delete/1]).
-export([is_dnd_at/2]).

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
    case imboy_cache:get(Key) of
        undefined ->
            Rule = user_dnd_rule_repo:find_by_uid(Uid),
            imboy_cache:set(Key, Rule, ?CACHE_TTL),
            Rule;
        {ok, Rule} ->
            Rule
    end.

%% @doc 保存（upsert）免打扰规则并失效缓存
%% @param Data 含 <<"user_id">> 的规则 map
%% @return ok
-spec save(map()) -> ok.
save(#{<<"user_id">> := Uid0} = Data) ->
    ok = user_dnd_rule_repo:upsert(Data),
    imboy_cache:delete(?CACHE_KEY(ec_cnv:to_integer(Uid0))),
    ok.

%% @doc 删除免打扰规则并失效缓存
%% @param Uid 用户ID（支持 binary 或 integer）
%% @return ok
-spec delete(binary() | integer()) -> ok.
delete(Uid0) ->
    Uid = ec_cnv:to_integer(Uid0),
    ok = user_dnd_rule_repo:delete_by_uid(Uid),
    imboy_cache:delete(?CACHE_KEY(Uid)),
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
