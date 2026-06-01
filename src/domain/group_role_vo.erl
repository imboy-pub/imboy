%%% @doc 群成员角色值对象 / Group Role Value Object
%%%
%%% DDD 充血改造地基（Phase 0 / T0.2）：将裸整型角色码封装为 opaque 值对象，
%%% 构造期校验取值落在合法枚举内（权威源 include/group_role.hrl）。
%%%
%%% Opaque VO over the integer role code, validated against the canonical
%%% enum at construction. Keeps role validity out of scattered guards.
-module(group_role_vo).

-export([new/1, value/1, equal/2, name/1]).

-export_type([t/0]).

-include("group_role.hrl").

-record(group_role, {v :: ?ROLE_UNDEFINED..?ROLE_VICE_OWNER}).
-opaque t() :: #group_role{}.

%% @doc 构造角色值对象，校验落在 [ROLE_UNDEFINED, ROLE_VICE_OWNER]。
-spec new(integer()) -> {ok, t()} | {error, invalid_group_role}.
new(V) when is_integer(V), V >= ?ROLE_UNDEFINED, V =< ?ROLE_VICE_OWNER ->
    {ok, #group_role{v = V}};
new(_) ->
    {error, invalid_group_role}.

%% @doc 取回底层角色码。
-spec value(t()) -> ?ROLE_UNDEFINED..?ROLE_VICE_OWNER.
value(#group_role{v = V}) ->
    V.

%% @doc 等值比较（按角色码）。
-spec equal(t(), t()) -> boolean().
equal(#group_role{v = A}, #group_role{v = B}) ->
    A =:= B.

%% @doc 角色中文显示名（未命名角色回退「未定义」）。
-spec name(t()) -> binary().
name(#group_role{v = V}) ->
    maps:get(V, ?ROLE_NAMES, <<"未定义"/utf8>>).
