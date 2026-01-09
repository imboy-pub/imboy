-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).
-export([gid/0]).


-export([member_uids/1]).
-export([is_member/2]).
-export([join/2]).
-export([leave/2]).
-export([dissolve/1]).

-include("cache.hrl").
-include("log.hrl").

-define(GROUP_CACHE_KEY(Gid), {group, Gid}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 检查用户是否为群组成员
%%
%% 检查指定用户是否为指定群组的成员
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns boolean() true表示是成员，false表示不是成员
% group_ds:is_member(62913, 11)
-spec is_member(integer(), integer()) -> boolean().
is_member(Uid, Gid) ->
    Res = group_member_repo:find(Gid, Uid, <<"id">>),
    % ?DEBUG_LOG(io:format("is_member/2  Uid ~p, Gid ~p, Res ~p, Size ~p\n", [Uid, Gid, Res, map_size(Res)])),
    case map_size(Res) of
        0 ->
            false;
        _ ->
            true
    end.

%% @doc 获取群组成员用户ID列表
%%
%% 获取指定群组所有成员的用户ID列表，使用缓存提高性能
%%
%% @param Gid 群组ID
%% @returns list() 成员用户ID列表
% group_ds:member_uids(1).
-spec member_uids(integer()) -> [integer()].
member_uids(Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case imboy_cache:get(CacheKey) of
        undefined ->
            case group_member_repo:list_by_gid(Gid, <<"user_id">>) of
                {ok, []} ->
                    [];
                {ok, Items} ->
                    Li = [Uid || #{<<"user_id">> := Uid} <- Items],
                    imboy_cache:set(CacheKey, Li, ?HOUR),
                    Li;
                _ ->
                    []
            end;
        {ok, Li} ->
            Li
    end.

%% @doc 用户加入群组
%%
%% 将用户添加到群组成员缓存中，如果用户已存在则不做任何操作
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:join(1,1), group_ds:join(2,1), group_ds:join(3,1), group_ds:join(4,1).
-spec join(integer(), integer()) -> ok.
join(Uid, Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case member_uids(Gid) of
        [] ->
            imboy_cache:set(CacheKey, [Uid], ?HOUR);
        Li ->
            case lists:member(Uid, Li) of
                true ->
                    ok;
                false ->
                    imboy_cache:set(CacheKey, [Uid | Li], ?HOUR)
            end
    end.

%% @doc 用户离开群组
%%
%% 从群组成员缓存中移除指定用户
%%
%% @param Uid 用户ID
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:leave(1,1).
-spec leave(integer(), integer()) -> ok.
leave(Uid, Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case member_uids(Gid) of
        [] ->
            ok;
        Li ->
            imboy_cache:set(CacheKey, lists:delete(Uid, Li))
    end.

%% @doc 解散群组
%%
%% 清除群组相关的缓存数据
%%
%% @param Gid 群组ID
%% @returns ok 表示操作成功
% group_ds:dissolve(Gid).
-spec dissolve(integer()) -> ok.
dissolve(Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    imboy_cache:flush(CacheKey).

% group_ds:member_uids(1).
% -spec member_uids(integer()) -> list().
% member_uids(Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:get(Key) of
%         {error,{khepri,node_not_found, _}} ->
%             [];
%         {ok, Val} ->
%             Val
%     end.

% group_ds:join(1,1), group_ds:join(2,1), group_ds:join(3,1), group_ds:join(4,1).
% join(Uid, Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:exists(Key) of
%         false ->
%             khepri:put(Key, [Uid]);
%         true ->
%             leave(Uid, Gid),
%             {ok, Li} = khepri:get(Key),
%             khepri:put(Key, [Uid | Li])
%     end.

% group_ds:leave(1,1).
% leave(Uid, Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     case khepri:exists(Key) of
%         false ->
%             ok;
%         true ->
%             {ok, Li} = khepri:get(Key),
%             khepri:put(Key, lists:delete(Uid, Li))
%     end.

% group_ds:dissolve(Gid).
% dissolve(Gid) ->
%     Key = "/:gorup_member/" ++ integer_to_list(Gid),
%     khepri:delete(Key).

%% @doc 生成新的群组ID
%%
%% 获取一个新的群组ID，显式使用 public schema 的序列，避免受 search_path 影响
%%
%% @returns integer() 新的群组ID
% group_ds:gid().
-spec gid() -> integer().
gid() ->
    {ok, [#{<<"gid">> := Gid}]} = imboy_pg:query("select nextval('public.group_id_seq') as gid", []),
    Gid.

%% @doc 检查和设置群组头像
%%
%% 检查群组头像是否为空，如果为空则设置默认头像
%%
%% @param Group 群组信息列表
%% @returns list() 处理后的群组信息列表
-spec check_avatar(map()) -> map().
check_avatar(Group) when is_map(Group) ->
    Default = <<"/static/image/group_default_avatar.jpeg">>,
    Avatar = maps:get(<<"avatar">>, Group, <<>>),
    case Avatar of
        <<>> -> Group#{<<"avatar">> => Default};
        _ -> Group
    end;
check_avatar(_) ->
    #{}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
