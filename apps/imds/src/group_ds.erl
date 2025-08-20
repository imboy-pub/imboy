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

-include_lib("imlib/include/cache.hrl").
-include_lib("imlib/include/log.hrl").

-define(GROUP_CACHE_KEY(Gid), {group, Gid}).

%% ===================================================================
%% API
%% ===================================================================

% group_ds:is_member(62913, 11)
is_member(Uid, Gid) ->
    Res = group_member_repo:find(Gid, Uid, <<"id">>),
    % ?DEBUG_LOG(io:format("is_member/2  Uid ~p, Gid ~p, Res ~p, Size ~p\n", [Uid, Gid, Res, map_size(Res)])),
    case map_size(Res) of
        0 ->
            false;
        _ ->
            true
    end.

% group_ds:member_uids(1).
-spec member_uids(integer()) -> list().
member_uids(Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case imboy_cache:get(CacheKey) of
        undefined ->
            case group_member_repo:list_by_gid(Gid, <<"user_id">>) of
                {ok, _, []} ->
                    [];
                {ok, _ColumnLi, Items} ->
                    Li = [Uid || {Uid} <- Items],
                    imboy_cache:set(CacheKey, Li, ?HOUR),
                    Li;
                _ ->
                    []
            end;
        {ok, Li} ->
            Li
    end.

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

% group_ds:leave(1,1).
leave(Uid, Gid) ->
    CacheKey = ?GROUP_CACHE_KEY(Gid),
    case member_uids(Gid) of
        [] ->
            ok;
        Li ->
            imboy_cache:set(CacheKey, lists:delete(Uid, Li))
    end.

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

% group_ds:gid().
%% @doc 获取一个新的群组ID，显式使用 public schema 的序列，避免受 search_path 影响
gid() ->
    case imboy_db:query("select nextval('public.group_id_seq');") of
        {ok,_,[{Gid}]} ->
            Gid
    end.

-spec check_avatar(list()) -> list().
%% 检查 group avatar 是否为空，如果为空设置默认
check_avatar([]) ->
    [];
check_avatar(Group) ->
    Default = <<"/static/image/group_default_avatar.jpeg">>,
    case lists:keyfind(<<"avatar">>, 1, Group) of
        {<<"avatar">>, <<>>} ->
            lists:keyreplace(<<"avatar">>, 1, Group, {<<"avatar">>, Default});
        {<<"avatar">>, _Aaatar} ->
            Group
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
