-module(group_ds).
%%%
% group_ds 是 group domain service 缩写
%%%
% -export ([find_by_id/2]).
-export([check_avatar/1]).
-export([gid/0]).


-export([member_uids/1]).
-export([dissolve/1]).
-export([join/2]).
-export([leave/2]).

% group_ds:join(1,1), group_ds:join(2,1), group_ds:join(3,1), group_ds:join(4,1).
join(Uid, Gid) ->
    Key = "/:gorup_member/" ++ integer_to_list(Gid),
    case khepri:exists(Key) of
        false ->
            khepri:put(Key, [Uid]);
        true ->
            leave(Uid, Gid),
            {ok, Li} = khepri:get(Key),
            khepri:put(Key, [Uid | Li])
    end.

% group_ds:leave(1,1).
leave(Uid, Gid) ->
    Key = "/:gorup_member/" ++ integer_to_list(Gid),
    case khepri:exists(Key) of
        false ->
            ok;
        true ->
            {ok, Li} = khepri:get(Key),
            khepri:put(Key, lists:delete(Uid, Li))
    end.

% group_ds:member_uids(1).
-spec member_uids(integer()) -> list().
member_uids(Gid) ->
    Key = "/:gorup_member/" ++ integer_to_list(Gid),
    case khepri:get(Key) of
        {error,{khepri,node_not_found, _}} ->
            [];
        {ok, Val} ->
            Val
    end.

% group_ds:dissolve(Gid).
dissolve(Gid) ->
    Key = "/:gorup_member/" ++ integer_to_list(Gid),
    khepri:delete(Key).

-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================

% group_ds:gid().
gid() ->
    case imboy_db:query("select nextval('group_id_seq');") of
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
