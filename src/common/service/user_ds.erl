-module (user_ds).
%%%
% user_ds 是 user domain service 缩写
%%%
-export ([is_offline/1]).
-export ([is_offline/2]).
-export ([online_state/1]).
-export ([mine_state/1]).
-export ([online/3]).
-export ([offline/1]).
-export ([find_by_id/1, find_by_id/2]).
-export ([find_by_ids/1, find_by_ids/2]).
-export ([change_sign/2]).

-include("common.hrl").

-spec is_offline(binary()) -> true | {pid(), binary(), any()}.
%% 检查用户是否在线
is_offline(Uid) when is_integer(Uid)  ->
    is_offline(integer_to_binary(Uid));
is_offline(Uid) when is_list(Uid)  ->
    is_offline(list_to_binary(Uid));
is_offline(Uid) ->
    L1 = chat_store_repo:lookup(Uid),
    case lists:keyfind(Uid, 3, L1) of
        {_, Pid, Uid, Type} ->
            {Pid, Uid, Type};
        false ->
            true
    end.

-spec is_offline(binary(), binary()) -> true | {pid(), binary(), any()}.
%% 检查用户是否在线
is_offline(Uid, ClientSystem) when is_integer(Uid)  ->
    is_offline(integer_to_binary(Uid), ClientSystem);
is_offline(Uid, ClientSystem) when is_list(Uid)  ->
    is_offline(list_to_binary(Uid), ClientSystem);
is_offline(Uid, ClientSystem) ->
    L1 = chat_store_repo:lookup(Uid, ClientSystem),
    case lists:keyfind(Uid, 3, L1) of
        {_, Pid, Uid, Type} ->
            {Pid, Uid, Type};
        false ->
            true
    end.

%% 把Uid标记为online
-spec online(binary(), pid(), any()) -> ok.
online(Uid, Pid, Type) ->
    %%插入数据
    chat_store_repo:dirty_insert(Uid, Pid, Type),
    ok.

-spec offline(pid()) -> ok.
offline(Pid) ->
    chat_store_repo:dirty_delete(Pid),
    ok.

mine_state(Uid) ->
    case user_setting_ds:chat_state_hide(Uid) of
        true ->
            {<<"status">>, hide};
        false ->
            {<<"status">>, online}
    end.

% 获取用户在线状态
online_state(User) ->
    {<<"id">>, Uid} = lists:keyfind(<<"id">>, 1, User),
    case chat_store_repo:lookup(Uid) of
        L1 when length(L1) > 0 ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    % 既然是 hide 就不能够返回hide 状态给API
                    [{<<"status">>, offline}|User];
                false ->
                    [{<<"status">>, online}|User]
            end;
        _ ->
            [{<<"status">>, offline}|User]
    end.

-spec find_by_id(binary()) -> list().
find_by_id(Id) ->
    Column = <<"`id`, `account`,`nickname`,`avatar`,`sign`">>,
    find_by_id(Id, Column).

find_by_id(Id, Column) ->
    case user_repo:find_by_id(Id, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, [Row]} ->
            check_avatar(lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row));
        _ ->
            []
    end.

find_by_ids(Ids) ->
    Column = <<"`id`, `account`,`nickname`,`avatar`,`sign`">>,
    find_by_ids(Ids, Column).

find_by_ids(Ids, Column) ->
    case user_repo:find_by_ids(Ids, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            [check_avatar(lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row)) || Row <- Rows];
        _ ->
            []
    end.

change_sign(Uid, Sign) ->
    Sql = <<"UPDATE `user` SET `sign` = ? WHERE `id` = ?">>,
    mysql_pool:query(Sql, [Sign, Uid]).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 检查 user avatar 是否为空，如果为空设置默认
check_avatar([]) ->
    [];
check_avatar(User) ->
    Default = <<"assets/images/def_avatar.png">>,
    case lists:keyfind(<<"avatar">>, 1, User) of
        {<<"avatar">>, <<>>} ->
            lists:keyreplace(<<"avatar">>, 1, User, {<<"avatar">>, Default});
        {<<"avatar">>, _Aaatar} ->
            User
    end.
