-module (user_ds).
%%%
% user_ds 是 user domain service 缩写
%%%
-export ([is_offline/1]).
-export ([online_state/1]).
-export ([online/3]).
-export ([offline/1]).
-export ([find_by_id/1, find_by_id/2]).
-export ([find_by_ids/1, find_by_ids/2]).

-include("imboy.hrl").

-spec is_offline(integer()) -> true | {integer(), pid(), any()}.
%% 检查用户是否在线
is_offline(Uid) ->
    L1 = chat_store_repo:lookup(Uid),
    case lists:keyfind(Uid, 1, L1) of
        {Uid, Pid, Type} ->
            {Uid, Pid, Type};
        false ->
            true
    end.

-spec online(integer(), pid(), any()) -> ok.
online(Uid, Pid, Type) ->
    %%插入数据
    chat_store_repo:dirty_insert(Uid, Pid, Type),
    ok.

-spec offline(integer()) -> ok.
offline(Uid) ->
    chat_store_repo:dirty_delete(Uid),
    ok.

% 获取用户在线状态
online_state(User) ->
    {<<"id">>, Uid} = lists:keyfind(<<"id">>, 1, User),
    case is_offline(Uid) of
        {_Uid, _Pid, _Type} ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    [{<<"status">>, offline}|User];
                false ->
                    [{<<"status">>, online}|User]
            end;
        true ->
            [{<<"status">>, offline}|User]
    end.

-spec find_by_id(binary()) -> list().
find_by_id(Id) ->
    Column = <<"`id`,`username`,`avatar`,`sign`">>,
    find_by_id(Id, Column).

find_by_id(Id, Column) ->
    {ok, ColumnList, Rows} = user_repo:find_by_id(Id, Column),
    if
        length(Rows) == 0  ->
            {ok, "用户不存在"};
        true ->
            [Row|_] = Rows,
            check_avatar(lists:zipwith(fun(X, Y) -> {X,Y} end, ColumnList, Row))
    end.

find_by_ids(Ids) ->
    Column = <<"`id`,`username`,`avatar`,`sign`">>,
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

%%%%%

%% 检查 user avatar 是否为空，如果为空设置默认
check_avatar([]) ->
    [];
check_avatar(User) ->
    Default = <<"/static/image/user_default_avatar.jpeg">>,
    case lists:keyfind(<<"avatar">>, 1, User) of
        {<<"avatar">>, <<>>} ->
            lists:keyreplace(<<"avatar">>, 1, User, {<<"avatar">>, Default});
        {<<"avatar">>, _Aaatar} ->
            User
    end.
