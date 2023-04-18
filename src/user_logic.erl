-module(user_logic).
%%%
% user 业务逻辑模块
%%%

-include_lib("imboy/include/log.hrl").
-include_lib("imboy/include/chat.hrl").

-export([online/4]).
-export([offline/3]).
-export([idle_timeout/1]).

-export([is_online/1]).
-export([is_online/2]).
-export([online_state/1]).
-export([mine_state/1]).
-export([find_by_id/1, find_by_id/2]).
-export([find_by_ids/1, find_by_ids/2]).
-export([update/3]).

% 顺序不能够随意修改，id 要放到第一个
-define (DEF_USER_COLUMN, <<"`id`,`account`,`mobile`,
        `nickname`,`avatar`,`sign`,`gender`,`region`">>).


%dtype 设备类型 web ios android macos windows等
-spec online(Uid::integer(), Pid::pid(), DType::binary(), DID :: binary()) -> ok.
online(Uid, Pid, DType, DID) ->
    ?LOG(["user_logic/online/4", Uid, Pid, DType, DID]),
    % 把Uid标记为online
    chat_online:dirty_insert(Uid, Pid, DType, DID),
    % syn:register(?CHAT_SCOPE, Uid, Pid,  #{did => DID, dtype => DType}),
    % syn:register(?CHAT_SCOPE, {Uid, DType}, Pid,  #{did => DID, dtype => DType}),
    % 用异步队列实现 检查离线消息 等
    user_server:cast_online(Uid, Pid, DID),
    ok.


-spec offline(Uid::integer(), Pid :: pid(), DID :: binary()) -> ok.
offline(Uid, Pid, DID) ->
    chat_online:dirty_delete(Pid),
    % syn:unregister(?CHAT_SCOPE, Uid),
    % 检查离线消息 用异步队列实现
    user_server:cast_offline(Uid, Pid, DID).


% 设置用户websocket超时时间，默认60秒
idle_timeout(_Uid) ->
    60000.

-spec is_online(binary() | integer() | list()) ->
          false | {pid(), binary(), any()}.
%% 检查用户是否在线
is_online(Uid) when is_integer(Uid) ->
    is_online(integer_to_binary(Uid));
is_online(Uid) when is_list(Uid) ->
    is_online(list_to_binary(Uid));
is_online(Uid) ->
    L1 = chat_online:lookup(Uid),
    case lists:keyfind(Uid, 3, L1) of
        {_, Pid, Uid, _DType, DID} ->
            {Pid, Uid, DID};
        false ->
            false
    end.
    % case syn:lookup(?CHAT_SCOPE, Uid) of
    %     {Pid, #{did := DID}} ->
    %         {Pid, Uid, DID};
    %     {_Pid, undefined} ->
    %         false;
    %     undefined ->
    %         false
    % end.


-spec is_online(binary(), binary()) -> false | {pid(), binary(), any()}.
%% 检查用户是否在线
is_online(Uid, ClientSystem) when is_integer(Uid) ->
    is_online(integer_to_binary(Uid), ClientSystem);
is_online(Uid, ClientSystem) when is_list(Uid) ->
    is_online(list_to_binary(Uid), ClientSystem);
is_online(Uid, ClientSystem) ->
    L1 = chat_online:lookup(Uid, ClientSystem),
    case lists:keyfind(Uid, 3, L1) of
        {_, Pid, Uid, _DType, DID} ->
            {Pid, Uid, DID};
        false ->
            false
    end.


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
    case chat_online:lookup(Uid) of
        L1 when length(L1) > 0 ->
            case user_setting_ds:chat_state_hide(Uid) of
                true ->
                    % 既然是 hide 就不能够返回hide 状态给API
                    [{<<"status">>, offline} | User];
                false ->
                    [{<<"status">>, online} | User]
            end;
        _ ->
            [{<<"status">>, offline} | User]
    end.

-spec find_by_id(binary()) -> list().
find_by_id(Id) ->
    find_by_id(Id, ?DEF_USER_COLUMN).

find_by_id(Id, Column) when is_binary(Id) ->
    find_by_id(imboy_hashids:uid_decode(Id), Column);
find_by_id(Id, Column) ->
    case user_repo:find_by_id(Id, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, [Row]} ->
            check_avatar(lists:zipwith(fun(X, Y) -> {X, Y} end,
                                       ColumnList,
                                       Row));
        _ ->
            []
    end.


find_by_ids(Ids) ->
    find_by_ids(Ids, ?DEF_USER_COLUMN).


find_by_ids([], _) ->
    [];
find_by_ids(Ids, Column) ->
    case user_repo:find_by_ids(Ids, Column) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            [check_avatar(lists:zipwith(fun(X, Y) -> {X, Y} end,
                                        ColumnList,
                                        Row)) || Row <- Rows];
        _ ->
            []
    end.


-spec update(Uid::any(), Field::binary(), list() | binary()) ->
    ok | {error, {integer(), binary(), Msg::binary()}}.
update(Uid, <<"sign">>, Val) ->
    mysql_pool:update(<<"user">>, Uid, <<"sign">>, Val);
update(Uid, <<"nickname">>, Val) ->
    mysql_pool:update(<<"user">>, Uid, <<"nickname">>, Val);
update(Uid, <<"avatar">>, Val) ->
    mysql_pool:update(<<"user">>, Uid, <<"avatar">>, Val);

update(Uid, <<"region">>, Val) ->
    mysql_pool:update(<<"user">>, Uid, <<"region">>, Val);

% 性别 1 男  2 女  3 保密
update(Uid, <<"gender">>, <<"1">>) ->
    mysql_pool:update(<<"user">>, Uid, <<"gender">>, <<"1">>);
update(Uid, <<"gender">>, <<"2">>) ->
    mysql_pool:update(<<"user">>, Uid, <<"gender">>, <<"2">>);
update(Uid, <<"gender">>, <<"3">>) ->
    mysql_pool:update(<<"user">>, Uid, <<"gender">>, <<"3">>);

update(_Uid, _Field, _Val) ->
    {error, {1, <<"">>, <<"Unsupported field">>}}.


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
            lists:keyreplace(<<"avatar">>,
                             1,
                             User,
                             {<<"avatar">>, Default});
        {<<"avatar">>, _Aaatar} ->
            User
    end.
