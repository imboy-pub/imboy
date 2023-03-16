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
-spec online(UID::any(), Pid::pid(), DType::binary(), DID :: binary()) -> ok.
online(UID, Pid, DType, DID) ->
    ?LOG(["user_logic/online/4", UID, Pid, DType, DID]),
    % 把UID标记为online
    chat_online:dirty_insert(UID, Pid, DType, DID),
    % syn:register(?CHAT_SCOPE, UID, Pid,  #{did => DID, dtype => DType}),
    % syn:register(?CHAT_SCOPE, {UID, DType}, Pid,  #{did => DID, dtype => DType}),
    % 用异步队列实现 检查离线消息 等
    user_server:cast_online(UID, Pid, DID),
    ok.


-spec offline(UID :: any(), Pid :: pid(), DID :: binary()) -> ok.
offline(UID, Pid, DID) ->
    chat_online:dirty_delete(Pid),
    % syn:unregister(?CHAT_SCOPE, UID),
    % 检查离线消息 用异步队列实现
    user_server:cast_offline(UID, Pid, DID).


% 设置用户websocket超时时间，默认60秒
idle_timeout(_UID) ->
    60000.

-spec is_online(binary() | integer() | list()) ->
          false | {pid(), binary(), any()}.
%% 检查用户是否在线
is_online(UID) when is_integer(UID) ->
    is_online(integer_to_binary(UID));
is_online(UID) when is_list(UID) ->
    is_online(list_to_binary(UID));
is_online(UID) ->
    L1 = chat_online:lookup(UID),
    case lists:keyfind(UID, 3, L1) of
        {_, Pid, UID, _DType, DID} ->
            {Pid, UID, DID};
        false ->
            false
    end.
    % case syn:lookup(?CHAT_SCOPE, UID) of
    %     {Pid, #{did := DID}} ->
    %         {Pid, UID, DID};
    %     {_Pid, undefined} ->
    %         false;
    %     undefined ->
    %         false
    % end.


-spec is_online(binary(), binary()) -> false | {pid(), binary(), any()}.
%% 检查用户是否在线
is_online(UID, ClientSystem) when is_integer(UID) ->
    is_online(integer_to_binary(UID), ClientSystem);
is_online(UID, ClientSystem) when is_list(UID) ->
    is_online(list_to_binary(UID), ClientSystem);
is_online(UID, ClientSystem) ->
    L1 = chat_online:lookup(UID, ClientSystem),
    case lists:keyfind(UID, 3, L1) of
        {_, Pid, UID, _DType, DID} ->
            {Pid, UID, DID};
        false ->
            false
    end.


mine_state(UID) ->
    case user_setting_ds:chat_state_hide(UID) of
        true ->
            {<<"status">>, hide};
        false ->
            {<<"status">>, online}
    end.


% 获取用户在线状态
online_state(User) ->
    {<<"id">>, UID} = lists:keyfind(<<"id">>, 1, User),
    case chat_online:lookup(UID) of
        L1 when length(L1) > 0 ->
            case user_setting_ds:chat_state_hide(UID) of
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


-spec update(UID::any(), Field::binary(), list() | binary()) ->
    ok | {error, {integer(), binary(), Msg::binary()}}.
update(UID, <<"sign">>, Val) ->
    mysql_pool:update(<<"user">>, UID, <<"sign">>, Val);
update(UID, <<"nickname">>, Val) ->
    mysql_pool:update(<<"user">>, UID, <<"nickname">>, Val);
update(UID, <<"avatar">>, Val) ->
    mysql_pool:update(<<"user">>, UID, <<"avatar">>, Val);

update(UID, <<"region">>, Val) ->
    mysql_pool:update(<<"user">>, UID, <<"region">>, Val);

% 性别 1 男  2 女  3 保密
update(UID, <<"gender">>, <<"1">>) ->
    mysql_pool:update(<<"user">>, UID, <<"gender">>, <<"1">>);
update(UID, <<"gender">>, <<"2">>) ->
    mysql_pool:update(<<"user">>, UID, <<"gender">>, <<"2">>);
update(UID, <<"gender">>, <<"3">>) ->
    mysql_pool:update(<<"user">>, UID, <<"gender">>, <<"3">>);

update(_UID, _Field, _Val) ->
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
