-module(friend_logic).
%%%
%  friend 业务逻辑模块
%%%
-export([search/1]).
-export([add_friend/4]).
-export([confirm_friend/4]).
-export([confirm_friend_resp/2]).
-export([move_to_category/3]).
-export([information/2]).
-export([category_friend/1]).
-export([friend_list/1]).

-include_lib("imboy/include/common.hrl").


-spec add_friend(CurrentUid::integer(),
    To::binary(),
    Payload::list(),
    CreatedAt::integer() | CreatedAt::binary()
) -> ok | {error, Msg::binary(), Param::binary()}.
add_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"to">>};
add_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"payload">>};
add_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"created_at">>};
add_friend(CurrentUid, To, Payload, CreatedAt) when is_binary(CreatedAt) ->
    add_friend(CurrentUid, To, Payload, binary_to_integer(CreatedAt));
add_friend(CurrentUid, To, Payload, CreatedAt) ->
    ToId = imboy_hashids:uid_decode(To),
    NowTs = imboy_dt:milliseconds(),
    From = imboy_hashids:uid_encode(CurrentUid),
    Id = <<"af_", From/binary, "_", To/binary>>,
    % ?LOG([is_binary(Payload), Payload]),
    % 存储消息
    msg_s2c_ds:write_msg(CreatedAt, Id, Payload,
        CurrentUid, ToId, NowTs),
    Msg = [{<<"id">>, Id},
        {<<"type">>, <<"S2C">>},
        {<<"from">>, From},
        {<<"to">>, To},
        {<<"payload">>, Payload},
        {<<"created_at">>, CreatedAt},
        {<<"server_ts">>, NowTs}
    ],
    % ?LOG(Msg),
    _TimerRefList = message_ds:send(ToId,
        jsone:encode(Msg, [native_utf8]),
        1),
    ok.

-spec confirm_friend(CurrentUid::integer(),
    From::binary(),
    To::binary(),
    Payload::list()
) -> {ok, list()} | {error, Msg::binary(), Param::binary()}.
confirm_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"from">>};
confirm_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"to">>};
confirm_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"payload">>};
confirm_friend(CurrentUid, From, To, Payload) ->
    FromID = imboy_hashids:uid_decode(From),
    ToID = imboy_hashids:uid_decode(To),
    NowTs = imboy_dt:milliseconds(),
    Payload2 = jsone:decode(Payload, [{object_format, proplist}]),

    FromSetting = proplists:get_value(<<"from">>, Payload2),
    % Remark1 为 from 对 to 定义的 remark
    Remark1 = proplists:get_value(<<"remark">>, FromSetting),
    FromToIsFriend = friend_ds:is_friend(FromID, ToID),
    friend_repo:confirm_friend(FromToIsFriend,
        FromID, ToID, Remark1, FromSetting, NowTs),

    ToSetting = proplists:get_value(<<"to">>, Payload2),
    ToFromIsFriend = friend_ds:is_friend(ToID, FromID),
    % Remark2 为 to 对 from 定义的 remark
    Remark2 = proplists:get_value(<<"remark">>, ToSetting),
    friend_repo:confirm_friend(ToFromIsFriend,
        ToID, FromID, Remark2, ToSetting, NowTs),

    % 因为是 ToID 通过API确认的，所以只需要给FromID 发送消息
    Id = <<"afc_", From/binary, "_", To/binary>>,
    MsgType = proplists:get_value(<<"msg_type">>, Payload2),
    Payload3 = confirm_friend_resp(ToID, Remark1),
    Payload4 = [{<<"msg_type">>, MsgType} | Payload3],

    % 存储消息
    msg_s2c_ds:write_msg(NowTs, Id, Payload4, CurrentUid, FromID, NowTs),

    Msg = [{<<"id">>, Id},
        {<<"type">>, <<"S2C">>},
        % 这里的需要对调，离线消息需要对调
        {<<"from">>, To},
        {<<"to">>, From},
        {<<"payload">>, Payload4},
        {<<"server_ts">>, NowTs}
    ],
    % ?LOG(Msg),
    _TimerRefList = message_ds:send(FromID,
        jsone:encode(Msg, [native_utf8]),
        1),
    {ok, FromID, Remark2}.

%%% 查找非好友
search(Uid) ->
    % 只能够搜索“用户被允许搜索”的用户
    %
    FriendIDs = friend_ids(Uid),
    Info = FriendIDs,
    Info.


move_to_category(CurrentUid, Uid, CategoryId) ->
    friend_repo:move_to_category(CurrentUid, Uid, CategoryId),
    ok.


information(CurrentUid, Uid) ->
    ?LOG([CurrentUid, Uid]),
    Info = [],
    Info.


friend_list(Uid) ->
    Column = <<"`to_user_id`,`remark`,`category_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            [];
        Friends ->
            FriendRemark = [{Id, {Remark, Cid}} ||
                               [{<<"to_user_id">>, Id},
                                {<<"remark">>, Remark},
                                {<<"category_id">>, Cid}] <- Friends],
            % ?LOG(Friends),
            Uids = [Id || {Id, _} <- FriendRemark],
            Users = user_logic:find_by_ids(Uids),
            % 替换朋友备注信息
            Users2 = [replace_remark({Id, Row}, FriendRemark, false) ||
                         [{<<"id">>, Id} | _] = Row <- Users],
            % 获取用户在线状态
            [user_logic:online_state(User) || {_Id, User} <- Users2]
    end.


category_friend(Uid) ->
    Column = <<"`to_user_id`,`remark`,`category_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            [];
        Friends ->
            FriendRemark = [{Id, {Remark, Cid}} ||
                               [{<<"to_user_id">>, Id},
                                {<<"remark">>, Remark},
                                {<<"category_id">>, Cid}] <- Friends],
            % ?LOG(Friends),
            Uids = [Id || {Id, _} <- FriendRemark],
            Users = user_logic:find_by_ids(Uids),
            % 替换朋友备注信息
            Users2 = [replace_remark({Id, Row}, FriendRemark, true) ||
                         [{<<"id">>, Id} | _] = Row <- Users],
            % 获取用户在线状态
            Users3 = [{Id, user_logic:online_state(User)} ||
                         {Id, User} <- Users2],
            % 把用户归并到相应的分组
            Groups = friend_category_ds:find_by_uid(Uid),
            [append_group_list(G, Users3) || G <- Groups]
    end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

confirm_friend_resp(Uid, Remark) ->
    Column = <<"`id`,`account`,`nickname`,`avatar`,`gender`,`sign`,`region`,`status`">>,
    User = user_logic:find_by_id(Uid, Column),
    [{<<"remark">>, Remark} | imboy_hashids:replace_id(User)].

%% 把用户归并到相应的分组
append_group_list(Group, Users) ->
    {<<"id">>, Cid} = lists:keyfind(<<"id">>, 1, Group),
    List = [User || {Id, User} <- Users, Cid == Id],
    [{<<"list">>, List} | Group].


%% 替换朋友备注信息
replace_remark({Uid, Row}, FriendRemark, Replace) ->
    case lists:keyfind(Uid, 1, FriendRemark) of
        {Uid, {<<>>, Cid}} ->
            {Cid, Row};
        {Uid, {Remark, Cid}} when Replace =:= true ->
            Row2 = lists:keyreplace(<<"account">>,
                                    1,
                                    Row,
                                    {<<"account">>, Remark}),
            {Cid, Row2};
        {Uid, {Remark, Cid}} ->
            {Cid, [{<<"remark">>, Remark} | Row]}
    end.


friend_ids(Uid) ->
    Column = <<"`to_user_id`">>,
    case friend_ds:find_by_uid(Uid, Column) of
        [] ->
            [];
        Friends ->
            [ID || [{<<"to_user_id">>, ID}] <- Friends]
    end.
