-module(friend_logic).
%%%
%  friend 业务逻辑模块
%%%
-export([add_friend/4]).
-export([confirm_friend/4]).
-export([confirm_friend_resp/2]).
-export([delete_friend/2]).
-export([move_to_category/3]).
-export([information/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec add_friend(integer(),
                 binary(),
                 list(),
                 binary() | integer()) -> ok | {error, binary(), binary()}.
add_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"to">>};
add_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"payload">>};
add_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"created_at">>};
add_friend(CurrentUid, To, Payload, CreatedAt) ->
    %% 统一转换时间戳为 RFC3339 binary 格式
    CreatedAt2 = imboy_dt:to_rfc3339(CreatedAt),
    do_add_friend(CurrentUid, To, Payload, CreatedAt2).


%% @doc 内部函数：实际执行添加好友操作
do_add_friend(CurrentUid, To, Payload, CreatedAt) ->
    ToId = imboy_hashids:decode(To),
    NowTs = imboy_dt:now(),
    From = imboy_hashids:encode(CurrentUid),
    MsgId = <<"af_", From/binary, "_", To/binary>>,
    % ?DEBUG_LOG([is_binary(Payload), Payload]),
    % 存储消息
    _ = msg_s2c_ds:write_msg(CreatedAt, MsgId, Payload, CurrentUid, ToId, NowTs),
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload, MsgId),
    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],
    message_ds:send_next(ToId, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.


-spec confirm_friend(integer(), binary(), binary(), binary()) -> {ok, integer(), binary(), binary()} | {error, binary(), binary()}.
confirm_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"from">>};
confirm_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"to">>};
confirm_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"payload">>};
confirm_friend(CurrentUid, From, To, Payload) ->
    FromID = imboy_hashids:decode(From),
    ToID = imboy_hashids:decode(To),
    NowTs = imboy_dt:now(),
    Payload2 = jsone:decode(Payload, [{object_format, map}]),

    FromSetting = maps:get(<<"from">>, Payload2, #{}),
    % Remark1 为 from 对 to 定义的 remark
    Remark1 = maps:get(<<"remark">>, FromSetting, <<>>),
    % ToTag 为 from 对 to 定义的 tag
    ToTag = maps:get(<<"tag">>, FromSetting, <<>>),
    Source = maps:get(<<"source">>, FromSetting, <<>>),
    FromToIsFriend = friend_ds:is_friend(FromID, ToID),
    % 好友关系写入数据库
    friend_repo:confirm_friend(FromToIsFriend, FromID, ToID, Remark1, FromSetting#{<<"is_from">> => 1}, ToTag, NowTs),

    ToSetting = maps:get(<<"to">>, Payload2, #{}),
    ToFromIsFriend = friend_ds:is_friend(ToID, FromID),
    % Remark2 为 to 对 from 定义的 remark
    Remark2 = maps:get(<<"remark">>, ToSetting, <<>>),
    % FromTag 为 to 对 from 定义的 tag
    FromTag = maps:get(<<"tag">>, ToSetting, <<>>),
    % 好友关系写入数据库
    friend_repo:confirm_friend(ToFromIsFriend,
                               ToID,
                               FromID,
                               Remark2,
                               ToSetting#{<<"source">> => Source},
                               FromTag,
                               NowTs),

    % 因为是 ToID 通过API确认的，所以只需要给FromID 发送消息
    MsgId = <<"afc_", From/binary, "_", To/binary>>,
    MsgType = maps:get(<<"msg_type">>, Payload2, <<>>),
    % Payload3 = confirm_friend_resp(ToID, Remark1),
    Payload6 = Payload2#{
        <<"is_from">> => 1,
        <<"source">> => Source,
        <<"msg_type">> => MsgType
    },

    % 存储消息
    _ = msg_s2c_ds:write_msg(NowTs, MsgId, Payload6, CurrentUid, FromID, NowTs),

    % 这里的From To 需要对调，离线消息需要对调
    Msg = message_ds:assemble_msg(<<"S2C">>, To, From, Payload6, MsgId),

    % ?DEBUG_LOG(Msg),
    MsLi = [0, 1500, 1500, 3000, 5000, 7000],
    message_ds:send_next(FromID, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

    if
        ToTag == <<>> ->
            ok;
        true ->
            ToTag2 = [ I || I <- binary:split(ToTag, <<",">>, [global]), I /= <<>> ],
            _ = user_tag_relation_logic:add(FromID, 2, ToID, ToTag2),
            ok
    end,
    if
        FromTag == <<>> ->
            ok;
        true ->
            FromTag2 = [ I || I <- binary:split(FromTag, <<",">>, [global]), I /= <<>> ],
            _ = user_tag_relation_logic:add(ToID, 2, FromID, FromTag2),
            ok
    end,
    % 为了简单，删除好友关系清理两个缓存
    imboy_cache:flush({is_friend, FromID, ToID}),
    imboy_cache:flush({is_friend, ToID, FromID}),
    {ok, FromID, Remark2, Source}.


confirm_friend_resp(Uid, Remark) ->
    Column = <<"id,account,nickname,avatar,gender,sign,region,status">>,
    User = user_logic:find_by_id(Uid, Column),
    % [{<<"remark">>, Remark} | imboy_hashids:replace_id(User)].
    User#{
        <<"id">> => imboy_hashids:encode(Uid),
        <<"remark">> => Remark
    }.


-spec delete_friend(integer(), binary() | integer()) -> ok.
delete_friend(CurrentUid, Uid) when is_binary(Uid) ->
    Uid2 = imboy_hashids:decode(Uid),
    delete_friend(CurrentUid, Uid2);
delete_friend(CurrentUid, Uid) ->
    _ = friend_repo:delete(CurrentUid, Uid),
    _ = user_tag_relation_repo:delete(<<"2">>, CurrentUid, Uid),
    % 为了简单，删除好友关系清理两个缓存
    imboy_cache:flush({is_friend, CurrentUid, Uid}),
    imboy_cache:flush({is_friend, Uid, CurrentUid}),
    ok.


move_to_category(CurrentUid, Uid, CategoryId) ->
    _ = friend_repo:move_to_category(CurrentUid, Uid, CategoryId),
    ok.


information(_CurrentUid, _Uid) ->
    % TODO
    % ?DEBUG_LOG([CurrentUid, Uid]),
    Info = [],
    Info.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
