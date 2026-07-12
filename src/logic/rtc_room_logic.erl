-module(rtc_room_logic).

%%%
% RTC 房间业务逻辑（LiveKit SFU）
% RTC room business logic (LiveKit SFU)
%
% 职责：成员资格校验 + 房间命名 + 签发 LiveKit 接入 JWT。
% 媒体面完全由 LiveKit 承载，本模块不接触任何媒体数据。
%%%

-export([join/4]).
-export([room_name/2]).

%% LiveKit 接入 token 有效期（秒）：房间入场券语义，短时有效
-define(TOKEN_TTL_SECONDS, 600).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 加入 RTC 房间：校验资格并签发 LiveKit token
%% Join an RTC room: verify membership and issue a LiveKit access token
%% Kind: <<"group">> | <<"c2c">>
%% @returns {ok, #{ws_url, token, room_name}} | {error, binary()}
-spec join(integer(), binary(), binary(), integer()) -> {ok, map()} | {error, binary()}.
join(Uid, Did, <<"group">> = Kind, Gid) ->
    case group_member_ds:is_member(Gid, Uid) of
        true ->
            {ok, build_grant(Uid, Did, room_name(Kind, Gid), interactive_perms())};
        false ->
            {error, <<"不是群成员，无权加入群通话"/utf8>>}
    end;
join(Uid, Did, <<"c2c">> = Kind, PeerUid) ->
    case friend_ds:is_friend(Uid, PeerUid) of
        true ->
            {ok, build_grant(Uid, Did, room_name(Kind, {Uid, PeerUid}), interactive_perms())};
        false ->
            {error, <<"不是好友关系，无法发起通话"/utf8>>}
    end;
join(_Uid, _Did, _Kind, _TargetId) ->
    {error, <<"不支持的房间类型"/utf8>>}.

%% @doc 房间命名：c2c 用双方 uid 排序保证两端得到同一房间名
%% Room naming: c2c sorts both uids so both sides derive the same room
-spec room_name(binary(), integer() | {integer(), integer()}) -> binary().
room_name(<<"group">>, Gid) ->
    <<"rtc_group_", (integer_to_binary(Gid))/binary>>;
room_name(<<"c2c">>, {Uid1, Uid2}) ->
    [Lo, Hi] = lists:sort([Uid1, Uid2]),
    <<"rtc_c2c_", (integer_to_binary(Lo))/binary, "_", (integer_to_binary(Hi))/binary>>.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 互动通话权限：可发布音视频与数据（群通话/1:1 双方均需互发流）
%% 安全：权限显式参数化，未来若复用到"频道直播/大会观众"等只订阅场景，
%%       须显式传只订阅权限，避免继承此处的可发布默认值。
-spec interactive_perms() -> map().
interactive_perms() ->
    #{can_publish => true, can_subscribe => true, can_publish_data => true}.

%% @doc 组装返回体：签发 LiveKit JWT（HS256，claims 结构见 LiveKit 认证文档）
-spec build_grant(integer(), binary(), binary(), map()) -> map().
build_grant(Uid, Did, RoomName, Perms) ->
    #{ws_url := WsUrl, api_key := ApiKey, api_secret := ApiSecret} =
        config_ds:env(livekit, #{}),
    Now = erlang:system_time(second),
    %% identity 带设备后缀，避免同账号多设备入同房被互踢
    Identity = <<(integer_to_binary(Uid))/binary, "_", Did/binary>>,
    Claims = #{
        iss => ApiKey,
        sub => Identity,
        nbf => Now - 10,
        exp => Now + ?TOKEN_TTL_SECONDS,
        video => #{
            room => RoomName,
            roomJoin => true,
            canPublish => maps:get(can_publish, Perms, false),
            canSubscribe => maps:get(can_subscribe, Perms, true),
            canPublishData => maps:get(can_publish_data, Perms, false)
        }
    },
    Token = jwerl:sign(Claims, hs256, ApiSecret),
    #{
        <<"ws_url">> => WsUrl,
        <<"token">> => Token,
        <<"room_name">> => RoomName
    }.
