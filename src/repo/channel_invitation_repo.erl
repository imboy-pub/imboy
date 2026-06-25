-module(channel_invitation_repo).
%%%===================================================================
%%% @doc 频道邀请 Repo 层
%%%
%%% 管理私有频道邀请的数据库操作
%%% @end
%%%===================================================================

-include("log.hrl").

%%===================================================================
%%% API Functions
%%===================================================================

-export([tablename/0]).
-export([create/1]).
-export([find_by_id/1]).
-export([find_by_channel_and_invitee/2]).
-export([find_pending_by_channel_and_invitee/2]).
-export([accept/2]).
-export([reject/2]).
-export([list_pending_by_invitee/1]).
-export([list_by_inviter/2]).
-export([is_invited/2]).
-export([generate_invitation_code/0]).

%%===================================================================
%%% Constants
%%===================================================================

-define(STATUS_PENDING, 0).
-define(STATUS_ACCEPTED, 1).
-define(STATUS_REJECTED, 2).
-define(STATUS_EXPIRED, 3).
-define(STATUS_CANCELLED, 4).

-define(INVITATION_EXPIRE_DAYS, 7).

%%===================================================================
%%% API Implementation
%%===================================================================

%% @doc 返回表名
-spec tablename() -> binary().
tablename() ->
    <<"channel_invitation">>.

%% @doc 创建邀请
%% @param Data 邀请数据 map
%% @returns {ok, Id} | {error, Reason}
-spec create(map()) -> {ok, integer()} | {error, term()}.
create(Data) ->
    ChannelId = maps:get(channel_id, Data),
    InviterUid = maps:get(inviter_uid, Data),
    InviteeUid = maps:get(invitee_uid, Data),
    InvitationCode = maps:get(invitation_code, Data, generate_invitation_code()),
    Message = maps:get(message, Data, <<>>),
    ExpiresAt = maps:get(expires_at, Data, default_expire_time()),
    CreatedAt = maps:get(created_at, Data, elib_dt:now()),

    Id = elib_tsid:generate(channel_invitation),
    Sql =
        <<"INSERT INTO channel_invitation ",
            "(id, channel_id, inviter_uid, invitee_uid, invitation_code, message, status, expires_at, created_at) ",
            "VALUES ($1, $2, $3, $4, $5, $6, $7, to_timestamp($8/1000), to_timestamp($9/1000))">>,
    case
        elib_pg:execute(Sql, [
            Id,
            ChannelId,
            InviterUid,
            InviteeUid,
            InvitationCode,
            Message,
            ?STATUS_PENDING,
            ExpiresAt,
            CreatedAt
        ])
    of
        {ok, _Count} ->
            {ok, Id};
        {error, {pgsql_error, #{code := <<"23505">>}}} ->
            % 唯一约束违规 - 已存在待处理邀请
            {error, already_invited};
        {error, Reason} ->
            ?ERROR_LOG([channel_invitation_create, failed, Reason]),
            {error, Reason}
    end.

%% @doc 根据ID查找邀请
-spec find_by_id(integer()) -> {ok, map()} | {error, not_found}.
find_by_id(Id) ->
    Sql =
        <<"SELECT id, channel_id, inviter_uid, invitee_uid, invitation_code, ",
            "status, message, expires_at, accepted_at, created_at, updated_at ",
            "FROM channel_invitation WHERE id = $1">>,
    case elib_pg:query(Sql, [Id]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据频道和被邀请人查找邀请
-spec find_by_channel_and_invitee(integer(), integer()) -> {ok, map()} | {error, not_found}.
find_by_channel_and_invitee(ChannelId, InviteeUid) ->
    Sql =
        <<"SELECT id, channel_id, inviter_uid, invitee_uid, invitation_code, ",
            "status, message, expires_at, accepted_at, created_at, updated_at ",
            "FROM channel_invitation ", "WHERE channel_id = $1 AND invitee_uid = $2 ",
            "ORDER BY created_at DESC LIMIT 1">>,
    case elib_pg:query(Sql, [ChannelId, InviteeUid]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 根据频道和被邀请人查找待处理邀请（仅 pending 且未过期）
-spec find_pending_by_channel_and_invitee(integer(), integer()) -> {ok, map()} | {error, not_found}.
find_pending_by_channel_and_invitee(ChannelId, InviteeUid) ->
    Sql =
        <<"SELECT id, channel_id, inviter_uid, invitee_uid, invitation_code, ",
            "status, message, expires_at, accepted_at, created_at, updated_at ",
            "FROM channel_invitation ", "WHERE channel_id = $1 AND invitee_uid = $2 ",
            "AND status = 0 AND expires_at > NOW() ", "ORDER BY created_at DESC LIMIT 1">>,
    case elib_pg:query(Sql, [ChannelId, InviteeUid]) of
        {ok, []} -> {error, not_found};
        {ok, [Row | _]} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 接受邀请
-spec accept(integer(), integer()) -> ok | {error, term()}.
accept(Id, InviteeUid) ->
    Sql =
        <<"UPDATE channel_invitation ", "SET status = $1, accepted_at = NOW(), updated_at = NOW() ",
            "WHERE id = $2 AND invitee_uid = $3 AND status = 0 AND expires_at > NOW()">>,
    case elib_pg:execute(Sql, [?STATUS_ACCEPTED, Id, InviteeUid]) of
        {ok, 0} -> {error, not_found_or_expired};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 拒绝邀请
-spec reject(integer(), integer()) -> ok | {error, term()}.
reject(Id, InviteeUid) ->
    Sql =
        <<"UPDATE channel_invitation ", "SET status = $1, updated_at = NOW() ",
            "WHERE id = $2 AND invitee_uid = $3 AND status = 0">>,
    case elib_pg:execute(Sql, [?STATUS_REJECTED, Id, InviteeUid]) of
        {ok, 0} -> {error, not_found};
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取用户的待处理邀请列表
-spec list_pending_by_invitee(integer()) -> {ok, [map()]} | {error, term()}.
list_pending_by_invitee(InviteeUid) ->
    Sql =
        <<"SELECT i.id, i.channel_id, i.inviter_uid, i.invitee_uid, i.invitation_code, ",
            "i.status, i.message, i.expires_at, i.created_at, ",
            "c.name as channel_name, c.avatar as channel_avatar ", "FROM channel_invitation i ",
            "JOIN channel c ON i.channel_id = c.id ",
            "WHERE i.invitee_uid = $1 AND i.status = 0 AND i.expires_at > NOW() ",
            "ORDER BY i.created_at DESC">>,
    case elib_pg:query(Sql, [InviteeUid]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 获取邀请人发出的邀请列表
-spec list_by_inviter(integer(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_inviter(InviterUid, Limit) ->
    Sql =
        <<"SELECT i.id, i.channel_id, i.inviter_uid, i.invitee_uid, i.invitation_code, ",
            "i.status, i.message, i.expires_at, i.created_at, ", "c.name as channel_name ",
            "FROM channel_invitation i ", "JOIN channel c ON i.channel_id = c.id ",
            "WHERE i.inviter_uid = $1 ", "ORDER BY i.created_at DESC LIMIT $2">>,
    case elib_pg:query(Sql, [InviterUid, Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 检查用户是否被邀请（有有效的待处理邀请）
-spec is_invited(integer(), integer()) -> boolean().
is_invited(ChannelId, Uid) ->
    Sql =
        <<"SELECT COUNT(*) as cnt FROM channel_invitation ",
            "WHERE channel_id = $1 AND invitee_uid = $2 ",
            "AND status = 0 AND expires_at > NOW()">>,
    case elib_pg:query(Sql, [ChannelId, Uid]) of
        {ok, [#{<<"cnt">> := Count}]} when Count > 0 -> true;
        _ -> false
    end.

%%===================================================================
%%% Internal Functions
%%===================================================================

%% @doc 生成邀请码（8位大写字母+数字）
-spec generate_invitation_code() -> binary().
generate_invitation_code() ->
    Chars = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>,
    generate_code_chars(8, Chars, <<>>).

generate_code_chars(0, _Chars, Acc) ->
    Acc;
generate_code_chars(N, Chars, Acc) ->
    % rand:uniform/1 returns 1..Len; convert to zero-based binary offset
    Pos = rand:uniform(byte_size(Chars)) - 1,
    <<_:Pos/binary, Char:1/binary, _/binary>> = Chars,
    generate_code_chars(N - 1, Chars, <<Acc/binary, Char/binary>>).

%% @doc 默认过期时间（7天后）
-spec default_expire_time() -> integer().
default_expire_time() ->
    elib_dt:now() + (?INVITATION_EXPIRE_DAYS * 24 * 60 * 60 * 1000).
