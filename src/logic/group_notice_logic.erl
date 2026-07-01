-module(group_notice_logic).
%%%
% group_notice 业务逻辑模块
% group_notice business logic module
%%%

%% API
-export([list/4]).
-export([detail/2]).
-export([pin/2]).
-export([unpin/2]).
-export([delete/2]).
-export([mark_as_read/2]).
-export([publish_notice/3]).
-export([insert/2]).
-export([update/3]).
-export([page/4]).
-export([latest_published/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").
-include("imboy_const.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 查询群公告列表（分页）
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, {Total, List}} | {error, Reason}
-spec list(integer(), integer(), integer(), integer()) ->
    {ok, {integer(), [map()]}} | {error, term()}.
list(CurrentUid, Gid, Page, Size) ->
    % 验证群成员身份
    case group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"id">>) of
        GM when map_size(GM) > 0 ->
            % 查询公告列表
            case group_notice_ds:list_by_group_id(Gid, Page, Size) of
                {ok, {Total, List}} ->
                    % 处理用户ID编码
                    List2 = [encode_notice_ids(Notice) || Notice <- List],
                    {ok, {Total, List2}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, ?ERR_NOT_GROUP_MEMBER}
    end.

%% @doc 查询公告详情
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, Reason}
-spec detail(integer(), integer()) -> {ok, map()} | {error, term()}.
detail(CurrentUid, NoticeId) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            % 验证群成员身份
            case group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"id">>) of
                GM when map_size(GM) > 0 ->
                    % 处理用户ID编码
                    Notice2 = encode_notice_ids(Notice),
                    {ok, Notice2};
                _ ->
                    {error, ?ERR_NOT_GROUP_MEMBER}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 置顶公告（仅群主和管理员）
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec pin(integer(), integer()) -> ok | {error, term()}.
pin(CurrentUid, NoticeId) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            % 验证权限（仅群主和管理员）
            case check_admin_permission(CurrentUid, Gid) of
                ok ->
                    group_notice_ds:pin(NoticeId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _Reason} ->
            {error, ?ERR_NOT_FOUND}
    end.

%% @doc 取消置顶公告（仅群主和管理员）
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec unpin(integer(), integer()) -> ok | {error, term()}.
unpin(CurrentUid, NoticeId) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            % 验证权限（仅群主和管理员）
            case check_admin_permission(CurrentUid, Gid) of
                ok ->
                    group_notice_ds:unpin(NoticeId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _Reason} ->
            {error, ?ERR_NOT_FOUND}
    end.

%% @doc 删除公告（仅群主和管理员）
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @return ok | {error, Reason}
-spec delete(integer(), integer()) -> ok | {error, term()}.
delete(CurrentUid, NoticeId) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            % 验证权限（仅群主和管理员）
            case check_admin_permission(CurrentUid, Gid) of
                ok ->
                    group_notice_ds:soft_delete(NoticeId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, _Reason} ->
            {error, ?ERR_NOT_FOUND}
    end.

%% @doc 标记公告为已读
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @return {ok, Notice} | {error, Reason}
-spec mark_as_read(integer(), integer()) -> {ok, map()} | {error, term()}.
mark_as_read(CurrentUid, NoticeId) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            % 验证群成员身份
            case group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"id">>) of
                GM when map_size(GM) > 0 ->
                    group_notice_ds:mark_as_read(NoticeId);
                _ ->
                    {error, ?ERR_NOT_GROUP_MEMBER}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发布公告后向群内所有成员广播 S2C `group_notice_published`
%%
%% W1.1：对齐客户端 `lib/service/group_notice_s2c.dart` 的解析契约：
%%   Payload = #{
%%     <<"gid">>                => Gid,           % int
%%     <<"notice_id">>          => NoticeId,      % int
%%     <<"publisher_id">>       => Uid,           % int
%%     <<"publisher_nickname">> => <<"Alice">>,   % binary
%%     <<"title">>              => <<"...">>,      % binary (or <<>>)
%%     <<"body">>               => <<"...">>,      % binary (or <<>>)
%%     <<"expired_at">>         => ExpiredAtMs,   % int ms or 0
%%     <<"published_at">>       => Now            % int ms
%%   }
%%
%% 客户端侧：
%%   - expired_at = 0 / 缺失 → null（永不过期语义）
%%   - published_at 缺失 → 0（"未知"标记）
%%
%% 本函数**仅做广播**，不负责持久化（持久化由 `group_notice_handler:publish/3`
%% 调用 `group_notice_ds:update/2` 完成）。
-spec publish_notice(integer(), integer(), integer()) -> ok.
publish_notice(Uid, Gid, NoticeId) ->
    ToUidLi = group_ds:member_uids(Gid),
    Publisher = user_ds:find_by_id(Uid, <<"nickname">>),
    Now = elib_dt:millisecond(),
    {Title, Body, ExpiredAt} =
        case group_notice_ds:find_by_id(NoticeId) of
            {ok, Notice} ->
                T = maps:get(<<"title">>, Notice, <<>>),
                B = maps:get(<<"body">>, Notice, <<>>),
                Exp = maps:get(<<"expired_at">>, Notice, 0),
                {T, B, Exp};
            _ ->
                {<<>>, <<>>, 0}
        end,
    Payload = #{
        <<"gid">> => Gid,
        <<"notice_id">> => NoticeId,
        <<"publisher_id">> => Uid,
        <<"publisher_nickname">> => maps:get(<<"nickname">>, Publisher, <<>>),
        <<"title">> => Title,
        <<"body">> => Body,
        <<"expired_at">> => ExpiredAt,
        <<"published_at">> => Now
    },
    Action = <<"group_notice_published">>,
    _ = msg_s2c_ds:send(Uid, ToUidLi, Action, <<>>, null, Payload, save),
    ok.

%% @doc 插入新群公告（仅群主和管理员），委托 DS 层持久化
%% @param CurrentUid 当前用户ID
%% @param Data 公告数据 Map（须含 group_id）
%% @return {ok, NoticeId} | {error, Reason}
-spec insert(integer(), map()) -> {ok, integer()} | {error, term()}.
insert(CurrentUid, Data) ->
    Gid = maps:get(group_id, Data, 0),
    case check_admin_permission(CurrentUid, Gid) of
        ok -> group_notice_ds:insert(Data);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 更新群公告字段（仅群主和管理员），委托 DS 层持久化
%% @param CurrentUid 当前用户ID
%% @param NoticeId 公告ID
%% @param Data 待更新字段 Map
%% @return {ok, integer()} | {error, term()}
-spec update(integer(), integer(), map()) -> {ok, integer()} | {error, term()}.
update(CurrentUid, NoticeId, Data) ->
    case group_notice_ds:find_by_id(NoticeId) of
        {ok, Notice} ->
            Gid = maps:get(<<"group_id">>, Notice),
            case check_admin_permission(CurrentUid, Gid) of
                ok -> group_notice_ds:update(NoticeId, Data);
                {error, Reason} -> {error, Reason}
            end;
        {error, _Reason} ->
            {error, not_found}
    end.

%% @doc 分页查询群公告列表，同时校验当前用户是否为群成员
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @param Column 查询字段
%% @param Order 排序表达式
%% @param Page 页码
%% @param Size 每页数量
%% @return {ok, Payload} | {error, Reason}
-spec page(integer(), integer(), pos_integer(), pos_integer()) ->
    {ok, map()} | {error, term()}.
page(CurrentUid, Gid, Page, Size) ->
    GM = group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"id">>),
    case maps:size(GM) of
        0 ->
            {error, ?ERR_NOT_GROUP_MEMBER};
        _ ->
            Column =
                <<
                    "id as notice_id, user_id, edit_user_id, body, status, expired_at, "
                    "updated_at, created_at"
                >>,
            group_notice_ds:page(Gid, Column, <<"expired_at desc">>, Page, Size)
    end.

%% @doc 查询群最新已发布公告，同时校验当前用户是否为群成员
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @return {ok, [map()]} | {error, Reason}
-spec latest_published(integer(), integer()) -> {ok, [map()]} | {error, term()}.
latest_published(CurrentUid, Gid) ->
    GM = group_member_ds:find_by_gid_and_uid(Gid, CurrentUid, <<"id">>),
    case maps:size(GM) of
        0 ->
            {error, ?ERR_NOT_GROUP_MEMBER};
        _ ->
            Column =
                <<
                    "id as notice_id, user_id, edit_user_id, body, status, expired_at, "
                    "updated_at, created_at"
                >>,
            group_notice_ds:latest_published(Gid, Column)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 检查管理员权限（群主或管理员）
%% @param CurrentUid 当前用户ID
%% @param Gid 群组ID
%% @return ok | {error, Reason}
-spec check_admin_permission(integer(), integer()) -> ok | {error, term()}.
check_admin_permission(CurrentUid, Gid) ->
    case group_member_ds:get_member_info(Gid, CurrentUid, <<"role">>) of
        {ok, MemberInfo} ->
            Role = maps:get(<<"role">>, MemberInfo, null),
            case Role of
                % 群主
                ?GROUP_ROLE_OWNER_INT -> ok;
                % 管理员
                ?GROUP_ROLE_ADMIN_INT -> ok;
                _ -> {error, ?ERR_GROUP_PERMISSION_DENIED}
            end;
        {error, _} ->
            {error, ?ERR_NOT_GROUP_MEMBER}
    end.

%% @doc 编码公告中的用户ID
%% @param Notice 公告数据
%% @return 编码后的公告数据
-spec encode_notice_ids(map()) -> map().
encode_notice_ids(Notice) ->
    Notice.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
