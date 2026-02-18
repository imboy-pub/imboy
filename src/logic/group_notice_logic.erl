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
    case group_member_repo:find(Gid, CurrentUid, <<"id">>) of
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
            case group_member_repo:find(Gid, CurrentUid, <<"id">>) of
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
            case group_member_repo:find(Gid, CurrentUid, <<"id">>) of
                GM when map_size(GM) > 0 ->
                    group_notice_ds:mark_as_read(NoticeId);
                _ ->
                    {error, ?ERR_NOT_GROUP_MEMBER}
            end;
        {error, Reason} ->
            {error, Reason}
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
                ?GROUP_ROLE_OWNER_INT -> ok;      % 群主
                ?GROUP_ROLE_ADMIN_INT -> ok;       % 管理员
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
    Notice1 = elib_hashids:replace_id(Notice, <<"user_id">>),
    Notice2 = elib_hashids:replace_id(Notice1, <<"edit_user_id">>),
    Notice3 = elib_hashids:replace_id(Notice2, <<"group_id">>),
    elib_hashids:replace_id(Notice3, <<"id">>).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
