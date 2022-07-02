-module(friend_repo).

-include_lib("imboy/include/common.hrl").

-export([confirm_friend/6]).
-export([is_friend/2]).
-export([find_by_uid/2]).
-export([move_to_category/3]).

-spec confirm_friend(
    IsFriend::boolean(),
    FromID::integer(),
    ToID::integer(),
    Remark::binary(),
    Setting::binary(),
    NowTs::integer()
) -> ok.
confirm_friend(true, _, _, _, _, _) ->
    ok;
confirm_friend(false, FromID, ToID, Remark, Setting, NowTs) ->
    From = integer_to_binary(FromID),
    To = integer_to_binary(ToID),
    Table = <<"`user_friend`">>,
    Column = <<"(`from_user_id`,`to_user_id`,`status`,
        `category_id`,`remark`,`updated_at`,`created_at`,
        `setting`)">>,
    CreatedAt = integer_to_binary(NowTs),

    SettingBin = jsone:encode(filter_friend_setting(Setting),
        [native_utf8]),
    Value1 = <<"(", From/binary, ", ", To/binary, ",1, 0, '",
        Remark/binary,"', 0, ", CreatedAt/binary,", '",
        SettingBin/binary, "')">>,
    mysql_pool:replace_into(Table, Column, Value1),
    ok.

is_friend(FromID, ToID) ->
    Where = <<"WHERE `from_user_id` = ? AND `to_user_id` = ?
        AND `status` = 1">>,
    Sql = <<"SELECT count(*) as count FROM `user_friend` ",
            Where/binary>>,
    mysql_pool:query(Sql, [FromID, ToID]).


find_by_uid(UID, Column) ->
    find_by_uid(UID, Column, 10000).


find_by_uid(UID, Column, Limit) ->
    Where = <<"WHERE `from_user_id` = ? AND `status` = 1 LIMIT ?">>,
    Sql = <<"SELECT ", Column/binary, " FROM `user_friend` ",
            Where/binary>>,
    mysql_pool:query(Sql, [UID, Limit]).


move_to_category(FromUID, ToUID, CategoryID) ->
    Sql = <<"UPDATE `user_friend` SET `category_id` = ?
        WHERE `status` = 1 AND `from_user_id` = ? AND `to_user_id` = ?">>,
    % ?LOG([Sql, CategoryID, FromUID, ToUID]),
    mysql_pool:query(Sql, [CategoryID, FromUID, ToUID]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% 对好友的一些权限控制配置
filter_friend_setting(Setting) ->
    [
        % 好友关系发起人 1 是 0 否
        {<<"isfrom">>, proplists:get_value(<<"isfrom">>, Setting, 0)},
        {<<"source">>, proplists:get_value(<<"source">>, Setting, "")},
        % 客户端约定
        % role 可能的值 all justchat
        {<<"role">>, proplists:get_value(<<"role">>, Setting, "all")},
        %  不让他（她）看
        {<<"donotlethimlook">>, proplists:get_value(
            <<"donotlethimlook">>, Setting, false)},
        % 不看他（她）
        {<<"donotlookhim">>, proplists:get_value(
            <<"donotlookhim">>, Setting, false)}
    ].
