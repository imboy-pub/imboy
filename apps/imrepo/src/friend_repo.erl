-module(friend_repo).

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([list_by_uid/2]).
-export([friend_field/3]).
-export([confirm_friend/7]).
-export([delete/2]).
-export([move_to_category/3]).

%% ===================================================================
%% API
%% ===================================================================


% friend_repo:tablename(),
tablename() ->
    imboy_db:public_tablename(<<"user_friend">>).


-spec confirm_friend(boolean(), integer(), integer(), binary(), binary(), binary(), integer()) -> ok.
confirm_friend(true, _, _, _, _, _, _) ->
    ok;
confirm_friend(false, FromID, ToID, Remark, Setting, Tag, NowTs) ->
    From = integer_to_binary(FromID),
    To = integer_to_binary(ToID),
    Tb = tablename(),
    Column = <<"(from_user_id,to_user_id,status,
        category_id,remark,updated_at,created_at,
        setting,tag)">>,

    SettingBin = jsone:encode(filter_friend_setting(Setting), [native_utf8]),
    Value1 = <<"(", From/binary, ", ", To/binary, ",1, 0, '", Remark/binary, "', 0, ", NowTs/binary, ", '",
               SettingBin/binary, "', '", Tag/binary, "')">>,
    imboy_db:insert_into(Tb, Column, Value1),
    ok.


friend_field(FromID, ToID, Field) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND to_user_id = $2 AND status = 1">>,
    Sql = <<"SELECT ", Field/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [FromID, ToID]).


list_by_uid(UID, Column) ->
    list_by_uid(UID, Column, 10000).


list_by_uid(UID, Column, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND status = 1 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [UID, Limit]).


-spec delete(integer(), integer()) -> ok.
delete(FromID, ToID) ->
    Tb = tablename(),
    Where = <<" WHERE from_user_id = $1 AND to_user_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    % ?LOG(io:format("~s  ~p ~p\n", [Sql, FromID, ToID])),
    imboy_db:execute(Sql, [FromID, ToID]),
    ok.


move_to_category(FromUID, ToUID, CategoryID) ->
    Tb = tablename(),
    Where = <<" WHERE status = 1 AND from_user_id = $2 AND to_user_id = $3">>,
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1", Where/binary>>,
    % ?LOG([Sql, CategoryID, FromUID, ToUID]),
    imboy_db:execute(Sql, [CategoryID, FromUID, ToUID]),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% 对好友的一些权限控制配置
filter_friend_setting(Setting) ->
    [
     % 好友关系发起人 1 是 0 否
     {<<"isfrom">>, proplists:get_value(<<"isfrom">>, Setting, 0)},
     {<<"source">>, proplists:get_value(<<"source">>, Setting, "")},
     % 客户端约定
     % role 可能的值 all just_chat
     {<<"role">>, proplists:get_value(<<"role">>, Setting, "all")},
     %  不让他（她）看
     {<<"donotlethimlook">>, proplists:get_value(<<"donotlethimlook">>, Setting, false)},
     % 不看他（她）
     {<<"donotlookhim">>, proplists:get_value(<<"donotlookhim">>, Setting, false)}].
