-module(friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export([is_friend/2]).
-export([is_friend/3]).
-export([page_by_uid/1, page_by_uid/3]).
-export([page_by_cid/4]).
-export([change_remark/3]).
-export([set_category_id/3]).

-include_lib("imboy/include/log.hrl").


%% ToUid 是 FromUid 的好友？

% friend_ds:is_friend(1,2)
-spec is_friend(integer(), integer()) -> boolean().
is_friend(FromUid, ToUid) ->
    {Res, _} = friend_ds:is_friend(FromUid, ToUid, <<"remark">>),
    Res.

% friend_ds:is_friend(1,2, <<"remark">>).
-spec is_friend(integer(), integer(), binary()) -> boolean().
is_friend(FromUid, ToUid, Field) ->
    Key = {is_friend, FromUid, ToUid},
    Fun = fun() ->
        case friend_repo:friend_field(FromUid, ToUid, Field) of
            {ok, _ColumnLi, [[Val]]} ->
                {true, Val};
            _ ->
                {false, <<"">>}
        end
    end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).

-spec page_by_uid(integer()) -> list().
page_by_uid(Uid) ->
    page_by_uid(Uid, 1000, 0).

-spec page_by_uid(integer(), integer(), integer()) -> list().
page_by_uid(Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = ? AND f.from_user_id = ? LIMIT ? OFFSET ?">>,
    WhereArgs = [1, Uid, Limit, Offset],
    page(Where, WhereArgs).

-spec page_by_cid(integer(), integer(), integer(), integer()) -> list().
page_by_cid(Cid, Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = ? AND f.from_user_id = ? AND f.category_id = ? LIMIT ? OFFSET ?">>,
    WhereArgs = [1, Uid, Cid, Limit, Offset],
    page(Where, WhereArgs).

-spec page(binary(), list()) -> list().
page(Where, WhereArgs) ->
    C1 = <<"u.`id`, u.`account`, u.`nickname`, u.`avatar`, u.`sign`, u.`gender`, u.`region`,">>,
    C_IsFrom = <<"JSON_UNQUOTE(json_extract(f.setting, '$.is_from')) AS is_from,">>,
    C_Source = <<"JSON_UNQUOTE(json_extract(f.setting, '$.source')) AS source,">>,
    C_IsFriend = <<" case when d.denied_user_id is null then 1 else 0 end as is_friend,">>,
    C2 = <<C_IsFrom/binary, C_Source/binary, C_IsFriend/binary, "f.`remark`, f.`category_id`">>,

    Join1 = <<"left join user_denylist as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join user as u on u.id = f.to_user_id ">>,

    Sql = <<"SELECT ", C1/binary, C2/binary, " FROM `user_friend` as f ",
        Join1/binary,
        Join2/binary,
        Where/binary>>,
    % ?LOG([Sql, WhereArgs]),
    case mysql_pool:query(Sql, WhereArgs) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            Friends = [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList, Row) ||
                Row <- Rows],
                [user_logic:online_state(User) || User <- Friends];
        _ ->
            []
    end.

change_remark(FromUid, ToUid, Remark) ->
    Sql = <<"UPDATE `user_friend` SET `remark` = ?, `updated_at` = ?
        WHERE `status` = 1 AND `from_user_id` = ? AND `to_user_id` = ?">>,
    mysql_pool:query(Sql,
                     [Remark, imboy_dt:millisecond(), FromUid, ToUid]).


set_category_id(Uid, CategoryId, NewCid) ->
    Sql = <<"UPDATE `user_friend` SET `category_id` = ?, `updated_at` = ?
        WHERE `status` = 1 AND `from_user_id` = ? AND `category_id` = ?">>,
    mysql_pool:query(Sql,
                     [NewCid, imboy_dt:millisecond(), Uid, CategoryId]).
