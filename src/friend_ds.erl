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
-include_lib("imboy/include/def_column.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% ToUid 是 FromUid 的好友？

% friend_ds:is_friend(1, 3)
-spec is_friend(integer(), integer()) -> boolean().
is_friend(FromUid, ToUid) ->
    {Res, _} = friend_ds:is_friend(FromUid, ToUid, <<"remark">>),
    Res.

% friend_ds:is_friend(1, 3, <<"remark">>).
-spec is_friend(integer(), integer(), binary()) -> boolean().
is_friend(FromUid, ToUid, Field) ->
    Key = {is_friend, FromUid, ToUid},
    Fun = fun() ->
        case friend_repo:friend_field(FromUid, ToUid, Field) of
            {ok, _ColumnLi, [{Val}]} ->
                {true, Val};
            _ ->
                {false, <<"">>}
        end
    end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 864000).

% friend_ds:page_by_uid(1).
-spec page_by_uid(integer()) -> list().
page_by_uid(Uid) ->
    page_by_uid(Uid, 1000, 0).

-spec page_by_uid(integer(), integer(), integer()) -> list().
page_by_uid(Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 LIMIT $2 OFFSET $3">>,
    WhereArgs = [Uid, Limit, Offset],
    page(Where, WhereArgs, fields(Uid)).

-spec page_by_cid(integer(), integer(), integer(), integer()) -> list().
page_by_cid(Cid, Uid, Limit, Offset) ->
    Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 AND f.category_id = $2 LIMIT $3 OFFSET $4">>,
    WhereArgs = [Uid, Cid, Limit, Offset],
    page(Where, WhereArgs, fields(Uid)).

-spec page(binary(), list(), binary()) -> list().
page(Where, WhereArgs, Fields) ->
    UserTable = imboy_db:public_tablename(<<"user">>),
    UserDTable = imboy_db:public_tablename(<<"user_denylist">>),
    Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,

    Tb = friend_repo:tablename(),
    Sql = <<"SELECT ", Fields/binary, " FROM ", Tb/binary, " as f ",
        Join1/binary,
        Join2/binary,
        Where/binary>>,
    % Res = imboy_db:query(Sql, WhereArgs),
    % ?LOG([Res]),
    % ok.
    % ?LOG([Sql, WhereArgs]),
    case imboy_db:query(Sql, WhereArgs) of
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            Friends = [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList, tuple_to_list(Row)) || Row <- Rows],
            [user_logic:online_state(User) || User <- Friends];
        _ ->
            []
    end.

% friend_ds:change_remark(1, 2, <<" 1 to 2 f">>).
-spec change_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
change_remark(FromUid, ToUid, Remark) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET remark = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND to_user_id = $5">>,
    imboy_db:execute(Sql, [Remark, imboy_dt:millisecond(), 1, FromUid, ToUid]).

% friend_ds:set_category_id(1, 1, 0).
-spec set_category_id(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
set_category_id(Uid, CategoryId, NewCid) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND category_id = $5">>,
    imboy_db:execute(Sql, [NewCid, imboy_dt:millisecond(), 1, Uid, CategoryId]).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


fields(Uid) when is_integer(Uid) ->
    fields(integer_to_binary(Uid));
fields(Uid) ->
    C_IsFriend = <<" case when d.user_id = ", Uid/binary, " and d.denied_user_id = u.id then 0 else 1 end as is_friend,">>,
    C_IsFrom = <<"f.setting::jsonb->>'is_from' AS is_from,">>,
    C_Source = <<"f.setting::jsonb->>'source' AS source,">>,
    C2 = <<C_IsFrom/binary, C_Source/binary, C_IsFriend/binary, "f.remark, f.category_id">>,
    <<"id,", F2/binary>> = ?DEF_USER_COLUMN,
    <<"u.id,", F2/binary, ",", C2/binary>>.

