-module(friend_ds).
%%%
% friend_ds 是 friend domain service 缩写
%%%
-export([is_friend/2]).
-export([is_friend/3]).
-export([list_by_uid/1]).
-export([page_by_uid/1, page_by_uid/3]).
-export([page_by_cid/4]).
-export([page_by_tag/5]).
-export([change_remark/3]).
-export([set_category_id/3]).

-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/def_column.hrl").

%% ===================================================================
%% API
%% ===================================================================

list_by_uid(Uid) ->
    Column = <<"to_user_id">>,
    case friend_repo:list_by_uid(Uid, Column) of
        {ok, _, []} ->
            [];
        {ok, _ColumnList, Rows} ->
            [ToUid || {ToUid} <- Rows]
    end.

%% ToUid 是 FromUid 的好友？


% friend_ds:is_friend(1, 3)
-spec is_friend(integer(), integer()) -> {boolean(), any()}.
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
                          {false, <<>>}
                  end
          end,
    %  缓存key挺多，是针对用户ID的，缓存时间不宜过长
    % 缓存1天，
    imboy_cache:memo(Fun, Key, 86400).


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


-spec page_by_tag(integer(), integer(), integer(), integer(), binary()) -> list().
page_by_tag(Uid, Page, Size, TagId, Kwd) when Page > 0 ->
    Offset = (Page - 1) * Size,
    TagId2 = integer_to_binary(TagId),
    % TagName = <<"aaa">>,
    TagName = imboy_db:pluck(<<"user_tag">>, <<"id = ", TagId2/binary>>, <<"name">>, <<>>),
    {Total0, Items0} =
        if
            TagName == <<>> ->
                {0, []};
            bit_size(Kwd) > 0 ->
                Where0 = imboy_cnv:implode(" AND ",
                                            ["f.status = 1",
                                             "f.from_user_id = $1",
                                             <<"f.tag like '%", TagName/binary, ",%'">>,
                                             <<"(f.tag like '%", Kwd/binary, ",%' OR f.remark like '%", Kwd/binary,
                                               "%' OR u.nickname like '%", Kwd/binary, "%' OR u.sign like '%",
                                               Kwd/binary, "%')">>]),
                Where = <<"WHERE ", Where0/binary, " LIMIT $2 OFFSET $3">>,

                WhereArgs = [Uid, Size, Offset],

                Items = page(Where, WhereArgs, fields(Uid)),
                Total = count(Where, WhereArgs),
                {Total, Items};
            true ->
                Where = <<"WHERE f.status = 1 AND f.from_user_id = $1 AND f.tag like '%", TagName/binary,
                          ",%' LIMIT $2 OFFSET $3">>,

                WhereArgs = [Uid, Size, Offset],

                Items = page(Where, WhereArgs, fields(Uid)),
                Total = count(Where, WhereArgs),
                {Total, Items}
        end,
    imboy_response:page_payload(Total0, Page, Size, Items0).


-spec count(binary(), list()) -> list().
count(Where, WhereArgs) ->
    case page(Where, WhereArgs, <<"count(*) count">>) of
        [{Count}] ->
            Count;
        _ ->
            0
    end.


-spec page(binary(), list(), binary()) -> list().
page(Where, WhereArgs, Fields) ->
    UserTable = imboy_db:public_tablename(<<"user">>),
    UserDTable = imboy_db:public_tablename(<<"user_denylist">>),
    Join1 = <<"left join ", UserDTable/binary, " as d on d.denied_user_id = f.to_user_id ">>,
    Join2 = <<"inner join ", UserTable/binary, " as u on u.id = f.to_user_id ">>,

    Tb = friend_repo:tablename(),
    Sql = <<"SELECT ", Fields/binary, " FROM ", Tb/binary, " as f ", Join1/binary, Join2/binary, Where/binary>>,
    % Res = imboy_db:query(Sql, WhereArgs),
    % ?DEBUG_LOG([Res]),
    % ok.
    % ?DEBUG_LOG([Sql, WhereArgs]),
    case imboy_db:query(Sql, WhereArgs) of
        {ok, _ColumnList, Rows} when Fields == <<"count(*) count">> ->
            Rows;
        {ok, _, []} ->
            [];
        {ok, ColumnList, Rows} ->
            Friends = [ lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnList, tuple_to_list(Row)) || Row <- Rows ],
            [ user_logic:online_state(imboy_hashids:replace_id(User)) || User <- Friends ];
        _ ->
            []
    end.


% friend_ds:change_remark(1, 2, <<" 1 to 2 f">>).
-spec change_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
change_remark(FromUid, ToUid, Remark) ->
    Tb = friend_repo:tablename(),
    Dt = imboy_dt:now(),
    ?DEBUG_LOG([Dt]),
    Sql = <<"UPDATE ", Tb/binary, " SET remark = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND to_user_id = $5">>,
    imboy_db:execute(Sql, [Remark, Dt, 1, FromUid, ToUid]).


% friend_ds:set_category_id(1, 1, 0).
-spec set_category_id(integer(), integer(), integer()) -> {ok, integer()} | {error, any()}.
set_category_id(Uid, CategoryId, NewCid) ->
    Tb = friend_repo:tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET category_id = $1, updated_at = $2
        WHERE status = $3 AND from_user_id = $4 AND category_id = $5">>,
    imboy_db:execute(Sql, [NewCid, imboy_dt:now(), 1, Uid, CategoryId]).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


fields(Uid) when is_integer(Uid) ->
    fields(integer_to_binary(Uid));
fields(Uid) ->
    C_IsFriend = <<" case when d.user_id = ", Uid/binary,
                   " and d.denied_user_id = u.id then 0 else 1 end as is_friend,">>,
    C_IsFrom = <<"f.setting::jsonb->>'is_from' AS is_from,">>,
    C_Source = <<"f.setting::jsonb->>'source' AS source,">>,
    C2 = <<C_IsFrom/binary, C_Source/binary, C_IsFriend/binary, "f.remark, f.tag, f.category_id,f.created_at">>,
    <<"id,", F2/binary>> = ?DEF_USER_COLUMN,
    <<"u.id,", F2/binary, ",", C2/binary>>.
