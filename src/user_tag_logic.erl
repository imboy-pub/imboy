-module(user_tag_logic).
%%%
% user_tag 业务逻辑模块
% user_tag business logic module
%%%

-export ([add/4]).
-export([merge_tag/3]).
-export([delete/3]).
-export([tag_page/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec tag_page(integer(), integer(), binary(), binary()) -> list().
tag_page(Page, Size, Where, OrderBy) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Total = tag_repo:count_for_where(Where),
    case tag_repo:page_for_where(Size, Offset, Where, OrderBy) of
        {ok, _, []} ->
            imboy_response:page_payload(Total, Page, Size, []);
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            Items2 = [lists:zipwith(
                fun(X, Y) -> {X, Y} end,
                ColumnLi, Row
                ) ||
                    Row <- Items1
            ],
            imboy_response:page_payload(Total, Page, Size, Items2);
        _ ->
            imboy_response:page_payload(Total, Page, Size, [])
    end.


%%% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
-spec delete(integer(), binary(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    Where = [
        imboy_func:implode("", ["creator_user_id = ", Uid]),
        imboy_func:implode("", ["scene = ", Scene]),
        imboy_func:implode("", ["name = '", Tag, "'"])
    ],
    TagWhere = imboy_func:implode(" AND ", Where),
    TagId = imboy_db:pluck(<<"tag">>, TagWhere, <<"id">>, 0),

    imboy_db:with_transaction(fun(Conn) ->
        % 删除 public.user_tag
        UserTagTb = user_tag_repo:tablename(),
        DelWhere = <<"scene = ", Scene/binary, " AND user_id = $1 AND tag_id = $2">>,
        DelSql = <<"DELETE FROM ", UserTagTb/binary," WHERE ", DelWhere/binary>>,
        lager:info(io_lib:format("user_tag_logic:delete/3 DelSql ~p, ~p; ~n", [DelSql, [Uid, TagId]])),
        epgsql:equery(Conn, DelSql, [Uid, TagId]),

        % 删除 public.tag
        TagTb = imboy_db:public_tablename(<<"tag">>),
        DelSql2 = <<"DELETE FROM ", TagTb/binary," WHERE id = $1">>,
        lager:info(io_lib:format("user_tag_logic:delete/3 DelSql2 ~p, p ~p; ~n", [DelSql2, TagId])),
        epgsql:equery(Conn, DelSql2, [TagId]),

        %
        UpTb = case Scene of
            <<"1">> ->
                imboy_db:public_tablename(<<"user_collect">>);
            <<"2">> ->
                imboy_db:public_tablename(<<"user_friend">>)
        end,
        UpSql = <<"UPDATE ", UpTb/binary, " SET tag = replace(tag, '", Tag/binary,",', '') WHERE tag like '%", Tag/binary, ",%';">>,
        lager:info(io_lib:format("user_tag_logic:delete/3 UpSql  ~p; ~n", [UpSql])),

        Res = epgsql:equery(Conn, UpSql),

        lager:info(io_lib:format("user_tag_logic:delete/3 UpSql  ~p, Res ~p; ~n", [UpSql, Res])),
        ok
    end),
    ok.

%%% 添加标签
-spec add(integer(), binary(), binary(), list()) -> ok.
add(Uid, Scene, <<>>, [Tag]) ->
    lager:info(io_lib:format("user_tag_logic:add/3 uid ~p scene ~p, tag: ~p; ~n", [Uid, Scene, Tag])),
    Count = imboy_db:pluck(
        <<"tag">>
        , <<"scene = ", Scene/binary, " AND name = ", Tag/binary>>
        , <<"id">>, 0
    ),
    case Count of
        0 ->
            Column = <<"(creator_user_id,scene,name,referer_time,created_at)">>,
            Value = [Uid, Scene, <<"'",Tag/binary, "'">>, 0, imboy_dt:millisecond()],
            imboy_db:insert_into(<<"tag">>, Column, Value),
            ok;
        _ ->
            <<"标签名已存在"/utf8>>
    end;

add(Uid, <<"1">>, ObjectId, Tag) ->
    do_add(<<"1">>, Uid, ObjectId, Tag),
    ok;
add(Uid, <<"2">>, ObjectId, Tag) ->
    do_add(<<"2">>, Uid, imboy_hashids:uid_decode(ObjectId), Tag),
    ok.
%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

do_add(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(Scene, Uid, integer_to_binary(ObjectId), Tag);

% Tag = [] 移除特定对象的标签
do_add(Scene, Uid, ObjectId, []) ->
    Uid2 = integer_to_binary(Uid),
    imboy_db:with_transaction(fun(Conn) ->
        {Table, Where} = case Scene of
            <<"1">> ->
                {
                    imboy_db:public_tablename(<<"user_collect">>)
                    , <<"user_id = ", Uid2/binary, " AND kind_id = '", ObjectId/binary, "'">>
                };
            <<"2">> ->
                {
                    imboy_db:public_tablename(<<"user_friend">>)
                    , <<"from_user_id = ", Uid2/binary, " AND to_user_id = ", ObjectId/binary>>
                }
        end,

        % 删除 public.user_tag
        DelTb = user_tag_repo:tablename(),
        DelWhere = <<"scene = ", Scene/binary, " AND user_id = $1 AND object_id = $2)">>,
        DelSql = <<"DELETE FROM ", DelTb/binary," WHERE ", DelWhere/binary>>,
        lager:info(io_lib:format("user_tag_logic:do_add/4 DelSql ~p; ~n", [DelSql])),
        epgsql:equery(Conn, DelSql, [Uid, ObjectId]),

        Sql = <<"UPDATE ", Table/binary," SET tag = '' WHERE ", Where/binary>>,
        lager:info(io_lib:format("user_tag_logic:do_add/4 sql ~p; ~n", [Sql])),
        epgsql:equery(Conn, Sql),
        ok
    end),
    ok;
%
do_add(Scene, Uid, ObjectId, Tag) ->
    % check public.tag
    % {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
    {ok, _, Tag2} = user_tag_repo:select_tag(
        <<"scene = ", Scene/binary, " AND name = any(string_to_array($1, ','))">>
        , [imboy_func:implode(",",Tag)]
        , <<"id, name">>
    ),

    Tag3 = [N1 || {_Id, N1} <- Tag2],
    % Tag4 差集，不在tag表里面的tag
    Tag4 = Tag -- Tag3,

    % check public.user_tag
    TagIdOld = [Id || {Id, _} <- Tag2],
    TagIdBin = imboy_func:implode(",", TagIdOld),
    UserTagId = case TagIdBin of
        <<>> ->
            [];
        _ ->
            {ok, _, UserTagId0} = user_tag_repo:select_user_tag(
                <<"scene = ", Scene/binary, " AND user_id = $1 AND object_id = $2 AND tag_id in(", TagIdBin/binary ,")">>
                , [Uid, ObjectId]
                , <<"tag_id">>
            ),
            UserTagId0
    end,
    lager:info(io_lib:format("user_tag_logic:add/4 UserTagId:~p;~n", [UserTagId])),

    NowTs = imboy_dt:millisecond(),
    Uid2 = integer_to_binary(Uid),
    CreatedAt = integer_to_binary(NowTs),
    imboy_db:with_transaction(fun(Conn) ->
         lager:info(io_lib:format("user_tag_logic:add/4 1 ~p, tag:~p;~n", [CreatedAt, Tag4])),

        % 插入 public.tag
        Tag5 = [user_tag_repo:save_tag(Conn, Uid2, Scene, CreatedAt, Name) || Name <- Tag4],
        Tag6 = [Id || {Id, _} <- Tag5] ++ TagIdOld -- [Id || {Id} <- UserTagId],
        lager:info(io_lib:format("user_tag_logic:add/4 Tag6 ~p, UserTagId:~p;~n", [Tag6, UserTagId])),

        % 插入 public.user_tag
        [
            user_tag_repo:save_user_tag(Conn, Scene, Uid2, integer_to_binary(TagId), ObjectId, CreatedAt) || TagId <- Tag6, TagId > 0
        ],
        {Table, Where} = case Scene of
            <<"1">> ->
                {
                    imboy_db:public_tablename(<<"user_collect">>)
                    , <<"user_id = ", Uid2/binary, " AND kind_id = '", ObjectId/binary, "'">>
                };
            <<"2">> ->
                {
                    imboy_db:public_tablename(<<"user_friend">>)
                    , <<"from_user_id = ", Uid2/binary, " AND to_user_id = ", ObjectId/binary>>
                }
        end,
        % 合并新旧tag，排重，不修改tag顺序
        TagBin = user_tag_logic:merge_tag(Table, Where, Tag),
        Sql = <<"UPDATE ", Table/binary," SET tag = '", TagBin/binary
            ,"' WHERE ", Where/binary>>,
        lager:info(io_lib:format("user_tag_logic:do_add/4 sql ~p; ~n", [Sql])),
        epgsql:equery(Conn, Sql),
        ok
    end),
    ok.

% 合并新旧tag，排重，不修改tag顺序
merge_tag(Table, Where, Tag) ->
    TagOld = imboy_db:pluck(Table, Where, <<"tag">>, <<>>),
    TagBin = imboy_func:implode(",", Tag),
    MergedTag = binary:split(<<TagBin/binary, ",", TagOld/binary>>, <<",">>, [global]),
    % lager:info(io_lib:format("user_tag_logic:merge_tag/3 old ~p, new ~p, merged: ~p; ~n", [TagOld, TagBin, MergedTag])),
    imboy_func:implode(",", imboy_func:remove_dups(MergedTag)).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
