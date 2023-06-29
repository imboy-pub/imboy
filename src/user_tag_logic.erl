-module(user_tag_logic).
%%%
% user_tag 业务逻辑模块
% user_tag business logic module
%%%

-export ([add/4, remove/4]).
-export([merge_tag/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%%% 添加标签
-spec add(integer(), binary(), binary(), list()) -> ok.
add(_Uid, _, <<>>, []) ->
    ok;
add(_Uid, _, _, []) ->
    ok;
add(_Uid, _, <<>>, _) ->
    ok;
add(Uid, <<"collect">>, ObjectId, Tag) ->
    do_add(<<"1">>, Uid, ObjectId, Tag),
    ok;
add(Uid, <<"friend">>, ObjectId, Tag) ->
    do_add(<<"2">>, Uid, imboy_hashids:uid_decode(ObjectId), Tag),
    ok;
add(_, _, _, _Tag) ->
    ok.

-spec remove(integer(), binary(), binary(), list()) -> ok.
remove(_Uid, _, <<>>, []) ->
    ok;
remove(_Uid, _, _, []) ->
    ok;
remove(_Uid, _, <<>>, _) ->
    ok;
remove(Uid, <<"collect">>, ObjectId, Tag) ->
    do_remove(<<"1">>, Uid, ObjectId, Tag),
    ok;
remove(Uid, <<"friend">>, ObjectId, Tag) ->
    do_remove(<<"2">>, Uid, imboy_hashids:uid_decode(ObjectId), Tag),
    ok;
remove(_, _, _, _Tag) ->
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-
do_remove(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_remove(Scene, Uid, integer_to_binary(ObjectId), Tag);
% do_remove(Scene, Uid, ObjectId, Tag) ->
do_remove(_Scene, _Uid, _ObjectId, _Tag) ->
    ok.

do_add(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(Scene, Uid, integer_to_binary(ObjectId), Tag);
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
