-module(user_tag_logic).
%%%
% user_tag 业务逻辑模块
% user_tag business logic module
%%%

-export([page/5]).
-export([change_name/5]).
-export([add/3]).
-export([merge_tag/5]).
-export([change_scene_tag/5]).
-export([delete/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec page(binary(), integer(), integer(), binary(), binary()) -> list().
page(Scene, Page, Size, Where, OrderBy) when Page > 0 ->
    Offset = (Page - 1) * Size,
    Column = <<"id, name, referer_time, updated_at, created_at">>,
    Tb = user_tag_repo:tablename(),
    Total = imboy_db:count_for_where(Tb, Where),
    Items2 = imboy_db:page_for_where(Tb,
        Size,
        Offset,
        Where,
        OrderBy,
        Column),
    Items3 = [ Item ++ [{<<"subtitle">>,
                         user_tag_relation_repo:tag_subtitle(Scene,
                                                             proplists:get_value(<<"id">>, Item, ""),
                                                             proplists:get_value(<<"referer_time">>,
                                                                                 Item,
                                                                                 0))}] || Item <- Items2 ],
    imboy_response:page_payload(Total, Page, Size, Items3).


%%% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
-spec delete(integer(), binary(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    Where = [imboy_cnv:implode("", ["creator_user_id = ", Uid]),
             imboy_cnv:implode("", ["scene = ", Scene]),
             imboy_cnv:implode("", ["name = '", Tag, "'"])],
    TagWhere = imboy_cnv:implode(" AND ", Where),
    TagId = imboy_db:pluck(<<"user_tag">>, TagWhere, <<"id">>, 0),

    imboy_db:with_transaction(fun(Conn) ->
                                      % 删除 public.user_tag_relation
                                      UserTagTb = user_tag_relation_repo:tablename(),
                                      DelWhere = <<"scene = ", Scene/binary, " AND user_id = $1 AND tag_id = $2">>,
                                      DelSql = <<"DELETE FROM ", UserTagTb/binary, " WHERE ", DelWhere/binary>>,
                                      % imboy_log:info(io_lib:format("user_tag_logic:delete/3 DelSql ~p, ~p; ~n", [DelSql, [Uid, TagId]])),
                                      %% 使用封装的执行接口
                                      ok = imboy_db:execute(Conn, DelSql, [Uid, TagId]),

                                       % 删除 public.user_tag
                                       TagTb = imboy_db:public_tablename(<<"user_tag">>),
                                       DelSql2 = <<"DELETE FROM ", TagTb/binary, " WHERE id = $1">>,
                                       % imboy_log:info(io_lib:format("user_tag_logic:delete/3 DelSql2 ~p, p ~p; ~n", [DelSql2, TagId])),
                                       ok = imboy_db:execute(Conn, DelSql2, [TagId]),

                                       %
                                       UpTb =
                                           case Scene of
                                               <<"1">> ->
                                                   imboy_db:public_tablename(<<"user_collect">>);
                                               <<"2">> ->
                                                   imboy_db:public_tablename(<<"user_friend">>)
                                           end,
                                       UpSql = <<"UPDATE ", UpTb/binary, " SET tag = replace(tag, '", Tag/binary,
                                                 ",', '') WHERE tag like '%", Tag/binary, ",%';">>,
                                       % imboy_log:info(io_lib:format("user_tag_logic:delete/3 UpSql  ~p; ~n", [UpSql])),
                                       ok = imboy_db:execute(Conn, UpSql, []),
                                       % 清理缓存
                                       user_tag_relation_repo:flush_subtitle(TagId),
                                       % imboy_log:info(io_lib:format("user_tag_logic:delete/3 UpSql  ~p, Res ~p; ~n", [UpSql, Res])),
                                       ok
                              end),
    ok.


-spec change_name(integer(), binary(), binary(), integer(), binary()) -> ok.
change_name(Count, _Uid, _Scene, _TagId, TagName) when Count > 0 ->
    <<TagName/binary, " 已存在"/utf8>>;
change_name(0, Uid, Scene, TagId, TagName) ->
    CreatedAt = imboy_dt:now(),
    % RefCount = imboy_db:pluck(<<"user_tag_relation">>,
    %                           <<"scene = ", Scene/binary, " AND tag_id = ", TagId/binary, " AND user_id = ",
    %                             Uid/binary>>,
    %                           <<"count(*)">>,
    %                           0),
    Sql = <<"SELECT object_id FROM public.user_tag_relation WHERE scene = ", Scene/binary, " AND user_id = ",
            Uid/binary, " AND tag_id = ", TagId/binary>>,
    ObjectIds2 = imboy_db:list(Sql),
    % imboy_log:error(io_lib:format("user_tag_logic:change_name/4 ~s ObjectIds2: ~p; ~n", [Sql, ObjectIds2])),
    imboy_db:with_transaction(fun(Conn) ->
                                      % 保存 public.user_tag
                                      user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid, CreatedAt),

                                      [ change_scene_tag(Conn, Scene, Uid, I, [{TagId, TagName}]) || {I} <- ObjectIds2 ],
                                      ok
                              end),
    % 清理缓存
    user_tag_relation_repo:flush_subtitle(TagId),
    ok.


%%% 添加标签
-spec add(integer(), binary(), binary()) -> [binary() | {ok, _, [{integer()}]}].
add(Uid, Scene, Tag) ->
    % imboy_log:info(io_lib:format("user_tag_logic:add/3 uid ~p scene ~p, tag: ~p; ~n", [Uid, Scene, Tag])),
    Uid2 = integer_to_binary(Uid),
    Count = imboy_db:pluck(<<"user_tag">>,
                           <<"scene = ", Scene/binary, " AND creator_user_id= ", Uid2/binary, " AND name = '",
                             Tag/binary, "'">>,
                           <<"id">>,
                           0),
    case Count of
        0 ->
            % {ok,1,[{10}]}
            imboy_db:insert_into(<<"user_tag">>, #{
                creator_user_id => Uid
                , scene => Scene
                , name => <<"'", Tag/binary, "'">>
                , referer_time => 0
                , created_at => imboy_dt:now()
            });
        _ ->
            <<"标签名已存在"/utf8>>
    end.


change_scene_tag(Conn, Scene, Uid2, ObjectId, Tag) when is_list(Tag) ->
    % imboy_log:error(io_lib:format("user_tag_relation_repo:change_scene_tag/5 args:~p;~n", [[Conn, Scene, Uid2, ObjectId, Tag]])),
    {Table, Where} =
        case Scene of
            <<"1">> ->
                {imboy_db:public_tablename(<<"user_collect">>),
                 <<"user_id = ", Uid2/binary, " AND kind_id = '", ObjectId/binary, "'">>};
            <<"2">> ->

                {imboy_db:public_tablename(<<"user_friend">>),
                 <<"from_user_id = ", Uid2/binary, " AND to_user_id = ", ObjectId/binary>>}
        end,
    % 合并新旧tag，排重，不修改tag顺序
    TagBin = user_tag_logic:merge_tag(Conn, Tag, Scene, Uid2, ObjectId),
    Sql = <<"UPDATE ", Table/binary, " SET tag = '", TagBin/binary, ",' WHERE ", Where/binary>>,

    % imboy_log:error(io_lib:format("user_tag_relation_repo:change_scene_tag/5 sql:~s;~n", [Sql])),
    % epgsql:equery(Conn, Sql),
    %% 使用统一封装的执行接口，避免直接依赖 epgsql
    ok = imboy_db:execute(Conn, Sql, []),
    % imboy_log:info(io_lib:format("user_tag_relation_repo:change_scene_tag/5 execute ok;~n", [])),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-


% 合并新旧tag，排重，不修改tag顺序
merge_tag(Conn, Tag, Scene, Uid, ObjectId) when is_list(Tag) ->
    Sql =
        <<"SELECT t.id, t.name FROM public.user_tag_relation ut INNER JOIN public.user_tag t ON t.id = ut.tag_id WHERE ut.scene = ",
          Scene/binary, " AND ut.user_id = ", Uid/binary, " AND ut.object_id = '", ObjectId/binary, "'">>,
    % imboy_log:error(io_lib:format("user_tag_logic:merge_tag/5 Sql: ~p; ~n", [Sql])),
    TagOldLi = imboy_db:list(Conn, Sql),
    % imboy_log:error(io_lib:format("user_tag_logic:merge_tag/5 Tag ~p, TagOldLi: ~p; ~n", [Tag, TagOldLi])),
    % TagIds = [Id || {Id, _} <- Tag],
    TagOld = imboy_cnv:implode(",",
                                [ Name
                                  || {Id, Name} <- TagOldLi, lists:keymember(integer_to_binary(Id), 1, Tag) == false ]),
    TagBin = imboy_cnv:implode(",", [ Name || {_, Name} <- Tag ]),
    MergedTag = binary:split(<<TagBin/binary, ",", TagOld/binary>>, <<",">>, [global]),
    % imboy_log:error(io_lib:format("user_tag_logic:merge_tag/5 old ~p, new ~p, merged: ~p; ~n", [TagOld, TagBin, MergedTag])),
    imboy_cnv:implode(",", imboy_cnv:remove_dups(MergedTag)).


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
