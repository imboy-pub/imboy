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
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec page(integer(), integer(), integer(), map(), binary()) -> map().
page(Scene, Page, Size, Where, _OrderBy) when Page > 0 ->
    Tb = user_tag_repo:tablename(),
    % 使用 page_with_total 同时获取总数和列表
    case imboy_pg:page_with_total(Tb, Where, Page, Size) of
        {ok, #{total := Total, page := Page, size := Size, list := Items2}} ->
            Items3 = [ maps:merge(Item, #{<<"subtitle">> =>
                             user_tag_relation_repo:tag_subtitle(Scene,
                                                                 maps:get(<<"id">>, Item, 0),
                                                                 maps:get(<<"referer_time">>, Item, 0))}) || Item <- Items2 ],
            #{total => Total, page => Page, size => Size, list => Items3};
        {error, Reason} ->
            #{total => 0, page => Page, size => Size, list => [], error => Reason}
    end.


%%% 删除标签，标签中的联系人不会被删除，使用此标签设置了分组的朋友圈，可见范围也将更新。
-spec delete(integer(), integer(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    % 使用 imboy_pg:pluck_value 替代复杂的 case 模式匹配
    TagId = imboy_pg:pluck_value(imboy_pg_sql:public_tablename(<<"user_tag">>),
        <<"id">>, #{creator_user_id => Uid, scene => Scene, name => Tag}, #{}, 0),

    _ = imboy_pg:with_tx(fun(Conn) ->
        % 删除 public.user_tag_relation
        UserTagTb = user_tag_relation_repo:tablename(),
        DelWhere = <<"scene = $1 AND user_id = $2 AND tag_id = $3">>,
        DelSql = <<"DELETE FROM ", UserTagTb/binary, " WHERE ", DelWhere/binary>>,
        {ok, _} = imboy_pg:execute(Conn, DelSql, [Scene, Uid, TagId]),

        % 删除 public.user_tag
        TagTb = imboy_pg_sql:public_tablename(<<"user_tag">>),
        DelSql2 = <<"DELETE FROM ", TagTb/binary, " WHERE id = $1">>,
        {ok, _} = imboy_pg:execute(Conn, DelSql2, [TagId]),

        %
        UpTb =
           case Scene of
               1 ->
                   imboy_pg_sql:public_tablename(<<"user_collect">>);
               2 ->
                   imboy_pg_sql:public_tablename(<<"user_friend">>)
           end,
        % 使用安全的参数化查询，避免SQL注入
        UpSql = <<"UPDATE ", UpTb/binary, " SET tag = replace(tag, $1, '') WHERE tag LIKE $2">>,
        TagPattern = <<Tag/binary, ",%">>,
        {ok, _} = imboy_pg:execute(Conn, UpSql, [TagPattern, TagPattern]),
        % 清理缓存
        user_tag_relation_repo:flush_subtitle(TagId),
        ok
        end),
    ok.


-spec change_name(integer(), integer(), integer(), integer(), binary()) -> ok | binary().
change_name(Count, _Uid, _Scene, _TagId, TagName) when Count > 0 ->
    <<TagName/binary, " 已存在"/utf8>>;
change_name(0, Uid, Scene, TagId, TagName) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT object_id FROM public.user_tag_relation WHERE scene = $1 AND user_id = $2 AND tag_id = $3">>,
    case imboy_pg:query(Sql, [Scene, Uid, TagId]) of
        {ok, Rows} ->
            % imboy_log:error(io_lib:format("user_tag_logic:change_name/4 ~s Rows: ~p; ~n", [Sql, Rows])),
            _ = imboy_pg:with_tx(fun(Conn) ->
                CreatedAt = imboy_dt:now(),
                % 保存 public.user_tag
                _ = user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid, CreatedAt),

                  [ change_scene_tag(Conn, Scene, Uid, maps:get(<<"object_id">>, Row), [{TagId, TagName}]) || Row <- Rows ],
              ok
            end),
            ok;
        _ ->
            ok
    end,
    % 清理缓存
    user_tag_relation_repo:flush_subtitle(TagId),
    ok.


%%% 添加标签
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
add(Uid, Scene, Tag) ->
    % imboy_log:info(io_lib:format("user_tag_logic:add/3 uid ~p scene ~p, tag: ~p; ~n", [Uid, Scene, Tag])),
    % 参数验证
    case {Scene, Tag} of
        {0, _} -> {error, <<"invalid_scene">>};
        {_, <<>>} -> {error, <<"invalid_tag">>};
        _ -> 
            % 将 Scene 从 binary 转换为 integer，因为数据库字段是 int 类型
            add_internal(Uid, Scene, Tag)
    end.

%% @doc 内部函数，假设 Scene 已经转换为 integer
add_internal(Uid, Scene, Tag) ->
    % 使用 imboy_pg:pluck_value 替代复杂的 case 模式匹配
    Tb = imboy_pg_sql:public_tablename(<<"user_tag">>),
    TagId = imboy_pg:pluck_value(Tb, <<"id">>,
        #{scene => Scene, creator_user_id => Uid, name => Tag}, #{}, 0),
    case TagId of
        0 ->
            Data = #{
                creator_user_id => Uid,
                scene => Scene,
                name => Tag,
                referer_time => 0,
                created_at => imboy_dt:now()
            },
            {ok, Id, _} = imboy_pg_sql:parse_result(imboy_pg:insert(Tb, Data, <<"RETURNING id">>)),
            {ok, Id};
        _ ->
            {ok, TagId}
            % <<"标签名已存在"/utf8>>
    end.


-spec change_scene_tag(any(), integer(), integer(), any(), list()) -> ok.
change_scene_tag(Conn, Scene, Uid, ObjectId, Tag) when is_list(Tag) ->
    % imboy_log:error(io_lib:format("user_tag_relation_repo:change_scene_tag/5 args:~p;~n", [[Conn, Scene, Uid, ObjectId, Tag]])),
    {Table, WhereColumn} =
        case Scene of
            1 ->
                {imboy_pg_sql:public_tablename(<<"user_collect">>), <<"kind_id">>};
            2 ->
                {imboy_pg_sql:public_tablename(<<"user_friend">>), <<"to_user_id">>}
        end,
    % 合并新旧tag，排重，不修改tag顺序
    TagBin = user_tag_logic:merge_tag(Conn, Tag, Scene, Uid, ObjectId),
    % 使用安全的参数化查询，避免SQL注入
    Sql = case Scene of
             1 ->
                 <<"UPDATE ", Table/binary, " SET tag = $1 WHERE user_id = $2 AND ", WhereColumn/binary, " = $3">>;
             2 ->
                 <<"UPDATE ", Table/binary, " SET tag = $1 WHERE from_user_id = $2 AND ", WhereColumn/binary, " = $3">>
         end,

    % imboy_log:error(io_lib:format("user_tag_relation_repo:change_scene_tag/5 sql:~s;~n", [Sql])),
    TagBinWithComma = <<TagBin/binary, ",">>,
    {ok, _} = imboy_pg:execute(Sql, [TagBinWithComma, Uid, ObjectId]),
    % imboy_log:info(io_lib:format("user_tag_relation_repo:change_scene_tag/5 execute ok;~n", [])),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

% 合并新旧tag，排重，不修改tag顺序
-spec merge_tag(any(), list(), integer(), integer(), any()) -> binary().
merge_tag(_Conn, Tag, Scene, Uid, ObjectId) when is_list(Tag) ->
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"SELECT t.id, t.name FROM public.user_tag_relation ut
             INNER JOIN public.user_tag t ON t.id = ut.tag_id
             WHERE ut.scene = $1 AND ut.user_id = $2 AND ut.object_id = $3">>,
    % imboy_log:error(io_lib:format("user_tag_logic:merge_tag/5 Sql: ~p; ~n", [Sql])),
    TagOldLi = case imboy_pg:query(Sql, [Scene, Uid, ObjectId]) of
        {ok, Rows} ->
            Rows;
        _ ->
            []
    end,
    % imboy_log:error(io_lib:format("user_tag_logic:merge_tag/5 Tag ~p, TagOldLi: ~p; ~n", [Tag, TagOldLi])),
    % TagIds = [Id || {Id, _} <- Tag],
    TagOld = imboy_cnv:implode(",",
                                [ maps:get(<<"name">>, Row)
                                  || Row <- TagOldLi, lists:keymember(integer_to_binary(maps:get(<<"id">>, Row)), 1, Tag) == false ]),
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
