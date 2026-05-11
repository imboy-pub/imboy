-module(user_tag_ds).
%%%
% user_tag_ds 是用户标签数据服务层
% 封装用户标签的数据操作和缓存管理
%%%

-include("log.hrl").
-include("common.hrl").

-export([page/5]).
-export([delete/3]).
-export([change_name/5]).
-export([add/3]).
-export([find_tag_id/3]).
-export([get_relation_objects/3]).
-export([change_scene_tag/5]).
-export([flush_subtitle/1]).
-export([find_name_by_id/2]).
-export([find_name_by_id/3]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 标签分页列表
%% @param Scene 场景（1 收藏 2 好友）
%% @param Page 页码
%% @param Size 每页大小
%% @param Where 查询条件
%% @param _OrderBy 排序（未使用）
%% @return map() 包含 total、page、size、list 的分页结果
-spec page(integer(), integer(), integer(), map(), binary()) -> map().
page(Scene, Page, Size, Where, _OrderBy) when Page > 0 ->
    Tb = user_tag_repo:tablename(),
    case elib_pg:page_with_total(Tb, Where, Page, Size) of
        {ok, #{total := Total, page := Page, size := Size, list := Items2}} ->
            % 丰富数据：添加 subtitle
            Items3 = [ maps:merge(Item, #{
                <<"subtitle">> => user_tag_relation_repo:tag_subtitle(
                    Scene,
                    maps:get(<<"id">>, Item, 0),
                    maps:get(<<"referer_time">>, Item, 0)
                )
            }) || Item <- Items2 ],
            #{total => Total, page => Page, size => Size, list => Items3};
        {error, Reason} ->
            #{total => 0, page => Page, size => Size, list => [], error => Reason}
    end.

%% @doc 删除标签
%% @param Uid 用户ID
%% @param Scene 场景（1 收藏 2 好友）
%% @param Tag 标签名称
%% @return ok
-spec delete(integer(), integer(), binary()) -> ok.
delete(Uid, Scene, Tag) ->
    case find_tag_id(Uid, Scene, Tag) of
        {ok, TagId} when TagId > 0 ->
            % 使用事务删除标签和相关数据
            elib_pg:with_tx(fun(Conn) ->
                % 删除 public.user_tag_relation
                UserTagTb = user_tag_relation_repo:tablename(),
                DelWhere = <<"scene = $1 AND user_id = $2 AND tag_id = $3">>,
                {ok, _} = elib_pg:execute(Conn,
                    <<"DELETE FROM ", UserTagTb/binary, " WHERE ", DelWhere/binary>>,
                    [Scene, Uid, TagId]),

                % 删除 public.user_tag
                TagTb = user_tag_repo:tablename(),
                {ok, _} = elib_pg:execute(Conn,
                    <<"DELETE FROM ", TagTb/binary, " WHERE id = $1">>,
                    [TagId]),

                % 更新相关表的 tag 字段
                update_tag_in_objects(Conn, Scene, Tag, TagId),
                ok
            end),

            % 清理缓存
            flush_subtitle(TagId),
            ok;
        _ ->
            ok
    end.

%% @doc 修改标签名称
%% @param Count 同名标签数量
%% @param Uid 用户ID
%% @param Scene 场景
%% @param TagId 标签ID
%% @param TagName 新标签名称
%% @return ok | binary()
-spec change_name(integer(), integer(), integer(), integer(), binary()) -> ok | binary().
change_name(Count, _Uid, _Scene, _TagId, TagName) when Count > 0 ->
    <<TagName/binary, " 已存在"/utf8>>;
change_name(0, Uid, Scene, TagId, TagName) ->
    case get_relation_objects(Scene, Uid, TagId) of
        {ok, Rows} when length(Rows) > 0 ->
            CreatedAt = elib_dt:now(),
            % 使用事务更新
            elib_pg:with_tx(fun(Conn) ->
                % 更新 user_tag
                UpdateResult = user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid, CreatedAt),
                case UpdateResult of
                    {ok, _} -> ok;
                    {error, Reason} -> ?ERROR_LOG([user_tag_update_failed, TagId, TagName, Uid, Reason]);
                    {_Count, _} -> ok
                end,

                % 更新所有关联对象的 tag
                [ change_scene_tag(Conn, Scene, Uid, maps:get(<<"object_id">>, Row), [{TagId, TagName}]) || Row <- Rows ],
                ok
            end);
        _ ->
            ok
    end,
    % 清理缓存
    flush_subtitle(TagId),
    ok.

%% @doc 添加标签
%% @param Uid 用户ID
%% @param Scene 场景（1 收藏 2 好友）
%% @param Tag 标签名称
%% @return {ok, TagId} | {error, binary()}
-spec add(integer(), integer(), binary()) -> {ok, integer()} | {error, binary()}.
add(Uid, Scene, Tag) ->
    % 参数验证
    case {Scene, Tag} of
        {0, _} -> {error, <<"invalid_scene">>};
        {_, <<>>} -> {error, <<"invalid_tag">>};
        _ ->
            add_internal(Uid, Scene, Tag)
    end.

%% @doc 查找标签ID
%% @param Uid 用户ID
%% @param Scene 场景
%% @param Tag 标签名称
%% @return {ok, TagId} | {ok, 0} (不存在)
-spec find_tag_id(integer(), integer(), binary()) -> {ok, integer()}.
find_tag_id(Uid, Scene, Tag) ->
    Tb = user_tag_repo:tablename(),
    TagId = elib_pg:pluck_value(Tb, <<"id">>,
        #{creator_user_id => Uid, scene => Scene, name => Tag}, #{}, 0),
    {ok, TagId}.

%% @doc 更新标签
%% @param Conn 数据库连接
%% @param TagId 标签ID
%% @param TagName 标签名称
%% @param Uid 用户ID
%% @param CreatedAt 创建时间
%% @return ok
-spec update_tag(pid(), integer(), binary(), integer(), binary()) -> ok.
update_tag(Conn, TagId, TagName, Uid, CreatedAt) ->
    case user_tag_relation_repo:update_tag(Conn, TagId, TagName, Uid, CreatedAt) of
        {ok, _} -> ok;
        {error, Reason} -> ?ERROR_LOG([user_tag_update_failed, TagId, TagName, Uid, Reason])
    end,
    ok.

%% @doc 获取标签关联的对象列表
%% @param Scene 场景
%% @param Uid 用户ID
%% @param TagId 标签ID
%% @return {ok, Rows}
-spec get_relation_objects(integer(), integer(), integer()) -> {ok, list()}.
get_relation_objects(Scene, Uid, TagId) ->
    Sql = <<"SELECT object_id FROM public.user_tag_relation
             WHERE scene = $1 AND user_id = $2 AND tag_id = $3">>,
    case elib_pg:query(Sql, [Scene, Uid, TagId]) of
        {ok, Rows} -> {ok, Rows};
        {error, _} -> {ok, []}
    end.

%% @doc 修改场景标签
%% @param Conn 数据库连接（可选）
%% @param Scene 场景
%% @param Uid 用户ID
%% @param ObjectId 对象ID
%% @param Tag 标签列表
%% @return ok
-spec change_scene_tag(pid() | undefined, integer(), integer(), any(), list()) -> ok.
change_scene_tag(_Conn, Scene, Uid, ObjectId, Tag) when is_list(Tag) ->
    {Table, WhereColumn} =
        case Scene of
            1 ->
                {elib_pg_sql:public_tablename(<<"user_collect">>), <<"kind_id">>};
            2 ->
                {elib_pg_sql:public_tablename(<<"user_friend">>), <<"to_user_id">>}
        end,

    % 合并新旧tag，排重，不修改tag顺序
    TagBin = merge_tag(Tag, Scene, Uid, ObjectId),

    Sql = case Scene of
        1 ->
            <<"UPDATE ", Table/binary, " SET tag = $1 WHERE user_id = $2 AND ", WhereColumn/binary, " = $3">>;
        2 ->
            <<"UPDATE ", Table/binary, " SET tag = $1 WHERE from_user_id = $2 AND ", WhereColumn/binary, " = $3">>
    end,

    TagBinWithComma = <<TagBin/binary, ",">>,
    ObjectId2 = normalize_scene_object_id(Scene, ObjectId),
    {ok, _} = elib_pg:execute(Sql, [TagBinWithComma, Uid, ObjectId2]),
    ok.

%% @doc 清理缓存
%% @param TagId 标签ID
%% @return ok
-spec flush_subtitle(integer()) -> ok.
flush_subtitle(TagId) ->
    user_tag_relation_repo:flush_subtitle(TagId),
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec normalize_scene_object_id(integer(), any()) -> any().
normalize_scene_object_id(2, ObjectId) when is_binary(ObjectId) ->
    try
        binary_to_integer(ObjectId)
    catch
        error:badarg ->
            ObjectId
    end;
normalize_scene_object_id(_Scene, ObjectId) ->
    ObjectId.

%% @doc 内部函数：添加标签（Scene 已验证）
%% @private
%% @param Uid 用户ID
%% @param Scene 场景（整数）
%% @param Tag 标签名称
%% @return {ok, TagId} | {error, term()}
-spec add_internal(integer(), integer(), binary()) -> {ok, integer()} | {error, term()}.
add_internal(Uid, Scene, Tag) ->
    Tb = elib_pg_sql:public_tablename(<<"user_tag">>),
    TagId = elib_pg:pluck_value(Tb, <<"id">>,
        #{scene => Scene, creator_user_id => Uid, name => Tag}, #{}, 0),
    case TagId of
        0 ->
            Data = #{
                creator_user_id => Uid,
                scene => Scene,
                name => Tag,
                referer_time => 0,
                created_at => elib_dt:now()
            },
            case elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)) of
                {ok, Id, _} ->
                    {ok, Id};
                {error, {error, error, <<"23505">>, unique_violation, _Msg, _Details}} ->
                    TagId2 = elib_pg:pluck_value(Tb, <<"id">>,
                        #{scene => Scene, creator_user_id => Uid, name => Tag}, #{}, 0),
                    case TagId2 of
                        0 ->
                            {error, <<"tag_already_exists">>};
                        _ ->
                            {ok, TagId2}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {ok, TagId}
    end.

%% @doc 更新对象中的 tag 字段
%% @private
%% @param Conn 数据库连接
%% @param Scene 场景
%% @param Tag 标签名称
%% @param TagId 标签ID
%% @return ok
-spec update_tag_in_objects(pid(), integer(), binary(), integer()) -> ok.
update_tag_in_objects(Conn, Scene, Tag, _TagId) ->
    UpTb =
        case Scene of
            1 -> elib_pg_sql:public_tablename(<<"user_collect">>);
            2 -> elib_pg_sql:public_tablename(<<"user_friend">>)
        end,
    UpSql = <<"UPDATE ", UpTb/binary, " SET tag = replace(tag, $1, '') WHERE tag LIKE $2">>,
    TagPattern = <<Tag/binary, ",%">>,
    {ok, _} = elib_pg:execute(Conn, UpSql, [TagPattern, TagPattern]),
    ok.

%% @doc 合并标签
%% @private
%% @param Tag 新标签列表
%% @param Scene 场景
%% @param Uid 用户ID
%% @param ObjectId 对象ID
%% @return binary() 合并后的标签字符串
-spec merge_tag(list(), integer(), integer(), any()) -> binary().
merge_tag(Tag, Scene, Uid, ObjectId) when is_list(Tag) ->
    % 获取现有标签
    Sql = <<"SELECT t.id, t.name FROM public.user_tag_relation ut
             INNER JOIN public.user_tag t ON t.id = ut.tag_id
             WHERE ut.scene = $1 AND ut.user_id = $2 AND ut.object_id = $3">>,

    TagOldLi = case elib_pg:query(Sql, [Scene, Uid, ObjectId]) of
        {ok, Rows} -> Rows;
        _ -> []
    end,

    % 合并新旧标签，去重
    TagOld = elib_cnv:implode(",",
        [ maps:get(<<"name">>, Row)
          || Row <- TagOldLi, lists:keymember(
                integer_to_binary(maps:get(<<"id">>, Row)), 1, Tag) == false ]),

    TagBin = elib_cnv:implode(",", [ Name || {_, Name} <- Tag ]),
    MergedTag = binary:split(<<TagBin/binary, ",", TagOld/binary>>, <<",">>, [global]),
    elib_cnv:implode(",", elib_cnv:remove_dups(MergedTag)).

%% G3: user_tag_relation_logic 不应直调 user_tag_repo:tablename()
-spec find_name_by_id(integer(), integer()) -> binary().
find_name_by_id(TagId, CreatorUid) ->
    find_name_by_id(TagId, CreatorUid, 2).

-spec find_name_by_id(integer(), integer(), integer()) -> binary().
find_name_by_id(TagId, CreatorUid, Scene) ->
    Tb = user_tag_repo:tablename(),
    case elib_pg:pluck_value(Tb, <<"name">>,
                             #{id => TagId, creator_user_id => CreatorUid, scene => Scene},
                             #{}, <<>>) of
        <<>> -> <<>>;
        Name -> Name
    end.
