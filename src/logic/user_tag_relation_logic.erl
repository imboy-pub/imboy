-module(user_tag_relation_logic).
%%%
% user_tag_relation 业务逻辑模块
% user_tag_relation business logic module
%%%

%% @doc 添加用户标签关系
%% 为对象添加标签，支持收藏和好友场景
%% @param Uid 用户ID
%% @param Scene 场景（1-收藏，2-好友）
%% @param ObjectId 对象ID
%% @param Tag 标签列表或单个标签
%% @returns ok | binary()
-export([add/4]).

%% @doc 移除用户标签关系
%% 从对象上移除指定标签
%% @param Uid 用户ID
%% @param Scene 场景
%% @param ObjectId 对象ID
%% @param TagId 标签ID
%% @returns ok
-export([remove/4]).

%% @doc 设置用户标签关系
%% 批量设置对象的标签，支持新增和删除
%% @param Uid 用户ID
%% @param Scene 场景
%% @param ObjectIds 对象ID列表
%% @param TagId 标签ID
%% @param TagName 标签名称
%% @returns ok | binary()
-export([set/5]).

%% @doc 删除对象标签
%% @private
-export([delete_object_tag/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").


%% ===================================================================
%% API
%% ===================================================================
-spec remove(integer(), binary(), integer(), integer()) -> ok.
remove(Uid, Scene, ObjectId, TagId) when is_binary(TagId) ->
    remove(Uid, Scene, ObjectId, binary_to_integer(TagId));
remove(Uid, Scene, ObjectId, TagId) ->
    Uid2 = integer_to_binary(Uid),
    % 使用 elib_pg:one 替代 pluck
    Tb = elib_pg_sql:public_tablename(<<"user_tag">>),
    Sql = <<"SELECT name FROM ", Tb/binary, " WHERE id = $1">>,
    TagName = case elib_pg:one(Sql, [TagId]) of
        {ok, #{<<"name">> := Name}} -> Name;
        _ -> <<>>
    end,
    _ = elib_pg:with_tx(fun(Conn) ->
              % 移除 public.user_tag_relation
              user_tag_relation_ds:remove_user_tag_relation(Conn,
                                                              Scene,
                                                              Uid2,
                                                              TagId,
                                                              ObjectId),
              user_tag_relation_ds:replace_object_tag(Conn,
                                                        Scene,
                                                        Uid2,
                                                        ObjectId,
                                                        binary_to_list(TagName),
                                                        []),
              ok
      end),
    % 清理缓存
    user_tag_relation_ds:flush_subtitle(TagId),
    ok.


%% 用户标签_联系人标签设置标签
-spec set(integer(), integer(), list(), integer(), binary()) -> ok | binary().
set(Uid, Scene, ObjectIds, TagId, TagName) when is_binary(TagId) ->
    set(Uid, Scene, ObjectIds, binary_to_integer(TagId), TagName);
set(Uid, Scene, ObjectIds, TagId, TagName) ->
    NowTs = elib_dt:now(),
    CreatedAt = NowTs,

    % 使用安全的参数化查询，避免SQL注入
    Tb2 = elib_pg_sql:public_tablename(<<"user_tag">>),
    CheckSql = <<"SELECT count(*) FROM ", Tb2/binary, " WHERE scene = $1 AND creator_user_id = $2 AND name = $3 AND id != $4">>,
    Check = case elib_pg:query(CheckSql, [Scene, Uid, TagName, TagId]) of
        {ok, [#{<<"count">> := Count}]} -> Count;
        _ -> 0
    end,
    if
        Check > 0 ->
            <<TagName/binary, " 已存在"/utf8>>;
        true ->
            % [elib_hashids:encode(108), elib_hashids:encode(62902), elib_hashids:encode(62903)].
            ObjectIds2 = [ integer_to_binary(elib_hashids:decode(I))
                           || I <- ObjectIds, elib_hashids:decode(I) > 0 ],
            Tb = elib_pg_sql:public_tablename(<<"user_friend">>),
            % 使用安全的参数化查询，避免SQL注入
            OldSql = <<"SELECT to_user_id::text FROM ", Tb/binary, " WHERE tag LIKE $1">>,
            {ok, OldObjectIds} = elib_pg:query(OldSql, [<<TagName/binary, ",%">>]),
            OldObjectIds2 = [ maps:get(<<"to_user_id">>, Row) || Row <- OldObjectIds ],

            DelObjectId = OldObjectIds2 -- ObjectIds2,

            _ = elib_pg:with_tx(fun(Conn) ->
                  %
                  [ user_tag_relation_ds:remove_user_tag_relation(Conn,
                                                                    Scene,
                                                                    Uid,
                                                                    TagId,
                                                                    I) || I <- DelObjectId ],
                  % elib_log:info(io_lib:format("user_tag_relation_ds:set/5 ObjectIds2:~p, RefCount, ~p;~n", [ObjectIds2, [Conn , TagId,TagName, RefCount, Uid, CreatedAt]])),
                  % 保存 public.user_tag
                  _ = user_tag_relation_ds:update_tag(Conn, TagId, TagName, Uid, CreatedAt),

                  % 插入 public.user_tag_relation
                  [ user_tag_relation_ds:save_user_tag_relation(Conn,
                                                                  Scene,
                                                                  Uid,
                                                                  TagId,
                                                                  I,
                                                                  CreatedAt)
                    || I <- ObjectIds2, I > 0 ],

                  [ user_tag_ds:change_scene_tag(Conn,
                                                    Scene,
                                                    Uid,
                                                    I,
                                                    [{TagId, TagName}]) || I <- ObjectIds2 ],
                  % Conn, Scene, Uid, ObjectId, FromName, ToName
                  %
                  [ user_tag_relation_ds:replace_object_tag(Conn,
                                                              Scene,
                                                              Uid,
                                                              I,
                                                              binary_to_list(TagName),
                                                              []) || I <- DelObjectId ],
                  ok
          end),
            % 清理缓存
            user_tag_relation_ds:flush_subtitle(TagId),
            ok
    end.


%%% 添加标签
-spec add(integer(), integer(), binary() | integer(), list()) -> ok | binary().
add(Uid, Scene, <<>>, [Tag]) ->
    ok = elib_log:info(io_lib:format("user_tag_relation_logic:add/3 uid ~p scene ~p, tag: ~p; ~n", [Uid, Scene, Tag])),
    % 使用安全的参数化查询，避免SQL注入
    Tb = elib_pg_sql:public_tablename(<<"user_tag">>),
    Count = case elib_pg:one(<<"SELECT id FROM ", Tb/binary, " WHERE scene = $1 AND name = $2">>, [Scene, Tag]) of
        {ok, #{<<"id">> := _}} ->
            1;
        _ ->
            0
    end,
    case Count of
        0 ->
            _ = elib_pg:insert(Tb, #{
                creator_user_id => Uid,
                scene => Scene,
                name => Tag,
                referer_time => 0,
                created_at => elib_dt:now()
            }),
            ok;
        _ ->
            <<"标签名已存在."/utf8>>
    end;

add(Uid, 1, ObjectId, Tag) ->
    do_add(1, Uid, ObjectId, Tag),
    ok;
add(Uid, 2, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(2, Uid, ObjectId, Tag),
    ok;
add(Uid, 2, ObjectId, Tag) ->
    do_add(2, Uid, elib_hashids:decode(ObjectId), Tag),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-


-spec do_add(integer(), integer(), any(), any()) -> ok.
do_add(Scene, Uid, ObjectId, Tag) when is_integer(ObjectId) ->
    do_add(Scene, Uid, integer_to_binary(ObjectId), Tag);

do_add(Scene, Uid, ObjectId, []) ->
    _ = elib_pg:with_tx(fun(Conn) ->
        {Table, WhereSql, WhereParams} =
            case Scene of
                1 ->
                    {elib_pg_sql:public_tablename(<<"user_collect">>),
                    <<"user_id = $1 AND kind_id = $2">>,
                   [Uid, ObjectId]};
                2 ->
                    {elib_pg_sql:public_tablename(<<"user_friend">>),
                    <<"from_user_id = $1 AND to_user_id = $2">>,
                    [Uid, ObjectId]}
            end,
            Sql = <<"UPDATE ", Table/binary, " SET tag = '' WHERE ", WhereSql/binary>>,
            % elib_log:info(io_lib:format("user_tag_relation_logic:do_add/4 sql ~p; ~n", [Sql])),
            {ok, _} = elib_pg:execute(Sql, WhereParams),
            % 删除 public.user_tag_relation
            delete_object_tag(Conn, Scene, Uid, ObjectId),
            ok
        end),
    ok;
do_add(Scene, Uid, ObjectId, Tag) ->
    % check public.user_tag
    % {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
    % {ok, _, Tag2} = user_tag_relation_ds:select_tag(
    %     <<"scene = ", Scene/binary, " AND name = any(string_to_array($1, ','))">>
    %     , [elib_cnv:implode(",",Tag)]
    %     , <<"id, name">>
    % ),
    % TagIdNewLi = [Id || {Id, _} <- Tag2],
    % elib_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),
    NowTs = elib_dt:now(),
    CreatedAt = NowTs,
    _ = elib_pg:with_tx(fun(Conn) ->
        % 删除 public.user_tag_relation
        delete_object_tag(Conn, Scene, Uid, ObjectId),

        % 插入 public.user_tag
        TagIdNewLi = [ user_tag_relation_ds:save_tag(Conn,
                                                     Uid,
                                                     Scene,
                                                     CreatedAt,
                                                     Name) || Name <- Tag ],

        % elib_log:info(io_lib:format("user_tag_relation_logic:add/4 TagIdNewLi:~p;~n", [TagIdNewLi])),

        % 插入 public.user_tag_relation
        [ user_tag_relation_ds:save_user_tag_relation(Conn,
                                                      Scene,
                                                      Uid,
                                                      TagId,
                                                      ObjectId,
                                                      CreatedAt)
        || {TagId, _Name} <- TagIdNewLi ],
        % change_scene_tag(Conn, Scene, Uid, ObjectId, Tag),
        [ user_tag_ds:change_scene_tag(Conn,
                                        Scene,
                                        Uid,
                                        ObjectId,
                                        [{TagId, N}])
        || {TagId, N} <- TagIdNewLi ],

        % 清理缓存
        [ user_tag_relation_ds:flush_subtitle(TagId) || {TagId, _} <- TagIdNewLi ],

        ok
        end),
    ok.


% 删除 public.user_tag_relation
-spec delete_object_tag(any(), integer(), integer(), binary()) -> ok.
delete_object_tag(Conn, Scene, Uid, ObjectId) ->
    DelTb = user_tag_relation_ds:tablename(),
    DelWhereSql = <<"scene = $1 AND user_id = $2 AND object_id = $3">>,
    DelWhereParams = [Scene, Uid, ObjectId],

    SelectSql = <<"SELECT tag_id FROM ", DelTb/binary, " WHERE ", DelWhereSql/binary>>,
    {ok, DelItems} = elib_pg:query(Conn, SelectSql, DelWhereParams),

    % elib_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelItems ~p; ~n", [DelItems])),

    DelSql = <<"DELETE FROM ", DelTb/binary, " WHERE ", DelWhereSql/binary>>,
    % elib_log:info(io_lib:format("user_tag_relation_logic:delete_object_tag/4 DelSql ~p; ~n", [DelSql])),
    {ok, _} = elib_pg:execute(Conn, DelSql, DelWhereParams),
     % elib_log:error(io_lib:format("user_tag_relation_ds:delete_object_tag/4 Res:~p ~n", [Res])),

    % 清理缓存
    [ user_tag_relation_ds:flush_subtitle(maps:get(<<"tag_id">>, Row)) || Row <- DelItems ],
    ok.


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
