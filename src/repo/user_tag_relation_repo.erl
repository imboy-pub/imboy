-module(user_tag_relation_repo).
%%%
% user_tag_relation 相关操作都放到该模块，存储库模块
% user_tag_relation related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([delete/3]).
-export([remove_user_tag_relation/5,
         replace_object_tag/6]).
-export([save_tag/5,
         update_tag/5,
         save_user_tag_relation/6]).
-export([select_tag/3,
         select_user_tag_relation/3]).
-export([tag_subtitle/3,
         flush_subtitle/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================


-spec tablename() -> any().
tablename() ->
    imboy_pg_sql:public_tablename(<<"user_tag_relation">>).


-spec delete(binary(), integer(), any()) -> any().
delete(Scene, Uid, ObjectId) when is_integer(Uid) ->
    delete(Scene, integer_to_binary(Uid), ObjectId);
delete(Scene, Uid, ObjectId) when is_integer(ObjectId) ->
    delete(Scene, Uid, integer_to_binary(ObjectId));
delete(Scene, Uid, ObjectId) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE scene = $1 AND user_id = $2 AND object_id = $3">>,
    imboy_log:info(io_lib:format("user_tag_relation_repo:delete/3 Sql ~p, params: ~p ~n", [Sql, [Scene, Uid, ObjectId]])),
    imboy_pg:execute(Sql, [Scene, Uid, ObjectId]).


-spec remove_user_tag_relation(any(), binary(), integer(), integer(), binary()) -> ok.
remove_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId) ->
    Tb = tablename(),
    % 使用安全的参数化查询，避免SQL注入
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE scene = $1 AND user_id = $2 AND object_id = $3 AND tag_id = $4">>,
    imboy_log:info(io_lib:format("user_tag_relation_repo:remove_user_tag_relation/5 Sql ~p, params: ~p ~n",
                                 [Sql, [Scene, Uid, ObjectId, TagId]])),
    %% 使用数据库封装接口执行，避免直接依赖驱动
    {ok, _} = imboy_pg:execute(Conn, Sql, [Scene, Uid, ObjectId, TagId]),
     ok.


-spec replace_object_tag(any(), binary(), integer(), any(), binary(), binary()) -> ok.
replace_object_tag(Conn, Scene, Uid, ObjectId, FromName, ToName) when is_integer(ObjectId) ->
    replace_object_tag(Conn, Scene, Uid, integer_to_binary(ObjectId), FromName, ToName);
replace_object_tag(Conn, Scene, Uid, ObjectId, FromName, ToName) ->
    % imboy_log:error(io_lib:format("user_tag_relation_repo:replace_object_tag/6 args:~p;~n", [[Conn, Scene, Uid, ObjectId, FromName, ToName]])),
    % 使用安全的参数化查询，避免SQL注入
    {Table, Where, Params} =
        case Scene of
            <<"1">> ->
                {imboy_pg_sql:public_tablename(<<"user_collect">>),
                 <<"user_id = $1 AND kind_id = $2">>,
                 [Uid, ObjectId]};
            <<"2">> ->
                {imboy_pg_sql:public_tablename(<<"user_friend">>),
                 <<"from_user_id = $1 AND to_user_id = $2">>,
                 [Uid, ObjectId]}
        end,
    % 构造安全的replace函数调用
    Sql = <<"UPDATE ", Table/binary, " SET tag = replace(tag, $3, $4) WHERE ", Where/binary>>,
    AllParams = Params ++ [FromName ++ ",", ToName ++ ","],
    imboy_log:error(io_lib:format("user_tag_relation_repo:replace_object_tag/6 sql:~s;~n", [Sql])),
    Res = imboy_pg:execute(Conn, Sql, AllParams),
     % {ok, Stmt} = epgsql:parse(Conn, Sql),
     % Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
     imboy_log:error(io_lib:format("user_tag_relation_repo:replace_object_tag/6 Res:~p;~n", [Res])),
     ok.


%%% 保存tag数据
-spec save_tag(any(), integer(), integer(), binary(), binary()) -> {integer(), binary()}.
save_tag(Conn, Uid, Scene, CreatedAt, Tag) ->
    Tb = imboy_pg_sql:public_tablename(<<"user_tag">>),
    Data = #{
        <<"creator_user_id">> => Uid,
        <<"scene">> => Scene,
        <<"name">> => Tag,
        <<"referer_time">> => 0,
        <<"created_at">> => CreatedAt,
        <<"updated_at">> => CreatedAt
    },

    OnConflict = <<
        "ON CONFLICT (creator_user_id, scene, name) DO "
        "UPDATE SET updated_at = EXCLUDED.updated_at "
        "RETURNING id"
    >>,

    % 构建带ON CONFLICT的INSERT SQL
    {Sql, Params} = imboy_pg_sql:insert(Tb, Data, <<"RETURNING id">>),
    FullSql = [Sql, <<" ">>, OnConflict],
    case imboy_pg:execute(Conn, FullSql, Params) of
        {ok, 1, [#{<<"id">> := Id}]} ->
            % ?LOG_INFO("Tag created: id=~p name=~s", [Id, Tag]),
            {Id, Tag};
        {ok, _Unexpected} ->
            % ?LOG_ERROR("Unexpected response: ~p", [_Unexpected]),
            {0, Tag};
        {error, _Reason} ->
            % ?LOG_ERROR("Failed to create tag: ~p", [_Reason]),
            {0, Tag}
    end.

-spec update_tag(any(), integer(), binary(), integer(), binary()) -> {integer(), binary()}.
update_tag(Conn, TagId, TagName, Uid, CreatedAt) ->
    %% 记录输入参数
    % ?LOG_DEBUG("Updating tag: id=~s, new_name=~s, user=~s",
    %           [TagId, TagName, Uid]),
    % 使用安全的参数化查询，避免SQL注入
    Tb = imboy_pg_sql:public_tablename(<<"user_tag">>),
    Sql = <<"UPDATE ", Tb/binary, " SET name = $1, updated_at = $2 WHERE id = $3 AND creator_user_id = $4">>,
    case imboy_pg:execute(Conn, Sql, [TagName, CreatedAt, TagId, Uid]) of
        {ok, 1} ->
            % ?LOG_INFO("Tag updated: id=~s", [TagId]),
            {TagId, TagName};
        {ok, N} when is_integer(N), N > 1 ->
            % ?LOG_ERROR("Unexpected affected rows: ~p", [N]),
            {0, TagName};
        {ok, 0} ->
            % ?LOG_WARNING("No tag found to update"),
            {0, TagName};
        {error, _Reason} ->
            % ?LOG_ERROR("Update failed: ~p", [Reason]),
            {0, TagName}
    end.

%%% 保存user_tag_relation数据
-spec save_user_tag_relation(any(), integer(), integer(), integer(), binary(), binary()) -> ok.
save_user_tag_relation(Conn, Scene, Uid, TagId, ObjectId, CreatedAt) ->
    Data = #{
        <<"scene">> => Scene,
        <<"user_id">> => Uid,
        <<"tag_id">> => TagId,
        <<"object_id">> => ObjectId,
        <<"created_at">> => CreatedAt
    },
    OnConflict = <<
        "ON CONFLICT (scene, user_id, object_id, tag_id) DO "
        "UPDATE SET created_at = EXCLUDED.created_at "
        "RETURNING id"
    >>,
    % 构建带ON CONFLICT的INSERT SQL
    {Sql, Params} = imboy_pg_sql:insert(tablename(), Data, <<"RETURNING id">>),
    FullSql = [Sql, <<" ">>, OnConflict],
    case imboy_pg_sql:parse_result(imboy_pg:execute(Conn, FullSql, Params)) of
        {ok, Id, _} ->
            % ?LOG_DEBUG("Operation success, id=~p", [Id]),
            Id;
        _ ->
            % ?LOG_ERROR("Unexpected response or error"),
            0
    end.


% user_tag_relation_repo:select_tag(<<"scene = $1 AND name = any($2)">>, [2, <<"['a', 'b']">>], <<"id, name">>).
% user_tag_relation_repo:select_tag(<<"scene = $1 AND name = any(string_to_array($2, ','))">>, [2, "a,b"], <<"id, name">>).
% {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
-spec select_tag(binary(), list(), binary()) -> any().
select_tag(Where, WhereArgs, Column) ->
    Tb = imboy_pg_sql:public_tablename(<<"user_tag">>),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % imboy_log:info(io_lib:format("user_tag_relation_repo:select_tag/3 sql:~p, ~p;~n", [Sql, WhereArgs])),
    imboy_pg:query(Sql, WhereArgs).


-spec select_user_tag_relation(binary(), list(), binary()) -> any().
select_user_tag_relation(Where, WhereArgs, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    imboy_log:info(io_lib:format("user_tag_relation_repo:select_user_tag_relation/3 sql:~p, ~p;~n", [Sql, WhereArgs])),
    imboy_pg:query(Sql, WhereArgs).


-spec tag_subtitle(integer(), integer(), any()) -> binary().
tag_subtitle(S, TagId, Count) when is_binary(TagId) ->
    tag_subtitle(S, binary_to_integer(TagId), Count);
tag_subtitle(1, _TagId, _Count) ->
    <<>>;
tag_subtitle(2, _TagId, 0) ->
    <<>>;
tag_subtitle(2, TagId, _Count) ->
    Key = imboy_cnv:implode("_", ["tag_subtitle_2", TagId]),
    Fun = fun() ->
        TagTb = tablename(),
        FTb = imboy_pg_sql:public_tablename(<<"user_friend">>),
        UTb = imboy_pg_sql:public_tablename(<<"user">>),
        % Sql = <<"SELECT f.remark,u.nickname,u.account FROM ", TagTb/binary, " t
        Sql = <<"SELECT CASE
                WHEN f.remark != '' then f.remark
                WHEN u.nickname != '' then u.nickname
                ELSE u.account
            END subtitle FROM ", TagTb/binary, " t
                LEFT JOIN ", FTb/binary, " f ON t.object_id::int = f.to_user_id
                LEFT JOIN ", UTb/binary, " u ON t.object_id::int = u.id
                WHERE f.from_user_id = t.user_id AND t.scene = 2 AND t.tag_id = $1
                order by t.id asc limit 10 ">>,
                  % imboy_log:info(io_lib:format("user_tag_relation_repo:tag_subtitle/2 query resp: ~s ~n", [Sql])),
        case imboy_pg:query(Sql, [TagId]) of
            {ok, Rows} when is_list(Rows) ->
                Items = [maps:get(<<"subtitle">>, Row, <<>>) || Row <- Rows],
                      imboy_cnv:implode(", ", Items);
                _ ->
                    <<>>
        end
    end,
    % 缓存1天
    imboy_cache:memo(Fun, Key, 86400).


% user_tag_relation_repo:flush_subtitle()
-spec flush_subtitle(any()) -> ok.
flush_subtitle(TagId) ->
    Key = imboy_cnv:implode("_", ["tag_subtitle_3", TagId]),
    imboy_cache:flush(Key).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

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
