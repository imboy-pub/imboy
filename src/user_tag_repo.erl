-module (user_tag_repo).
%%%
% user_tag 相关操作都放到该模块，存储库模块
% user_tag related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([save_tag/5, update_tag/6, save_user_tag/6]).
-export ([select_tag/3, select_user_tag/3]).
-export ([tag_subtitle/3, flush_subtitle/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"user_tag">>).

%%% 保存tag数据
-spec save_tag(any(), integer(), binary(), binary(), binary()) ->
    ok.
save_tag(Conn, Uid, Scene, CreatedAt, Tag) ->
    UpSql = <<" UPDATE SET updated_at = ", CreatedAt/binary, " RETURNING id;">>,
    Tb = imboy_db:public_tablename(<<"tag">>),
    Column = <<"(creator_user_id,scene,name,referer_time,created_at)">>,
    Sql = <<"INSERT INTO ", Tb/binary," ",
           Column/binary, " VALUES(",
           Uid/binary, ", ",
           Scene/binary, ", '",
           Tag/binary, "', ",
           "0, ",
           CreatedAt/binary, ") ON CONFLICT (creator_user_id,scene,name) DO ", UpSql/binary>>,
    lager:info(io_lib:format("user_tag_repo:save_tag/5-------------------------------------------------------------------------------- sql:~p;~n", [Sql])),
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
    case Res of
        [{ok, _, [{Id}]}] ->
            {Id, Tag};
        _ ->
            lager:error(io_lib:format("user_tag_repo:save_tag/5 Res:~p ~n", [Res])),
            {0, Tag}
    end.

-spec update_tag(any(), binary(), integer(), binary(), binary(), binary()) ->
    ok.
update_tag(Conn, TagId, TagName, RefCount, Uid, CreatedAt) ->
    Tb = imboy_db:public_tablename(<<"tag">>),
    % lager:info(io_lib:format("user_tag_repo:update_tag/6 Tb:~p ~n", [Tb])),
    Args = [
        "UPDATE ", Tb, <<" SET name = '", TagName/binary, "', referer_time = ">>, RefCount, ", updated_at = ", CreatedAt, " WHERE id =", TagId, " AND creator_user_id = ", Uid
    ],
    % lager:error(io_lib:format("user_tag_repo:update_tag/6 Args:~p ~n", [Args])),
    UpSql = imboy_func:implode("", Args),
    % lager:info(io_lib:format("user_tag_repo:update_tag/6 sql:~p;~n", [UpSql])),
    {ok, Stmt} = epgsql:parse(Conn, UpSql),
    Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
    lager:error(io_lib:format("user_tag_repo:update_tag/6 Res:~p;~n", [Res])),
    case Res of
        [{ok,1}] ->
            {TagId, TagName};
        _ ->
            % lager:error(io_lib:format("user_tag_repo:update_tag/6 Res:~p ~n", [Res])),
            {0, TagName}
    end.

%%% 保存user_tag数据
-spec save_user_tag(any(), binary(), binary(), binary(), binary(), binary()) ->
    ok.
save_user_tag(Conn, Scene, Uid, TagId, ObjectId, CreatedAt) ->
    UpSql = <<" UPDATE SET created_at = ", CreatedAt/binary, " RETURNING id;">>,
    Tb = imboy_db:public_tablename(<<"user_tag">>),
    Column = <<"(scene, user_id, tag_id, object_id, created_at)">>,
    Sql = <<"INSERT INTO ", Tb/binary," ",
           Column/binary, " VALUES(",
           Scene/binary, ", ",
           Uid/binary, ", ",
           TagId/binary, ", ",
           ObjectId/binary, ", ",
           CreatedAt/binary, ") ON CONFLICT (scene, user_id, object_id, tag_id) DO ", UpSql/binary>>,
    lager:info(io_lib:format("user_tag_repo:save_user_tag/5 sql:~p;~n", [Sql])),
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
    % lager:error(io_lib:format("user_tag_repo:save_user_tag/5 Res:~p ~n", [Res])),
    case Res of
        % [{ok,1,[{18}]}]
        [{ok, _, [{Id}]}] ->
            Id;
        _ ->
            lager:error(io_lib:format("user_tag_repo:save_user_tag/5 Res:~p ~n", [Res])),
            0
    end.

% user_tag_repo:select_tag(<<"scene = $1 AND name = any($2)">>, [2, <<"['a', 'b']">>], <<"id, name">>).
% user_tag_repo:select_tag(<<"scene = $1 AND name = any(string_to_array($2, ','))">>, [2, "a,b"], <<"id, name">>).
% {ok,[<<"id">>,<<"name">>],[{1,<<"a">>},{4,<<"b">>}]}
select_tag(Where, WhereArgs, Column) ->
    Tb = imboy_db:public_tablename(<<"tag">>),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    % lager:info(io_lib:format("user_tag_repo:select_tag/3 sql:~p, ~p;~n", [Sql, WhereArgs])),
    imboy_db:query(Sql, WhereArgs).

select_user_tag(Where, WhereArgs, Column) ->
    Tb = imboy_db:public_tablename(<<"user_tag">>),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE ", Where/binary>>,
    lager:info(io_lib:format("user_tag_repo:select_user_tag/3 sql:~p, ~p;~n", [Sql, WhereArgs])),
    imboy_db:query(Sql, WhereArgs).

tag_subtitle(S, TagId, Count) when is_integer(TagId) ->
    tag_subtitle(S, integer_to_binary(TagId), Count) ;
tag_subtitle(<<"1">>, _TagId, _Count) ->
    <<>>;
tag_subtitle(<<"2">>, _TagId, 0) ->
    <<>>;
tag_subtitle(<<"2">>, TagId, _Count) ->
    Key = imboy_func:implode("_", ["tag_subtitle_2", TagId]),
    Fun = fun() ->
        TagTb = imboy_db:public_tablename(<<"user_tag">>),
        FTb = imboy_db:public_tablename(<<"user_friend">>),
        UTb = imboy_db:public_tablename(<<"user">>),
        % Sql = <<"SELECT f.remark,u.nickname,u.account FROM ", TagTb/binary, " t
        Sql = <<"SELECT CASE
                WHEN f.remark != '' then f.remark
                WHEN u.nickname != '' then u.nickname
                ELSE u.account
            END subtitle FROM ", TagTb/binary, " t
                LEFT JOIN ", FTb/binary, " f ON t.object_id::int = f.to_user_id
                LEFT JOIN ", UTb/binary, " u ON t.object_id::int = u.id
                WHERE f.from_user_id = t.user_id AND t.scene = 2 AND t.tag_id = "
                , TagId/binary
                , " order by t.id asc limit 10 ">>,
        % lager:info(io_lib:format("user_tag_repo:tag_subtitle/2 query resp: ~p ;~n", [imboy_db:query(Sql)])),
        Items = imboy_db:list(Sql),
        % lager:info(io_lib:format("user_tag_repo:tag_subtitle/2 Items: ~p ;~n", [Items])),
        imboy_func:implode(", ", [I || {I} <- Items])
    end,
    % 缓存10天
    imboy_cache:memo(Fun, Key, 1).

% user_tag_repo:flush_subtitle()
flush_subtitle(TagId) ->
    Key = imboy_func:implode("_", ["tag_subtitle_2", TagId]),
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