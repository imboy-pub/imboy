-module (user_tag_repo).
%%%
% user_tag 相关操作都放到该模块，存储库模块
% user_tag related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([save_tag/5, save_user_tag/6]).
-export ([select_tag/3, select_user_tag/3]).

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
    lager:info(io_lib:format("user_tag_repo:save_tag/5 sql:~p;~n", [Sql])),
    {ok, Stmt} = epgsql:parse(Conn, Sql),
    Res = epgsql:execute_batch(Conn, [{Stmt, []}]),
    case Res of
        [{ok, _, [{Id}]}] ->
            {Id, Tag};
        _ ->
            lager:error(io_lib:format("user_tag_repo:save_tag/5 Res:~p ~n", [Res])),
            {0, Tag}
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
    case Res of
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
