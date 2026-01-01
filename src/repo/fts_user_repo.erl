-module(fts_user_repo).
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export([tablename/0]).
-export([allow_search/1]).
-export([count_for_user_search_page/1,
         user_search_page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("def_column.hrl").

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_pg_sql:public_tablename(<<"fts_user">>).

% fts_user_repo:allow_search(108).
allow_search(Uid) ->
    % allow_search 用户允许被搜索 1 是 2 否
    case imboy_pg:pluck(tablename(), <<"allow_search">>, #{user_id => Uid}, #{}, 2) of
        {ok, Allow} when Allow == 1 ->
            true;
        _ ->
            false
    end.

% fts_user_repo:user_search_page(<<"东区"/utf8>>, 10, 0).
%%% 分页搜索好友
-spec user_search_page(binary(), integer(), integer()) -> {ok, list(map())} | {error, any()}.
user_search_page(Keyword, Limit, Offset) ->
    % Sql = <<"select ", ?DEF_USER_COLUMN/binary,",ts_rank_cd(fts.token, to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | '))) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', $2)::text, ' <-> ', ' | ')) order by rank desc LIMIT $3 OFFSET $4">>,
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', '", Keyword/binary,
               "')::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case imboy_pg:query(Sql1, []) of
        {ok, [#{<<"keyword">> := Keyword2}]} ->
            Sql = <<"select ", ?DEF_USER_COLUMN/binary,
                    ",ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', $2) order by rank desc LIMIT $3 OFFSET $4">>,
            imboy_pg:query(Sql, [Keyword2, Keyword2, Limit, Offset]);
        _ ->
            {ok, []}
    end.



% fts_user_repo:count_for_user_search_page(<<"leeyi"/utf8>>).
% fts_user_repo:count_for_user_search_page(<<"东区"/utf8>>).
count_for_user_search_page(<<>>) ->
    0;
count_for_user_search_page(Keyword) ->
    % 使用安全的参数化查询
    % 先准备关键词
    Sql1 = <<"select replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | ') as keyword from (select 1) as temp">>,
    case imboy_pg:one(Sql1, [Keyword]) of
        {ok, #{<<"keyword">> := Keyword2}} ->
            % count(*) 只返回一行，无需 LIMIT
            Sql = <<"SELECT count(*) as count FROM ", (tablename())/binary,
                    " WHERE allow_search = 1 AND token @@ to_tsquery('jiebacfg', $1)">>,
            case imboy_pg:one(Sql, [Keyword2]) of
                {ok, #{<<"count">> := Count}} ->
                    Count;
                _ ->
                    0
            end;
        _ ->
            0
    end.


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
