-module (fts_repo).
%%%
% fts 相关操作都放到该模块，存储库模块
% fts related operations are put in this module, repository module
%%%

-export ([tablename/0]).
-export ([count_for_user_search_page/1, user_search_page/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").
-include_lib("imboy/include/def_column.hrl").

%% ===================================================================
%% API
%% ===================================================================

tablename() ->
    imboy_db:public_tablename(<<"fts_user">>).


% fts_repo:user_search_page(<<"东区"/utf8>>, 10, 0).
%%% 分页搜索好友
-spec user_search_page(binary(), integer(), integer()) ->
    {ok, list(), list()} | {error, any()}.
user_search_page(Keyword, Limit, Offset) ->
    % Sql = <<"select ", ?DEF_USER_COLUMN/binary,",ts_rank_cd(fts.token, to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', $1)::text, ' <-> ', ' | '))) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', $2)::text, ' <-> ', ' | ')) order by rank desc LIMIT $3 OFFSET $4">>,
    Keyword2 = imboy_db:pluck(<<"replace(to_tsquery('jiebacfg', ", Keyword/binary ,")::text, ' <-> ', ' | ')">>, Keyword),
    Sql = <<"select ", ?DEF_USER_COLUMN/binary,",ts_rank_cd(fts.token, to_tsquery('jiebacfg', $1)) as rank from public.fts_user fts left join public.user u on u.id = fts.user_id where fts.allow_search = 1 AND fts.token @@ to_tsquery('jiebacfg', $2) order by rank desc LIMIT $3 OFFSET $4">>,
    imboy_db:query(Sql, [Keyword2, Keyword2, Limit, Offset]).

% fts_repo:count_for_user_search_page(<<"leeyi"/utf8>>).
% fts_repo:count_for_user_search_page(<<"东区"/utf8>>).
count_for_user_search_page(<<"">>) ->
    0;
count_for_user_search_page(Keyword) ->
    % use index uk_UserId_DeniedUserId
    imboy_db:pluck(
        tablename()
        , <<"allow_search = 1 AND token @@ to_tsquery('jiebacfg', replace(to_tsquery('jiebacfg', '", Keyword/binary,"')::text, ' <-> ', ' | '))">>
        , <<"count(*) as count">>
        , 0
    ).

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
