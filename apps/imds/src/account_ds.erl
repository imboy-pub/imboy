-module(account_ds).
%%%
% config 领域服务模块
% config domain service 缩写
%%%

%
%% API
-export([init/0]).
-export([allocate/0]).
-export([safe_get_max_account_id/0]).


-define(ACCOUNT_ID_CACHE_TTL, 8640000).  % 100天
% -define(BATCH_SIZE, 10).  % 每次从数据库获取的ID数量

init() ->
    Q = <<"CREATE SEQUENCE IF NOT EXISTS imboy_account_id_seq START 50000;">>,
    imboy_db:execute(Q),
    ok.

%% @doc 分配一个账户ID
% account_ds:allocate().
allocate() ->
    Key = {local_cache, account_list},
    % 使用 Depcache 的 get_wait 机制避免竞争条件
    case imboy_cache:get_wait(Key) of
        undefined ->
            % 缓存未命中，获取新ID
            case safe_get_max_account_id() of
                [] ->
                    {error, no_ids};
                [AllocatedId | Rest] ->
                    % 将剩余ID存入缓存
                    imboy_cache:set(Key, Rest, ?ACCOUNT_ID_CACHE_TTL, []),
                    AllocatedId
            end;
        {error, Reason} ->
            {error, Reason};
        {ok, IdList} when is_list(IdList) ->
            case IdList of
                [] ->
                    % 缓存为空，获取新ID
                    case safe_get_max_account_id() of
                        [] ->
                            {error, no_ids};
                        [AllocatedId | Rest] ->
                            imboy_cache:set(Key, Rest, ?ACCOUNT_ID_CACHE_TTL, []),
                            AllocatedId
                    end;
                [AllocatedId | Rest] ->
                    % 从缓存中取出一个ID并更新缓存
                    imboy_cache:set(Key, Rest, ?ACCOUNT_ID_CACHE_TTL, []),
                    AllocatedId
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% account_ds:safe_get_max_account_id().
safe_get_max_account_id() ->
    Q = <<"SELECT nextval('imboy_account_id_seq') FROM generate_series(1, 10);">>,
    Res = imboy_db:list(Q),
    create_rand_list(Res).

create_rand_list(List) ->
    [X || {_,X} <- lists:sort([{rand:uniform(), N} || {N} <- List])].
