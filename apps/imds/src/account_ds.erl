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

-define(ACCOUNT_SEQ, <<"public.imboy_account_id_seq">>).
-define(ACCOUNT_ID_CACHE_TTL, 8640000).  % 100天
% -define(BATCH_SIZE, 10).  % 每次从数据库获取的ID数量

%% @doc 初始化：显式在 public 模式下创建序列，避免受 search_path 影响。
init() ->
    Q = <<"CREATE SEQUENCE IF NOT EXISTS ", ?ACCOUNT_SEQ/binary, " START 50000;">>,
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
                    % 关键修复：当无法从数据库获取到可用 ID 时，需要写入一个短 TTL 的空列表以释放 get_wait 锁，
                    % 否则其他并发进程会一直等待直到 30s 超时（{timeout,{gen_server,call,[imboy_cache,{get_wait, Key},30000]}}）。
                    % 这里设置 3 秒的 TTL，既能快速唤醒等待者，也避免长时间缓存空值。
                    imboy_cache:set(Key, [], 3, []),
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
                            % 同上：写入一个短 TTL 的空列表，释放等待队列，避免后续进程超时
                            imboy_cache:set(Key, [], 3, []),
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
%% @doc 从序列中批量申请 10 个 ID；为避免 search_path 异常，使用 public 前缀。
safe_get_max_account_id() ->
    Q = <<"SELECT nextval('", ?ACCOUNT_SEQ/binary, "') FROM generate_series(1, 10);">>,
    Res = imboy_db:list(Q),
    create_rand_list(Res).

create_rand_list(List) ->
    [X || {_,X} <- lists:sort([{rand:uniform(), N} || {N} <- List])].
