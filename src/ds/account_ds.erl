-module(account_ds).

%%%
% config 领域服务模块
% config domain service 缩写
%%%

-include("log.hrl").

%
%% API
-export([init/0]).
-export([allocate/0]).

%% @doc 初始化账户ID序列
%% 创建账户ID序列，如果序列不存在则创建并设置起始值为50000。
%% 使用public模式显式创建序列，避免受search_path影响。
-spec init() -> ok.
-define(ACCOUNT_SEQ, <<"public.imboy_account_id_seq">>).
% 100天
-define(ACCOUNT_ID_CACHE_TTL, 8640000).

%% @doc 初始化：显式在 public 模式下创建序列，避免受 search_path 影响。
%% 注意：此函数始终返回 ok，即使数据库操作失败
init() ->
    Q = <<"CREATE SEQUENCE IF NOT EXISTS ", ?ACCOUNT_SEQ/binary, " START 50000;">>,
    try elib_pg:execute(Q, []) of
        _ ->
            ok
    catch
        Class:Reason ->
            ok = ?ERROR_LOG([account_ds_init_failed, Class, Reason]),
            ok
    end.

%% @doc 分配一个账户ID
%% 从缓存或数据库中分配一个唯一的账户ID。
%% 使用Depcache的get_wait机制避免竞争条件，确保ID的唯一性。
%% 当缓存为空或数据库无法获取ID时，返回{error, no_ids}。
-spec allocate() -> non_neg_integer() | {error, term() | no_ids}.
allocate() ->
    Key = {local_cache, account_list},
    % 使用 Depcache 的 get_wait 机制避免竞争条件
    case imboy_cache:get_wait(Key) of
        undefined ->
            init(),
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

%% @doc 从序列中批量申请 10 个 ID
%% 使用public模式显式访问序列，避免search_path异常。
%% 返回随机排序的ID列表，避免连续分配。
%% @returns 账户ID列表或空列表
-spec safe_get_max_account_id() -> [non_neg_integer()].
safe_get_max_account_id() ->
    Q = <<"SELECT nextval('", ?ACCOUNT_SEQ/binary, "') FROM generate_series(1, 10);">>,
    try elib_pg:query(Q, []) of
        {ok, Res} ->
            create_rand_list(Res);
        Error ->
            ok = ?ERROR_LOG([safe_get_max_account_id_failed, Error]),
            []
    catch
        Class:Reason ->
            ok = ?ERROR_LOG([safe_get_max_account_id_failed, Class, Reason]),
            []
    end.

%% @doc 创建随机排序的ID列表
%% 为避免连续分配ID导致的安全问题，对ID列表进行随机排序。
%% @param List 数据库查询返回的原始ID列表
%% @returns 随机排序后的ID列表
-spec create_rand_list([map()]) -> [non_neg_integer()].
create_rand_list([]) ->
    [];
create_rand_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || #{<<"nextval">> := N} <- List])].
