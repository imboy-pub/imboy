-module(account_ds).

-moduledoc "账号/配置领域服务模块 / account config domain service。".
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
%% 专用分配进程串行领取（G-01 修复，收口 C06）：批量号来自 PG Sequence
%% （nextval x 10 随机序，跨节点全局唯一），节点内由
%% imboy_account_allocator gen_server 串行分配——每个 ID 恰好领取一次；
%% PG 不可用时明确 {error, no_ids}，不虚假成功。
-spec allocate() -> non_neg_integer() | {error, term() | no_ids}.
allocate() ->
    imboy_account_allocator:allocate().

%%%===================================================================
%%% Internal functions
%%%===================================================================
