-module(attachment_ds).
%%%
% attachment_ds 是附件数据服务层
% 封装附件的数据操作
%%%

-include("log.hrl").
-include("common.hrl").

%% ==================== API ====================

-export([tablename/0]).
-export([save/4]).
-export([stats/0]).
-export([page/3]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% @doc 获取附件表名
%% @returns binary() 表名
-spec tablename() -> binary().
tablename() ->
    attachment_repo:tablename().

%% @doc 保存附件信息
%% 保存附件信息，如果 MD5 已存在则更新引用次数
%% @param Conn 数据库连接
%% @param CreatedAt 创建时间
%% @param Uid 用户ID
%% @param Attach 附件信息列表
%% @returns ok
-spec save(pid(), binary(), integer(), [map()]) -> ok.
save(Conn, CreatedAt, Uid, Attach) ->
    attachment_repo:save(Conn, CreatedAt, Uid, Attach).

%% G3: adm_attach_handler 不应直调 attachment_repo
-spec stats() -> map().
stats() -> attachment_repo:stats().

-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Opts) -> attachment_repo:page(Page, Size, Opts).

%% ===================================================================
%% Internal Functions
%% ===================================================================
