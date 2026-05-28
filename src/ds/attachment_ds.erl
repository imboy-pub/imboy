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
-export([disable/1, enable/1, soft_delete/1]).
-export([orphan_stats/1, orphan_cleanup/1]).

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

-spec disable(integer() | binary()) -> ok | {error, term()}.
disable(Id) -> attachment_repo:update_status(Id, 0).

-spec enable(integer() | binary()) -> ok | {error, term()}.
enable(Id) -> attachment_repo:update_status(Id, 1).

-spec soft_delete(integer() | binary()) -> ok | {error, term()}.
soft_delete(Id) -> attachment_repo:update_status(Id, -1).

-spec orphan_stats(map()) -> {ok, map()} | {error, term()}.
orphan_stats(Opts) -> attachment_repo:orphan_stats(Opts).

%% @doc 批量物理删除孤儿附件（先删 S3，再删 DB）
-spec orphan_cleanup(map()) -> {ok, #{cleaned := integer(), errors := integer()}} | {error, term()}.
orphan_cleanup(Opts) ->
    case attachment_repo:orphan_list_for_delete(Opts) of
        {ok, []} ->
            {ok, #{cleaned => 0, errors => 0}};
        {ok, Rows} ->
            {OkIds, ErrorCount} = lists:foldl(
                fun(Row, {Acc, ErrCnt}) ->
                    Key = maps:get(<<"path">>, Row),
                    Id = maps:get(<<"id">>, Row),
                    case elib_oss:delete_object(Key) of
                        ok -> {[Id | Acc], ErrCnt};
                        {error, _Rsn} -> {Acc, ErrCnt + 1}
                    end
                end,
                {[], 0},
                Rows
            ),
            case attachment_repo:hard_delete_by_ids(OkIds) of
                ok ->
                    {ok, #{cleaned => length(OkIds), errors => ErrorCount}};
                {error, DbReason} ->
                    ?ERROR_LOG(["orphan_cleanup hard_delete_by_ids failed: ", DbReason]),
                    {error, DbReason}
            end;
        {error, R} ->
            {error, R}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================
