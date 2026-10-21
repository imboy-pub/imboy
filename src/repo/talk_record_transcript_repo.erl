-module(talk_record_transcript_repo).
-moduledoc "面聊转写版本仓储（T15：当前可见版本定位；迁移 00000211）。".
%%%
% talk_record_transcript 数据仓库层（T15 原文/摘要域）。
%
% 职责（§2/§4/§8）：
%   * 当前可见转写版本：record.current_transcript_id → transcript 行
%     （复合 FK 已保证同记录；查询仍按 record_id 关联，防跨记录混用）；
%   * 人工修订不改 transcript 谱系——模型重跑才创建新版本（§8）；
%     本仓储只读。
%%%

-export([find_current/2, find_current_tx/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

%% @doc 当前可见转写版本行（含 version/status）；未发布（current_transcript_id
%% NULL）返回 {error, not_found}。只读路径（连接外，走池）。
-spec find_current(integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_current(RecordId, Columns) ->
    Sql = current_sql(Columns),
    case elib_pg:one(Sql, [RecordId]) of
        {ok, Row} when map_size(Row) > 0 -> {ok, Row};
        {ok, _} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内版本（同 find_current/2；写路径事务内一致读）。
-spec find_current_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_current_tx(Conn, RecordId, Columns) ->
    Sql = current_sql(Columns),
    case elib_pg:query(Conn, Sql, [RecordId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @private 经 record.current_transcript_id 关联（§8 复合 FK 目标）。
-spec current_sql(binary()) -> binary().
current_sql(Columns) ->
    <<"SELECT ", Columns/binary, " FROM talk_record_transcript t",
        " WHERE t.id = (SELECT tr.current_transcript_id FROM talk_record tr",
        " WHERE tr.id = $1 AND tr.deleted_at IS NULL", " AND tr.current_transcript_id IS NOT NULL)",
        " AND t.record_id = $1">>.
