-module(sensitive_word_repo).

%%%
% 敏感词黑名单数据仓库模块
% Sensitive word blacklist repository module
%%%

-export([tablename/0]).
-export([page/3]).
-export([insert/3]).
-export([delete_by_id/1]).

-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"sensitive_word">>).

%% @doc 分页查询敏感词（keyword 命中 word 模糊；category 精确）
-spec page(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Filters) ->
    Tb = tablename(),
    Category = maps:get(category, Filters, undefined),
    Keyword = maps:get(keyword, Filters, undefined),
    Where0 = #{},
    Where1 =
        case Category of
            C when is_binary(C), byte_size(C) > 0 -> maps:put(category, C, Where0);
            _ -> Where0
        end,
    Where2 =
        case Keyword of
            K when is_binary(K), byte_size(K) > 0 ->
                maps:put(
                    word, {op, <<"ILIKE">>, <<"%", (elib_pg:escape_like(K))/binary, "%">>}, Where1
                );
            _ ->
                Where1
        end,
    Column = <<"id, word, category, severity, created_at">>,
    elib_pg:page_with_total(Tb, Column, Where2, <<"id desc">>, Page, Size).

%% @doc 插入敏感词；word 唯一冲突则跳过（用于批量导入去重）
-spec insert(binary(), binary(), binary()) ->
    {ok, created, integer()} | {ok, skipped} | {error, term()}.
insert(Word, Category, Severity) ->
    Tb = tablename(),
    Id = elib_tsid:generate(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, word, category, severity, created_at)"
            " VALUES ($1, $2, $3, $4, NOW())"
            " ON CONFLICT (word) DO NOTHING"
            " RETURNING id">>,
    case elib_pg:query(Sql, [Id, Word, Category, Severity]) of
        {ok, [#{<<"id">> := RetId}]} ->
            {ok, created, RetId};
        {ok, []} ->
            {ok, skipped};
        {error, Reason} ->
            ?LOG_ERROR("sensitive_word_repo:insert error ~p", [Reason]),
            {error, Reason}
    end.

-spec delete_by_id(integer()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_id(Id) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE id = $1">>,
    elib_pg:execute(Sql, [Id]).
