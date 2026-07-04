-module(moderation_ds).

%%%
% 内容审核数据服务模块（敏感词 + 复审队列）
% Content moderation data service (sensitive words + review queue)
%%%

-export([sensitive_word_page/3]).
-export([sensitive_word_create/3]).
-export([sensitive_word_delete/1]).
-export([review_page/3]).
-export([review_moderate/4]).

-include("common.hrl").
-include("log.hrl").

%% ===================================================================
%% 敏感词
%% ===================================================================

-spec sensitive_word_page(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
sensitive_word_page(Page, Size, Filters) ->
    sensitive_word_repo:page(Page, Size, Filters).

-spec sensitive_word_create(binary(), binary(), binary()) ->
    {ok, created, integer()} | {ok, skipped} | {error, term()}.
sensitive_word_create(Word, Category, Severity) ->
    sensitive_word_repo:insert(Word, Category, Severity).

-spec sensitive_word_delete(integer()) -> {ok, non_neg_integer()} | {error, term()}.
sensitive_word_delete(Id) ->
    sensitive_word_repo:delete_by_id(Id).

%% ===================================================================
%% 复审队列
%% ===================================================================

-spec review_page(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
review_page(Page, Size, Filters) ->
    case review_queue_repo:page(Filters, Page, Size) of
        {ok, P} ->
            List = maps:get(list, P, []),
            {ok, maps:put(list, [shape_row(R) || R <- List], P)};
        Err ->
            Err
    end.

-spec review_moderate(integer(), binary(), binary() | undefined, integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
review_moderate(Id, Status, Reason, ReviewerId) ->
    review_queue_repo:moderate(Id, Status, Reason, ReviewerId).

%% ===================================================================
%% Internal helpers
%% ===================================================================

%% @doc hit_words 存储为逗号分隔文本，向前端返回字符串数组
%% ponytail: 逗号分隔存储，敏感词本身不含逗号；若将来需支持含逗号词，改 jsonb 列
-spec shape_row(map()) -> map().
shape_row(Row) ->
    Raw = maps:get(<<"hit_words">>, Row, <<>>),
    maps:put(<<"hit_words">>, split_words(Raw), Row).

-spec split_words(term()) -> [binary()].
split_words(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    [W || W <- binary:split(Bin, <<",">>, [global]), W =/= <<>>];
split_words(_) ->
    [].
