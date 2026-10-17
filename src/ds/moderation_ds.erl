-module(moderation_ds).

-moduledoc "内容审核数据服务 —— 敏感词与复审队列。".
%%%
% 内容审核数据服务模块（敏感词 + 复审队列）
% Content moderation data service (sensitive words + review queue)
%%%

-export([sensitive_word_page/3]).
-export([sensitive_word_create/3]).
-export([sensitive_word_delete/1]).
-export([review_page/3]).
-export([review_find/1]).
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
    case sensitive_word_repo:insert(Word, Category, Severity) of
        {ok, created, _} = Result ->
            ok = imboy_cache:invalidate({moderation_sensitive_words}),
            Result;
        Other ->
            Other
    end.

-spec sensitive_word_delete(integer()) -> {ok, non_neg_integer()} | {error, term()}.
sensitive_word_delete(Id) ->
    case sensitive_word_repo:delete_by_id(Id) of
        {ok, Count} = Result when Count > 0 ->
            ok = imboy_cache:invalidate({moderation_sensitive_words}),
            Result;
        Other ->
            Other
    end.

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

-spec review_find(integer()) -> {ok, map()} | {error, term()}.
review_find(Id) ->
    review_queue_repo:find_by_id(Id).

-spec review_moderate(integer(), binary(), binary() | undefined, integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
review_moderate(Id, Status, Reason, ReviewerId) ->
    review_queue_repo:moderate(Id, Status, Reason, ReviewerId).

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec shape_row(map()) -> map().
shape_row(Row) ->
    Raw = maps:get(<<"hit_words">>, Row, <<>>),
    Metadata = maps:get(<<"metadata">>, Row, #{}),
    Words =
        case metadata_words(Metadata) of
            {ok, Structured} -> Structured;
            error -> split_words(Raw)
        end,
    maps:put(<<"hit_words">>, Words, Row).

metadata_words(Bin) when is_binary(Bin) ->
    try
        metadata_words(jsone:decode(Bin))
    catch
        _:_ -> error
    end;
metadata_words(#{<<"hit_words">> := Words}) when is_list(Words) ->
    case lists:all(fun is_binary/1, Words) of
        true -> {ok, Words};
        false -> error
    end;
metadata_words(_) ->
    error.

-spec split_words(term()) -> [binary()].
split_words(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    [W || W <- binary:split(Bin, <<",">>, [global]), W =/= <<>>];
split_words(_) ->
    [].
