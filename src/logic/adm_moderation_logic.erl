-module(adm_moderation_logic).

%%%
% 内容审核业务逻辑模块（敏感词 + 复审队列）
% Content moderation business logic (sensitive words + review queue)
%%%

-export([list_sensitive_words/3]).
-export([add_sensitive_word/3]).
-export([import_sensitive_words/1]).
-export([delete_sensitive_word/1]).
-export([list_review_queue/3]).
-export([moderate/4]).

-include("common.hrl").
-include("log.hrl").

-define(SEVERITIES, [<<"low">>, <<"medium">>, <<"high">>]).

%% ===================================================================
%% 敏感词
%% ===================================================================

-spec list_sensitive_words(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
list_sensitive_words(Page, Size, Filters) ->
    moderation_ds:sensitive_word_page(Page, Size, Filters).

-spec add_sensitive_word(binary(), binary(), binary()) ->
    {ok, map()} | {error, binary()}.
add_sensitive_word(Word0, Category0, Severity0) ->
    Word = trim(Word0),
    case Word of
        <<>> ->
            {error, <<"关键词不能为空"/utf8>>};
        _ ->
            Category = norm_category(Category0),
            Severity = norm_severity(Severity0),
            case moderation_ds:sensitive_word_create(Word, Category, Severity) of
                {ok, created, Id} ->
                    {ok, #{
                        <<"id">> => Id,
                        <<"word">> => Word,
                        <<"category">> => Category,
                        <<"severity">> => Severity
                    }};
                {ok, skipped} ->
                    {error, <<"该关键词已存在"/utf8>>};
                {error, _} ->
                    {error, <<"添加敏感词失败"/utf8>>}
            end
    end.

%% @doc 批量导入，返回 {imported, skipped} 计数
-spec import_sensitive_words(list()) -> {ok, map()}.
import_sensitive_words(Words) when is_list(Words) ->
    {Imported, Skipped} =
        lists:foldl(
            fun(Item, {I, S}) ->
                Word = trim(get_bin(Item, <<"word">>)),
                case Word of
                    <<>> ->
                        {I, S + 1};
                    _ ->
                        Category = norm_category(get_bin(Item, <<"category">>)),
                        Severity = norm_severity(get_bin(Item, <<"severity">>)),
                        case moderation_ds:sensitive_word_create(Word, Category, Severity) of
                            {ok, created, _} -> {I + 1, S};
                            _ -> {I, S + 1}
                        end
                end
            end,
            {0, 0},
            Words
        ),
    {ok, #{<<"imported">> => Imported, <<"skipped">> => Skipped}};
import_sensitive_words(_) ->
    {ok, #{<<"imported">> => 0, <<"skipped">> => 0}}.

-spec delete_sensitive_word(integer()) -> {ok, non_neg_integer()} | {error, binary()}.
delete_sensitive_word(Id) when is_integer(Id), Id > 0 ->
    case moderation_ds:sensitive_word_delete(Id) of
        {ok, N} -> {ok, N};
        {error, _} -> {error, <<"删除敏感词失败"/utf8>>}
    end;
delete_sensitive_word(_) ->
    {error, <<"参数错误"/utf8>>}.

%% ===================================================================
%% 复审队列
%% ===================================================================

-spec list_review_queue(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
list_review_queue(Page, Size, Filters) ->
    moderation_ds:review_page(Page, Size, Filters).

%% @doc 人工复审：approve -> approved，reject -> rejected
-spec moderate(integer(), binary(), binary() | undefined, integer()) ->
    ok | {error, binary()}.
moderate(Id, Action, Reason, ReviewerId) when is_integer(Id), Id > 0 ->
    case action_to_status(Action) of
        {ok, Status} ->
            case moderation_ds:review_moderate(Id, Status, Reason, ReviewerId) of
                {ok, N} when N > 0 -> ok;
                {ok, 0} -> {error, <<"记录不存在或已审核"/utf8>>};
                {error, _} -> {error, <<"审核操作失败"/utf8>>}
            end;
        error ->
            {error, <<"无效的审核操作"/utf8>>}
    end;
moderate(_, _, _, _) ->
    {error, <<"参数错误"/utf8>>}.

%% ===================================================================
%% Internal helpers
%% ===================================================================

-spec action_to_status(binary()) -> {ok, binary()} | error.
action_to_status(<<"approve">>) -> {ok, <<"approved">>};
action_to_status(<<"reject">>) -> {ok, <<"rejected">>};
action_to_status(_) -> error.

-spec norm_severity(term()) -> binary().
norm_severity(S) when is_binary(S) ->
    case lists:member(S, ?SEVERITIES) of
        true -> S;
        false -> <<"medium">>
    end;
norm_severity(_) ->
    <<"medium">>.

-spec norm_category(term()) -> binary().
norm_category(C) when is_binary(C), byte_size(C) > 0 -> C;
norm_category(_) -> <<"custom">>.

-spec get_bin(term(), binary()) -> binary().
get_bin(Map, Key) when is_map(Map) ->
    case maps:get(Key, Map, <<>>) of
        V when is_binary(V) -> V;
        V when is_list(V) -> unicode:characters_to_binary(V);
        _ -> <<>>
    end;
get_bin(_, _) ->
    <<>>.

-spec trim(term()) -> binary().
trim(B) when is_binary(B) ->
    list_to_binary(string:trim(binary_to_list(B)));
trim(_) ->
    <<>>.
