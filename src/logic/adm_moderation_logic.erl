-module(adm_moderation_logic).
-compile([nowarn_deprecated_catch]).

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

%% @doc 复审队列分页。pending 行附带 SLA 标记（overdue=超 24h 未审），
%% 供管理端按 SLA 优先级处理；误报处理=approve 放行（同时提示调删误报词表项）。
-spec list_review_queue(integer(), integer(), map()) -> {ok, map()} | {error, term()}.
list_review_queue(Page, Size, Filters) ->
    case moderation_ds:review_page(Page, Size, Filters) of
        {ok, #{list := Rows} = Payload} when is_list(Rows) ->
            Rows2 = [apply_sla_flag(Row) || Row <- Rows],
            {ok, Payload#{list => Rows2}};
        Other ->
            Other
    end.

%% @doc 人工复审：approve -> approved（误报放行，内容保留）；
%% reject -> rejected 并联动撤下内容（R-03：频道帖/动态；失败仅记日志，
%% 审核状态已 truthful 落库，撤下可由内容侧幂等重试）。
-spec moderate(integer(), binary(), binary() | undefined, integer()) ->
    ok | {error, binary()}.
moderate(Id, Action, Reason, ReviewerId) when is_integer(Id), Id > 0 ->
    case action_to_status(Action) of
        {ok, Status} ->
            case moderation_ds:review_moderate(Id, Status, Reason, ReviewerId) of
                {ok, N} when N > 0 ->
                    maybe_remove_rejected_content(Action, Id);
                {ok, 0} ->
                    {error, <<"记录不存在或已审核"/utf8>>};
                {error, _} ->
                    {error, <<"审核操作失败"/utf8>>}
            end;
        error ->
            {error, <<"无效的审核操作"/utf8>>}
    end;
moderate(_, _, _, _) ->
    {error, <<"参数错误"/utf8>>}.

%% ===================================================================
%% R-03：reject 联动处置（撤下已发布内容）
%% ===================================================================

maybe_remove_rejected_content(<<"reject">>, Id) ->
    %% catch：撤下是尽力而为的联动（review_find/内容删除基础设施故障
    %% 均不回滚已落库的审核判定），与 remove_surface_content 的容错同口径
    case catch moderation_ds:review_find(Id) of
        {ok, Row} when is_map(Row) ->
            remove_surface_content(Row);
        _ ->
            ok
    end;
maybe_remove_rejected_content(_Action, _Id) ->
    ok.

%% 按内容面撤下：频道帖删消息并广播；动态走 admin 删除；旧消息型行无公开
%% 内容可撤。撤下失败仅记日志（审核判定已落库，不因撤下失败回滚）。
remove_surface_content(Row) ->
    MsgType = get_bin(Row, <<"msg_type">>),
    MsgId = row_positive_int(Row, <<"msg_id">>),
    case {MsgType, MsgId > 0} of
        {<<"channel_message">>, true} ->
            ChannelId = row_positive_int(Row, <<"to_id">>),
            case catch channel_message_ds:delete(MsgId) of
                {ok, _} ->
                    _ = catch channel_logic_notify:notify_message_deleted(ChannelId, MsgId),
                    ok;
                {error, Reason} ->
                    ?ERROR_LOG(["moderation reject channel remove failed: ", MsgId, Reason]),
                    ok;
                Other ->
                    ?ERROR_LOG(["moderation reject channel remove failed: ", MsgId, Other]),
                    ok
            end;
        {<<"moment_post">>, true} ->
            case catch moment_ds:delete_post_by_admin(MsgId) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ERROR_LOG(["moderation reject moment remove failed: ", MsgId, Reason]),
                    ok;
                Other ->
                    ?ERROR_LOG(["moderation reject moment remove failed: ", MsgId, Other]),
                    ok
            end;
        _ ->
            ok
    end.

%% SLA：pending 且创建超过 24h → overdue=true
-define(REVIEW_SLA_HOURS, 24).

apply_sla_flag(Row) when is_map(Row) ->
    Overdue =
        case get_bin(Row, <<"review_status">>) of
            <<"pending">> ->
                case sla_age_hours(Row) of
                    {ok, Hours} -> Hours >= ?REVIEW_SLA_HOURS;
                    error -> false
                end;
            _ ->
                false
        end,
    Row#{<<"overdue">> => Overdue};
apply_sla_flag(Row) ->
    Row.

sla_age_hours(Row) ->
    CreatedAt = get_bin(Row, <<"created_at">>),
    case catch elib_dt:rfc3339_to(CreatedAt, millisecond) of
        Ts when is_integer(Ts) ->
            Now = elib_dt:millisecond(),
            {ok, (Now - Ts) div 3600000};
        _ ->
            error
    end.

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

%% msg_id/to_id 在 DB 行中是 bigint 整数，历史数据可能序列化成 binary——两者都收
-spec row_positive_int(map(), binary()) -> integer().
row_positive_int(Row, Key) when is_map(Row) ->
    case maps:get(Key, Row, 0) of
        V when is_integer(V), V > 0 ->
            V;
        V when is_binary(V) ->
            case catch ec_cnv:to_integer(V) of
                I when is_integer(I), I > 0 -> I;
                _ -> 0
            end;
        _ ->
            0
    end;
row_positive_int(_, _) ->
    0.

-spec trim(term()) -> binary().
trim(B) when is_binary(B) ->
    list_to_binary(string:trim(binary_to_list(B)));
trim(_) ->
    <<>>.
