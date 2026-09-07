-module(moderation_policy).
-compile([nowarn_deprecated_catch]).
%%%
%%% R-03：非 E2EE 公开内容（频道帖子/动态）的唯一审核 policy 入口。
%%% * 决定性关键词规则，无 AI provider；
%%% * severity=high 命中 → blocked（发布前直接拒绝，quarantine 语义）；
%%% * severity=medium/low 命中 → queued（先发后审：照常发布并写人工复核队列，
%%%   Admin reject 由 adm_moderation_logic 联动撤下内容，approve=误报放行）；
%%% * 词表读取失败 fail-open（放行 + ERROR LOG）：检查设施故障≠违规证据，
%%%   不应把公开内容发布面整体打挂；
%%% * E2EE 私信（C2C/C2G）路径绝不接入本模块——strict 模式下服务端只有
%%%   密文，举报面走 R-01 的客户端明文证据模型。
%%%

-export([inspect/2]).
-export([enqueue/7]).
-export([normalize_text/1]).

-include("log.hrl").

-define(WORDS_CACHE_TTL, 60).
-define(SURFACE_CHANNEL, <<"channel_message">>).
-define(SURFACE_MOMENT, <<"moment_post">>).

-type surface() :: channel_message | moment_post.
-type hit() :: #{word := binary(), severity := binary()}.

-export_type([surface/0, hit/0]).

%% @doc 审核判定：allow | {blocked, Hits} | {queued, Hits}。
%% 文本先归一化（小写 + 全角转半角 + 去零宽字符）再做包含匹配，
%% 词表侧同样归一化，防大小写/全角/零宽绕过。
-spec inspect(surface(), binary()) -> allow | {blocked, [hit()]} | {queued, [hit()]}.
inspect(_Surface, Text) when not is_binary(Text) ->
    allow;
inspect(_Surface, Text) ->
    case normalize_text(Text) of
        <<>> ->
            allow;
        Norm ->
            case cached_words() of
                {ok, Words} ->
                    decide(match_hits(Norm, Words, []));
                {error, Reason} ->
                    ?ERROR_LOG(["moderation_policy wordlist unavailable, fail-open: ", Reason]),
                    allow
            end
    end.

%% @doc 命中入队（先发后审复核行）。入队失败由调用方决定策略（发布面 fail-open）。
-spec enqueue(
    surface(), integer(), integer(), integer(), binary(), binary(), [hit()]
) -> ok | {error, term()}.
enqueue(Surface, MsgId, ToId, FromId, FromAccount, Content, Hits) ->
    Id = elib_tsid:generate(),
    HitWords = iolist_to_binary(lists:join(<<",">>, [hit_word(H) || H <- Hits])),
    {MsgType, ToType} = surface_types(Surface),
    Tb = review_table(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, msg_id, msg_type, content, from_id, from_account, to_id, to_type,"
            " hit_words, review_status)"
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, 'pending')">>,
    case
        elib_pg:query(Sql, [
            Id, MsgId, MsgType, Content, FromId, FromAccount, ToId, ToType, HitWords
        ])
    of
        {ok, _} ->
            ok;
        {error, Reason} = Err ->
            ?ERROR_LOG(["moderation_policy enqueue error: ", Reason]),
            Err
    end.

%% @doc 文本归一化：小写 + 全角 ASCII 变体（U+FF01..FF5E）转半角 + 去零宽字符。
-spec normalize_text(binary()) -> binary().
normalize_text(Text) when is_binary(Text) ->
    normalize_chars(string:lowercase(Text), <<>>);
normalize_text(Text) ->
    normalize_text(ec_cnv:to_binary(Text)).

%% ===================================================================
%% Internal
%% ===================================================================

decide([]) ->
    allow;
decide(Hits) ->
    case lists:any(fun(H) -> maps:get(severity, H) =:= <<"high">> end, Hits) of
        true -> {blocked, Hits};
        false -> {queued, Hits}
    end.

match_hits(_Norm, [], Acc) ->
    lists:reverse(Acc);
match_hits(Norm, [Word | Rest], Acc) ->
    W = normalize_text(word_bin(Word)),
    case W =/= <<>> andalso binary:match(Norm, W) =/= nomatch of
        true ->
            Hit = #{word => word_bin(Word), severity => severity_bin(Word)},
            match_hits(Norm, Rest, [Hit | Acc]);
        false ->
            match_hits(Norm, Rest, Acc)
    end.

cached_words() ->
    Fun = fun sensitive_word_repo:all/0,
    %% catch：缓存进程/ETS 不可用时（eunit 未 mock、imboy_cache flake）按词表
    %% 不可得处理 → 上层 fail-open，不炸发布面
    case catch imboy_cache:memo(Fun, {moderation_sensitive_words}, ?WORDS_CACHE_TTL) of
        {ok, Words} when is_list(Words) ->
            {ok, Words};
        Other ->
            {error, Other}
    end.

word_bin(Word) when is_binary(Word) ->
    Word;
word_bin(#{<<"word">> := W}) when is_binary(W) ->
    W;
word_bin(_) ->
    <<>>.

severity_bin(#{<<"severity">> := S}) when is_binary(S) ->
    case S of
        <<"high">> -> <<"high">>;
        <<"low">> -> <<"low">>;
        _ -> <<"medium">>
    end;
severity_bin(_) ->
    <<"medium">>.

hit_word(H) ->
    maps:get(word, H).

surface_types(channel_message) ->
    {?SURFACE_CHANNEL, <<"channel">>};
surface_types(moment_post) ->
    {?SURFACE_MOMENT, <<"moment">>}.

review_table() ->
    elib_pg_sql:public_tablename(<<"review_queue">>).

%% 全角 ASCII 变体与零宽字符逐字符处理；文本量级为帖子/动态（KB 级），可接受。
normalize_chars(<<>>, Acc) ->
    Acc;
normalize_chars(<<U/utf8, Rest/binary>>, Acc) when U >= 16#FF01, U =< 16#FF5E ->
    normalize_chars(Rest, <<Acc/binary, (U - 16#FEE0)/utf8>>);
normalize_chars(<<U/utf8, Rest/binary>>, Acc) when
    U =:= 16#200B; U =:= 16#200C; U =:= 16#200D; U =:= 16#FEFF
->
    normalize_chars(Rest, Acc);
normalize_chars(<<C/utf8, Rest/binary>>, Acc) ->
    normalize_chars(Rest, <<Acc/binary, C/utf8>>);
%% 非 UTF-8 尾部（损坏字节）直接丢弃，不因编码损坏放过检查
normalize_chars(<<_, Rest/binary>>, Acc) ->
    normalize_chars(Rest, Acc).
