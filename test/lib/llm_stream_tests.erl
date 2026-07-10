-module(llm_stream_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc llm_stream EUnit 测试（Phase 2 T2.2）
%%% 覆盖：字节阈值节流、flush_tail 残留、full_text 累积、index 递增、
%%%       帧格式（stream_delta + payload）、stream_capable 判定。
%%% 进程字典状态跨测试用唯一 stream_id 隔离。
%%%===================================================================

%% 每个测试用唯一 stream_id，避免进程字典跨测试污染
ctx(Sid) ->
    #{
        stream_id => Sid,
        target_uid => 7,
        from => <<"42">>,
        to => <<"7">>,
        type => <<"C2C">>
    }.

%% ===================================================================
%% 节流 + 帧格式
%% ===================================================================

%% 未达字节阈值不 publish，达阈值 flush 一帧且帧字段正确
push_throttles_until_threshold_test_() ->
    ?WITH_MECKS(
        [{imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}],
        fun() ->
            Ctx = ctx(<<"throttle">>),
            %% 3 字节 < 12：缓冲不推
            llm_stream:push(Ctx, <<"abc">>),
            ?assertEqual(0, length(meck:history(imboy_syn))),
            %% +9 字节 = 12 >= 12：flush 一帧
            llm_stream:push(Ctx, <<"defghijkl">>),
            History = meck:history(imboy_syn),
            ?assertEqual(1, length(History)),
            [{_, {_, publish, [7, Json]}, _}] = History,
            D = jsone:decode(Json),
            ?assertEqual(<<"stream_delta">>, maps:get(<<"msg_type">>, D)),
            ?assertEqual(<<"throttle">>, maps:get(<<"id">>, D)),
            ?assertEqual(<<"C2C">>, maps:get(<<"type">>, D)),
            ?assertEqual(<<"42">>, maps:get(<<"from">>, D)),
            ?assertEqual(<<"7">>, maps:get(<<"to">>, D)),
            P = maps:get(<<"payload">>, D),
            ?assertEqual(<<"abcdefghijkl">>, maps:get(<<"delta">>, P)),
            ?assertEqual(0, maps:get(<<"index">>, P)),
            ?assertEqual(<<"throttle">>, maps:get(<<"stream_id">>, P)),
            ?assertEqual(false, maps:get(<<"is_end">>, P))
        end
    ).

%% flush_tail 推残留缓冲作收尾帧，带 is_end=true 结束信号
flush_tail_emits_remainder_with_is_end_test_() ->
    ?WITH_MECKS(
        [{imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}],
        fun() ->
            Ctx = ctx(<<"tail">>),
            llm_stream:push(Ctx, <<"ab">>),
            ?assertEqual(0, length(meck:history(imboy_syn))),
            llm_stream:flush_tail(Ctx),
            [{_, {_, publish, [7, Json]}, _}] = meck:history(imboy_syn),
            P = maps:get(<<"payload">>, jsone:decode(Json)),
            ?assertEqual(<<"ab">>, maps:get(<<"delta">>, P)),
            ?assertEqual(true, maps:get(<<"is_end">>, P))
        end
    ).

%% reset 清空 buf/full/seq：避免同进程重试跨尝试串味
reset_clears_state_test_() ->
    ?WITH_MECKS(
        [{imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}],
        fun() ->
            Ctx = ctx(<<"reset">>),
            %% 首轮：flush 一帧（seq0）+ 缓冲一段
            llm_stream:push(Ctx, <<"aaaaaaaaaaaa">>),
            llm_stream:push(Ctx, <<"部分"/utf8>>),
            ?assertEqual(<<"aaaaaaaaaaaa部分"/utf8>>, llm_stream:full_text(Ctx)),
            %% reset：full 清空
            llm_stream:reset(Ctx),
            ?assertEqual(<<>>, llm_stream:full_text(Ctx)),
            %% reset 后再 flush 帧：seq 归零（index=0）
            llm_stream:push(Ctx, <<"bbbbbbbbbbbb">>),
            Frames = [
                jsone:decode(J)
             || {_, {_, publish, [_, J]}, _} <- meck:history(imboy_syn)
            ],
            LastP = maps:get(<<"payload">>, lists:last(Frames)),
            ?assertEqual(0, maps:get(<<"index">>, LastP))
        end
    ).

%% 多帧 index 递增 0,1,2...
index_increments_across_frames_test_() ->
    ?WITH_MECKS(
        [{imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}],
        fun() ->
            Ctx = ctx(<<"idx">>),
            llm_stream:push(Ctx, <<"aaaaaaaaaaaa">>),
            llm_stream:push(Ctx, <<"bbbbbbbbbbbb">>),
            llm_stream:push(Ctx, <<"cccccccccccc">>),
            Indexes = [
                maps:get(<<"index">>, maps:get(<<"payload">>, jsone:decode(J)))
             || {_, {_, publish, [_, J]}, _} <- meck:history(imboy_syn)
            ],
            ?assertEqual([0, 1, 2], Indexes)
        end
    ).

%% ===================================================================
%% full_text 累积（不受节流影响）
%% ===================================================================

full_text_accumulates_all_deltas_test_() ->
    ?WITH_MECKS(
        [{imboy_syn, [{'publish', 2, fun(_, _) -> {ok, 1} end}]}],
        fun() ->
            Ctx = ctx(<<"full">>),
            llm_stream:push(Ctx, <<"你好"/utf8>>),
            llm_stream:push(Ctx, <<"世界"/utf8>>),
            ?assertEqual(<<"你好世界"/utf8>>, llm_stream:full_text(Ctx))
        end
    ).

%% ===================================================================
%% stream_capable 判定
%% ===================================================================

%% 默认（llm_stream_enabled 未设）→ false
stream_capable_disabled_by_default_test() ->
    application:unset_env(imboy, llm_stream_enabled),
    ?assertEqual(false, llm_stream:stream_capable(imboy_llm_openai)).

%% 开关开 + provider 支持流式 → true
stream_capable_enabled_openai_test() ->
    application:set_env(imboy, llm_stream_enabled, true),
    try
        ?assertEqual(true, llm_stream:stream_capable(imboy_llm_openai))
    after
        application:unset_env(imboy, llm_stream_enabled)
    end.

%% 开关开但 provider 无 chat_stream/4（qianfan）→ false
stream_capable_provider_without_stream_test() ->
    application:set_env(imboy, llm_stream_enabled, true),
    try
        ?assertEqual(false, llm_stream:stream_capable(imboy_llm_qianfan))
    after
        application:unset_env(imboy, llm_stream_enabled)
    end.
