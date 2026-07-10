-module(imboy_llm_openai_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_llm_openai 模块的 EUnit 测试
%%%
%%% 目标：验证 OpenAI 兼容适配器（DeepSeek/Qwen/vLLM/Ollama 通用）
%%% 覆盖：请求构造（URL/Authorization/body）、choices 响应解析、
%%%        base_url 尾斜杠、错误路径（无 choices、HTTP 错误、缺配置）
%%%===================================================================

-define(OPTS, #{
    base_url => <<"https://api.deepseek.com/v1">>,
    api_key => <<"sk-test">>,
    model => <<"deepseek-chat">>
}).

%% ===================================================================
%% 辅助函数
%% ===================================================================

setup_req_mock() ->
    meck:new(elib_req, [no_link, passthrough]),
    meck:expect(elib_req, post, 3, fun(_URL, _Data, _Headers) ->
        {ok, #{
            <<"choices">> => [
                #{<<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"回复"/utf8>>}}
            ]
        }}
    end).

cleanup_req_mock(_State) ->
    meck:unload(elib_req).

user_msg(Text) ->
    [#{<<"role">> => <<"user">>, <<"content">> => Text}].

%% ===================================================================
%% 请求构造 + 响应解析
%% ===================================================================

chat_builds_request_and_parses_choices_test_() ->
    {setup, fun setup_req_mock/0, fun cleanup_req_mock/1, fun(_) ->
        ?_test(begin
            Messages = user_msg(<<"你好"/utf8>>),
            Result = imboy_llm_openai:chat(1, Messages, ?OPTS),
            ?assertEqual({ok, #{<<"result">> => <<"回复"/utf8>>}}, Result),
            ?assert(
                meck:called(elib_req, post, [
                    <<"https://api.deepseek.com/v1/chat/completions">>,
                    #{<<"model">> => <<"deepseek-chat">>, <<"messages">> => Messages},
                    [
                        {"Content-Type", "application/json"},
                        {"Authorization", "Bearer sk-test"}
                    ]
                ])
            )
        end)
    end}.

chat_base_url_trailing_slash_test_() ->
    {setup, fun setup_req_mock/0, fun cleanup_req_mock/1, fun(_) ->
        ?_test(begin
            Opts = ?OPTS#{base_url => <<"http://127.0.0.1:11434/v1/">>},
            {ok, _} = imboy_llm_openai:chat(1, user_msg(<<"hi">>), Opts),
            History = meck:history(elib_req),
            [{_, {elib_req, post, [URL | _]}, _}] = History,
            ?assertEqual(<<"http://127.0.0.1:11434/v1/chat/completions">>, URL)
        end)
    end}.

%% ===================================================================
%% 错误路径
%% ===================================================================

chat_empty_messages_returns_error_test() ->
    ?assertEqual({error, empty_messages}, imboy_llm_openai:chat(1, [], ?OPTS)).

chat_resp_without_choices_returns_error_test_() ->
    {setup, fun setup_req_mock/0, fun cleanup_req_mock/1, fun(_) ->
        ?_test(begin
            meck:expect(elib_req, post, 3, fun(_, _, _) ->
                {ok, #{<<"error">> => #{<<"message">> => <<"model not found">>}}}
            end),
            ?assertMatch(
                {error, #{<<"error">> := _}},
                imboy_llm_openai:chat(1, user_msg(<<"hi">>), ?OPTS)
            )
        end)
    end}.

chat_http_error_returns_error_test_() ->
    {setup, fun setup_req_mock/0, fun cleanup_req_mock/1, fun(_) ->
        ?_test(begin
            meck:expect(elib_req, post, 3, fun(_, _, _) -> {error, timeout} end),
            ?assertEqual({error, timeout}, imboy_llm_openai:chat(1, user_msg(<<"hi">>), ?OPTS)),
            meck:expect(elib_req, post, 3, fun(_, _, _) ->
                {error, 401, #{<<"error">> => <<"invalid api key">>}}
            end),
            ?assertMatch({error, {401, _}}, imboy_llm_openai:chat(1, user_msg(<<"hi">>), ?OPTS))
        end)
    end}.

chat_missing_opts_returns_error_test() ->
    % 缺 api_key/base_url 等配置错误不应崩溃，应返回 {error, _}
    ?assertMatch(
        {error, _},
        imboy_llm_openai:chat(1, user_msg(<<"hi">>), #{})
    ).

%% ===================================================================
%% capabilities/0
%% ===================================================================

capabilities_test() ->
    ?assertEqual(
        #{stream => true, vision => false, tools => false},
        imboy_llm_openai:capabilities()
    ).

%% ===================================================================
%% chat_stream 边界
%% ===================================================================

chat_stream_empty_messages_returns_error_test() ->
    ?assertEqual(
        {error, empty_messages},
        imboy_llm_openai:chat_stream(1, [], ?OPTS, fun(_) -> ok end)
    ).

%% ===================================================================
%% SSE 解析纯函数（parse_sse/1）
%% ===================================================================

%% 单个完整 event → 抽出一个 delta，buffer 清空
parse_sse_single_event_test() ->
    Buf = <<"data: {\"choices\":[{\"delta\":{\"content\":\"你好\"}}]}\n\n"/utf8>>,
    ?assertEqual({[<<"你好"/utf8>>], <<>>}, imboy_llm_openai:parse_sse(Buf)).

%% 多个 event 连续 → 按序抽出多个 delta
parse_sse_multiple_events_test() ->
    Buf = <<
        "data: {\"choices\":[{\"delta\":{\"content\":\"你\"}}]}\n\n"/utf8,
        "data: {\"choices\":[{\"delta\":{\"content\":\"好\"}}]}\n\n"/utf8
    >>,
    ?assertEqual({[<<"你"/utf8>>, <<"好"/utf8>>], <<>>}, imboy_llm_openai:parse_sse(Buf)).

%% 不完整尾部（无 \n\n）留在 RestBuffer 等待下一片
parse_sse_partial_buffer_test() ->
    Buf = <<
        "data: {\"choices\":[{\"delta\":{\"content\":\"A\"}}]}\n\n",
        "data: {\"choices\":[{\"delta\":{\"cont"
    >>,
    {Deltas, Rest} = imboy_llm_openai:parse_sse(Buf),
    ?assertEqual([<<"A">>], Deltas),
    ?assertEqual(<<"data: {\"choices\":[{\"delta\":{\"cont">>, Rest).

%% 跨片续拼：把上轮 Rest 与新片拼接后能解析出被截断的 delta
parse_sse_resume_across_chunks_test() ->
    Buf1 = <<"data: {\"choices\":[{\"delta\":{\"cont">>,
    {D1, Rest1} = imboy_llm_openai:parse_sse(Buf1),
    ?assertEqual([], D1),
    Buf2 = <<Rest1/binary, "ent\":\"X\"}}]}\n\n">>,
    ?assertEqual({[<<"X">>], <<>>}, imboy_llm_openai:parse_sse(Buf2)).

%% [DONE] 终止：其后内容丢弃，buffer 清空
parse_sse_done_terminates_test() ->
    Buf = <<
        "data: {\"choices\":[{\"delta\":{\"content\":\"末\"}}]}\n\n"/utf8,
        "data: [DONE]\n\n"
    >>,
    ?assertEqual({[<<"末"/utf8>>], <<>>}, imboy_llm_openai:parse_sse(Buf)).

%% role-only 首帧（delta 无 content）跳过，不产生空 delta
parse_sse_skips_role_only_delta_test() ->
    Buf = <<
        "data: {\"choices\":[{\"delta\":{\"role\":\"assistant\"}}]}\n\n",
        "data: {\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}\n\n"
    >>,
    ?assertEqual({[<<"Hi">>], <<>>}, imboy_llm_openai:parse_sse(Buf)).

%% 空 buffer → 无 delta，原样留存
parse_sse_empty_test() ->
    ?assertEqual({[], <<>>}, imboy_llm_openai:parse_sse(<<>>)).

%% ===================================================================
%% stream_loop 集成：进程消息注入模拟 httpc streaming
%% ===================================================================

%% happy path：stream_start → stream(data) → [DONE] → stream_end，定稿全文正确
stream_loop_happy_path_test() ->
    ReqId = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    Self = self(),
    Collect = fun(D) ->
        Self ! {delta, D},
        ok
    end,
    self() ! {http, {ReqId, stream_start, []}},
    self() !
        {http,
            {ReqId, stream, <<"data: {\"choices\":[{\"delta\":{\"content\":\"你好\"}}]}\n\n"/utf8>>}},
    self() ! {http, {ReqId, stream, <<"data: [DONE]\n\n">>}},
    self() ! {http, {ReqId, stream_end, []}},
    Result = imboy_llm_openai:stream_loop(ReqId, <<>>, <<>>, Collect, Deadline),
    ?assertEqual({ok, #{<<"result">> => <<"你好"/utf8>>}}, Result),
    receive
        {delta, D} -> ?assertEqual(<<"你好"/utf8>>, D)
    after 0 -> ?assert(false)
    end.

%% error 分支：httpc 报错原样返回
stream_loop_error_test() ->
    ReqId = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    self() ! {http, {ReqId, {error, socket_closed_remotely}}},
    ?assertEqual(
        {error, socket_closed_remotely},
        imboy_llm_openai:stream_loop(ReqId, <<>>, <<>>, fun(_) -> ok end, Deadline)
    ).

%% 非 200：httpc 未进入流式，返回整体响应状态码
stream_loop_non_200_test() ->
    ReqId = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    self() ! {http, {ReqId, {{"HTTP/1.1", 401, "Unauthorized"}, [], <<"invalid api key">>}}},
    ?assertMatch(
        {error, {401, _}},
        imboy_llm_openai:stream_loop(ReqId, <<>>, <<>>, fun(_) -> ok end, Deadline)
    ).

%% 总时长上限：Deadline 已过期立即中止（cancel_request 需 inets 启动）
stream_loop_total_timeout_test() ->
    _ = application:ensure_started(inets),
    ReqId = make_ref(),
    Deadline = erlang:monotonic_time(millisecond) - 1,
    ?assertEqual(
        {error, stream_total_timeout},
        imboy_llm_openai:stream_loop(ReqId, <<>>, <<>>, fun(_) -> ok end, Deadline)
    ).
