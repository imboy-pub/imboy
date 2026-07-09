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
        #{stream => false, vision => false, tools => false},
        imboy_llm_openai:capabilities()
    ).
