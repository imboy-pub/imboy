-module(imboy_llm_qianfan_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_llm_qianfan 模块的 EUnit 测试
%%%
%%% 目标：验证 imboy_llm behaviour 的千帆适配器
%%% 覆盖：chat/3 转发到 qianfan_api:create_chat/3、
%%%        OpenAI Messages → (Content, History) 映射、
%%%        capabilities/0、错误路径（空消息、无 result、API 崩溃）
%%%===================================================================

%% ===================================================================
%% 辅助函数
%% ===================================================================

setup_qianfan_mock() ->
    meck:new(qianfan_api, [no_link, passthrough]),
    meck:expect(qianfan_api, create_chat, 3, fun(_Uid, _Content, _History) ->
        #{<<"result">> => <<"回复"/utf8>>}
    end).

cleanup_qianfan_mock(_State) ->
    meck:unload(qianfan_api).

%% ===================================================================
%% chat/3 转发与映射
%% ===================================================================

chat_forwards_single_user_message_test_() ->
    {setup, fun setup_qianfan_mock/0, fun cleanup_qianfan_mock/1, fun(_) ->
        ?_test(begin
            Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"你好"/utf8>>}],
            Result = imboy_llm_qianfan:chat(1, Messages, #{}),
            ?assertEqual({ok, #{<<"result">> => <<"回复"/utf8>>}}, Result),
            % 末条 user content → Content，History 为空
            ?assert(meck:called(qianfan_api, create_chat, [1, <<"你好"/utf8>>, []]))
        end)
    end}.

chat_maps_messages_to_content_and_history_test_() ->
    {setup, fun setup_qianfan_mock/0, fun cleanup_qianfan_mock/1, fun(_) ->
        ?_test(begin
            M1 = #{<<"role">> => <<"user">>, <<"content">> => <<"q1">>},
            M2 = #{<<"role">> => <<"assistant">>, <<"content">> => <<"a1">>},
            M3 = #{<<"role">> => <<"user">>, <<"content">> => <<"q2">>},
            {ok, _} = imboy_llm_qianfan:chat(42, [M1, M2, M3], #{}),
            % 末条 user content → Content，其余按原顺序 → History
            ?assert(meck:called(qianfan_api, create_chat, [42, <<"q2">>, [M1, M2]]))
        end)
    end}.

chat_empty_messages_returns_error_test() ->
    ?assertEqual({error, empty_messages}, imboy_llm_qianfan:chat(1, [], #{})).

chat_resp_without_result_returns_error_test_() ->
    {setup, fun setup_qianfan_mock/0, fun cleanup_qianfan_mock/1, fun(_) ->
        ?_test(begin
            meck:expect(qianfan_api, create_chat, 3, fun(_, _, _) ->
                #{<<"error_code">> => 110, <<"error_msg">> => <<"token invalid">>}
            end),
            Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}],
            ?assertMatch(
                {error, #{<<"error_code">> := 110}},
                imboy_llm_qianfan:chat(1, Messages, #{})
            )
        end)
    end}.

chat_api_crash_returns_error_test_() ->
    {setup, fun setup_qianfan_mock/0, fun cleanup_qianfan_mock/1, fun(_) ->
        ?_test(begin
            meck:expect(qianfan_api, create_chat, 3, fun(_, _, _) ->
                erlang:error(badmatch)
            end),
            Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}],
            ?assertMatch({error, _}, imboy_llm_qianfan:chat(1, Messages, #{}))
        end)
    end}.

%% ===================================================================
%% capabilities/0
%% ===================================================================

capabilities_test() ->
    ?assertEqual(
        #{stream => false, vision => false, tools => false},
        imboy_llm_qianfan:capabilities()
    ).
