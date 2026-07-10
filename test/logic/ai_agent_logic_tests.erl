-module(ai_agent_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_logic:list_assistants/1 EUnit 测试
%%% 覆盖：字段映射（存储行→卡片）、分页信封透传、空结果、keyword 透传、
%%%       repo 出错向上冒泡。可见性过滤是 repo 的 SQL 职责，此处 mock repo。
%%%===================================================================

%% 存储行（repo 返回）→ 前端卡片字段映射
maps_row_to_card_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, Page, Size) ->
                    {ok, #{
                        total => 2,
                        page => Page,
                        size => Size,
                        list => [
                            #{
                                <<"user_id">> => 101,
                                <<"nickname">> => <<"客服助手"/utf8>>,
                                <<"avatar">> => <<"http://a/1.png">>,
                                <<"sign">> => <<"7x24 在线"/utf8>>
                            }
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, Paged} = ai_agent_logic:list_assistants(#{page => 1, size => 10}),
            ?assertEqual(2, maps:get(total, Paged)),
            ?assertEqual(1, maps:get(page, Paged)),
            ?assertEqual(10, maps:get(size, Paged)),
            [Card] = maps:get(list, Paged),
            ?assertEqual(
                #{
                    <<"id">> => 101,
                    <<"name">> => <<"客服助手"/utf8>>,
                    <<"avatar">> => <<"http://a/1.png">>,
                    <<"description">> => <<"7x24 在线"/utf8>>
                },
                Card
            )
        end
    ).

%% 缺字段时卡片回退为空 binary，不崩溃
missing_fields_default_empty_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, _P, _S) ->
                    {ok, #{
                        total => 1,
                        page => 1,
                        size => 10,
                        list => [#{<<"user_id">> => 202}]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, #{list := [Card]}} = ai_agent_logic:list_assistants(#{page => 1, size => 10}),
            ?assertEqual(202, maps:get(<<"id">>, Card)),
            ?assertEqual(<<>>, maps:get(<<"name">>, Card)),
            ?assertEqual(<<>>, maps:get(<<"avatar">>, Card)),
            ?assertEqual(<<>>, maps:get(<<"description">>, Card))
        end
    ).

%% 空结果：list 为空，信封字段保留
empty_result_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, P, S) ->
                    {ok, #{total => 0, page => P, size => S, list => []}}
                end}
            ]}
        ],
        fun() ->
            {ok, Paged} = ai_agent_logic:list_assistants(#{page => 3, size => 5}),
            ?assertEqual(0, maps:get(total, Paged)),
            ?assertEqual([], maps:get(list, Paged))
        end
    ).

%% 分页与 keyword 透传到 repo
passes_pagination_and_keyword_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(Kw, Page, Size) ->
                    {ok, #{total => 0, page => Page, size => Size, list => [], kw => Kw}}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_logic:list_assistants(#{
                page => 2, size => 15, keyword => <<"翻译"/utf8>>
            }),
            ?assert(meck:called(ai_agent_repo, page_assistants, [<<"翻译"/utf8>>, 2, 15]))
        end
    ).

%% keyword 缺省为空 binary
default_keyword_empty_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, P, S) ->
                    {ok, #{total => 0, page => P, size => S, list => []}}
                end}
            ]}
        ],
        fun() ->
            {ok, _} = ai_agent_logic:list_assistants(#{page => 1, size => 10}),
            ?assert(meck:called(ai_agent_repo, page_assistants, [<<>>, 1, 10]))
        end
    ).

%% repo 出错向上冒泡
repo_error_bubbles_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, _P, _S) -> {error, db_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, db_down},
                ai_agent_logic:list_assistants(#{page => 1, size => 10})
            )
        end
    ).
