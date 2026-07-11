-module(ai_agent_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc ai_agent_logic:list_assistants/1 EUnit 测试
%%% 覆盖：字段映射（存储行→卡片，description 用 ai_agent.description 真实列）、
%%%       分页信封透传、空结果、keyword 透传、repo 出错向上冒泡。
%%% ⚠️ visibility=1 才可发现是 repo 的 SQL WHERE 职责（a.visibility=1），此处
%%%    mock repo：mock 返回「repo 已过滤后」的行，本层只验证投影与透传。
%%%    私有 visibility=0 不返回 → repo 层根本不会把它放进 list（见下方
%%%    private_agents_not_in_repo_result 契约测试）。
%%%===================================================================

%% 存储行（repo 返回）→ 前端卡片字段映射（description 取真实列）
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
                                <<"description">> => <<"7x24 智能客服"/utf8>>
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
                    <<"description">> => <<"7x24 智能客服"/utf8>>
                },
                Card
            )
        end
    ).

%% description 取 ai_agent.description 真实列，不再回落到 user.sign：
%% 即使行里带了 <<"sign">>，卡片也只认 <<"description">>。
description_uses_real_column_not_sign_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, P, S) ->
                    {ok, #{
                        total => 1,
                        page => P,
                        size => S,
                        list => [
                            #{
                                <<"user_id">> => 303,
                                <<"nickname">> => <<"翻译助手"/utf8>>,
                                <<"avatar">> => <<"http://a/3.png">>,
                                %% 干扰字段：sign 存在但必须被忽略
                                <<"sign">> => <<"个性签名不该出现"/utf8>>,
                                <<"description">> => <<"多语种实时翻译"/utf8>>
                            }
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, #{list := [Card]}} = ai_agent_logic:list_assistants(#{page => 1, size => 10}),
            ?assertEqual(<<"多语种实时翻译"/utf8>>, maps:get(<<"description">>, Card)),
            ?assertNot(maps:is_key(<<"sign">>, Card))
        end
    ).

%% 契约：私有 agent（visibility=0）由 repo 的 SQL 过滤掉，不会进入 list；
%% 只有 visibility=1 的行返回给上层。此处 mock 已过滤结果（仅公开行）。
only_visible_agents_returned_test_() ->
    ?WITH_MECKS(
        [
            {ai_agent_repo, [
                {'page_assistants', 3, fun(_Kw, P, S) ->
                    %% repo 已按 a.visibility=1 过滤：私有(0)行不在此列表
                    {ok, #{
                        total => 1,
                        page => P,
                        size => S,
                        list => [
                            #{
                                <<"user_id">> => 404,
                                <<"nickname">> => <<"公开助手"/utf8>>,
                                <<"avatar">> => <<>>,
                                <<"description">> => <<"公开可发现"/utf8>>
                            }
                        ]
                    }}
                end}
            ]}
        ],
        fun() ->
            {ok, #{list := List}} = ai_agent_logic:list_assistants(#{page => 1, size => 10}),
            ?assertEqual(1, length(List)),
            [#{<<"id">> := Id}] = List,
            ?assertEqual(404, Id)
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
