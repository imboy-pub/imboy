-module(agent_card_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% Phase 4 T4.1：agent-card 发现端点 build_card/0 单测（纯构建，不起 cowboy）。
%%%

card_has_identity_and_mcp_endpoint_test_() ->
    ?WITH_MECK(
        imboy_plugin_registry,
        [
            {'mcp_tool_declarations', 0, fun() -> [] end}
        ],
        fun() ->
            Card = agent_card_handler:build_card(),
            ?assertEqual(<<"imboy">>, maps:get(<<"name">>, Card)),
            ?assertEqual(<<"mcp">>, maps:get(<<"protocol">>, Card)),
            ?assertEqual(<<"/api/v1/mcp">>, maps:get(<<"mcp_endpoint">>, Card)),
            ?assertEqual([], maps:get(<<"plugin_tools">>, Card))
        end
    ).

card_reflects_plugin_declared_tools_test_() ->
    ?WITH_MECK(
        imboy_plugin_registry,
        [
            {'mcp_tool_declarations', 0, fun() ->
                [
                    #{
                        name => <<"t1">>,
                        module => m,
                        function => f,
                        description => <<"desc1"/utf8>>,
                        input_schema => #{}
                    },
                    #{
                        name => <<"t2">>,
                        module => m,
                        function => f,
                        description => <<"desc2"/utf8>>,
                        input_schema => #{}
                    }
                ]
            end}
        ],
        fun() ->
            Card = agent_card_handler:build_card(),
            Tools = maps:get(<<"plugin_tools">>, Card),
            ?assertEqual(2, length(Tools)),
            %% 只暴露 name/description，不泄露 module/function/schema
            ?assertEqual([<<"description">>, <<"name">>], lists:sort(maps:keys(hd(Tools)))),
            ?assert(
                lists:member(
                    #{<<"name">> => <<"t1">>, <<"description">> => <<"desc1"/utf8>>}, Tools
                )
            )
        end
    ).

etag_stable_and_changes_with_content_test_() ->
    ?WITH_MECK(
        imboy_plugin_registry,
        [
            {'mcp_tool_declarations', 0, fun() -> [] end}
        ],
        fun() ->
            E1 = agent_card_handler:compute_etag(#{<<"a">> => 1}),
            E2 = agent_card_handler:compute_etag(#{<<"a">> => 1}),
            E3 = agent_card_handler:compute_etag(#{<<"a">> => 2}),
            ?assertEqual(E1, E2),
            ?assertNotEqual(E1, E3),
            %% 带引号的强 ETag 格式
            ?assertMatch(<<"\"", _/binary>>, E1)
        end
    ).
