-module(plugin_mcp_tools_bridge_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%
% Phase 4 T4.1：插件 manifest mcp_tools 声明 → MCP 注册表桥接 单测。
% 覆盖：采集纯函数 mcp_tools_from/1 + 桥接 reg_plugin_tools/0（含结构不完整跳过）。
%%%

%% ===================================================================
%% imboy_plugin_registry:mcp_tools_from/1 —— 纯采集
%% ===================================================================

mcp_tools_from_empty_manifests_test() ->
    ?assertEqual([], imboy_plugin_registry:mcp_tools_from(#{})).

mcp_tools_from_manifest_without_field_test() ->
    %% 无 mcp_tools 字段的 manifest（向后兼容）→ 空
    Manifests = #{channel => #{feature_keys => [channel]}},
    ?assertEqual([], imboy_plugin_registry:mcp_tools_from(Manifests)).

mcp_tools_from_flattens_across_plugins_test() ->
    T1 = #{
        name => <<"a">>,
        module => m1,
        function => f1,
        description => <<"da">>,
        input_schema => #{}
    },
    T2 = #{
        name => <<"b">>,
        module => m2,
        function => f2,
        description => <<"db">>,
        input_schema => #{}
    },
    Manifests = #{
        p1 => #{mcp_tools => [T1]},
        p2 => #{mcp_tools => [T2]},
        p3 => #{feature_keys => []}
    },
    Got = imboy_plugin_registry:mcp_tools_from(Manifests),
    ?assertEqual(2, length(Got)),
    ?assert(lists:member(T1, Got)),
    ?assert(lists:member(T2, Got)).

%% normalize_manifest 补默认 mcp_tools=[]（向后兼容），已声明则保留
normalize_manifest_defaults_mcp_tools_test() ->
    %% 经 manifests/0 归一化的生产 manifest 必含 mcp_tools 键（默认 []）
    M = imboy_plugin_registry:manifest(channel),
    ?assertEqual([], maps:get(mcp_tools, M, undefined)).

%% ===================================================================
%% imboy_mcp_tools:reg_plugin_tools/0 —— 桥接注册
%% ===================================================================

%% 合法声明 → barrel_mcp_registry:reg(tool, ...) 被调
reg_plugin_tools_registers_valid_test_() ->
    ?WITH_MECKS(
        [
            {imboy_plugin_registry, [
                {'mcp_tool_declarations', 0, fun() ->
                    [
                        #{
                            name => <<"plugin_tool_x">>,
                            module => some_plugin_mod,
                            function => handle_x,
                            description => <<"desc x"/utf8>>,
                            input_schema => #{<<"type">> => <<"object">>}
                        }
                    ]
                end}
            ]},
            {barrel_mcp_registry, [
                {'reg', 5, fun(tool, _N, _M, _F, _Opts) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, imboy_mcp_tools:reg_plugin_tools()),
            ?assert(
                meck:called(
                    barrel_mcp_registry,
                    reg,
                    [tool, <<"plugin_tool_x">>, some_plugin_mod, handle_x, '_']
                )
            )
        end
    ).

%% 结构不完整声明 → 跳过（不注册、不崩），合法的仍注册
reg_plugin_tools_skips_malformed_test_() ->
    ?WITH_MECKS(
        [
            {imboy_plugin_registry, [
                {'mcp_tool_declarations', 0, fun() ->
                    [
                        % 缺 function/description/input_schema
                        #{name => <<"bad">>, module => m},
                        #{
                            name => <<"good">>,
                            module => gm,
                            function => gf,
                            description => <<"d">>,
                            input_schema => #{}
                        }
                    ]
                end}
            ]},
            {barrel_mcp_registry, [
                {'reg', 5, fun(tool, _N, _M, _F, _Opts) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, imboy_mcp_tools:reg_plugin_tools()),
            %% 只有 good 被注册；bad 被跳过
            ?assertEqual(1, meck:num_calls(barrel_mcp_registry, reg, '_')),
            ?assert(
                meck:called(
                    barrel_mcp_registry,
                    reg,
                    [tool, <<"good">>, gm, gf, '_']
                )
            )
        end
    ).
