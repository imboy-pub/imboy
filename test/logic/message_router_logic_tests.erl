-module(message_router_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% message_router_logic 模块的 EUnit 测试
%%%
%%% 目标：验证消息路由功能
%%% 覆盖：普通消息路由、Action 消息路由、错误处理
%%%===================================================================

%% ===================================================================
%% route/5 普通消息测试
%% ===================================================================

route_c2c_message_success_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello"/utf8>>}},
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2g_message_success_test_() ->
    ?WITH_MECK(msg_c2g_logic, [
        {'c2g', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_456">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello group"/utf8>>}},
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2s_message_success_test_() ->
    ?WITH_MECK(msg_c2s_logic, [
        {'c2s', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_789">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => #{<<"status">> => <<"typing">>}},
        Type = <<"C2S">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_s2c_message_success_test_() ->
    ?WITH_MECK(msg_s2c_logic, [
        {'s2c', 4, fun(_Action, _MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_101">>,
        CurrentUid = 1,
        Data = #{<<"action">> => <<"pull_offline_msg">>, <<"payload">> => #{<<"count">> => 10}},
        Type = <<"S2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_webrtc_message_success_test_() ->
    ?WITH_MECK(elib_hashids, [
        {'decode', 1, fun(<<"encoded_2">>) -> 2 end}
    ], fun() ->
        ?WITH_MECK(webrtc_ws_logic, [
            {'event', 4, fun(_FromUid, _ToUid, _MsgId, _OriginalMsg) -> ok end}
        ], fun() ->
            MsgId = <<"msg_webrtc">>,
            CurrentUid = 1,
            Data = #{<<"to">> => <<"encoded_2">>},
            Type = <<"webrtc_offer">>,
            OriginalMsg = <<"{\"sdp\":\"...\"}">>,

            Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
            ?assertEqual(ok, Result)
        end)
    end).

route_unknown_message_type_returns_ok_test_() ->
    MsgId = <<"msg_unknown">>,
    CurrentUid = 1,
    Data = #{<<"payload">> => <<"{}">>},
    Type = <<"UNKNOWN_TYPE">>,
    OriginalMsg = <<"{}">>,

    Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
    ?assertEqual(ok, Result).

%% ===================================================================
%% route/5 Action 消息测试
%% ===================================================================

route_c2c_revoke_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_revoke', 3, fun(_MsgId, _CurrentUid, _Data) -> {reply, #{<<"type">> => <<"S2C">>}} end}
    ], fun() ->
        MsgId = <<"msg_revoke">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_revoke">>,
            <<"to">> => <<"encoded_2">>
        }, #{<<"payload">> => #{<<"old_msg_id">> => <<"old_123">>}}),
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

route_c2c_revoke_ack_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_revoke_ack', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_revoke_ack">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_revoke_ack">>
        }, #{<<"payload">> => #{<<"msg_id">> => <<"old_123">>}}),
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2c_edit_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_edit', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_edit">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_edit">>,
            <<"to">> => <<"encoded_2">>
        }, #{<<"payload">> => #{<<"content">> => <<"updated"/utf8>>}}),
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2c_edit_ack_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_edit_ack', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_edit_ack">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_edit_ack">>
        }, #{<<"payload">> => #{<<"msg_id">> => <<"old_123">>}}),
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2g_revoke_action_test_() ->
    ?WITH_MECK(msg_c2g_logic, [
        {'c2g_revoke', 3, fun(_MsgId, _CurrentUid, _Data) -> {reply, #{<<"type">> => <<"S2C">>}} end}
    ], fun() ->
        MsgId = <<"msg_revoke">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_revoke">>,
            <<"to">> => <<"encoded_group">>
        }, #{<<"payload">> => #{<<"old_msg_id">> => <<"old_456">>}}),
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

route_c2g_edit_action_test_() ->
    ?WITH_MECK(msg_c2g_logic, [
        {'c2g_edit', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_edit">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_edit">>,
            <<"to">> => <<"encoded_group">>
        }, #{<<"payload">> => #{<<"content">> => <<"updated group msg"/utf8>>}}),
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 错误处理测试
%% ===================================================================

route_c2c_unknown_action_returns_error_test_() ->
    ?WITH_MECK(message_ds, [
        {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
            #{<<"type">> => <<"S2C">>, <<"code">> => <<"unknown_action">>}
        end}
    ], fun() ->
        MsgId = <<"msg_unknown">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"unknown_action">>
        }, #{<<"payload">> => #{<<"data">> => <<"test">>}}),
        Type = <<"C2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"unknown_action">>}}, Result)
    end).

route_c2g_unknown_action_returns_error_test_() ->
    ?WITH_MECK(message_ds, [
        {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
            #{<<"type">> => <<"S2C">>, <<"code">> => <<"unknown_action">>}
        end}
    ], fun() ->
        MsgId = <<"msg_unknown">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"unknown_action">>
        }, #{<<"payload">> => #{<<"data">> => <<"test">>}}),
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"unknown_action">>}}, Result)
    end).

route_unsupported_action_type_returns_error_test_() ->
    ?WITH_MECK(message_ds, [
        {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
            #{<<"type">> => <<"S2C">>, <<"code">> => <<"invalid_message_type">>}
        end}
    ], fun() ->
        MsgId = <<"msg_unsupported">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_revoke">>
        }, #{<<"payload">> => #{<<"data">> => <<"test">>}}),
        Type = <<"C2S">>,  % C2S 不支持 action
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"invalid_message_type">>}}, Result)
    end).

%% ===================================================================
%% 大小写测试
%% ===================================================================

route_with_uppercase_type_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_upper">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => <<"{}">>},
        Type = <<"C2C">>,  % 大写
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_with_uppercase_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_revoke', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_action_upper">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"MESSAGE_REVOKE">>,
            <<"to">> => <<"encoded_2">>
        }, #{<<"payload">> => #{<<"old_msg_id">> => <<"old_123">>}}),
        Type = <<"c2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

route_with_missing_action_field_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        % 没有 action 字段的消息应作为普通消息处理
        MsgId = <<"msg_no_action">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => <<"{}">>},
        Type = <<"c2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_with_null_action_field_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        % action 为 null 的消息应作为普通消息处理
        MsgId = <<"msg_null_action">>,
        CurrentUid = 1,
        Data = #{<<"action">> => null, <<"payload">> => <<"{}">>},
        Type = <<"c2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_with_empty_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        % action 为空字符串的消息应作为普通消息处理
        MsgId = <<"msg_empty_action">>,
        CurrentUid = 1,
        Data = #{<<"action">> => <<>>, <<"payload">> => <<"{}">>},
        Type = <<"c2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% C2G 其他 Action 测试
%% ===================================================================

route_c2g_revoke_ack_action_test_() ->
    ?WITH_MECK(msg_c2g_logic, [
        {'c2g_revoke_ack', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_revoke_ack">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_revoke_ack">>
        }, #{<<"payload">> => #{<<"msg_id">> => <<"old_456">>}}),
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_c2g_edit_ack_action_test_() ->
    ?WITH_MECK(msg_c2g_logic, [
        {'c2g_edit_ack', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_edit_ack">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"message_edit_ack">>
        }, #{<<"payload">> => #{<<"msg_id">> => <<"old_789">>}}),
        Type = <<"C2G">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% WebRTC 各种事件类型测试
%% ===================================================================

route_webrtc_answer_event_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_2">>) -> 2 end}
        ]},
        {webrtc_ws_logic, [
            {'event', 4, fun(_FromUid, _ToUid, _MsgId, _OriginalMsg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_webrtc_answer">>,
        CurrentUid = 1,
        Data = #{<<"to">> => <<"encoded_2">>},
        Type = <<"webrtc_answer">>,
        OriginalMsg = <<"{\"sdp\":\"answer_sdp\"}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_webrtc_ice_candidate_event_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_2">>) -> 2 end}
        ]},
        {webrtc_ws_logic, [
            {'event', 4, fun(_FromUid, _ToUid, _MsgId, _OriginalMsg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_webrtc_ice">>,
        CurrentUid = 1,
        Data = #{<<"to">> => <<"encoded_2">>},
        Type = <<"webrtc_ice_candidate">>,
        OriginalMsg = <<"{\"candidate\":\"...\"}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_webrtc_hangup_event_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_2">>) -> 2 end}
        ]},
        {webrtc_ws_logic, [
            {'event', 4, fun(_FromUid, _ToUid, _MsgId, _OriginalMsg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_webrtc_hangup">>,
        CurrentUid = 1,
        Data = #{<<"to">> => <<"encoded_2">>},
        Type = <<"webrtc_hangup">>,
        OriginalMsg = <<"{\"reason\":\"user_hangup\"}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% S2C 不同 action 测试
%% ===================================================================

route_s2c_with_pull_offline_msg_action_test_() ->
    ?WITH_MECK(msg_s2c_logic, [
        {'s2c', 4, fun(_Action, _MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_pull_offline">>,
        CurrentUid = 1,
        Data = #{
            <<"action">> => <<"pull_offline_msg">>,
            <<"payload">> => #{<<"count">> => 50}
        },
        Type = <<"S2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_s2c_with_delete_msg_action_test_() ->
    ?WITH_MECK(msg_s2c_logic, [
        {'s2c', 4, fun(_Action, _MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_delete">>,
        CurrentUid = 1,
        Data = #{
            <<"action">> => <<"delete_msg">>,
            <<"payload">> => #{<<"msg_ids">> => [<<"msg1">>, <<"msg2">>]}
        },
        Type = <<"S2C">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 大小写混合测试
%% ===================================================================

route_with_mixed_case_type_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_mixed">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => <<"{}">>},
        Type = <<"C2c">>,  % 混合大小写
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_with_mixed_case_action_test_() ->
    ?WITH_MECK(msg_c2c_logic, [
        {'c2c_edit', 3, fun(_MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_edit_mixed">>,
        CurrentUid = 1,
        Data = maps:merge(#{
            <<"action">> => <<"Message_Edit">>,  % 混合大小写
            <<"to">> => <<"encoded_2">>
        }, #{<<"payload">> => #{<<"content">> => <<"updated"/utf8>>}}),
        Type = <<"c2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 其他边界条件测试
%% ===================================================================

route_with_lowercase_webrtc_type_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_2">>) -> 2 end}
        ]},
        {webrtc_ws_logic, [
            {'event', 4, fun(_FromUid, _ToUid, _MsgId, _OriginalMsg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_webrtc_lower">>,
        CurrentUid = 1,
        Data = #{<<"to">> => <<"encoded_2">>},
        Type = <<"webrtc_offer">>,  % 小写
        OriginalMsg = <<"{\"sdp\":\"...\"}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

route_s2c_without_action_field_test_() ->
    ?WITH_MECK(msg_s2c_logic, [
        {'s2c', 4, fun(_Action, _MsgId, _CurrentUid, _Data) -> ok end}
    ], fun() ->
        % S2C 消息没有 action 字段
        MsgId = <<"msg_s2c_no_action">>,
        CurrentUid = 1,
        Data = #{<<"payload">> => #{<<"data">> => <<"test"/utf8>>}},
        Type = <<"s2c">>,
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 不支持的类型带 Action 测试
%% ===================================================================

route_s2s_with_action_returns_error_test_() ->
    ?WITH_MECK(message_ds, [
        {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
            #{<<"type">> => <<"S2C">>, <<"code">> => <<"invalid_message_type">>}
        end}
    ], fun() ->
        MsgId = <<"msg_s2s_unsupported">>,
        CurrentUid = 1,
        Data = #{
            <<"action">> => <<"message_revoke">>,
            <<"payload">> => #{<<"data">> => <<"test">>}
        },
        Type = <<"S2S">>,  % S2S 不支持 action
        OriginalMsg = <<"{}">>,

        Result = message_router_logic:route(MsgId, CurrentUid, Data, Type, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"invalid_message_type">>}}, Result)
    end).
