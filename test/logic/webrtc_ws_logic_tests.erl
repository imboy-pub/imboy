-module(webrtc_ws_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% webrtc_ws_logic 模块的 EUnit 测试
%%%
%%% 目标：验证 WebRTC WebSocket 信令处理功能
%%% 覆盖：好友关系检查、黑名单检查、消息投递、错误处理
%%%
%%% WebRTC 信令流程：
%%% 1. 客户端发送 WebRTC 信令消息（offer/answer/ice_candidate）
%%% 2. 服务器检查好友关系
%%% 3. 服务器检查黑名单状态
%%% 4. 如果是好友且不在黑名单，转发消息
%%% 5. 如果不是好友或在黑名单，返回错误响应
%%%===================================================================

%% ===================================================================
%% event/4 测试 - WebRTC 信令事件处理
%% ===================================================================

event_sends_webrtc_signal_when_users_are_friends_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,
        Msg = <<"{\"type\":\"offer\",\"sdp\":\"...\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertEqual(ok, Result)
    end).

event_returns_ok_when_friend_and_not_in_denylist_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(ToUid, MsgId, Msg, MsLi) ->
                % 验证消息投递参数
                ?assertEqual(456, ToUid),
                ?assertEqual(<<"msg_webrtc_123">>, MsgId),
                ?assertEqual([0], MsLi),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,
        Msg = <<"{\"type\":\"offer\",\"sdp\":\"...\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 黑名单检查测试
%% ===================================================================

event_returns_in_denylist_error_when_user_blocked_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 1 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,
        Msg = <<"{\"type\":\"offer\",\"sdp\":\"...\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,
        ?assert(is_binary(ReplyMsg)),
        ?assert(string:find(ReplyMsg, "in_denylist") =/= nomatch)
    end).

event_returns_in_denylist_for_multiple_denied_users_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 5 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, <<"{}">>),
        ?assertMatch({reply, _}, Result)
    end).

%% ===================================================================
%% 非好友关系检查测试
%% ===================================================================

event_returns_not_a_friend_error_when_not_friends_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> false end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,
        Msg = <<"{\"type\":\"answer\",\"sdp\":\"...\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,
        ?assert(is_binary(ReplyMsg)),
        ?assert(string:find(ReplyMsg, "not_a_friend") =/= nomatch)
    end).

event_prioritizes_denylist_over_friend_status_test_() ->
    % 优先级：黑名单 > 好友关系
    % 即使是好友，如果在黑名单中也返回 in_denylist
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 1 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_webrtc_123">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, <<"{}">>),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,
        % 应该是 in_denylist 错误，而不是 not_a_friend
        ?assert(string:find(ReplyMsg, "in_denylist") =/= nomatch),
        ?assert(string:find(ReplyMsg, "not_a_friend") =:= nomatch)
    end).

%% ===================================================================
%% SDP 交换测试
%% ===================================================================

event_forwards_sdp_offer_to_friend_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 SDP offer 消息格式
                ?assert(string:find(Msg, "offer") =/= nomatch),
                ?assert(string:find(Msg, "sdp") =/= nomatch),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"sdp_offer_123">>,
        SdpOffer = <<"{\"type\":\"offer\",\"sdp\":\"v=0\\r\\no=- 123 2 IN IP4 127.0.0.1\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, SdpOffer),
        ?assertEqual(ok, Result)
    end).

event_forwards_sdp_answer_to_friend_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 SDP answer 消息格式
                ?assert(string:find(Msg, "answer") =/= nomatch),
                ?assert(string:find(Msg, "sdp") =/= nomatch),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"sdp_answer_456">>,
        SdpAnswer = <<"{\"type\":\"answer\",\"sdp\":\"v=0\\r\\no=- 456 2 IN IP4 127.0.0.1\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, SdpAnswer),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% ICE 候选测试
%% ===================================================================

event_forwards_ice_candidate_to_friend_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 ICE candidate 消息格式
                ?assert(string:find(Msg, "ice_candidate") =/= nomatch orelse
                        string:find(Msg, "candidate") =/= nomatch),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"ice_candidate_789">>,
        IceCandidate = <<"{\"type\":\"ice_candidate\",\"candidate\":\"candidate:1 1 UDP 2130706431 192.168.1.1 54321 typ host\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, IceCandidate),
        ?assertEqual(ok, Result)
    end).

event_forwards_multiple_ice_candidates_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,

        % 发送多个 ICE 候选
        Ice1 = <<"{\"type\":\"ice_candidate\",\"candidate\":\"...\"}">>,
        Ice2 = <<"{\"type\":\"ice_candidate\",\"candidate\":\"...\"}">>,

        Result1 = webrtc_ws_logic:event(CurrentUid, ToUid, <<"ice1">>, Ice1),
        Result2 = webrtc_ws_logic:event(CurrentUid, ToUid, <<"ice2">>, Ice2),

        ?assertEqual(ok, Result1),
        ?assertEqual(ok, Result2)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

event_handles_empty_message_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 即使是空消息也应该发送
                ?assertEqual(<<>>, Msg),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"empty_msg">>,
        EmptyMsg = <<>>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, EmptyMsg),
        ?assertEqual(ok, Result)
    end).

event_handles_binary_message_with_utf8_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 UTF-8 编码的中文字符串
                ?assert(is_binary(Msg)),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"utf8_msg">>,
        Utf8Msg = <<"{\"type\":\"offer\",\"comment\":\"测试消息\"}/utf8">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Utf8Msg),
        ?assertEqual(ok, Result)
    end).

event_handles_self_communication_test_() ->
    % 测试自己给自己发消息的场景
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(ToUid, CurrentUid) when ToUid =:= CurrentUid -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(ToUid, _MsgId, _Msg, _MsLi) ->
                % 验证目标用户是当前用户
                ?assertEqual(123, ToUid),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 123,  % 自己
        MsgId = <<"self_msg">>,
        Msg = <<"{\"type\":\"offer\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertEqual(ok, Result)
    end).

event_handles_large_message_payload_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证大消息
                ?assert(byte_size(Msg) > 1000),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"large_msg">>,
        LargeSdp = io_lib:format("{\"type\":\"offer\",\"sdp\":\"~s\"}",
                                 [lists:duplicate(1000, $x)]),
        LargeMsg = list_to_binary(LargeSdp),

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, LargeMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 错误响应格式测试
%% ===================================================================

event_returns_valid_json_for_in_denylist_error_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 1 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_123">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, <<"{}">>),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,

        % 验证返回的是有效的 JSON
        ?assert(is_binary(ReplyMsg)),
        ?assertNotEqual(<<>>, ReplyMsg),

        % 尝试解析 JSON（不抛出异常即为有效）
        try
            _ = jsone:decode(ReplyMsg),
            ?assert(true)
        catch
            _:_ -> ?assert(false, "Invalid JSON response")
        end
    end).

event_returns_valid_json_for_not_a_friend_error_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> false end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_456">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, <<"{}">>),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,

        % 验证返回的是有效的 JSON
        ?assert(is_binary(ReplyMsg)),
        ?assertNotEqual(<<>>, ReplyMsg),

        % 尝试解析 JSON
        try
            _ = jsone:decode(ReplyMsg),
            ?assert(true)
        catch
            _:_ -> ?assert(false, "Invalid JSON response")
        end
    end).

%% ===================================================================
%% 消息ID传递测试
%% ===================================================================

event_preserves_msg_id_in_forwarded_message_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, MsgId, _Msg, _MsLi) ->
                % 验证消息ID被正确传递
                ?assertEqual(<<"test_msg_id_12345">>, MsgId),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"test_msg_id_12345">>,
        Msg = <<"{\"type\":\"offer\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 并发场景测试
%% ===================================================================

event_handles_concurrent_requests_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,

        % 模拟并发发送多个 WebRTC 信令
        Results = lists:map(fun(N) ->
            MsgId = list_to_binary("msg_" ++ integer_to_list(N)),
            Msg = <<"{\"type\":\"offer\",\"id\":", (integer_to_binary(N))/binary, "}">>,
            webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, Msg)
        end, lists:seq(1, 10)),

        % 验证所有请求都成功
        ?assertEqual(10, length([ok || ok <- Results]))
    end).

%% ===================================================================
%% 组合场景测试
%% ===================================================================

event_not_friend_and_in_denylist_returns_denylist_error_test_() ->
    % 既不是好友又在黑名单中，应该返回 in_denylist（优先级更高）
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> false end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 1 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, Action, To) ->
                #{
                    <<"id">> => MsgId,
                    <<"action">> => Action,
                    <<"to">> => To
                }
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"msg_combined">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, <<"{}">>),
        ?assertMatch({reply, _}, Result),
        {reply, ReplyMsg} = Result,
        % 应该返回 in_denylist（黑名单优先级高于好友关系）
        ?assert(string:find(ReplyMsg, "in_denylist") =/= nomatch)
    end).

%% ===================================================================
%% 特殊消息类型测试
%% ===================================================================

event_handles_hangup_message_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 hangup 消息
                ?assert(string:find(Msg, "hangup") =/= nomatch),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"hangup_msg">>,
        HangupMsg = <<"{\"type\":\"hangup\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, HangupMsg),
        ?assertEqual(ok, Result)
    end).

event_handles_reject_message_test_() ->
    ?WITH_MECKS([
        {friend_ds, [
            {'is_friend', 2, fun(_ToUid, _CurrentUid) -> true end}
        ]},
        {user_denylist_logic, [
            {'in_denylist', 2, fun(_ToUid, _CurrentUid) -> 0 end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, Msg, _MsLi) ->
                % 验证 reject 消息
                ?assert(string:find(Msg, "reject") =/= nomatch),
                ok
            end}
        ]}
    ], fun() ->
        CurrentUid = 123,
        ToUid = 456,
        MsgId = <<"reject_msg">>,
        RejectMsg = <<"{\"type\":\"reject\"}">>,

        Result = webrtc_ws_logic:event(CurrentUid, ToUid, MsgId, RejectMsg),
        ?assertEqual(ok, Result)
    end).
