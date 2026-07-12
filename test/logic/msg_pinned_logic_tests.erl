-module(msg_pinned_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("error_code.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_pinned_logic 模块的 EUnit 测试（基于 meck mock，不需要数据库）
%%%
%%% 目标：验证消息置顶权限校验与广播通知
%%% 覆盖：C2C 参与者校验、C2G 群成员校验、pin/unpin、message_pinned 广播
%%% MIRROR test/logic/msg_reaction_logic_tests.erl 的 ?WITH_MECKS 模式
%%%===================================================================

-define(FROM_UID, 999901).
-define(TO_UID, 999902).
-define(OUTSIDER_UID, 999903).
-define(GID, 888801).
-define(C2C_MSG, <<"pin_test_c2c_001">>).
-define(C2G_MSG, <<"pin_test_c2g_001">>).

%% C2C 消息 mock（find 返回 from_id/to_id）
c2c_find_mock() ->
    {msg_c2c_ds, [
        {'find_msg_by_id', 1, fun
            (?C2C_MSG) ->
                {ok, #{<<"from_id">> => ?FROM_UID, <<"to_id">> => ?TO_UID}};
            (_) ->
                {error, not_found}
        end},
        %% 断言：update_pinned 必须传消息真实 to_id（非 CurrentUid），否则 function_clause
        {'update_pinned', 3, fun(?C2C_MSG, ?TO_UID, _Pinned) -> {ok, 1} end}
    ]}.

s2c_mock() ->
    {msg_s2c_ds, [
        {'send', 7, fun(_From, _ToUids, _Action, _MsgId, _E2EE, _Payload, _Save) -> ok end}
    ]}.

%% ===================================================================
%% C2C 权限校验
%% ===================================================================

%% ① 非参与者 pin c2c 被拒
pin_c2c_non_participant_denied_test_() ->
    ?WITH_MECKS([c2c_find_mock()], fun() ->
        ?assertEqual({error, permission_denied}, msg_pinned_logic:pin(?C2C_MSG, ?OUTSIDER_UID)),
        %% 无权限时不得触碰置顶状态
        ?assertEqual(0, meck:num_calls(msg_c2c_ds, update_pinned, '_'))
    end).

%% ② 参与者（发送者）pin c2c 通过，且 update_pinned 用消息真实 to_id
pin_c2c_sender_succeeds_test_() ->
    ?WITH_MECKS([c2c_find_mock(), s2c_mock()], fun() ->
        ?assertEqual(ok, msg_pinned_logic:pin(?C2C_MSG, ?FROM_UID)),
        ?assert(meck:called(msg_c2c_ds, update_pinned, [?C2C_MSG, ?TO_UID, true]))
    end).

%% ②b 参与者（接收者）pin c2c 通过
pin_c2c_receiver_succeeds_test_() ->
    ?WITH_MECKS([c2c_find_mock(), s2c_mock()], fun() ->
        ?assertEqual(ok, msg_pinned_logic:pin(?C2C_MSG, ?TO_UID))
    end).

%% ⑤ unpin 同校验：非参与者拒、参与者过（pinned=false）
unpin_c2c_permission_test_() ->
    ?WITH_MECKS([c2c_find_mock(), s2c_mock()], fun() ->
        ?assertEqual({error, permission_denied}, msg_pinned_logic:unpin(?C2C_MSG, ?OUTSIDER_UID)),
        ?assertEqual(ok, msg_pinned_logic:unpin(?C2C_MSG, ?FROM_UID)),
        ?assert(meck:called(msg_c2c_ds, update_pinned, [?C2C_MSG, ?TO_UID, false]))
    end).

%% ⑥ 广播被调用断言：C2C pin 成功后向对端发 message_pinned（nosave）
pin_c2c_broadcast_test_() ->
    ?WITH_MECKS([c2c_find_mock(), s2c_mock()], fun() ->
        ?assertEqual(ok, msg_pinned_logic:pin(?C2C_MSG, ?FROM_UID)),
        ?assert(
            meck:called(msg_s2c_ds, send, [
                ?FROM_UID, [?TO_UID], <<"message_pinned">>, ?C2C_MSG, null, '_', nosave
            ])
        )
    end).

%% ===================================================================
%% C2G 权限校验
%% ===================================================================

c2g_mocks(IsMember) ->
    [
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_) -> {error, not_found} end}
        ]},
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun
                (?C2G_MSG) ->
                    {ok, #{<<"from_id">> => ?FROM_UID, <<"to_id">> => ?GID}};
                (_) ->
                    {error, not_found}
            end},
            %% 断言：update_pinned 必须传群ID（非 CurrentUid），否则 function_clause
            {'update_pinned', 3, fun(?C2G_MSG, ?GID, _Pinned) -> {ok, 1} end}
        ]},
        {group_member_ds, [
            {'is_member', 2, fun(?GID, _Uid) -> IsMember end}
        ]},
        {group_ds, [
            {'member_uids', 1, fun(?GID) -> [?FROM_UID, ?TO_UID, ?OUTSIDER_UID] end}
        ]},
        s2c_mock()
    ].

%% ③ 非群成员 pin c2g 被拒
pin_c2g_non_member_denied_test_() ->
    ?WITH_MECKS(c2g_mocks(false), fun() ->
        ?assertEqual({error, permission_denied}, msg_pinned_logic:pin(?C2G_MSG, ?OUTSIDER_UID)),
        ?assertEqual(0, meck:num_calls(msg_c2g_ds, update_pinned, '_'))
    end).

%% ④ 群成员 pin c2g 通过 + ⑥ 广播给除操作者外的全部成员
pin_c2g_member_succeeds_and_broadcasts_test_() ->
    ?WITH_MECKS(c2g_mocks(true), fun() ->
        ?assertEqual(ok, msg_pinned_logic:pin(?C2G_MSG, ?FROM_UID)),
        ?assert(meck:called(msg_c2g_ds, update_pinned, [?C2G_MSG, ?GID, true])),
        ?assert(
            meck:called(msg_s2c_ds, send, [
                ?FROM_UID,
                [?TO_UID, ?OUTSIDER_UID],
                <<"message_pinned">>,
                ?C2G_MSG,
                null,
                '_',
                nosave
            ])
        )
    end).

%% ⑤b unpin c2g 同校验
unpin_c2g_permission_test_() ->
    ?WITH_MECKS(c2g_mocks(true), fun() ->
        ?assertEqual(ok, msg_pinned_logic:unpin(?C2G_MSG, ?TO_UID)),
        ?assert(meck:called(msg_c2g_ds, update_pinned, [?C2G_MSG, ?GID, false]))
    end).

%% 消息不存在
pin_not_found_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_ds, [{'find_msg_by_id', 1, fun(_) -> {error, not_found} end}]},
            {msg_c2g_ds, [{'find_msg_by_id', 1, fun(_) -> {error, not_found} end}]}
        ],
        fun() ->
            ?assertEqual({error, not_found}, msg_pinned_logic:pin(<<"no_such_msg">>, ?FROM_UID))
        end
    ).
