%%% @doc 群解散接收者快照时机测试（Q05/F07/AC10）。
%%%
%%% 修复前：接收者快照在事务开启前取自 member_uids（1 小时缓存，
%%% DB 抖动折 []）——stale 缓存成员会收到多余通知、缓存空则通知整体
%%% 静默丢失。修复后：快照从事务内 group_member_repo:list_by_gid_tx
%%% 读取（与 201 成员审计同一份列表），事务提交后按该快照直发
%%% group_dissolve（save），保持"仅直发、不迁事件总线"的既有形态。
%%%
%%% oracle：msg_s2c_ds:send 恰 1 次、接收者 == 事务内成员快照、
%%% 非事务版 list_by_gid（缓存路径）0 次调用。
-module(group_dissolve_snapshot_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(GID, 910001).
-define(OWNER, 910101).

define_group() ->
    #{
        <<"id">> => ?GID,
        <<"owner_uid">> => ?OWNER,
        <<"name">> => <<"snapshot tests"/utf8>>,
        <<"scope">> => <<"workspace">>
    }.

member_rows() ->
    [
        #{<<"id">> => 1, <<"group_id">> => ?GID, <<"user_id">> => ?OWNER},
        #{<<"id">> => 2, <<"group_id">> => ?GID, <<"user_id">> => 910102},
        #{<<"id">> => 3, <<"group_id">> => ?GID, <<"user_id">> => 910103}
    ].

%% 解散通知接收者 == 事务内成员快照（user_id 列表），且不经缓存路径。
dissolve_recipients_from_tx_snapshot_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(_Conn, _Target) -> ok end}
            ]},
            {group_member_ds, [
                {'close_group_history_generations', 3, fun(_Conn, _Gid, _Reason) -> ok end}
            ]},
            {group_log_repo, [
                {'add', 2, fun(_Conn, _Data) -> {ok, 1} end},
                {'batch_add', 2, fun(_Conn, _Logs) -> {ok, 3} end}
            ]},
            {group_member_repo, [
                {'list_by_gid_tx', 4, fun(_Conn, ?GID, <<"*">>, _Limit) ->
                    {ok, member_rows()}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(From, To, Action, _MsgType, _E2EE, Payload, Save) ->
                    ?assertEqual(?OWNER, From),
                    ?assertEqual([?OWNER, 910102, 910103], To),
                    ?assertEqual(<<"group_dissolve">>, Action),
                    ?assertEqual(?GID, maps:get(<<"gid">>, Payload)),
                    ?assertEqual(save, Save),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                group_ds:dissolve_group(?OWNER, ?GID, ?OWNER, define_group())
            ),
            %% 恰一次直发，无事件总线路径（不双发哨兵）
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
            %% 快照不再取缓存路径（member_uids → list_by_gid/2,3 均 0 次）
            ?assertEqual(0, meck:num_calls(group_member_repo, list_by_gid, 2)),
            ?assertEqual(0, meck:num_calls(group_member_repo, list_by_gid, 3))
        end
    ).

%% 空群：事务内快照为 []，通知直发空列表（保持既有行为，不额外放大）。
dissolve_empty_group_recipients_test_() ->
    ?WITH_MECKS(
        [
            {workspace_guard, [
                {'ensure_writable_tx', 2, fun(_Conn, _Target) -> ok end}
            ]},
            {group_member_ds, [
                {'close_group_history_generations', 3, fun(_Conn, _Gid, _Reason) -> ok end}
            ]},
            {group_log_repo, [
                {'add', 2, fun(_Conn, _Data) -> {ok, 1} end},
                {'batch_add', 2, fun(_Conn, _Logs) -> {ok, 0} end}
            ]},
            {group_member_repo, [
                {'list_by_gid_tx', 4, fun(_Conn, ?GID, <<"*">>, _Limit) ->
                    {ok, []}
                end}
            ]},
            {elib_pg, [
                {'with_tx', 1, fun(Fun) -> Fun(self()) end},
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
            ]},
            {imboy_cache, [
                {'flush', 1, fun(_Key) -> ok end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(_From, _To, _Action, _MsgType, _E2EE, _Payload, _Save) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                group_ds:dissolve_group(?OWNER, ?GID, ?OWNER, define_group())
            ),
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).
