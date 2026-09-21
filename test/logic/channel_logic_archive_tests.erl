-module(channel_logic_archive_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc GZAPP-02/G4：频道归档/恢复单元测试
%%%
%%% 覆盖：创建者归档成功（status 1→0 + 订阅者通知）、非创建者拒绝、
%%% 重复归档 409、恢复成功（0→1）、非归档态恢复 409、
%%% ws 工作区已归档时写守卫稳定错误码 980 透传。
%%%===================================================================

-define(CID, 666001).
-define(CREATOR, 900001).
-define(OTHER, 900002).

archive_mocks(Role, ArchiveResult) ->
    [
        {channel_logic_common, [
            {'get_user_role', 2, fun
                (?CID, ?CREATOR) -> Role;
                (?CID, _) -> 0
            end}
        ]},
        {channel_ds, [
            {'archive', 1, fun(?CID) -> ArchiveResult end},
            {'find_by_id', 2, fun(?CID, _Cols) ->
                #{<<"id">> => ?CID, <<"status">> => 0}
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_update', 2, fun(_Cid, _Channel) -> ok end}
        ]}
    ].

archive_test_() ->
    [
        {"creator archives active channel (status->archived, notifies)", fun() ->
            ?WITH_MECKS(archive_mocks(3, {ok, 1}), fun() ->
                ?assertMatch(
                    {ok, #{channel_id := ?CID, status := <<"archived">>}},
                    channel_logic:archive_channel(?CREATOR, integer_to_binary(?CID))
                ),
                ?assertEqual(1, meck:num_calls(channel_logic_notify, notify_channel_update, 2))
            end)
        end},
        {"non creator cannot archive", fun() ->
            ?WITH_MECKS(archive_mocks(3, {ok, 1}), fun() ->
                ?assertMatch(
                    {error, <<"只有创建者可以归档频道"/utf8>>},
                    channel_logic:archive_channel(?OTHER, integer_to_binary(?CID))
                ),
                ?assertEqual(0, meck:num_calls(channel_ds, archive, 1))
            end)
        end},
        {"double archive rejected 409", fun() ->
            ?WITH_MECKS(archive_mocks(3, {ok, 0}), fun() ->
                ?assertMatch(
                    {error, {409, _}},
                    channel_logic:archive_channel(?CREATOR, integer_to_binary(?CID))
                )
            end)
        end},
        {"workspace-guard 980 passthrough on archive", fun() ->
            ?WITH_MECKS(archive_mocks(3, {error, {980, <<"工作区已归档"/utf8>>}}), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_logic:archive_channel(?CREATOR, integer_to_binary(?CID))
                )
            end)
        end},
        {"invalid channel id rejected", fun() ->
            ?WITH_MECKS(archive_mocks(3, {ok, 1}), fun() ->
                ?assertMatch(
                    {error, <<"频道不存在"/utf8>>},
                    channel_logic:archive_channel(?CREATOR, <<"not-a-number">>)
                )
            end)
        end}
    ].

restore_mocks(Role, RestoreResult) ->
    [
        {channel_logic_common, [
            {'get_user_role', 2, fun
                (?CID, ?CREATOR) -> Role;
                (?CID, _) -> 0
            end}
        ]},
        {channel_ds, [
            {'restore', 1, fun(?CID) -> RestoreResult end},
            {'find_by_id', 2, fun(?CID, _Cols) ->
                #{<<"id">> => ?CID, <<"status">> => 1}
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_update', 2, fun(_Cid, _Channel) -> ok end}
        ]}
    ].

restore_test_() ->
    [
        {"creator restores archived channel", fun() ->
            ?WITH_MECKS(restore_mocks(3, {ok, 1}), fun() ->
                ?assertMatch(
                    {ok, #{channel_id := ?CID, status := <<"active">>}},
                    channel_logic:restore_channel(?CREATOR, integer_to_binary(?CID))
                )
            end)
        end},
        {"restore non-archived or deleted channel rejected 409", fun() ->
            ?WITH_MECKS(restore_mocks(3, {ok, 0}), fun() ->
                ?assertMatch(
                    {error, {409, _}},
                    channel_logic:restore_channel(?CREATOR, integer_to_binary(?CID))
                )
            end)
        end},
        {"non creator cannot restore", fun() ->
            ?WITH_MECKS(restore_mocks(3, {ok, 1}), fun() ->
                ?assertMatch(
                    {error, <<"只有创建者可以恢复频道"/utf8>>},
                    channel_logic:restore_channel(?OTHER, integer_to_binary(?CID))
                )
            end)
        end},
        {"workspace-guard 980 passthrough on restore", fun() ->
            ?WITH_MECKS(restore_mocks(3, {error, {980, <<"工作区已归档"/utf8>>}}), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_logic:restore_channel(?CREATOR, integer_to_binary(?CID))
                )
            end)
        end}
    ].

%% ===================================================================
%% GZAPP-03/D04：Organization owner/admin 第二授权源（非创建者）
%% ===================================================================

org_manager_mocks(Role, ManagerResult, ArchiveResult) ->
    [
        {channel_logic_common, [
            {'get_user_role', 2, fun(_Cid, _Uid) -> Role end}
        ]},
        {organization_resource_authority, [
            {'ensure_manager', 2, fun(_Resource, _Uid) -> ManagerResult end}
        ]},
        {channel_ds, [
            {'archive', 1, fun(_Cid) -> ArchiveResult end},
            {'restore', 1, fun(_Cid) -> ArchiveResult end},
            {'find_by_id', 2, fun(?CID, _Cols) ->
                #{<<"id">> => ?CID, <<"status">> => 0}
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_update', 2, fun(_Cid, _Channel) -> ok end}
        ]}
    ].

org_manager_authority_test_() ->
    [
        {"org admin (non creator) can archive", fun() ->
            ?WITH_MECKS(org_manager_mocks(0, ok, {ok, 1}), fun() ->
                ?assertMatch(
                    {ok, #{channel_id := ?CID, status := <<"archived">>}},
                    channel_logic:archive_channel(?OTHER, integer_to_binary(?CID))
                ),
                ?assertEqual(
                    1,
                    meck:num_calls(
                        organization_resource_authority, ensure_manager, 2
                    )
                )
            end)
        end},
        {"org admin (non creator) can restore", fun() ->
            ?WITH_MECKS(org_manager_mocks(0, ok, {ok, 1}), fun() ->
                ?assertMatch(
                    {ok, #{channel_id := ?CID, status := <<"active"/utf8>>}},
                    channel_logic:restore_channel(?OTHER, integer_to_binary(?CID))
                )
            end)
        end},
        {"non creator non manager keeps legacy message and no write", fun() ->
            ?WITH_MECKS(
                org_manager_mocks(
                    0, {error, {403, <<"仅资源所有者或组织 Owner/Admin 可执行该操作"/utf8>>}}, {ok, 1}
                ),
                fun() ->
                    ?assertMatch(
                        {error, <<"只有创建者可以归档频道"/utf8>>},
                        channel_logic:archive_channel(?OTHER, integer_to_binary(?CID))
                    ),
                    ?assertMatch(
                        {error, <<"只有创建者可以恢复频道"/utf8>>},
                        channel_logic:restore_channel(?OTHER, integer_to_binary(?CID))
                    ),
                    ?assertEqual(0, meck:num_calls(channel_ds, archive, 1)),
                    ?assertEqual(0, meck:num_calls(channel_ds, restore, 1))
                end
            )
        end},
        {"authority db error propagates 503 fail-closed", fun() ->
            ?WITH_MECKS(
                org_manager_mocks(0, {error, {503, <<"权限校验暂时不可用，请稍后重试"/utf8>>}}, {ok, 1}),
                fun() ->
                    ?assertMatch(
                        {error, <<"权限校验暂时不可用", _/binary>>},
                        channel_logic:archive_channel(?OTHER, integer_to_binary(?CID))
                    ),
                    ?assertEqual(0, meck:num_calls(channel_ds, archive, 1))
                end
            )
        end},
        {"creator path skips org authority lookup (zero behavior change)", fun() ->
            ?WITH_MECKS(org_manager_mocks(3, {error, not_used}, {ok, 1}), fun() ->
                ?assertMatch(
                    {ok, #{status := <<"archived">>}},
                    channel_logic:archive_channel(?CREATOR, integer_to_binary(?CID))
                ),
                ?assertEqual(
                    0,
                    meck:num_calls(
                        organization_resource_authority, ensure_manager, 2
                    )
                )
            end)
        end}
    ].
