-module(user_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_logic 模块的 EUnit 测试
%%%
%%% 目标：验证用户逻辑服务功能
%%% 覆盖：在线状态、用户查找、密码修改、更新信息、上下线
%%%===================================================================

%% ===================================================================
%% is_online/1 测试
%% ===================================================================

is_online_returns_true_when_user_online_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'count_user', 1, fun(_Uid) -> 1 end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:is_online(Uid),
        ?assertEqual(true, Result)
    end).

is_online_returns_false_when_user_offline_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'count_user', 1, fun(_Uid) -> 0 end}
    ], fun() ->
        Uid = 999,
        Result = user_logic:is_online(Uid),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% is_online/2 测试
%% ===================================================================

is_online_with_dtype_returns_true_when_online_test_() ->
    ?WITH_MECK(user_device_logic, [
        {'is_online', 2, fun(_Uid, _Condition) -> true end}
    ], fun() ->
        Uid = 1,
        DType = <<"ios">>,
        Result = user_logic:is_online(Uid, DType),
        ?assertEqual(true, Result)
    end).

is_online_with_dtype_returns_false_when_offline_test_() ->
    ?WITH_MECK(user_device_logic, [
        {'is_online', 2, fun(_Uid, _Condition) -> false end}
    ], fun() ->
        Uid = 999,
        DType = <<"android">>,
        Result = user_logic:is_online(Uid, DType),
        ?assertEqual(false, Result)
    end).

%% ===================================================================
%% online_state/1 测试
%% ===================================================================

online_state_returns_online_when_user_online_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'count_user', 1, fun(_Uid) -> 1 end}
        ]},
        {user_setting_ds, [
            {'chat_state_hide', 1, fun(_Uid) -> false end}
        ]}
    ], fun() ->
        User = #{<<"id">> => 1, <<"nickname">> => <<"测试用户"/utf8>>, <<"last_seen_at">> => <<"2023-01-01T00:00:00Z">>},
        Result = user_logic:online_state(User),
        ?assertMatch(#{<<"status">> := online}, Result)
    end).

online_state_returns_offline_when_user_offline_test_() ->
    ?WITH_MECK(imboy_syn, [
        {'count_user', 1, fun(_Uid) -> 0 end}
    ], fun() ->
        User = #{<<"id">> => 999, <<"nickname">> => <<"离线用户"/utf8>>, <<"last_seen_at">> => <<"2023-01-01T00:00:00Z">>},
        Result = user_logic:online_state(User),
        ?assertMatch(#{<<"status">> := offline}, Result)
    end).

online_state_returns_offline_when_hide_status_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'count_user', 1, fun(_Uid) -> 1 end}
        ]},
        {user_setting_ds, [
            {'chat_state_hide', 1, fun(_Uid) -> true end}
        ]}
    ], fun() ->
        User = #{<<"id">> => 1, <<"nickname">> => <<"隐身用户"/utf8>>, <<"last_seen_at">> => <<"2023-01-01T00:00:00Z">>},
        Result = user_logic:online_state(User),
        ?assertMatch(#{<<"status">> := offline}, Result)
    end).

%% ===================================================================
%% batch_online_state/1 测试
%% ===================================================================

batch_online_state_processes_multiple_users_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'count_user', 1, fun(1) -> 1; (2) -> 0; (3) -> 1 end}
        ]},
        {user_setting_ds, [
            {'chat_state_hide', 1, fun(_Uid) -> false end}
        ]}
    ], fun() ->
        Users = [
            #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
            #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>},
            #{<<"id">> => 3, <<"nickname">> => <<"用户3"/utf8>>}
        ],
        Result = user_logic:batch_online_state(Users),
        ?assertEqual(3, length(Result)),
        ?assertMatch([#{<<"status">> := online}, #{<<"status">> := offline}, #{<<"status">> := online}], Result)
    end).

batch_online_state_with_empty_list_test_() ->
    Result = user_logic:batch_online_state([]),
    ?assertEqual([], Result).

%% ===================================================================
%% mine_state/1 测试
%% ===================================================================

mine_state_returns_hide_when_hidden_test_() ->
    ?WITH_MECK(user_setting_ds, [
        {'chat_state_hide', 1, fun(_Uid) -> true end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:mine_state(Uid),
        ?assertEqual({<<"status">>, hide}, Result)
    end).

mine_state_returns_online_when_not_hidden_test_() ->
    ?WITH_MECK(user_setting_ds, [
        {'chat_state_hide', 1, fun(_Uid) -> false end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:mine_state(Uid),
        ?assertEqual({<<"status">>, online}, Result)
    end).

%% ===================================================================
%% find_by_id/1 测试
%% ===================================================================

find_by_id_with_integer_returns_user_test_() ->
    ?WITH_MECK(user_ds, [
        {'find_by_id', 2, fun(_Uid, _Column) ->
            #{<<"id">> => 1, <<"nickname">> => <<"测试用户"/utf8>>, <<"avatar">> => <<"avatar.jpg">>}
        end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:find_by_id(Uid),
        ?assertMatch(#{<<"id">> := 1, <<"nickname">> := <<"测试用户"/utf8>>}, Result)
    end).

find_by_id_with_binary_decodes_id_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"encoded_123">>) -> 123 end}
        ]},
        {user_ds, [
            {'find_by_id', 2, fun(_Uid, _Column) ->
                #{<<"id">> => 123, <<"nickname">> => <<"测试用户"/utf8>>}
            end}
        ]}
    ], fun() ->
        Uid = <<"encoded_123">>,
        Result = user_logic:find_by_id(Uid),
        ?assertMatch(#{<<"id">> := 123}, Result)
    end).

find_by_id_sets_default_avatar_when_empty_test_() ->
    ?WITH_MECK(user_ds, [
        {'find_by_id', 2, fun(_Uid, _Column) ->
            #{<<"id">> => 1, <<"nickname">> => <<"测试用户"/utf8>>, <<"avatar">> => <<>>}
        end}
    ], fun() ->
        Uid = 1,
        Result = user_logic:find_by_id(Uid),
        ?assertEqual(<<"assets/images/def_avatar.png">>, maps:get(<<"avatar">>, Result))
    end).

%% ===================================================================
%% find_by_id/2 测试
%% ===================================================================

find_by_id_with_custom_column_test_() ->
    ?WITH_MECK(user_ds, [
        {'find_by_id', 2, fun(_Uid, _Column) ->
            #{<<"id">> => 1, <<"account">> => <<"test_account">>}
        end}
    ], fun() ->
        Uid = 1,
        Column = <<"account">>,
        Result = user_logic:find_by_id(Uid, Column),
        ?assertMatch(#{<<"account">> := <<"test_account">>}, Result)
    end).

%% ===================================================================
%% find_by_ids/1 测试
%% ===================================================================

find_by_ids_returns_users_test_() ->
    ?WITH_MECK(user_ds, [
        {'list_by_ids', 2, fun(_Ids, _Column) ->
            {ok, [
                #{<<"id">> => 1, <<"nickname">> => <<"用户1"/utf8>>},
                #{<<"id">> => 2, <<"nickname">> => <<"用户2"/utf8>>}
            ]}
        end}
    ], fun() ->
        Ids = [1, 2],
        Result = user_logic:find_by_ids(Ids),
        ?assertEqual(2, length(Result))
    end).

find_by_ids_with_empty_list_returns_empty_test_() ->
    Result = user_logic:find_by_ids([]),
    ?assertEqual([], Result).

find_by_ids_when_not_found_returns_empty_test_() ->
    ?WITH_MECK(user_ds, [
        {'list_by_ids', 2, fun(_Ids, _Column) -> {ok, []} end}
    ], fun() ->
        Ids = [999, 888],
        Result = user_logic:find_by_ids(Ids),
        ?assertEqual([], Result)
    end).

%% ===================================================================
%% update/3 测试
%% ===================================================================

update_nickname_success_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"nickname">>,
        Val = <<"新昵称"/utf8>>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_avatar_success_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"avatar">>,
        Val = <<"new_avatar.jpg">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_sign_success_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"sign">>,
        Val = <<"个性签名"/utf8>>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_region_success_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"region">>,
        Val = <<"北京"/utf8>>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_gender_with_valid_value_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"gender">>,
        Val = <<"1">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_gender_with_invalid_value_fails_test_() ->
    ?_test(begin
        Result = user_logic:update(1, <<"gender">>, <<"99">>),
        ?assertMatch({error, _}, Result)
    end).

update_allow_search_with_valid_value_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_allow_search', 2, fun(_Uid, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"allow_search">>,
        Val = <<"1">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_allow_search_with_invalid_value_fails_test_() ->
    ?_test(begin
        Result = user_logic:update(1, <<"allow_search">>, <<"3">>),
        ?assertMatch({error, _}, Result)
    end).

update_email_with_valid_format_test_() ->
    ?WITH_MECKS([
        {user_ds, [
            {'find_by_email', 2, fun(_Email, _Column) -> #{} end}
        ]},
        {elib_type, [
            {'is_email', 1, fun(_Email) -> true end}
        ]}
    ], fun() ->
        Uid = 1,
        Key = <<"email">>,
        Val = <<"test@example.com">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).

update_email_with_invalid_format_fails_test_() ->
    ?WITH_MECK(elib_type, [
        {'is_email', 1, fun(_Email) -> false end}
    ], fun() ->
        Uid = 1,
        Key = <<"email">>,
        Val = <<"invalid_email">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({error, _}, Result)
    end).

update_email_already_in_use_fails_test_() ->
    ?WITH_MECKS([
        {elib_type, [
            {'is_email', 1, fun(_Email) -> true end}
        ]},
        {user_ds, [
            {'find_by_email', 2, fun(_Email, _Column) -> #{<<"id">> => 999} end}
        ]}
    ], fun() ->
        Uid = 1,
        Key = <<"email">>,
        Val = <<"existing@example.com">>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({error, _}, Result)
    end).

update_unsupported_field_fails_test_() ->
    ?_test(begin
        Result = user_logic:update(1, <<"unknown_field">>, <<"value">>),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% online/4 测试
%% ===================================================================

online_joins_syn_and_casts_online_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'join', 4, fun(_Uid, _DType, _Pid, _DID) -> ok end}
        ]},
        {user_server, [
            {'cast_online', 4, fun(_Uid, _Pid, _DID, _DType) -> ok end}
        ]}
    ], fun() ->
        Uid = 1,
        DType = <<"ios">>,
        Pid = self(),
        DID = <<"device_123">>,
        Result = user_logic:online(Uid, DType, Pid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% offline/3 测试
%% ===================================================================

offline_leaves_syn_and_casts_offline_test_() ->
    ?WITH_MECKS([
        {imboy_syn, [
            {'leave', 2, fun(_Uid, _Pid) -> ok end}
        ]},
        {user_server, [
            {'cast_offline', 3, fun(_Uid, _Pid, _DID) -> ok end}
        ]}
    ], fun() ->
        Uid = 1,
        Pid = self(),
        DID = <<"device_123">>,
        Result = user_logic:offline(Uid, Pid, DID),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% apply_logout/2 测试
%% ===================================================================

apply_logout_sets_status_to_pending_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(self()) end}
        ]},
        {user_ds, [
            {'update_status_in_tx', 2, fun(_Conn, _UidStatus) -> {ok, 1} end}
        ]},
        {user_log_ds, [
            {'add_logout_apply_log', 3, fun(_Conn, _Uid, _Req0) -> ok end}
        ]}
    ], fun() ->
        Uid = 123,
        Req0 = #{},
        Result = user_logic:apply_logout(Uid, Req0),
        ?assertEqual({ok, "success"}, Result)
    end).

%% ===================================================================
%% cancel_logout/2 测试
%% ===================================================================

cancel_logout_restores_status_to_active_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_status', 2, fun(_Uid, _Status) -> {ok, 1} end}
    ], fun() ->
        Uid = 123,
        Req0 = #{},
        Result = user_logic:cancel_logout(Uid, Req0),
        ?assertEqual({ok, <<"success">>}, Result)
    end).

cancel_logout_with_error_returns_error_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_status', 2, fun(_Uid, _Status) -> {error, database_error} end}
    ], fun() ->
        Uid = 123,
        Req0 = #{},
        Result = user_logic:cancel_logout(Uid, Req0),
        ?assertMatch({error, _}, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

find_by_id_with_zero_uid_test_() ->
    ?WITH_MECK(user_ds, [
        {'find_by_id', 2, fun(_Uid, _Column) -> #{} end}
    ], fun() ->
        Uid = 0,
        Result = user_logic:find_by_id(Uid),
        ?assertEqual(#{}, Result)
    end).

update_with_empty_value_test_() ->
    ?WITH_MECK(user_ds, [
        {'update_field', 3, fun(_Uid, _Key, _Val) -> {ok, 1} end}
    ], fun() ->
        Uid = 1,
        Key = <<"sign">>,
        Val = <<>>,
        Result = user_logic:update(Uid, Key, Val),
        ?assertMatch({ok, _}, Result)
    end).
