-module(channel_invitation_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_invitation_repo 的 repo 层单元测试（基于 mock，无数据库依赖）
%%%===================================================================

tablename_returns_channel_invitation_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"channel_invitation">>, channel_invitation_repo:tablename())
    end).

generate_invitation_code_is_alnum_and_fixed_length_test_() ->
    ?TEST_SIMPLE(fun() ->
        Codes = [channel_invitation_repo:generate_invitation_code() || _ <- lists:seq(1, 100)],
        lists:foreach(fun(Code) ->
            ?assertEqual(8, byte_size(Code)),
            ?assertMatch({match, _}, re:run(Code, <<"^[A-Z0-9]{8}$">>))
        end, Codes)
    end).

generate_invitation_code_supports_last_charset_position_test_() ->
    CharsetSize = byte_size(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>),
    ?WITH_MECKS([
        {rand, [
            {'uniform', 1, fun(Arg) ->
                ?assertEqual(CharsetSize, Arg),
                CharsetSize
            end}
        ]}
    ], fun() ->
        ?assertEqual(<<"99999999">>, channel_invitation_repo:generate_invitation_code()),
        ?assertEqual(8, meck:num_calls(rand, uniform, 1))
    end).

create_uses_default_pending_status_and_expiry_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> 1700000000000 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(Sql, Params) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"INSERT INTO channel_invitation">>) =/= nomatch),
                ?assertEqual(
                    [11, 1001, 2002, <<"INVITE01">>, <<"hello">>, 0,
                     1700604800000, 1700000000000],
                    Params
                ),
                {ok, 1, [{901}]}
            end}
        ]}
    ], fun() ->
        Data = #{
            channel_id => 11,
            inviter_uid => 1001,
            invitee_uid => 2002,
            invitation_code => <<"INVITE01">>,
            message => <<"hello">>
        },
        ?assertEqual({ok, 901}, channel_invitation_repo:create(Data))
    end).

create_maps_unique_violation_to_already_invited_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> 1700000000000 end}
        ]},
        {elib_pg, [
            {'execute', 2, fun(_Sql, _Params) ->
                {error, {pgsql_error, #{code => <<"23505">>}}}
            end}
        ]}
    ], fun() ->
        Data = #{channel_id => 11, inviter_uid => 1001, invitee_uid => 2002},
        ?assertEqual({error, already_invited}, channel_invitation_repo:create(Data))
    end).

find_pending_by_channel_and_invitee_returns_row_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(Sql, [11, 2002]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"WHERE channel_id = \\$1 AND invitee_uid = \\$2">>) =/= nomatch),
                ?assert(re:run(SqlBin, <<"status = 0">>) =/= nomatch),
                ?assert(re:run(SqlBin, <<"expires_at > NOW">>) =/= nomatch),
                ?assert(re:run(SqlBin, <<"ORDER BY created_at DESC LIMIT 1">>) =/= nomatch),
                Row = #{<<"id">> => 777, <<"status">> => 0, <<"invitee_uid">> => 2002},
                {ok, [Row]}
            end}
        ]}
    ], fun() ->
        ?assertMatch(
            {ok, #{<<"id">> := 777, <<"status">> := 0}},
            channel_invitation_repo:find_pending_by_channel_and_invitee(11, 2002)
        )
    end).

find_pending_by_channel_and_invitee_returns_not_found_when_empty_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, [11, 2002]) ->
                {ok, []}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, not_found},
            channel_invitation_repo:find_pending_by_channel_and_invitee(11, 2002)
        )
    end).

find_pending_by_channel_and_invitee_propagates_db_error_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(_Sql, [11, 2002]) ->
                {error, db_down}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, db_down},
            channel_invitation_repo:find_pending_by_channel_and_invitee(11, 2002)
        )
    end).

accept_returns_not_found_or_expired_when_no_row_updated_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(Sql, [1, 777, 2002]) ->
                SqlBin = iolist_to_binary(Sql),
                ?assert(re:run(SqlBin, <<"status = 0 AND expires_at > NOW">>) =/= nomatch),
                {ok, 0}
            end}
        ]}
    ], fun() ->
        ?assertEqual(
            {error, not_found_or_expired},
            channel_invitation_repo:accept(777, 2002)
        )
    end).

accept_returns_ok_when_row_updated_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'execute', 2, fun(_Sql, [1, 777, 2002]) ->
                {ok, 1}
            end}
        ]}
    ], fun() ->
        ?assertEqual(ok, channel_invitation_repo:accept(777, 2002))
    end).

invitation_expiry_test_() ->
    ?TEST_SIMPLE(fun() ->
        SevenDaysMs = 7 * 24 * 60 * 60 * 1000,
        Now = 1700000000000,
        ExpiresAt = Now + SevenDaysMs,
        ?assert(ExpiresAt > Now),
        ?assertEqual(SevenDaysMs, ExpiresAt - Now)
    end).
