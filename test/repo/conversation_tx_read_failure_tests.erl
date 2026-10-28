-module(conversation_tx_read_failure_tests).
-include_lib("eunit/include/eunit.hrl").

-define(UID, 101).
-define(GID, 202).
-define(UK3, <<"C2G_101_202">>).

transaction_read_failure_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun count_reads_abort/0,
        fun count_reads_preserve_values/0,
        fun generation_read_aborts/0,
        fun generation_read_preserves_values/0,
        fun settings_reject_invalid_json/0,
        fun settings_preserve_empty_and_object/0,
        fun settings_read_errors_abort_before_write/0,
        fun legacy_writes_reject_invalid_settings/0,
        fun settings_write_errors_abort/0,
        fun settings_update_preserves_other_keys/0,
        fun manual_pending_noop_and_conflict/0
    ]}.

setup() ->
    Modules = [elib_pg, elib_pg_sql, elib_dt, elib_tsid],
    lists:foreach(fun(Module) -> meck:new(Module, [no_link]) end, Modules),
    meck:expect(elib_pg_sql, public_tablename, fun(Table) ->
        <<"public.", Table/binary>>
    end),
    meck:expect(elib_dt, now, fun() -> 123456 end),
    meck:expect(elib_tsid, generate, fun(_Table) -> 404 end),
    meck:expect(elib_pg, with_tx, fun(Transaction) ->
        try
            Transaction(self())
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end),
    Modules.

cleanup(Modules) ->
    lists:foreach(fun meck:unload/1, Modules).

query_result(Result) ->
    meck:expect(elib_pg, query, fun(Conn, Sql, Params) ->
        ?assertEqual(self(), Conn),
        ?assert(lists:member(?UID, Params)),
        query_reply(Sql, Result)
    end).

query_reply(Sql, Result) ->
    case iolist_to_binary(Sql) of
        <<"SELECT pg_advisory_xact_lock(", _/binary>> ->
            {ok, [#{<<"locked">> => null}]};
        _ ->
            Result
    end.

count_readers() ->
    [
        {is_pinned, fun conversation_pin_repo:is_pinned_tx/4},
        {is_deleted, fun conversation_delete_repo:is_deleted_tx/4}
    ].

count_reads_abort() ->
    lists:foreach(
        fun({Operation, Read}) ->
            query_result({error, read_failed}),
            ?assertThrow(
                {abort_tx, {Operation, read_failed}},
                Read(self(), ?UID, ?GID, <<"c2g">>)
            ),
            query_result({ok, [#{<<"count">> => <<"invalid">>}]}),
            ?assertThrow(
                {abort_tx, {Operation, invalid_result}},
                Read(self(), ?UID, ?GID, <<"c2g">>)
            )
        end,
        count_readers()
    ).

count_reads_preserve_values() ->
    lists:foreach(
        fun({_Operation, Read}) ->
            lists:foreach(
                fun({Rows, Expected}) ->
                    query_result({ok, Rows}),
                    ?assertEqual(Expected, Read(self(), ?UID, ?GID, <<"c2g">>)),
                    ?assertEqual(Expected, Read(self(), ?UID, ?GID, <<"c2c">>))
                end,
                [
                    {[], false},
                    {[#{<<"count">> => 0}], false},
                    {[#{<<"count">> => 1}], true}
                ]
            )
        end,
        count_readers()
    ).

generation_read_aborts() ->
    query_result({error, read_failed}),
    ?assertThrow(
        {abort_tx, {active_generation_id, read_failed}},
        group_member_ds:active_generation_id_tx(self(), ?GID, ?UID)
    ),
    ?assertThrow(
        {abort_tx, {active_generation_id, read_failed}},
        conversation_pin_ds:pin_conversation_tx(self(), ?UID, ?GID, <<"c2g">>)
    ),
    ?assertThrow(
        {abort_tx, {active_generation_id, read_failed}},
        conversation_delete_ds:mark_deleted_tx(self(), ?UID, ?GID, <<"c2g">>)
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, execute_count, ['_', '_', '_'])),
    query_result({ok, [#{<<"id">> => 0}]}),
    ?assertThrow(
        {abort_tx, {active_generation_id, invalid_result}},
        group_member_ds:active_generation_id_tx(self(), ?GID, ?UID)
    ).

generation_read_preserves_values() ->
    query_result({ok, []}),
    ?assertEqual(0, group_member_ds:active_generation_id_tx(self(), ?GID, ?UID)),
    query_result({ok, [#{<<"id">> => 303}]}),
    ?assertEqual(303, group_member_ds:active_generation_id_tx(self(), ?GID, ?UID)).

settings_reject_invalid_json() ->
    lists:foreach(
        fun(Bin) ->
            query_result({ok, [#{<<"setting">> => Bin}]}),
            ?assertEqual(
                {error, invalid_setting_json},
                user_setting_repo:find_setting_for_update_tx(self(), ?UID)
            )
        end,
        [<<"{broken">>, <<"[]">>, <<"null">>, <<"false">>, <<"1">>, <<"\"text\"">>]
    ),
    lists:foreach(
        fun(Row) ->
            query_result({ok, [Row]}),
            ?assertEqual(
                {error, invalid_setting_result},
                user_setting_repo:find_setting_for_update_tx(self(), ?UID)
            )
        end,
        [#{}, #{<<"setting">> => []}]
    ).

settings_preserve_empty_and_object() ->
    lists:foreach(
        fun(Rows) ->
            query_result({ok, Rows}),
            ?assertEqual(
                {ok, absent},
                user_setting_repo:find_setting_for_update_tx(self(), ?UID)
            )
        end,
        [[], [#{<<"setting">> => null}], [#{<<"setting">> => <<>>}]]
    ),
    query_result({ok, [#{<<"setting">> => <<"{}">>}]}),
    ?assertEqual(
        {ok, #{}},
        user_setting_repo:find_setting_for_update_tx(self(), ?UID)
    ).

settings_read_errors_abort_before_write() ->
    lists:foreach(
        fun({Result, Reason}) ->
            query_result(Result),
            ?assertEqual(
                {error, {user_setting_read, Reason}},
                user_setting_ds:set_manual_pending(?UID, ?UK3, true)
            ),
            ?assertThrow(
                {abort_tx, {user_setting_read, Reason}},
                user_setting_ds:bump_conversation_version_tx(self(), ?UID, ?UK3)
            )
        end,
        [
            {{error, read_failed}, read_failed},
            {{ok, [#{<<"setting">> => <<"{broken">>}]}, invalid_setting_json},
            {{ok, [#{<<"setting">> => <<"[]">>}]}, invalid_setting_json},
            {{ok, [#{}]}, invalid_setting_result}
        ]
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, execute, ['_', '_', '_'])).

legacy_writes_reject_invalid_settings() ->
    meck:expect(elib_pg, query, fun(_Conn, Sql, _Params) ->
        Result =
            case binary:match(Sql, <<"FOR UPDATE">>) of
                nomatch -> {ok, [#{<<"count">> => 0}]};
                _ -> {ok, [#{<<"setting">> => <<"[]">>}]}
            end,
        query_reply(Sql, Result)
    end),
    meck:expect(elib_pg, execute_count, fun(_Conn, _Sql, _Params) -> {ok, 1} end),
    lists:foreach(
        fun(Write) ->
            ?assertMatch({error, _}, Write(?UID, ?GID, <<"c2c">>))
        end,
        [
            fun conversation_pin_logic:pin/3,
            fun conversation_pin_logic:unpin/3,
            fun conversation_logic:delete/3,
            fun conversation_logic:restore/3
        ]
    ),
    ?assertEqual(4, meck:num_calls(elib_pg, execute_count, ['_', '_', '_'])),
    ?assertEqual(0, meck:num_calls(elib_pg, execute, ['_', '_', '_'])).

settings_write_errors_abort() ->
    query_result({ok, []}),
    meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) ->
        {error, write_failed}
    end),
    ?assertEqual(
        {error, {user_setting_write, write_failed}},
        user_setting_ds:set_manual_pending(?UID, ?UK3, true, 0)
    ),
    ?assertThrow(
        {abort_tx, {user_setting_write, write_failed}},
        user_setting_ds:bump_conversation_version_tx(self(), ?UID, ?UK3)
    ).

settings_update_preserves_other_keys() ->
    Existing = #{
        <<"font_size">> => <<"large">>,
        <<"manual_pending">> => #{
            <<"C2C_101_404">> => #{<<"value">> => true, <<"version">> => 7}
        }
    },
    lists:foreach(
        fun(Setting) ->
            query_result({ok, [#{<<"setting">> => jsone:encode(Setting)}]}),
            meck:expect(elib_pg, execute, fun(Conn, _Sql, [?UID, Bin, 123456]) ->
                ?assertEqual(self(), Conn),
                Written = jsone:decode(Bin, [{object_format, map}]),
                ?assertEqual(
                    maps:remove(<<"manual_pending">>, Setting),
                    maps:remove(<<"manual_pending">>, Written)
                ),
                Pending = maps:get(<<"manual_pending">>, Written),
                ?assertMatch(
                    #{<<"value">> := true, <<"version">> := 1},
                    maps:get(?UK3, Pending)
                ),
                ?assertEqual(
                    maps:get(<<"manual_pending">>, Setting, #{}),
                    maps:remove(?UK3, Pending)
                ),
                {ok, 1}
            end),
            ?assertMatch(
                {ok, #{<<"version">> := 1}},
                user_setting_ds:set_manual_pending(?UID, ?UK3, true, 0)
            )
        end,
        [#{}, Existing]
    ),
    query_result({ok, []}),
    meck:expect(elib_pg, execute, fun(_Conn, _Sql, _Params) -> {ok, 1} end),
    ?assertMatch(
        {ok, #{<<"value">> := false, <<"version">> := 1}},
        user_setting_ds:bump_conversation_version_tx(self(), ?UID, ?UK3)
    ).

manual_pending_noop_and_conflict() ->
    Entry = #{<<"value">> => true, <<"version">> => 7, <<"updated_at">> => 123},
    Setting = #{<<"manual_pending">> => #{?UK3 => Entry}},
    query_result({ok, [#{<<"setting">> => jsone:encode(Setting)}]}),
    ?assertEqual(
        {ok, Entry},
        user_setting_ds:set_manual_pending(?UID, ?UK3, true, 7)
    ),
    ?assertEqual(
        {conflict, Entry},
        user_setting_ds:set_manual_pending(?UID, ?UK3, false, 6)
    ),
    ?assertEqual(0, meck:num_calls(elib_pg, execute, ['_', '_', '_'])).
