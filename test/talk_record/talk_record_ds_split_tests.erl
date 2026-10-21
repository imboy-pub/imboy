-module(talk_record_ds_split_tests).
-include_lib("eunit/include/eunit.hrl").

%% Pure compatibility checks exercise real entrypoints after responsibility split.
facade_exports_test() ->
    Exports = talk_record_ds:module_info(exports),
    lists:foreach(
        fun(E) -> ?assert(lists:member(E, Exports)) end,
        [
            {capabilities, 0},
            {creation_contexts, 3},
            {create, 4},
            {detail, 2},
            {update, 4},
            {list, 4},
            {etag, 1},
            {create_capture, 6},
            {list_captures, 2},
            {append_event, 4},
            {register_part, 6},
            {confirm_part, 5},
            {recover_capture, 4},
            {abandon_capture, 4},
            {fold_events, 1},
            {encode_cursor, 3},
            {decode_page_cursor, 3}
        ]
    ).

fold_event_facade_test() ->
    Events = [
        #{<<"event">> => Kind, <<"position_ms">> => Pos}
     || {Kind, Pos} <-
            [{<<"start">>, 0}, {<<"pause">>, 1000}, {<<"resume">>, 1000}, {<<"end">>, 2000}]
    ],
    ?assertEqual({<<"ended">>, 2000}, talk_record_ds:fold_events(Events)),
    ?assertEqual(talk_record_capture_ds:fold_events(Events), talk_record_ds:fold_events(Events)).

cursor_facade_binding_test() ->
    Previous = application:get_env(imboy, talk_record_cursor_signing_key),
    application:set_env(
        imboy,
        talk_record_cursor_signing_key,
        <<"split-regression-synthetic-cursor-key-0123456789">>
    ),
    try
        Binding = #{<<"uid">> => 700001},
        {ok, Token} = talk_record_ds:encode_cursor(<<"talk_record_list">>, Binding, [
            <<"time">>, 123
        ]),
        ?assertEqual(
            {ok, [<<"time">>, 123]},
            talk_record_ds:decode_page_cursor(Token, <<"talk_record_list">>, Binding)
        ),
        ?assertEqual(
            {error, invalid},
            talk_record_ds:decode_page_cursor(Token, <<"talk_record_list">>, #{<<"uid">> => 700002})
        )
    after
        restore_env(talk_record_cursor_signing_key, Previous)
    end.

module_size_test() ->
    lists:foreach(
        fun(Name) ->
            {ok, Body} = file:read_file(filename:join("src/ds", atom_to_list(Name) ++ ".erl")),
            ?assert(length(binary:split(Body, <<"\n">>, [global])) < 800)
        end,
        [
            talk_record_ds,
            talk_record_context_ds,
            talk_record_view_ds,
            talk_record_cursor_ds,
            talk_record_capture_ds,
            talk_record_parts_ds,
            talk_record_capture_support_ds
        ]
    ).

real_detail_identity_acl_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t05_support:teardown_all/1, fun(State) ->
            ?_test(real_detail_identity_acl(State))
        end}}.
setup() ->
    State = talk_record_t05_support:setup_all(),
    talk_record_t05_support:seed_fixtures(maps:get(conn, State)),
    State.
real_detail_identity_acl(State) ->
    Ids = talk_record_t05_support:ids(),
    Uid = maps:get(u1, Ids),
    Stranger = maps:get(u7, Ids),
    Client = <<"cb6ed1f7-6bf0-4459-a3ea-83d953ab7b11">>,
    Body = #{
        <<"client_recording_id">> => Client,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>
    },
    {ok, Validated} = talk_record_logic:parse_create(Body),
    {ok, Digest} = talk_record_logic:request_digest(Body),
    {ok, created, Created, _} = talk_record_ds:create(
        Uid, Validated, <<"split-detail-identity">>, Digest
    ),
    Rid = maps:get(<<"id">>, Created),
    {ok, #{payload := Payload}} = talk_record_ds:detail(Uid, Rid),
    ?assertEqual(Uid, maps:get(<<"owner_user_id">>, Payload)),
    ?assertEqual(Client, maps:get(<<"client_recording_id">>, Payload)),
    ?assertEqual(Uid, maps:get(<<"owner_user_id">>, Created)),
    ?assertEqual(Client, maps:get(<<"client_recording_id">>, Created)),
    ?assertEqual({error, not_found}, talk_record_ds:detail(Stranger, Rid)),
    Conn = maps:get(conn, State),
    {ok, _} = elib_pg:query(Conn, <<"UPDATE talk_record SET deleted_at=now() WHERE id=$1">>, [Rid]),
    ?assertEqual({error, not_found}, talk_record_ds:detail(Uid, Rid)),
    ?assertEqual({error, not_found}, talk_record_ds:detail(Stranger, Rid)).

restore_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value);
restore_env(Key, undefined) -> application:unset_env(imboy, Key).
