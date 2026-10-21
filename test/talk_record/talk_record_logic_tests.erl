%%% @doc talk_record_logic 纯校验层单测（无 DB）。
-module(talk_record_logic_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% parse_create
%%%===================================================================

parse_create_minimal_personal_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000001">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>
    },
    {ok, V} = talk_record_logic:parse_create(Body),
    ?assertEqual(<<"personal">>, maps:get(scope, V)),
    ?assertEqual(<<"record_only">>, maps:get(processing_mode, V)),
    ?assertEqual(<<>>, maps:get(title, V)).

parse_create_org_ok_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000002">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"organization">>,
        <<"organization_id">> => 710001,
        <<"workspace_id">> => 720001,
        <<"title">> => <<"项目例会"/utf8>>,
        <<"language">> => <<"zh-CN">>
    },
    {ok, V} = talk_record_logic:parse_create(Body),
    ?assertEqual(710001, maps:get(organization_id, V)),
    ?assertEqual(720001, maps:get(workspace_id, V)).

parse_create_missing_notice_ack_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000003">>,
        <<"policy_version">> => <<"talk-record-notice-v1">>
    },
    ?assertEqual(
        {error, notice_ack_required},
        talk_record_logic:parse_create(Body#{<<"notice_ack">> => false})
    ),
    ?assertEqual({error, notice_ack_required}, talk_record_logic:parse_create(Body)).

parse_create_personal_rejects_org_fields_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000004">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"organization_id">> => 710001
    },
    ?assertEqual({error, personal_with_org}, talk_record_logic:parse_create(Body)).

parse_create_org_requires_org_id_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000005">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"organization">>
    },
    ?assertEqual({error, invalid_organization_id}, talk_record_logic:parse_create(Body)).

parse_create_bad_uuid_test() ->
    Body = #{
        <<"client_recording_id">> => <<"not-a-uuid">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>
    },
    ?assertEqual({error, invalid_client_recording_id}, talk_record_logic:parse_create(Body)).

parse_create_future_source_type_test() ->
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000006">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"source_type">> => <<"remote_call">>
    },
    ?assertEqual(
        {error, unsupported_source_type}, talk_record_logic:parse_create(Body)
    ).

parse_create_policy_version_test() ->
    Base = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000007">>,
        <<"notice_ack">> => true
    },
    ?assertEqual(
        {error, policy_version_missing}, talk_record_logic:parse_create(Base)
    ),
    ?assertEqual(
        {error, policy_version_changed},
        talk_record_logic:parse_create(Base#{<<"policy_version">> => <<"old-v0">>})
    ).

parse_create_title_too_long_test() ->
    Long = binary:copy(<<"字"/utf8>>, 121),
    Body = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000008">>,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"title">> => Long
    },
    ?assertEqual({error, invalid_title}, talk_record_logic:parse_create(Body)).

%%%===================================================================
%%% parse_update
%%%===================================================================

parse_update_immutable_fields_test() ->
    ?assertEqual(
        {error, immutable_field},
        talk_record_logic:parse_update(#{<<"scope">> => <<"personal">>})
    ),
    ?assertEqual(
        {error, immutable_field},
        talk_record_logic:parse_update(#{<<"owner_user_id">> => 123})
    ),
    ?assertEqual(
        {error, immutable_field},
        talk_record_logic:parse_update(#{<<"notes">> => <<"n">>, <<"workspace_id">> => 5})
    ).

parse_update_empty_test() ->
    ?assertEqual(
        {error, missing_editable_field}, talk_record_logic:parse_update(#{})
    ).

parse_update_ok_test() ->
    {ok, Changes} = talk_record_logic:parse_update(#{
        <<"title">> => <<"新标题"/utf8>>, <<"notes">> => <<"备注"/utf8>>
    }),
    ?assertEqual(<<"新标题"/utf8>>, maps:get(<<"title">>, Changes)),
    ?assert(maps:is_key(<<"notes">>, Changes)),
    ?assertNot(maps:is_key(<<"language">>, Changes)).

%%%===================================================================
%%% parse_if_match / parse_filters / limit / idem key
%%%===================================================================

parse_if_match_test() ->
    ?assertEqual({error, precondition_required}, talk_record_logic:parse_if_match(undefined)),
    ?assertEqual({ok, 3}, talk_record_logic:parse_if_match(<<"\"3\"">>)),
    ?assertEqual({ok, 7}, talk_record_logic:parse_if_match(<<"7">>)),
    ?assertEqual({error, invalid_if_match}, talk_record_logic:parse_if_match(<<"abc">>)).

parse_filters_test() ->
    ?assertMatch(
        {ok, #{filter := <<"all">>}},
        talk_record_logic:parse_filters(#{<<"scope">> => <<>>, <<"filter">> => <<"all">>})
    ),
    %% personal + organization_id 组合非法
    ?assertEqual(
        {error, invalid_filter},
        talk_record_logic:parse_filters(#{
            <<"scope">> => <<"personal">>, <<"organization_id">> => 1, <<"filter">> => <<"all">>
        })
    ),
    ?assertEqual(
        {error, invalid_filter},
        talk_record_logic:parse_filters(#{<<"filter">> => <<"everyone">>})
    ),
    ?assertEqual(
        {error, invalid_filter},
        talk_record_logic:parse_filters(#{<<"q">> => binary:copy(<<"a">>, 101)})
    ).

parse_limit_clamp_test() ->
    ?assertEqual(20, talk_record_logic:parse_limit(undefined)),
    ?assertEqual(20, talk_record_logic:parse_limit(<<"1">>)),
    ?assertEqual(100, talk_record_logic:parse_limit(<<"500">>)),
    ?assertEqual(50, talk_record_logic:parse_limit(<<"50">>)),
    ?assertEqual(20, talk_record_logic:parse_limit(<<"garbage">>)).

idempotency_key_test() ->
    ?assert(talk_record_logic:valid_idempotency_key(<<"abc-123">>)),
    ?assertNot(talk_record_logic:valid_idempotency_key(undefined)),
    ?assertNot(talk_record_logic:valid_idempotency_key(<<>>)),
    ?assertNot(talk_record_logic:valid_idempotency_key(binary:copy(<<"a">>, 129))),
    ?assertNot(talk_record_logic:valid_idempotency_key(<<"键"/utf8>>)).

%%%===================================================================
%%% request_digest（freeze §2.5：canonical_json key 序不影响 digest）
%%%===================================================================

request_digest_key_order_independent_test() ->
    A = #{
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000009">>,
        <<"notice_ack">> => true,
        <<"b">> => 1
    },
    B = #{
        <<"b">> => 1,
        <<"notice_ack">> => true,
        <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000000009">>
    },
    {ok, Da} = talk_record_logic:request_digest(A),
    {ok, Db} = talk_record_logic:request_digest(B),
    ?assertEqual(Da, Db),
    %% 不同 payload → 不同 digest
    {ok, Dc} = talk_record_logic:request_digest(A#{<<"b">> => 2}),
    ?assert(Da =/= Dc),
    %% 含 float 不可规范化
    ?assertEqual(
        {error, non_canonical},
        talk_record_logic:request_digest(#{<<"x">> => 1.5})
    ).
