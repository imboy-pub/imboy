-module(textguard_business_tests).

-include_lib("eunit/include/eunit.hrl").
-export([manifest/0]).
-define(F, textguard_business_fixture).

manifest() -> textguard_business_cases:manifest().

business_test_() ->
    {timeout, 600,
        {setup, fun ?F:setup/0, fun ?F:cleanup/1, fun(State) ->
            [
                {
                    binary_to_list(maps:get(id, Case)),
                    {timeout, 20, fun() -> execute(Case, State) end}
                }
             || Case <- manifest()
            ]
        end}}.

execute(Case, State) ->
    ?F:reset(State, maps:get(transport, Case)),
    try run(maps:get(kind, Case), maps:get(transport, Case), State) of
        _ -> receipt(Case, passed)
    catch
        Class:Reason:Stack ->
            receipt(Case, failed),
            erlang:raise(Class, Reason, Stack)
    end.

run(channel_allow, _, _) ->
    {{ok, _}, [_]} = ?F:observe(fun() -> publish(<<"ordinary">>) end),
    assert_counts(1, 0, 1),
    ?assertEqual(<<"ordinary">>, content());
run(channel_block, _, _) ->
    {{error, Message}, [_]} = ?F:observe(fun() -> publish(<<"测试词"/utf8>>) end),
    ?assertEqual(<<"内容包含违规词汇，发布失败"/utf8>>, Message),
    assert_counts(0, 0, 0);
run(channel_review, _, _) ->
    {ok, _} = publish(<<"demo">>),
    assert_counts(1, 1, 1),
    assert_metadata(<<"demo">>);
run(channel_duplicate, _, _) ->
    First = publish(<<"demo">>),
    ?assertMatch({ok, _}, First),
    ?assertEqual(First, publish(<<"demo">>)),
    assert_counts(1, 1, 1);
run(channel_edit_block, _, _) ->
    {ok, _} = publish(<<"ordinary">>),
    Before = message(),
    ?assertMatch({error, _}, edit(<<"测试词"/utf8>>)),
    ?assertEqual(Before, message()),
    assert_counts(1, 0, 1),
    ?assertEqual(0, maps:get(edited, ?F:counts()));
run(channel_edit_review, _, _) ->
    {ok, _} = publish(<<"ordinary">>),
    ok = edit(<<"demo">>),
    ?assertEqual(<<"demo">>, content()),
    assert_counts(1, 1, 1),
    ?assertEqual(1, maps:get(edited, ?F:counts())),
    assert_metadata(<<"demo">>);
run(channel_denied, _, _) ->
    {Result, []} = ?F:observe(fun() ->
        channel_logic_message:publish_message(
            ?F:outsider(), channel_id(), <<"demo">>, <<"text">>, #{}
        )
    end),
    ?assertMatch({error, _}, Result),
    assert_counts(0, 0, 0);
run(private_channel, _, _) ->
    ?F:sql(<<"UPDATE channel SET visibility=1 WHERE id=$1">>, [?F:channel()]),
    {{ok, _}, []} = ?F:observe(fun() -> publish(<<"ordinary">>) end),
    assert_counts(1, 0, 1);
run(encrypted_channel, _, _) ->
    lists:foreach(
        fun(Marker) ->
            {{error, _}, []} = ?F:observe(fun() ->
                channel_logic_message:publish_message(
                    ?F:owner(), channel_id(), <<"ordinary">>, <<"text">>, #{Marker => true}
                )
            end)
        end,
        [<<"ciphertext">>, <<"e2ee">>, <<"encrypted">>]
    ),
    lists:foreach(
        fun(Payload) ->
            {{error, _}, []} = ?F:observe(fun() ->
                channel_logic_message:publish_message(
                    ?F:owner(), channel_id(), <<"ordinary">>, <<"text">>, Payload
                )
            end)
        end,
        [<<"{\"encrypted\":true}">>, <<"{\"e2ee\":null}">>, <<"{\"ciphertext\":\"\"}">>]
    ),
    assert_counts(0, 0, 0);
run(bad_payload_json, _, _) ->
    {{error, _}, []} = ?F:observe(fun() ->
        channel_logic_message:publish_message(
            ?F:owner(), channel_id(), <<"ordinary">>, <<"text">>, <<"{invalid">>
        )
    end),
    assert_counts(0, 0, 0);
run(binary_payload_json, _, _) ->
    {ok, _} = publish(<<"ordinary">>),
    ?assert(is_binary(maps:get(<<"payload">>, message()))),
    {ok, [_]} = ?F:observe(fun() -> edit(<<"ordinary edited">>) end),
    Before = message(),
    {{error, _}, [_]} = ?F:observe(fun() -> edit(<<"测试词"/utf8>>) end),
    ?assertEqual(Before, message()),
    assert_counts(1, 0, 1),
    ?assertEqual(1, maps:get(edited, ?F:counts()));
run(incoming_binary_payload_rejected, _, _) ->
    {{error, _}, []} = ?F:observe(fun() ->
        channel_logic_message:publish_message(
            ?F:owner(), channel_id(), <<"ordinary">>, <<"text">>, <<"{}">>
        )
    end),
    assert_counts(0, 0, 0);
run(historical_profile_version, _, _) ->
    Previous = <<"12345678-1234-4234-9234-123456789abc">>,
    ?F:sql(
        <<"UPDATE public.\"user\" SET moderation_versions=jsonb_build_object('sign',$1::text,'school','opaque-school') WHERE id=$2">>,
        [Previous, ?F:owner()]
    ),
    {{ok, 1}, [_]} = ?F:observe(fun() -> profile(<<"ordinary">>) end),
    Current = profile_version(),
    ?assertMatch({match, _}, re:run(Current, <<"^[0-9a-f]{32}$">>)),
    ?assertEqual(<<"opaque-school">>, maps:get(<<"school">>, versions())),
    ?assertEqual(
        {ok, 0},
        user_ds:update_field_checked(
            ?F:owner(),
            <<"sign">>,
            <<"stale">>,
            #{
                content => <<"ordinary">>,
                version => Previous,
                next_version => binary:copy(<<"b">>, 32)
            }
        )
    ),
    ?assertEqual(<<"ordinary">>, profile_value()),
    ?assertEqual(Current, profile_version());
run(moment_block, _, _) ->
    ?assertMatch({error, _}, moment(<<"测试词"/utf8>>, 0)),
    assert_moment(0, 0);
run(moment_review, _, _) ->
    ?assertMatch({ok, _}, moment(<<"demo">>, 0)),
    assert_moment(1, 1),
    assert_metadata(<<"demo">>);
run(private_moment, _, _) ->
    {{ok, _}, []} = ?F:observe(fun() -> moment(<<"ordinary">>, 2) end),
    assert_moment(1, 0);
run(profile_block, _, _) ->
    ?assertMatch({error, _}, profile(<<"测试词"/utf8>>)),
    ?assertEqual(<<"before">>, profile_value()),
    ?assertEqual([], ?F:queue());
run(profile_review, _, _) ->
    ?assertEqual({ok, 1}, profile(<<"demo">>)),
    ?assertEqual(<<"demo">>, profile_value()),
    assert_metadata(<<"demo">>);
run(profile_field_scope, _, _) ->
    lists:foreach(
        fun(Field) ->
            {{ok, 1}, [#{field_ids := [Field]}]} = ?F:observe(fun() ->
                user_logic:update(?F:owner(), Field, <<"ordinary">>)
            end)
        end,
        [<<"nickname">>, <<"sign">>, <<"profession">>, <<"school">>, <<"interests">>]
    ),
    {{ok, 1}, []} = ?F:observe(fun() ->
        user_logic:update(?F:owner(), <<"avatar">>, <<"fixture-avatar">>)
    end);
run(multi_field, _, _) ->
    {{queued, Hits}, [#{field_ids := [<<"title">>, <<"body">>]}]} = ?F:observe(fun() ->
        moderation_business:inspect(
            channel_message,
            [{<<"title">>, <<"ordinary">>}, {<<"body">>, <<"demo">>}],
            true
        )
    end),
    ?assert(lists:all(fun(Hit) -> maps:get(field_id, Hit) =:= <<"body">> end, Hits)),
    assert_trace(Hits);
run(alias_review, _, _) ->
    ?assertMatch({ok, _}, publish(<<"ceshici">>)),
    assert_counts(1, 1, 1),
    [Row] = ?F:queue(),
    Metadata = metadata(Row),
    [Hit | _] = maps:get(<<"hits">>, Metadata),
    ?assertEqual(<<"high">>, maps:get(<<"severity">>, Hit)),
    ?assertEqual(<<"review">>, maps:get(<<"effective_action">>, Hit)),
    assert_metadata(<<"ceshici">>);
run(comma_metadata, _, _) ->
    {ok, _} = publish(<<"comma,canonical">>),
    [Row] = ?F:queue(),
    Metadata = metadata(Row),
    ?assertEqual([<<"comma,canonical">>], maps:get(<<"hit_words">>, Metadata)),
    [Hit] = maps:get(<<"hits">>, Metadata),
    ?assertEqual(<<"9007199254741001">>, maps:get(<<"rule_id">>, Hit)),
    {ok, Page} = moderation_ds:review_page(1, 10, #{}),
    [Shown] = maps:get(list, Page),
    ?assertEqual([<<"comma,canonical">>], maps:get(<<"hit_words">>, Shown)),
    assert_metadata(<<"comma,canonical">>);
run(original_text, _, _) ->
    Text = <<"ＤＥＭＯ"/utf8>>,
    {ok, _} = publish(Text),
    ?assertEqual(Text, content()),
    assert_metadata(Text);
run(stale_channel_review, _, _) ->
    {ok, _} = publish(<<"demo">>),
    [Review] = ?F:queue(),
    ok = edit(<<"new ordinary">>),
    reject(Review),
    ?assertEqual(<<"new ordinary">>, content()),
    ?assertEqual(1, maps:get(<<"status">>, message())),
    ?assertEqual(0, maps:get(deleted, ?F:counts()));
run(stale_profile_review, _, _) ->
    {ok, 1} = profile(<<"demo">>),
    [Review] = ?F:queue(),
    {ok, 1} = profile(<<"new ordinary">>),
    reject(Review),
    ?assertEqual(<<"new ordinary">>, profile_value());
run(channel_aba, _, _) ->
    {ok, _} = publish(<<"ordinary">>),
    ok = edit(<<"demo">>),
    [OldReview] = ?F:queue(),
    Old = channel_version(),
    ok = edit(<<"ordinary">>),
    ok = edit(<<"demo">>),
    New = channel_version(),
    ?assert(is_binary(Old) andalso byte_size(Old) > 0),
    ?assertNotEqual(Old, New),
    ?assertEqual(Old, maps:get(<<"content_version">>, metadata(OldReview))),
    reject(OldReview),
    ?assertEqual(<<"demo">>, content()),
    ?assertEqual(1, maps:get(<<"status">>, message())),
    ?assertEqual(0, maps:get(deleted, ?F:counts()));
run(profile_aba, _, _) ->
    {ok, 1} = profile(<<"demo">>),
    [OldReview] = ?F:queue(),
    Old = profile_version(),
    {ok, 1} = profile(<<"ordinary">>),
    {ok, 1} = profile(<<"demo">>),
    New = profile_version(),
    ?assert(is_binary(Old) andalso byte_size(Old) > 0),
    ?assertNotEqual(Old, New),
    ?assertEqual(Old, maps:get(<<"content_version">>, metadata(OldReview))),
    reject(OldReview),
    ?assertEqual(<<"demo">>, profile_value());
run(matching_channel_review, _, _) ->
    {ok, _} = publish(<<"demo">>),
    [Review] = ?F:queue(),
    reject(Review),
    ?assertEqual(-1, maps:get(<<"status">>, message())),
    ?assertEqual(1, maps:get(deleted, ?F:counts()));
run(matching_profile_review, _, _) ->
    {ok, 1} = profile(<<"demo">>),
    [Review] = ?F:queue(),
    reject(Review),
    ?assertEqual(<<>>, profile_value());
run(explicit_unchecked, Transport, _) ->
    Status =
        case Transport of
            http -> 429;
            grpc -> 8
        end,
    ?F:remote_fault(Transport, Status, <<"OVERLOADED">>, fun(Count) ->
        ?assertEqual(
            {unchecked_degraded, overloaded},
            moderation_business:inspect_profile(
                <<"sign">>, <<"ordinary">>
            )
        ),
        ?assertEqual(1, Count()),
        ?assertEqual([], ?F:queue())
    end);
run(legacy_profile, Transport, _) ->
    ?F:config([{mode, legacy}, {transport, Transport}]),
    {{ok, 1}, []} = ?F:observe(fun() -> profile(<<"ordinary">>) end),
    ?assertEqual(<<"ordinary">>, profile_value());
run(canary_truncation, _, _) ->
    Text = <<(?F:canary())/binary, " ", (binary:copy(<<"demo ">>, 101))/binary>>,
    {ok, _} = publish(Text),
    assert_counts(1, 1, 1),
    assert_metadata(Text),
    [Review] = ?F:queue(),
    Metadata = metadata(Review),
    Hits = maps:get(<<"hits">>, Metadata),
    ?assertEqual(100, length(Hits)),
    lists:foreach(
        fun(Hit) ->
            Trace = maps:get(<<"trace">>, Hit),
            ?assertEqual(true, maps:get(<<"hits_truncated">>, Trace)),
            ?assertEqual(true, maps:get(<<"scan_complete">>, Trace)),
            ?assertEqual(1, maps:get(<<"fields_checked">>, Trace)),
            ?assertEqual(<<"review">>, maps:get(<<"effective_action">>, Hit))
        end,
        Hits
    ),
    ?assertEqual(0, maps:get(privacy_leak, ?F:counts()));
run(second_publication, _, _) ->
    {ok, 1} = profile(<<"demo">>),
    [First] = ?F:queue(),
    OldMetadata = metadata(First),
    [OldHit | _] = maps:get(<<"hits">>, OldMetadata),
    OldTrace = maps:get(<<"trace">>, OldHit),
    OldRevision = ?F:export(),
    try
        ?F:sql(<<"UPDATE sensitive_word SET category='tg20-next-version' WHERE id=1002">>, []),
        NewRevision = ?F:republish(),
        ?assertNotEqual(OldRevision, NewRevision),
        {ok, 1} = profile(<<"demo second">>),
        Second = ?F:row(<<"SELECT * FROM review_queue WHERE id<>$1">>, [maps:get(<<"id">>, First)]),
        [NewHit | _] = maps:get(<<"hits">>, metadata(Second)),
        NewTrace = maps:get(<<"trace">>, NewHit),
        ?assertEqual(NewRevision, maps:get(<<"ruleset_revision">>, NewTrace)),
        ?assertEqual(OldRevision, maps:get(<<"ruleset_revision">>, OldTrace)),
        ?assert(
            binary_to_integer(maps:get(<<"generation">>, NewTrace)) >
                binary_to_integer(maps:get(<<"generation">>, OldTrace))
        ),
        Saved = ?F:row(<<"SELECT * FROM review_queue WHERE id=$1">>, [maps:get(<<"id">>, First)]),
        ?assertEqual(OldMetadata, metadata(Saved)),
        reject(First),
        ?assertEqual(<<"demo second">>, profile_value())
    after
        {1002, _, Category, _} = lists:keyfind(1002, 1, textguard_golden:rows()),
        ?F:sql(<<"UPDATE sensitive_word SET category=$1 WHERE id=1002">>, [Category]),
        ?F:republish()
    end;
run(profile_trigger, _, _) ->
    lists:foreach(
        fun(Field) ->
            Before = versions(),
            ?assertMatch({ok, 1}, user_repo:update(?F:owner(), #{Field => <<"trigger-a">>})),
            After = versions(),
            Token = maps:get(Field, After),
            ?assertEqual(32, byte_size(Token)),
            ?assertEqual(maps:remove(Field, Before), maps:remove(Field, After)),
            ?assertMatch({ok, 1}, user_repo:update(?F:owner(), #{Field => <<"trigger-a">>})),
            ?assertEqual(After, versions()),
            Next = binary:copy(<<"a">>, 32),
            ?assertEqual(
                {ok, 1},
                user_ds:update_field_checked(
                    ?F:owner(),
                    Field,
                    <<"trigger-b">>,
                    #{content => <<"trigger-a">>, version => Token, next_version => Next}
                )
            ),
            ?assertEqual(Next, maps:get(Field, versions())),
            ?assertEqual(maps:remove(Field, After), maps:remove(Field, versions()))
        end,
        [<<"nickname">>, <<"sign">>, <<"profession">>, <<"school">>, <<"interests">>]
    );
run(migration_roundtrip, _, _) ->
    {ok, _} = publish(<<"ordinary">>),
    {ok, 1} = profile(<<"ordinary">>),
    ?F:sql(<<"INSERT INTO review_queue(id,content) VALUES (9007199254741099,'old-row')">>, []),
    try
        ok = ?F:apply_migration(down),
        ?assertEqual(0, moderation_column_count()),
        ?assertEqual(0, trigger_count()),
        ?F:sql(<<"UPDATE public.\"user\" SET sign='down-save' WHERE id=$1">>, [?F:owner()]),
        ?assertEqual(<<"ordinary">>, content()),
        ?assertEqual(<<"down-save">>, profile_value())
    after
        ok = ?F:apply_migration(up)
    end,
    ?assertEqual(3, moderation_column_count()),
    ?assertEqual(1, trigger_count()),
    ?assertEqual(<<>>, channel_version()),
    ?assertEqual(#{}, versions()),
    ?assertEqual(
        #{}, metadata(?F:row(<<"SELECT * FROM review_queue WHERE id=9007199254741099">>, []))
    ),
    ?F:sql(<<"DELETE FROM review_queue WHERE id=9007199254741099">>, []),
    ?F:sql(<<"UPDATE public.\"user\" SET sign='up-save' WHERE id=$1">>, [?F:owner()]),
    ?assertEqual(32, byte_size(maps:get(<<"sign">>, versions()))),
    ?assertEqual({ok, 1}, profile(<<"demo">>)),
    assert_metadata(<<"demo">>);
run({private_e2ee, Type}, _, _) ->
    ok = ?F:private_message(Type),
    assert_counts(0, 0, 0);
run({deadline, Policy}, Transport, _) ->
    ?F:remote_hang(Transport, Policy, fun(Count) ->
        Started = erlang:monotonic_time(millisecond),
        {Result, [_]} = ?F:observe(fun() -> publish(<<"ordinary">>) end),
        Elapsed = erlang:monotonic_time(millisecond) - Started,
        ?assert(Elapsed < 1000),
        case Policy of
            availability_compat ->
                ?assertMatch({ok, _}, Result),
                assert_counts(1, 0, 1);
            strict_reject ->
                ?assertMatch({error, Message} when is_binary(Message), Result),
                assert_counts(0, 0, 0)
        end,
        receive
            tg20_unexpected_retry -> error(unexpected_message)
        after 120 -> ok
        end,
        ?assertEqual(1, Count())
    end);
run({profile_external_aba, Kind}, Transport, _) ->
    {ok, 1} = profile(<<"demo">>),
    [Review] = ?F:queue(),
    Old = profile_version(),
    case Kind of
        legacy -> ?F:config([{mode, legacy}, {transport, Transport}]);
        _ -> ok
    end,
    lists:foreach(
        fun(Value) ->
            Previous = profile_version(),
            case Kind of
                raw_sql ->
                    ?F:sql(<<"UPDATE public.\"user\" SET sign=$1 WHERE id=$2">>, [Value, ?F:owner()]);
                generic_repo ->
                    ?assertEqual({ok, 1}, user_repo:update(?F:owner(), #{<<"sign">> => Value}));
                legacy ->
                    {{ok, 1}, []} = ?F:observe(fun() -> profile(Value) end)
            end,
            ?assertNotEqual(Previous, profile_version())
        end,
        [<<"ordinary">>, <<"demo">>]
    ),
    ?assertNotEqual(Old, profile_version()),
    reject(Review),
    ?assertEqual(<<"demo">>, profile_value());
run({review_binding, Surface, Variant}, _, _) ->
    case Surface of
        channel -> {ok, _} = publish(<<"demo">>);
        profile -> {ok, 1} = profile(<<"demo">>)
    end,
    [Review] = ?F:queue(),
    Invalid = invalid_binding(Variant),
    ?F:review_binding(Review, Invalid, fun() ->
        reject(Review),
        case Variant of
            legacy_empty ->
                assert_legacy_removal(Surface);
            legacy_missing ->
                assert_legacy_removal(Surface);
            legacy_json ->
                assert_legacy_removal(Surface);
            _ ->
                ?assertEqual({0, 0}, ?F:removal_counts()),
                ?assertEqual(1, maps:get(review_binding_invalid, ?F:counts())),
                case Surface of
                    channel ->
                        ?assertEqual(1, maps:get(<<"status">>, message())),
                        ?assertEqual(<<"demo">>, content()),
                        ?assertEqual(0, maps:get(deleted, ?F:counts()));
                    profile ->
                        ?assertEqual(<<"demo">>, profile_value())
                end
        end
    end);
run({queue_failure, Surface, Failure}, _, _) ->
    ?F:review_failure(Failure, fun() ->
        Result =
            case Surface of
                channel -> publish(<<"demo">>);
                moment -> moment(<<"demo">>, 0);
                profile -> profile(<<"demo">>)
            end,
        ?assertMatch({ok, _}, Result)
    end),
    ?assertEqual([], ?F:queue()),
    ?assertEqual(1, maps:get(review_enqueue_failed, ?F:counts())),
    case Surface of
        channel ->
            assert_counts(1, 0, 1),
            ?assertEqual(<<"demo">>, content());
        moment ->
            assert_moment(1, 0);
        profile ->
            ?assertEqual(<<"demo">>, profile_value())
    end;
run({failure, Policy, Code}, Transport, _) ->
    {Http, Grpc, Wire} = wire_error(Code),
    Status =
        case Transport of
            http -> Http;
            grpc -> Grpc
        end,
    ?F:remote_fault(Transport, Status, Wire, fun(Count) ->
        Conf = config_ds:env(moderation_detector, []),
        application:set_env(
            imboy,
            moderation_detector,
            [{failure_policy, Policy} | proplists:delete(failure_policy, Conf)]
        ),
        Result = publish(<<"ordinary">>),
        Facility = lists:member(Code, [overloaded, not_ready, connection_failed, timeout]),
        case Facility andalso Policy =:= availability_compat of
            true ->
                ?assertMatch({ok, _}, Result),
                assert_counts(1, 0, 1);
            false ->
                ?assertMatch({error, Message} when is_binary(Message), Result),
                assert_counts(0, 0, 0)
        end,
        ?assertEqual(1, Count())
    end);
run({race, profile_value}, _, State) ->
    Result = ?F:race(
        fun() -> profile(<<"demo">>) end,
        fun(Connection) ->
            {ok, 1} = elib_pg:execute(
                Connection,
                <<"UPDATE public.\"user\" SET sign='concurrent' WHERE id=$1">>,
                [?F:owner()]
            ),
            ok
        end,
        State
    ),
    ?assertMatch({error, _}, Result),
    ?assertEqual(<<"concurrent">>, profile_value()),
    ?assertEqual([], ?F:queue());
run({race, Kind}, _, State) ->
    {ok, _} = publish(<<"ordinary">>),
    Query =
        case Kind of
            channel_content ->
                <<"UPDATE channel_message SET content='concurrent' WHERE id=$1">>;
            channel_timestamp ->
                <<"UPDATE channel_message SET updated_at=updated_at+interval '1 second' WHERE id=$1">>;
            channel_revoked ->
                <<"UPDATE channel_message SET revoked=true WHERE id=$1">>;
            channel_status ->
                <<"UPDATE channel_message SET status=-1 WHERE id=$1">>
        end,
    MessageId = maps:get(<<"id">>, message()),
    Result = ?F:race(
        fun() -> edit(<<"demo">>) end,
        fun(Connection) ->
            {ok, 1} = elib_pg:execute(Connection, Query, [MessageId]),
            ok
        end,
        State
    ),
    ?assertMatch({error, _}, Result),
    ?assertEqual([], ?F:queue()),
    ?assertEqual(0, maps:get(edited, ?F:counts())),
    ?assertNotEqual(<<"demo">>, content()).

publish(Text) ->
    channel_logic_message:publish_message(
        ?F:owner(),
        channel_id(),
        Text,
        <<"text">>,
        #{},
        <<"tg20-idempotency">>
    ).
edit(Text) ->
    channel_logic_message:edit_message(
        ?F:owner(),
        channel_id(),
        integer_to_binary(maps:get(<<"id">>, message())),
        Text
    ).
moment(Text, Visibility) ->
    moment_logic:create_post(
        ?F:owner(),
        #{<<"content">> => Text, <<"visibility">> => Visibility}
    ).
profile(Text) -> user_logic:update(?F:owner(), <<"sign">>, Text).
channel_id() -> integer_to_binary(?F:channel()).
message() -> ?F:row(<<"SELECT * FROM channel_message">>, []).
content() -> maps:get(<<"content">>, message()).
profile_value() -> ?F:scalar(<<"SELECT sign FROM public.\"user\" WHERE id=$1">>, [?F:owner()]).
channel_version() -> maps:get(<<"moderation_version">>, message()).
profile_version() ->
    Versions = decode(
        ?F:scalar(<<"SELECT moderation_versions FROM public.\"user\" WHERE id=$1">>, [?F:owner()])
    ),
    maps:get(<<"sign">>, Versions).
reject(Row) ->
    ok = adm_moderation_logic:moderate(
        maps:get(<<"id">>, Row),
        <<"reject">>,
        <<"fixture-review">>,
        ?F:outsider()
    ).
metadata(Row) -> decode(maps:get(<<"metadata">>, Row)).
decode(Value) when is_map(Value) -> Value;
decode(Value) -> jsone:decode(Value).
assert_counts(Messages, Reviews, Published) ->
    ?assertEqual(Messages, ?F:scalar(<<"SELECT count(*) FROM channel_message">>, [])),
    ?assertEqual(Reviews, length(?F:queue())),
    ?assertEqual(Published, maps:get(published, ?F:counts())).
assert_moment(Posts, Reviews) ->
    ?assertEqual(Posts, ?F:scalar(<<"SELECT count(*) FROM moment_post">>, [])),
    ?assertEqual(Posts, ?F:scalar(<<"SELECT count(*) FROM moment_timeline">>, [])),
    ?assertEqual(Reviews, length(?F:queue())),
    ?assertEqual(Posts, maps:get(moment, ?F:counts())).
assert_metadata(Text) ->
    [Row] = ?F:queue(),
    Metadata = metadata(Row),
    ?assertEqual(1, maps:get(<<"schema_version">>, Metadata)),
    ?assertEqual(Text, maps:get(<<"content">>, Row)),
    ?assertEqual(
        binary:encode_hex(crypto:hash(sha256, Text), lowercase),
        maps:get(<<"content_sha256">>, Metadata)
    ),
    case maps:get(<<"msg_type">>, Row) of
        <<"channel_message">> ->
            ?assertEqual(channel_version(), maps:get(<<"content_version">>, Metadata));
        <<"profile_field:sign">> ->
            ?assertEqual(profile_version(), maps:get(<<"content_version">>, Metadata));
        _ ->
            ok
    end,
    Hits = maps:get(<<"hits">>, Metadata),
    ?assert(Hits =/= []),
    lists:foreach(
        fun(Hit) ->
            Trace = maps:get(<<"trace">>, Hit),
            ?assertEqual(?F:export(), maps:get(<<"ruleset_revision">>, Trace)),
            ?assert(is_binary(maps:get(<<"generation">>, Trace))),
            ?assert(maps:is_key(<<"hits_truncated">>, Trace)),
            lists:foreach(
                fun(Key) -> ?assert(maps:is_key(Key, Trace)) end,
                [
                    <<"build_id">>,
                    <<"ruleset_revision">>,
                    <<"generation">>,
                    <<"semantics_id">>,
                    <<"base_rules_digest">>,
                    <<"normalization_id">>,
                    <<"hits_truncated">>,
                    <<"scan_complete">>,
                    <<"fields_checked">>
                ]
            ),
            ?assertEqual(true, maps:get(<<"scan_complete">>, Trace)),
            ?assertEqual(1, maps:get(<<"fields_checked">>, Trace)),
            ?assertEqual(<<"imboy-base-v1">>, maps:get(<<"normalization_id">>, Trace)),
            ?assertEqual(64, byte_size(maps:get(<<"base_rules_digest">>, Trace))),
            lists:foreach(
                fun(Key) -> ?assert(maps:is_key(Key, Hit)) end,
                [<<"field_id">>, <<"rule_id">>, <<"variant_id">>, <<"effective_action">>]
            )
        end,
        Hits
    ).

versions() ->
    decode(
        ?F:scalar(<<"SELECT moderation_versions FROM public.\"user\" WHERE id=$1">>, [?F:owner()])
    ).
moderation_column_count() ->
    ?F:scalar(
        <<
            "SELECT count(*) FROM information_schema.columns "
            "WHERE table_schema='public' AND (table_name,column_name) IN "
            "(('review_queue','metadata'),('channel_message','moderation_version'),('user','moderation_versions'))"
        >>,
        []
    ).
trigger_count() ->
    ?F:scalar(
        <<
            "SELECT count(*) FROM pg_trigger WHERE "
            "tgrelid='public.\"user\"'::regclass AND tgname='textguard_profile_version' AND NOT tgisinternal"
        >>,
        []
    ).
invalid_binding(null_version) ->
    #{<<"schema_version">> => 1, <<"content_version">> => null};
invalid_binding(missing_version) ->
    #{<<"schema_version">> => 1};
invalid_binding(number_version) ->
    #{<<"schema_version">> => 1, <<"content_version">> => 12};
invalid_binding(short_version) ->
    #{<<"schema_version">> => 1, <<"content_version">> => <<"short">>};
invalid_binding(empty_version) ->
    #{<<"schema_version">> => 1, <<"content_version">> => <<>>};
invalid_binding(wrong_schema) ->
    #{<<"schema_version">> => 2, <<"content_version">> => binary:copy(<<"a">>, 32)};
invalid_binding(malformed_json) ->
    <<"{invalid">>;
invalid_binding(json_null) ->
    <<"null">>;
invalid_binding(json_array) ->
    <<"[]">>;
invalid_binding(legacy_empty) ->
    #{};
invalid_binding(legacy_json) ->
    <<"{}">>;
invalid_binding(legacy_missing) ->
    missing.
assert_legacy_removal(channel) ->
    ?assertEqual({1, 0}, ?F:removal_counts()),
    ?assertEqual(-1, maps:get(<<"status">>, message())),
    ?assertEqual(1, maps:get(deleted, ?F:counts()));
assert_legacy_removal(profile) ->
    ?assertEqual({0, 1}, ?F:removal_counts()),
    ?assertEqual(<<>>, profile_value()).
assert_trace(Hits) ->
    lists:foreach(
        fun(Hit) ->
            Trace = maps:get(trace, Hit),
            ?assertEqual(?F:export(), maps:get(ruleset_revision, Trace)),
            ?assert(is_binary(maps:get(generation, Trace))),
            ?assert(maps:is_key(hits_truncated, Trace))
        end,
        Hits
    ).
wire_error(overloaded) -> {429, 8, <<"OVERLOADED">>};
wire_error(not_ready) -> {503, 14, <<"NOT_READY">>};
wire_error(connection_failed) -> {503, 14, <<"CONNECTION_FAILED">>};
wire_error(timeout) -> {504, 4, <<"CHECK_DEADLINE">>};
wire_error(invalid_argument) -> {400, 3, <<"INVALID_ARGUMENT">>};
wire_error(invalid_utf8) -> {400, 3, <<"INVALID_UTF8">>};
wire_error(text_too_large) -> {413, 8, <<"TEXT_TOO_LARGE">>};
wire_error(normalization_limit) -> {422, 8, <<"NORMALIZATION_LIMIT">>};
wire_error(match_budget_exceeded) -> {422, 8, <<"MATCH_BUDGET_EXCEEDED">>};
wire_error(unsupported_ruleset) -> {503, 9, <<"UNSUPPORTED_RULESET">>};
wire_error(protocol_error) -> {502, 15, <<"PROTOCOL_ERROR">>};
wire_error(unauthenticated) -> {401, 16, <<"UNAUTHENTICATED">>};
wire_error(permission_denied) -> {403, 7, <<"PERMISSION_DENIED">>};
wire_error(normalizer_incompatible) -> {500, 13, <<"NORMALIZER_INCOMPATIBLE">>};
wire_error(internal_error) -> {500, 13, <<"INTERNAL_ERROR">>}.
receipt(Case, Outcome) ->
    Row = (maps:with([id, acceptance], Case))#{outcome => Outcome},
    file:write_file(
        filename:join(os:getenv("TG19_EVIDENCE"), "case-results.jsonl"),
        [jsone:encode(Row), <<"\n">>],
        [append]
    ).
