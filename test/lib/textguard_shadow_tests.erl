-module(textguard_shadow_tests).

-include_lib("eunit/include/eunit.hrl").
-export([manifest/0]).
-define(F, textguard_business_fixture).
-define(C, textguard_shadow_cases).

manifest() -> ?C:manifest().

shadow_test_() ->
    {timeout, 600,
        {setup, fun ?C:setup/0, fun ?C:cleanup/1, fun(State) ->
            [
                {
                    binary_to_list(maps:get(id, Case)),
                    {timeout, 20, fun() -> execute(Case, State) end}
                }
             || Case <- manifest()
            ]
        end}}.

execute(Case, State) ->
    ?C:reset(State, maps:get(transport, Case)),
    try
        run(maps:get(kind, Case), maps:get(transport, Case), State),
        ?C:idle(),
        assert_counter_keys(),
        ?assertEqual(0, maps:get(privacy_leak, ?F:counts())),
        ?C:receipt(Case, passed)
    catch
        Class:Reason:Stack ->
            ?C:receipt(Case, failed),
            erlang:raise(Class, Reason, Stack)
    after
        ok = moderation_shadow:set_mode(legacy),
        ?C:idle()
    end.

run(digest_encoding, _, _) ->
    Rows = [
        {<<"z">>, <<>>, <<"low">>},
        {<<"测,试"/utf8>>, <<"类别"/utf8>>, <<"high">>},
        {<<"A">>, <<"test">>, <<"medium">>}
    ],
    Bytes =
        <<"TGBASEv1", 0, 3:32, 1:32, "A", 4:32, "test", 6:32, "medium", 1:32, "z", 0:32, 3:32,
            "low", 7:32, "测,试"/utf8, 6:32, "类别"/utf8, 4:32, "high">>,
    ?assertEqual(hex(crypto:hash(sha256, Bytes)), textguard_base_digest:digest(Rows)),
    ?assertEqual(
        hex(crypto:hash(sha256, <<"TGBASEv1", 0, 0:32>>)),
        textguard_base_digest:digest([])
    ),
    {ok, Export} = textguard_export:export_package(),
    ?assertEqual(maps:get(base_digest, Export), ?C:digest()),
    Response = ?C:real_response(profile_field, fields(<<"ordinary">>)),
    ?assertEqual(?C:digest(), maps:get(base_rules_digest, Response));
run(digest_identity, _, _) ->
    Rows = [{<<"ab">>, <<"c">>, <<"low">>}, {<<"a">>, <<"bc">>, <<"high">>}],
    Base = textguard_base_digest:digest(Rows),
    ?assertEqual(Base, textguard_base_digest:digest(lists:reverse(Rows))),
    lists:foreach(
        fun(Row) ->
            ?assertNotEqual(Base, textguard_base_digest:digest([Row, lists:last(Rows)]))
        end,
        [
            {<<"AB">>, <<"c">>, <<"low">>},
            {<<"ab">>, <<"d">>, <<"low">>},
            {<<"ab">>, <<"c">>, <<"high">>}
        ]
    ),
    ?assertNotEqual(
        textguard_base_digest:digest([{<<"ab">>, <<"c">>, <<"low">>}]),
        textguard_base_digest:digest([{<<"a">>, <<"bc">>, <<"low">>}])
    );
run(classifier_matrix, _, _) ->
    classifier_matrix();
run(mode_defaults, _, State) ->
    {ok, Conf} = application:get_env(imboy, moderation_detector),
    ok = gen_server:stop(?C:worker(State), normal, 2000),
    application:unset_env(imboy, moderation_detector),
    try
        ok = ?C:start_owned(State),
        ?assertMatch(#{mode := legacy, queued := 0, running := 0}, moderation_shadow:status()),
        ?assertEqual(allow, moderation_business:inspect_profile(<<"sign">>, <<"ordinary">>)),
        ?assertEqual(#{}, ?C:counters()),
        ?assertEqual(
            dropped,
            moderation_shadow:submit(
                profile_field,
                fields(<<"ordinary">>),
                snapshot(allow)
            )
        )
    after
        ok = application:set_env(imboy, moderation_detector, Conf),
        ok = moderation_shadow:set_mode(shadow)
    end;
run(unavailable_snapshot, _, _) ->
    meck:new(sensitive_word_repo, [passthrough, no_link, no_history]),
    meck:expect(sensitive_word_repo, all, fun() -> {error, fixture_wordlist_unavailable} end),
    try
        imboy_cache:flush({moderation_sensitive_words}),
        ?assertEqual(
            {allow, unavailable},
            moderation_policy:inspect_snapshot(profile_field, fields(<<"ordinary">>))
        ),
        ?assertEqual(
            legacy_unavailable,
            moderation_shadow:classify(unavailable, {ok, response(allow)})
        ),
        Before = ?C:counter(legacy_unavailable),
        ?assertEqual(
            accepted,
            moderation_shadow:submit(
                profile_field,
                fields(<<"ordinary">>),
                unavailable
            )
        ),
        ?C:await(fun() -> ?C:counter(legacy_unavailable) =:= Before + 1 end)
    after
        meck:unload(sensitive_word_repo),
        imboy_cache:flush({moderation_sensitive_words})
    end;
run({business, Scene, Decision}, Transport, State) ->
    business(Scene, Decision, Transport, State);
run(compat_vectors, _, _) ->
    lists:foreach(
        fun(Text) ->
            Snapshot = ?C:snapshot(profile_field, fields(Text)),
            ?assertEqual(?C:digest(), maps:get(base_rules_digest, Snapshot)),
            Response = ?C:real_response(profile_field, fields(Text)),
            ?assertEqual(comparable_match, moderation_shadow:classify(Snapshot, {ok, Response})),
            ?C:expect_category(profile_field, fields(Text), comparable_match)
        end,
        [
            <<"ordinary">>,
            <<"DEMO">>,
            <<"ＤＥＭＯ"/utf8>>,
            <<"测"/utf8, 16#200B/utf8, "试词"/utf8>>,
            <<"İ"/utf8>>,
            <<"ΟΣ"/utf8>>
        ]
    ),
    Multiple = [{<<"first">>, <<"demo">>}, {<<"second">>, <<"测试词"/utf8>>}],
    ?assertEqual(block, maps:get(decision, ?C:snapshot(profile_field, Multiple))),
    ?C:expect_category(profile_field, Multiple, comparable_match);
run(compat_inert, _, _) ->
    lists:foreach(
        fun(Text) ->
            Response = ?C:real_response(profile_field, fields(Text)),
            ?assertEqual(<<"compat-v1">>, maps:get(semantics_id, Response)),
            ?assertEqual(
                maps:get(decision, ?C:snapshot(profile_field, fields(Text))),
                maps:get(decision, Response)
            ),
            ?C:expect_category(profile_field, fields(Text), comparable_match)
        end,
        [
            <<"ceshici">>,
            <<"测 试 词"/utf8>>,
            <<"測試詞"/utf8>>,
            <<"x12">>,
            <<"demo report">>
        ]
    );
run(cached_version, _, _) ->
    cached_version();
run(package_identity, _, _) ->
    package_identity();
run(enhanced_witnesses, _, _) ->
    ?C:with_enhanced(fun() ->
        lists:foreach(
            fun({Text, Expected}) ->
                Snapshot = ?C:snapshot(channel_message, fields(Text)),
                Response = ?C:real_response(channel_message, fields(Text)),
                ?assertEqual(<<"enhanced-v1">>, maps:get(semantics_id, Response)),
                ?assertEqual(Expected, moderation_shadow:classify(Snapshot, {ok, Response})),
                ?C:expect_category(channel_message, fields(Text), Expected)
            end,
            [
                {<<"ceshici">>, enhanced_new_hit},
                {<<"測試詞"/utf8>>, enhanced_new_hit},
                {<<"测 试 词"/utf8>>, enhanced_new_hit},
                {<<"x12">>, enhanced_new_hit},
                {<<"demo report">>, enhanced_difference},
                {<<"ordinary">>, enhanced_observation}
            ]
        )
    end);
run(private_exclusion, _, _) ->
    Before = ?C:counters(),
    ?F:sql(<<"UPDATE channel SET visibility=1 WHERE id=$1">>, [?F:channel()]),
    ?assertMatch({ok, _}, publish(<<"ordinary">>)),
    ?assertMatch(
        {ok, _},
        moment_logic:create_post(
            ?F:owner(),
            #{<<"content">> => <<"ordinary">>, <<"visibility">> => 2}
        )
    ),
    ok = ?F:private_message(c2c),
    ok = ?F:private_message(c2g),
    pause(80),
    ?C:idle(),
    ?assertEqual(Before, ?C:counters());
run(facility_error, Transport, _) ->
    {HttpStatus, GrpcStatus, Code} = {429, 8, <<"OVERLOADED">>},
    Status =
        case Transport of
            http -> HttpStatus;
            grpc -> GrpcStatus
        end,
    ?F:remote_fault(Transport, Status, Code, fun(Count) ->
        ok = moderation_shadow:set_mode(shadow),
        Before = ?C:counter(facility_failure),
        ?assertMatch({ok, _}, publish(<<"ordinary">>)),
        ?C:await(fun() -> ?C:counter(facility_failure) =:= Before + 1 end),
        ?C:idle(),
        ?assertEqual(1, Count()),
        ?assertEqual([], ?F:queue()),
        assert_published(<<"ordinary">>)
    end),
    ?F:remote_hang(Transport, availability_compat, fun(Count) ->
        ok = moderation_shadow:set_mode(shadow),
        ?C:expect_category(profile_field, fields(<<"ordinary">>), facility_failure),
        pause(80),
        ?assertEqual(1, Count())
    end);
run(nonblocking, Transport, _) ->
    ?C:with_hang(Transport, fun(Count) ->
        Started = erlang:monotonic_time(millisecond),
        ?assertMatch({ok, _}, publish(<<"ordinary">>)),
        ?assert(erlang:monotonic_time(millisecond) - Started < 800),
        assert_published(<<"ordinary">>),
        ?C:await(fun() -> Count() =:= 1 end),
        ?assertEqual(1, maps:get(running, moderation_shadow:status())),
        ok = moderation_shadow:set_mode(legacy),
        ?C:idle(),
        pause(80),
        ?assertEqual(1, Count())
    end);
run(queue_bounds, Transport, _) ->
    ?C:with_hang(Transport, fun(Count) ->
        fill_running(Count),
        Snapshot = ?C:snapshot(profile_field, fields(<<"ordinary">>)),
        lists:foreach(
            fun(_) ->
                ?assertEqual(
                    accepted,
                    moderation_shadow:submit(profile_field, fields(<<"ordinary">>), Snapshot)
                )
            end,
            lists:seq(1, 64)
        ),
        ?assertMatch(#{queued := 64, running := 2}, moderation_shadow:status()),
        Before = ?C:counter(queue_full),
        ?assertEqual(
            dropped,
            moderation_shadow:submit(
                profile_field,
                fields(<<"ordinary">>),
                Snapshot
            )
        ),
        ?assertEqual(Before + 1, ?C:counter(queue_full)),
        ?assertEqual(2, Count()),
        ok = moderation_shadow:set_mode(legacy),
        ?C:idle()
    end);
run(admission_ttl, Transport, State) ->
    ?C:with_hang(Transport, fun(Count) ->
        Pid = ?C:worker(State),
        Snapshot = ?C:snapshot(profile_field, fields(<<"ordinary">>)),
        Before = ?C:counter(expired),
        ok = sys:suspend(Pid),
        try
            AdmittedAt = erlang:monotonic_time(millisecond),
            lists:foreach(
                fun(_) ->
                    ?assertEqual(
                        accepted,
                        moderation_shadow:submit(profile_field, fields(<<"ordinary">>), Snapshot)
                    )
                end,
                lists:seq(1, 64)
            ),
            AdmissionEnded = erlang:monotonic_time(millisecond),
            Jobs = ets:match_object(moderation_shadow_samples, {{job, '_'}, '_'}),
            ?assertEqual(64, length(Jobs)),
            ?assert(
                lists:all(
                    fun({_, {_, Deadline, _, _, _}}) ->
                        Deadline >= AdmittedAt + 1000 andalso Deadline =< AdmissionEnded + 1000
                    end,
                    Jobs
                )
            ),
            pause(1100)
        after
            ok = sys:resume(Pid)
        end,
        ?C:await(fun() -> ?C:counter(expired) =:= Before + 64 end),
        ?C:idle(),
        ?assertEqual(0, Count())
    end);
run(running_ttl, Transport, _) ->
    ?C:with_hang(Transport, fun(Count) ->
        Before = ?C:counter(expired),
        Started = erlang:monotonic_time(millisecond),
        fill_running(Count),
        ?C:await(fun() -> ?C:counter(expired) =:= Before + 2 end),
        ?C:idle(),
        ?assert(erlang:monotonic_time(millisecond) - Started < 1900),
        pause(80),
        ?assertEqual(2, Count())
    end);
run(mode_legacy, Transport, State) ->
    mode_switch(Transport, legacy, State);
run(mode_rust, Transport, State) ->
    mode_switch(Transport, rust, State);
run(input_bounds, Transport, _) ->
    input_bounds(Transport);
run(mailbox_privacy, _, State) ->
    mailbox_privacy(State);
run(grpc_log_filter, _, _) ->
    grpc_log_filter();
run(grpc_stream_lifecycle, grpc, State) ->
    ?C:grpc_stream_lifecycle(State);
run(grpc_guardian_failure, grpc, State) ->
    textguard_shadow_faults:run(State);
run(grpc_coordinator_restart, grpc, State) ->
    textguard_shadow_restart:run(State);
run(grpc_total_deadline, grpc, State) ->
    textguard_shadow_restart:deadline(State);
run(grpc_connection_failure, grpc, State) ->
    textguard_shadow_restart:connection_failure(State);
run(worker_unavailable, Transport, State) ->
    ?C:with_hang(Transport, fun(Count) ->
        fill_running(Count),
        Children = ?C:running_workers(State),
        ?assertEqual(2, length(Children)),
        Before = ?C:counter(worker_unavailable),
        exit(hd(Children), kill),
        ?C:await(fun() -> ?C:counter(worker_unavailable) =:= Before + 1 end),
        ?assertEqual(1, maps:get(running, moderation_shadow:status())),
        pause(80),
        ?assertEqual(2, Count()),
        ok = moderation_shadow:set_mode(legacy),
        ?C:idle()
    end),
    Pid = ?C:worker(State),
    ok = gen_server:stop(Pid, normal, 2000),
    try
        ?assertEqual(
            dropped,
            moderation_shadow:submit(
                profile_field,
                fields(<<"ordinary">>),
                snapshot(allow)
            )
        )
    after
        ?C:restart(State)
    end,
    ?F:config([{transport, Transport}]),
    ok = moderation_shadow:set_mode(shadow),
    ?C:expect_category(profile_field, fields(<<"ordinary">>), comparable_match).

grpc_log_filter() ->
    Token = <<"TG21-TOKEN-CANARY">>,
    Body = ?F:canary(),
    Report = #{
        label => {gen_statem, terminate},
        nested => [{state, #{service => 'imboy.textguard.v1.TextGuard'}}],
        reason => {Token, Body}
    },
    Event = #{
        level => error,
        msg => {report, Report},
        extra => Token,
        meta => #{report_cb => Token, error_logger => Body}
    },
    lists:foreach(
        fun(Family) ->
            ?C:filter_sender(fun() -> privacy_chain(Family, Report, Event, [Token, Body]) end)
        end,
        [gen_statem, proc_lib, supervisor]
    ),
    ?C:filter_sender(fun() ->
        lists:foreach(
            fun(Other) ->
                ?assert(textguard_grpc_call:filter_log(Other, []) =:= ignore)
            end,
            [
                Event#{
                    msg => {report, #{label => {gen_statem, terminate}, service => 'other.Service'}}
                },
                Event#{msg => {report, Report#{label => {gen_server, terminate}}}},
                Event#{msg => {string, Body}},
                Event#{msg => {report, #{label => ordinary}}}
            ]
        )
    end).

privacy_chain(Family, Report, Event, [_, Body] = Sensitive) ->
    ?C:assert_private_event(Event#{msg => {report, Report#{label => {Family, crash}}}}, Sensitive),
    lists:foreach(
        fun(Next) ->
            Followup = Event#{msg => {report, #{label => {Next, crash}, body => Body}}},
            case Family of
                gen_statem -> ?C:assert_private_event(Followup, Sensitive);
                _ -> ?assert(textguard_grpc_call:filter_log(Followup, []) =:= ignore)
            end
        end,
        [proc_lib, supervisor, gen_statem]
    ),
    ?assert(textguard_grpc_call:filter_log(Event#{msg => {string, Body}}, []) =:= ignore).

assert_counter_keys() ->
    Allowed = [
        comparable_match,
        comparable_difference,
        version_incomparable,
        normalization_incomparable,
        enhanced_new_hit,
        enhanced_observation,
        enhanced_difference,
        semantics_incomparable,
        legacy_unavailable,
        facility_failure,
        queue_full,
        expired,
        mode_switch,
        oversize,
        worker_unavailable
    ],
    ?assert(
        lists:all(
            fun(Key) -> is_atom(Key) andalso lists:member(Key, Allowed) end,
            maps:keys(?C:counters())
        )
    ).

classifier_matrix() ->
    Same = snapshot(allow),
    Base = response(allow),
    Cases = [
        {Same, {ok, Base}, comparable_match},
        {Same, {ok, response(block)}, comparable_difference},
        {snapshot(review), {ok, response(allow)}, comparable_difference},
        {snapshot(block), {ok, response(block)}, comparable_match},
        {Same, {ok, Base#{base_rules_digest => binary:copy(<<"b">>, 64)}}, version_incomparable},
        {Same, {ok, Base#{normalization_id => <<"future-normalizer">>}},
            normalization_incomparable},
        {Same, {ok, Base#{semantics_id => <<"future-semantics">>}}, semantics_incomparable},
        {unavailable, {ok, Base}, legacy_unavailable},
        {Same, {ok, enhanced(allow, [])}, enhanced_observation},
        {Same, {ok, enhanced(review, [#{variant_type => <<"pinyin">>}])}, enhanced_new_hit},
        {Same, {ok, enhanced(review, [#{match_methods => [<<"gap">>]}])}, enhanced_new_hit},
        {Same, {ok, enhanced(review, [#{match_methods => [<<"t2s">>]}])}, enhanced_new_hit},
        {Same, {ok, enhanced(review, [#{match_methods => [<<"regex">>]}])}, enhanced_new_hit},
        {Same,
            {ok,
                enhanced(review, [
                    #{
                        variant_type => <<"canonical">>,
                        match_methods => [<<"literal">>]
                    }
                ])},
            enhanced_difference},
        {snapshot(review), {ok, enhanced(allow, [])}, enhanced_difference},
        {
            snapshot(review),
            {ok, enhanced(block, [#{variant_type => <<"pinyin">>}])},
            enhanced_difference
        },
        {Same,
            {ok, (enhanced(review, [#{variant_type => <<"pinyin">>}]))#{
                base_rules_digest => binary:copy(<<"b">>, 64)
            }},
            version_incomparable},
        {Same, {ok, Base#{ruleset_revision => binary:copy(<<"d">>, 64), generation => <<"999">>}},
            comparable_match}
    ],
    lists:foreach(
        fun({Snapshot, Result, Expected}) ->
            ?assertEqual(Expected, moderation_shadow:classify(Snapshot, Result))
        end,
        Cases
    ),
    lists:foreach(
        fun(Reason) ->
            ?assertEqual(facility_failure, moderation_shadow:classify(Same, {error, Reason}))
        end,
        [
            timeout,
            overloaded,
            not_ready,
            connection_failed,
            unauthenticated,
            permission_denied,
            protocol_error,
            normalizer_incompatible,
            internal
        ]
    ).

snapshot(Decision) ->
    #{
        decision => Decision,
        base_rules_digest => binary:copy(<<"a">>, 64),
        semantics_id => <<"compat-v1">>,
        normalization_id => <<"imboy-base-v1">>
    }.
response(Decision) ->
    (snapshot(Decision))#{
        hits => [],
        ruleset_revision => binary:copy(<<"c">>, 64),
        generation => <<"1">>,
        scan_complete => true,
        fields_checked => 1,
        hits_truncated => false
    }.
enhanced(Decision, Hits) ->
    (response(Decision))#{semantics_id => <<"enhanced-v1">>, hits => Hits}.

business(Scene, Decision, Transport, State) ->
    Text =
        case Decision of
            allow -> <<"ordinary">>;
            review -> <<"demo">>;
            block -> <<"测试词"/utf8>>
        end,
    ok = moderation_shadow:set_mode(legacy),
    LegacyResult = result(business_call(Scene, Text)),
    LegacyEffects = effects(Scene),
    ?C:reset(State, Transport),
    Snapshot = ?C:snapshot(Scene, fields(Text)),
    ?assertEqual(Decision, maps:get(decision, Snapshot)),
    ?assertEqual(?C:digest(), maps:get(base_rules_digest, Snapshot)),
    Before = ?C:counter(comparable_match),
    ?assertEqual(LegacyResult, result(business_call(Scene, Text))),
    ?assertEqual(LegacyEffects, effects(Scene)),
    ?C:await(fun() -> ?C:counter(comparable_match) =:= Before + 1 end),
    ?C:idle(),
    ?assertEqual(LegacyEffects, effects(Scene)).

business_call(channel_message, Text) ->
    publish(Text);
business_call(moment_post, Text) ->
    moment_logic:create_post(?F:owner(), #{<<"content">> => Text, <<"visibility">> => 0});
business_call(profile_field, Text) ->
    user_logic:update(?F:owner(), <<"sign">>, Text).
result({ok, _}) -> ok;
result({error, Message}) -> {error, Message}.

effects(Scene) ->
    Content =
        case Scene of
            channel_message ->
                ?F:sql(<<"SELECT content FROM channel_message ORDER BY id">>, []);
            moment_post ->
                ?F:sql(<<"SELECT content FROM moment_post ORDER BY id">>, []);
            profile_field ->
                ?F:sql(<<"SELECT sign FROM public.\"user\" WHERE id=$1">>, [?F:owner()])
        end,
    Queue = [
        maps:with([<<"content">>, <<"hit_words">>, <<"review_status">>], Row)
     || Row <- ?F:queue()
    ],
    #{content => Content, queue => Queue, notifications => ?F:notifications()}.

cached_version() ->
    Fields = fields(<<"demo">>),
    Snapshot = ?C:snapshot(profile_field, Fields),
    OldDigest = maps:get(base_rules_digest, Snapshot),
    ?assertEqual(?C:digest(), OldDigest),
    try
        ?F:sql(<<"UPDATE sensitive_word SET category='tg21-new-category' WHERE id=1002">>, []),
        ?F:republish(),
        Response = ?C:real_response(profile_field, Fields),
        ?assertNotEqual(OldDigest, maps:get(base_rules_digest, Response)),
        ?assertEqual(Snapshot, ?C:snapshot(profile_field, Fields)),
        ?assertEqual(version_incomparable, moderation_shadow:classify(Snapshot, {ok, Response})),
        ?C:expect_category(profile_field, Fields, version_incomparable),
        imboy_cache:flush({moderation_sensitive_words}),
        Current = ?C:snapshot(profile_field, Fields),
        ?assertEqual(maps:get(base_rules_digest, Response), maps:get(base_rules_digest, Current)),
        ?C:expect_category(profile_field, Fields, comparable_match)
    after
        ok = moderation_shadow:set_mode(legacy),
        ?F:sql(<<"UPDATE sensitive_word SET category='test' WHERE id=1002">>, []),
        ?F:republish(),
        imboy_cache:flush({moderation_sensitive_words}),
        ok = moderation_shadow:set_mode(shadow)
    end.

package_identity() ->
    Fields = fields(<<"ordinary">>),
    Before = ?C:real_response(profile_field, Fields),
    Path = os:getenv("TG19_ENH_FILE"),
    {ok, Original} = file:read_file(Path),
    Enhancement = jsone:decode(Original),
    [First | Rest] = maps:get(<<"exceptions">>, Enhancement),
    Changed = Enhancement#{
        <<"exceptions">> =>
            [First#{<<"phrase">> => <<"demo harmless-report">>} | Rest]
    },
    try
        ok = file:write_file(Path, jsone:encode(Changed)),
        ?F:republish(),
        After = ?C:real_response(profile_field, Fields),
        ?assertNotEqual(maps:get(ruleset_revision, Before), maps:get(ruleset_revision, After)),
        ?assert(
            binary_to_integer(maps:get(generation, After)) >
                binary_to_integer(maps:get(generation, Before))
        ),
        ?assertEqual(maps:get(base_rules_digest, Before), maps:get(base_rules_digest, After)),
        ?C:expect_category(profile_field, Fields, comparable_match)
    after
        ok = moderation_shadow:set_mode(legacy),
        ok = file:write_file(Path, Original),
        ?F:republish(),
        ok = moderation_shadow:set_mode(shadow)
    end.

fill_running(Count) ->
    Snapshot = ?C:snapshot(profile_field, fields(<<"ordinary">>)),
    lists:foreach(
        fun(_) ->
            ?assertEqual(
                accepted,
                moderation_shadow:submit(profile_field, fields(<<"ordinary">>), Snapshot)
            )
        end,
        [first, second]
    ),
    ?C:await(fun() ->
        maps:get(running, moderation_shadow:status()) =:= 2 andalso Count() =:= 2
    end).

mode_switch(Transport, Mode, State) ->
    ?C:with_hang(Transport, fun(Count) ->
        fill_running(Count),
        Snapshot = ?C:snapshot(profile_field, fields(<<"ordinary">>)),
        lists:foreach(
            fun(_) ->
                ?assertEqual(
                    accepted,
                    moderation_shadow:submit(profile_field, fields(<<"ordinary">>), Snapshot)
                )
            end,
            [first, second, third]
        ),
        ?assertMatch(#{queued := 3, running := 2}, moderation_shadow:status()),
        Children = ?C:running_workers(State),
        ?assertEqual(2, length(Children)),
        Before = ?C:counter(mode_switch),
        Categories = classification_counts(),
        ok = moderation_shadow:set_mode(Mode),
        ?C:idle(),
        ?assert(lists:all(fun(Child) -> not is_process_alive(Child) end, Children)),
        ?assertEqual(Mode, maps:get(mode, moderation_shadow:status())),
        ?assertEqual(Before + 5, ?C:counter(mode_switch)),
        {ok, Conf} = application:get_env(imboy, moderation_detector),
        ?assertEqual(Mode, proplists:get_value(mode, Conf)),
        pause(80),
        ?assertEqual(Categories, classification_counts()),
        ?assertEqual(2, Count())
    end),
    ?F:config([{transport, Transport}]),
    ok = moderation_shadow:set_mode(Mode),
    case Mode of
        legacy ->
            Before = ?C:counters(),
            ?assertMatch({error, _}, publish(<<"测试词"/utf8>>)),
            pause(50),
            ?assertEqual(Before, ?C:counters());
        rust ->
            ?C:with_enhanced(fun() ->
                ok = moderation_shadow:set_mode(rust),
                Before = ?C:counters(),
                ?assertMatch({error, _}, publish(<<"測試詞"/utf8>>)),
                ?assertEqual([], ?F:queue()),
                ?assertEqual(Before, ?C:counters())
            end)
    end,
    ok = moderation_shadow:set_mode(shadow),
    ?C:expect_category(profile_field, fields(<<"ordinary">>), comparable_match).

classification_counts() ->
    maps:with(
        [
            comparable_match,
            comparable_difference,
            version_incomparable,
            normalization_incomparable,
            enhanced_new_hit,
            enhanced_observation,
            enhanced_difference,
            semantics_incomparable,
            legacy_unavailable,
            facility_failure
        ],
        ?C:counters()
    ).

input_bounds(Transport) ->
    ?C:with_hang(Transport, fun(Count) ->
        Snapshot = ?C:snapshot(profile_field, fields(<<"ordinary">>)),
        Invalid = [
            [],
            fields(binary:copy(<<"a">>, 16385)),
            [{integer_to_binary(Index), <<"a">>} || Index <- lists:seq(1, 9)],
            [{integer_to_binary(Index), binary:copy(<<"a">>, 12000)} || Index <- [1, 2, 3]]
        ],
        Before = ?C:counter(oversize),
        lists:foreach(
            fun(Fields) ->
                ?assertEqual(dropped, moderation_shadow:submit(profile_field, Fields, Snapshot))
            end,
            Invalid
        ),
        ?assertEqual(Before + length(Invalid), ?C:counter(oversize)),
        ?assertEqual(0, Count()),
        ?assertMatch(#{queued := 0, running := 0}, moderation_shadow:status()),
        lists:foreach(
            fun(Fields) ->
                ?assertEqual(accepted, moderation_shadow:submit(profile_field, Fields, Snapshot))
            end,
            [
                fields(binary:copy(<<"a">>, 16384)),
                [
                    {<<"first">>, binary:copy(<<"a">>, 16384)},
                    {<<"second">>, binary:copy(<<"a">>, 16384)}
                ],
                [{integer_to_binary(Index), binary:copy(<<"a">>, 4096)} || Index <- lists:seq(1, 8)]
            ]
        ),
        ok = moderation_shadow:set_mode(legacy),
        ?C:idle()
    end).

mailbox_privacy(State) ->
    Pid = ?C:worker(State),
    Canary = ?F:canary(),
    Snapshot = ?C:snapshot(profile_field, fields(Canary)),
    ok = sys:suspend(Pid),
    try
        ?assertEqual(accepted, moderation_shadow:submit(profile_field, fields(Canary), Snapshot)),
        lists:foreach(
            fun(_) ->
                ?assertEqual(
                    dropped,
                    moderation_shadow:submit(
                        profile_field,
                        fields(binary:copy(Canary, 1000)),
                        Snapshot
                    )
                )
            end,
            lists:seq(1, 16)
        ),
        {messages, Messages} = process_info(Pid, messages),
        ?assertEqual(nomatch, binary:match(term_to_binary(Messages), Canary))
    after
        ok = sys:resume(Pid)
    end,
    ?C:idle(),
    ?assertEqual(0, maps:get(privacy_leak, ?F:counts())).

fields(Text) -> [{<<"content">>, Text}].
publish(Text) ->
    channel_logic_message:publish_message(
        ?F:owner(),
        integer_to_binary(?F:channel()),
        Text,
        <<"text">>,
        #{},
        <<"tg21-idempotency">>
    ).
assert_published(Text) ->
    ?assertEqual(Text, ?F:scalar(<<"SELECT content FROM channel_message">>, [])),
    ?assertEqual(1, maps:get(published, ?F:counts())).
hex(Bytes) -> binary:encode_hex(Bytes, lowercase).
pause(Milliseconds) -> receive
    after Milliseconds -> ok
    end.
