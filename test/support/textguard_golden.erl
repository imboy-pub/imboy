-module(textguard_golden).

-export([rows/0, enhancement/0, cases/0, detection/1, expected/1, request/1]).

rows() ->
    [
        {1001, <<"测试词"/utf8>>, <<"test">>, <<"high">>},
        {1002, <<"demo">>, <<"test">>, <<"medium">>},
        {1003, <<"aba">>, <<"test">>, <<"high">>},
        {1004, <<"σ"/utf8>>, <<"test">>, <<"high">>},
        {1005, <<"i", 16#CC, 16#87>>, <<"test">>, <<"high">>},
        {1006, <<"low">>, <<"test">>, <<"low">>}
    ].

enhancement() ->
    Pin = pin(1001),
    #{
        version => 1,
        semantics_by_scene => #{
            channel_message => <<"enhanced-v1">>,
            moment_post => <<"enhanced-v1">>,
            profile_field => <<"compat-v1">>
        },
        aliases => [
            Pin#{
                variants => [
                    #{
                        variant_id => <<"pinyin-01">>,
                        type => <<"pinyin">>,
                        pattern => <<"ceshici">>,
                        action => <<"review">>,
                        boundary => <<"ascii_word">>
                    }
                ]
            }
        ],
        gap => [Pin#{enabled => true, action => <<"review">>}],
        regex_rules => [
            #{
                id => <<"re:code">>,
                expression => <<"x[0-9]{2}">>,
                display_word => <<"numeric-code">>,
                category => <<"test">>,
                severity => <<"low">>,
                action => <<"review">>,
                scenes => [<<"channel_message">>, <<"moment_post">>, <<"profile_field">>]
            }
        ],
        exceptions => [
            (pin(1002))#{
                id => <<"exc-demo">>,
                phrase => <<"demo report">>,
                scenes => [<<"channel_message">>],
                variant_ids => [<<"canonical">>],
                views => [<<"B">>, <<"T">>]
            },
            (pin(1006))#{
                id => <<"exc-low-base-only">>,
                phrase => <<"low report">>,
                scenes => [<<"channel_message">>],
                variant_ids => [<<"canonical">>],
                views => [<<"B">>]
            }
        ]
    }.

pin(Id) ->
    {Id, Word, _, _} = lists:keyfind(Id, 1, rows()),
    #{
        canonical_id => integer_to_binary(Id),
        canonical_word_sha256 => textguard_export:canonical_word_sha256(Word)
    }.

cases() ->
    [
        c(<<"allow">>, <<"ordinary">>, []),
        c(<<"empty-text">>, <<>>, []),
        c(<<"canonical">>, <<"测试词"/utf8>>, [h(1001, 0, 9)]),
        c(<<"literal-repeat">>, <<"测试词测试词"/utf8>>, [h(1001, 0, 9), h(1001, 9, 18)]),
        c(<<"overlap">>, <<"ababa">>, [h(1003, 0, 3), h(1003, 2, 5)]),
        c(<<"medium-review">>, <<"demo">>, [h(1002, 0, 4)]),
        c(<<"low-review">>, <<"low">>, [h(1006, 0, 3)]),
        c(<<"ascii-case">>, <<"DEMO">>, [h(1002, 0, 4)]),
        c(<<"width-fold">>, <<"ＤＥＭＯ"/utf8>>, [h(1002, 0, 12)]),
        c(<<"utf8-prefix">>, <<"😀测试词"/utf8>>, [h(1001, 4, 13)]),
        c(<<"zero-width">>, <<"测"/utf8, 16#E2, 16#80, 16#8B, "试词"/utf8>>, [h(1001, 0, 12)]),
        c(<<"one-to-many-lowercase">>, <<"İ"/utf8>>, [h(1005, 0, 2)]),
        c(<<"enhanced-final-sigma">>, <<"ΟΣ"/utf8>>, []),
        scene(c(<<"compat-final-sigma">>, <<"ΟΣ"/utf8>>, [h(1004, 2, 4)]), profile_field),
        c(<<"t2s">>, <<"測試詞"/utf8>>, [methods(h(1001, 0, 9), [<<"literal">>, <<"t2s">>])]),
        scene(c(<<"compat-t2s-inert">>, <<"測試詞"/utf8>>, []), profile_field),
        c(<<"gap">>, <<"测 试 词"/utf8>>, [gap(0, 11)]),
        scene(c(<<"compat-gap-inert">>, <<"测 试 词"/utf8>>, []), profile_field),
        c(<<"alias-review">>, <<"ceshici">>, [alias_hit(0, 7)]),
        c(<<"alias-boundary">>, <<"xceshicix">>, []),
        scene(c(<<"compat-alias-inert">>, <<"ceshici">>, []), profile_field),
        c(<<"exception-local">>, <<"demo report demo">>, [h(1002, 12, 16)]),
        c(<<"exception-B-only">>, <<"low report low">>, [
            methods(h(1006, 0, 3), [<<"literal">>, <<"t2s">>]), h(1006, 11, 14)
        ]),
        scene(c(<<"exception-scene">>, <<"demo report">>, [h(1002, 0, 4)]), moment_post),
        scene(c(<<"compat-exception-inert">>, <<"demo report">>, [h(1002, 0, 4)]), profile_field),
        c(<<"regex-first">>, <<"x12 x34">>, [regex_hit(0, 3)]),
        scene(c(<<"compat-regex-inert">>, <<"x12">>, []), profile_field),
        c(<<"strongest-action">>, <<"demo测试词"/utf8>>, [h(1001, 4, 13), h(1002, 0, 4)]),
        multi(),
        scene(c(<<"moment-canonical">>, <<"测试词"/utf8>>, [h(1001, 0, 9)]), moment_post),
        scene(c(<<"profile-canonical">>, <<"测试词"/utf8>>, [h(1001, 0, 9)]), profile_field),
        truncated()
    ].

c(Id, Text, Hits) ->
    #{
        id => Id,
        scene => channel_message,
        fields => [{<<"body">>, Text}],
        hits => Hits,
        truncated => false
    }.

scene(Case, Scene) -> Case#{scene => Scene}.

multi() ->
    (c(<<"multi-field-order">>, <<>>, [
        (h(1001, 0, 9))#{field_id => <<"second">>},
        (h(1002, 0, 4))#{field_id => <<"first">>}
    ]))#{
        fields => [{<<"first">>, <<"demo">>}, {<<"second">>, <<"测试词"/utf8>>}]
    }.

truncated() ->
    (c(<<"truncated-block-witness">>, binary:copy(<<"aba ">>, 101), [
        h(1003, Index * 4, Index * 4 + 3)
     || Index <- lists:seq(0, 99)
    ]))#{
        truncated => true
    }.

h(Id, Start, End) ->
    {Id, Word, Category, Severity} = lists:keyfind(Id, 1, rows()),
    Action =
        case Severity of
            <<"high">> -> <<"block">>;
            _ -> <<"review">>
        end,
    #{
        field_id => <<"body">>,
        rule_id => integer_to_binary(Id),
        variant_id => <<"canonical">>,
        variant_type => <<"canonical">>,
        word => Word,
        category => Category,
        severity => Severity,
        effective_action => Action,
        start_byte => Start,
        end_byte => End,
        match_methods => [<<"literal">>]
    }.

methods(Hit, Methods) -> Hit#{match_methods => Methods}.

gap(Start, End) ->
    (methods(h(1001, Start, End), [<<"literal">>, <<"gap">>]))#{effective_action => <<"review">>}.

alias_hit(Start, End) ->
    (methods(h(1001, Start, End), [<<"alias">>]))#{
        variant_id => <<"pinyin-01">>,
        variant_type => <<"pinyin">>,
        word => <<"ceshici">>,
        effective_action => <<"review">>
    }.

regex_hit(Start, End) ->
    #{
        field_id => <<"body">>,
        rule_id => <<"re:code">>,
        variant_id => <<"regex">>,
        variant_type => <<"regex_rule">>,
        word => <<"numeric-code">>,
        category => <<"test">>,
        severity => <<"low">>,
        effective_action => <<"review">>,
        start_byte => Start,
        end_byte => End,
        match_methods => [<<"regex">>]
    }.

detection(Case) ->
    #{
        request_id => <<"tg19-", (maps:get(id, Case))/binary>>,
        scene => maps:get(scene, Case),
        fields => maps:get(fields, Case)
    }.

request(Detection) ->
    {ok, Scene} = moderation_detector_transport:scene_to_binary(maps:get(scene, Detection)),
    Detection#{
        scene => Scene,
        fields => [
            #{field_id => FieldId, text => Text}
         || {FieldId, Text} <- maps:get(fields, Detection)
        ]
    }.

expected(Case) ->
    Hits = maps:get(hits, Case),
    Decision =
        case
            {Hits, lists:any(fun(Hit) -> maps:get(effective_action, Hit) =:= <<"block">> end, Hits)}
        of
            {[], _} -> allow;
            {_, true} -> block;
            _ -> review
        end,
    Semantics =
        case maps:get(scene, Case) of
            profile_field -> <<"compat-v1">>;
            _ -> <<"enhanced-v1">>
        end,
    #{
        decision => Decision,
        hits => Hits,
        scan_complete => true,
        hits_truncated => maps:get(truncated, Case),
        fields_checked => length(maps:get(fields, Case)),
        semantics_id => Semantics,
        hit_semantics => <<"literal-all-regex-first-v1">>
    }.
