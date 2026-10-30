-module(attachment_w403_http_fixture_request_proof_tests).
-include_lib("eunit/include/eunit.hrl").

binds_only_the_exact_observed_request_test() ->
    Table = ets:new(attachment_w403_http_evidence, [named_table, public]),
    try
        Leaf = <<"leaf">>,
        Id = <<"w403-request">>,
        Other = <<"control">>,
        true = ets:insert(Table, [
            {{request, Leaf, Id}, #{status => 410}},
            {{request, Leaf, Other}, #{status => 200}}
        ]),
        ok = attachment_w403_http_fixture:bind_request(
            #{leaf => Leaf}, #{request_id => Id}, <<"owned/key">>, put
        ),
        [{{request, Leaf, Id}, Proof}] = ets:lookup(Table, {request, Leaf, Id}),
        ?assertEqual(<<"owned/key">>, maps:get(object_key, Proof)),
        ?assertEqual(put, maps:get(action, Proof)),
        [{{request, Leaf, Other}, Control}] = ets:lookup(Table, {request, Leaf, Other}),
        ?assertNot(maps:is_key(object_key, Control)),
        ?assertError(
            {badmatch, true},
            attachment_w403_http_fixture:bind_request(
                #{leaf => Leaf}, #{request_id => Id}, <<"other/key">>, put
            )
        ),
        ?assertError(
            {badmatch, []},
            attachment_w403_http_fixture:bind_request(
                #{leaf => <<"other-leaf">>}, #{request_id => Id}, <<"owned/key">>, put
            )
        )
    after
        ets:delete(Table)
    end.
