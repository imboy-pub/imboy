-module(adm_cs_ai_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("generated/imboy_product_features.hrl").

-ifdef(IMBOY_FEATURE_CUSTOMER_SERVICE).

response_and_preview_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(atom_and_binary_ids_preserve_payload()) end,
        fun(_) -> ?_test(turns_preserve_faq_ids_and_numeric_fields()) end,
        fun(_) -> ?_test(preview_forwards_confidence_and_config()) end
    ]}.

setup() ->
    [ok = meck:new(Mod, [non_strict, no_link]) || Mod <- mock_modules()],
    ok = meck:expect(adm_acl, ensure_permission, fun(_, _, _) -> ok end),
    ok = meck:expect(cowboy_req, method, fun(Req) -> maps:get(method, Req, <<"GET">>) end),
    ok = meck:expect(cowboy_req, parse_qs, fun(_) -> [{<<"org_id">>, <<"11">>}] end),
    ok = meck:expect(elib_response, success, fun(Req, Data) ->
        Req#{response_status => 200, payload => Data}
    end),
    ok = meck:expect(elib_param, int, fun
        (session_id, _, _) -> {ok, 99};
        (workspace_id, _, _) -> {ok, 22};
        (limit, _, _) -> {ok, 200}
    end).

mock_modules() ->
    [adm_acl, cowboy_req, elib_response, elib_param, customer_service_facade].

cleanup(_) ->
    [meck:unload(Mod) || Mod <- mock_modules()],
    ok.

call(Action, Req) ->
    {ok, Resp, #{adm_user_id := 1}} =
        adm_cs_ai_handler:init(Req, #{action => Action, adm_user_id => 1}),
    ?assertEqual(200, maps:get(response_status, Resp)),
    maps:get(payload, Resp).

atom_and_binary_ids_preserve_payload() ->
    Id = 9007199254740993,
    Config = #{
        id => Id,
        <<"seat_identity_id">> => Id,
        config_version => 3,
        created_at => 1700000000,
        keywords => [<<"refund">>, <<"help">>],
        detail => #{workspace_id => Id},
        <<"unknown_numeric_field">> => 9
    },
    ok = meck:expect(customer_service_facade, ai_config_view, fun(11, #{}) ->
        {ok, Config}
    end),
    Expected = Config#{
        id := integer_to_binary(Id),
        <<"seat_identity_id">> := integer_to_binary(Id),
        detail := #{workspace_id => integer_to_binary(Id)}
    },
    ?assertEqual(Expected, call(config_view, #{})).

turns_preserve_faq_ids_and_numeric_fields() ->
    Id = 9007199254740993,
    Rows = [
        #{
            faq_ids => [Id, <<"99">>],
            seq => 7,
            latency_ms => 35,
            model_reported_confidence => 0.9,
            created_at => 1700000000,
            detail => #{
                <<"faq_ids">> => [Id],
                keywords => [<<"refund">>],
                numbers => [1, 2],
                evidence => [#{<<"id">> => Id}, null]
            }
        }
    ],
    ok = meck:expect(customer_service_facade, ai_session_turns, fun(
        11, #{session_id := 99, workspace_id := 22, limit := 200}
    ) ->
        {ok, Rows}
    end),
    [Turn] = call(session_turns, #{}),
    ?assertEqual([integer_to_binary(Id), <<"99">>], maps:get(faq_ids, Turn)),
    ?assertEqual(7, maps:get(seq, Turn)),
    ?assertEqual(35, maps:get(latency_ms, Turn)),
    ?assertEqual(0.9, maps:get(model_reported_confidence, Turn)),
    ?assertEqual(1700000000, maps:get(created_at, Turn)),
    ?assertEqual(
        #{
            <<"faq_ids">> => [integer_to_binary(Id)],
            keywords => [<<"refund">>],
            numbers => [1, 2],
            evidence => [#{<<"id">> => integer_to_binary(Id)}, null]
        },
        maps:get(detail, Turn)
    ).

preview_forwards_confidence_and_config() ->
    Data = #{
        <<"org_id">> => <<"11">>,
        <<"text">> => <<"hello">>,
        <<"confidence">> => 0.7,
        <<"resolved">> => true,
        <<"config">> => #{
            <<"threshold">> => 0.6,
            <<"max_unresolved_turns">> => 4,
            <<"faq_min_score">> => 0.3,
            <<"unknown">> => 9
        }
    },
    ok = meck:expect(cowboy_req, read_body, fun(Req) ->
        {ok, jsone:encode(Data), Req}
    end),
    ok = meck:expect(customer_service_facade, ai_preview, fun(11, Params) ->
        ?assertEqual(
            #{
                text => <<"hello">>,
                confidence => 0.7,
                resolved => true,
                config => #{threshold => 0.6, max_unresolved_turns => 4, faq_min_score => 0.3}
            },
            Params
        ),
        cs_ai_admin_app:preview(11, Params)
    end),
    ?assertMatch(
        #{action := auto_reply, reason := <<"confident_reply">>},
        call(preview, #{method => <<"POST">>})
    ).

-endif.
