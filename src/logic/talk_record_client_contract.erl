-module(talk_record_client_contract).
-moduledoc "Pure BYOK wire validation; no credentials, endpoints or provider side effects.".
-export([request/2, plan/3, result/4, hash/1, allowed/2, constant_equal/2]).

request(Body, Public) ->
    checked(fun() ->
        true = allowed(Body, [
            <<"mode">>,
            <<"execution_mode">>,
            <<"provider">>,
            <<"model">>,
            <<"policy_version">>,
            <<"config_version">>,
            <<"processing_consent">>
        ]),
        <<"client_byok">> = maps:get(<<"execution_mode">>, Body),
        Kind = maps:get(<<"mode">>, Body),
        {Provider, Model} = provider(Kind, Public),
        Provider = maps:get(<<"provider">>, Body),
        Model = maps:get(<<"model">>, Body),
        PV = maps:get(<<"policy_version">>, Public),
        PV = maps:get(<<"policy_version">>, Body),
        CV = maps:get(<<"config_version">>, Public),
        CV = maps:get(<<"config_version">>, Body),
        #{<<"confirmed">> := true} = Consent = maps:get(<<"processing_consent">>, Body),
        true = allowed(Consent, [
            <<"confirmed">>,
            <<"execution_mode">>,
            <<"provider">>,
            <<"model">>,
            <<"policy_version">>,
            <<"config_version">>
        ]),
        true =
            maps:without([<<"mode">>, <<"processing_consent">>], Body) =:=
                maps:remove(<<"confirmed">>, Consent),
        #{
            kind => Kind,
            provider => Provider,
            model => Model,
            policy_version => PV,
            config_version => CV
        }
    end).

provider(Kind, Public) ->
    Selected = maps:get(Kind, maps:get(<<"providers">>, Public)),
    true = maps:get(<<"available">>, Selected, true),
    Provider = maps:get(<<"provider">>, Selected),
    Model = maps:get(<<"model">>, Selected),
    true = lists:member({Kind, Provider, Model}, [
        {<<"transcribe">>, <<"minimax">>, <<"asr-1.0">>},
        {<<"transcribe">>, <<"doubao">>, <<"volc.seedasr.auc">>},
        {<<"summarize">>, <<"zhipu">>, <<"glm-5.3-flash">>}
    ]),
    {Provider, Model}.

plan(<<"transcribe">>, Body, #{<<"source_kind">> := <<"assembly">>} = Snapshot) ->
    checked(fun() ->
        Expected = talk_record_provider_contract:plan(Snapshot),
        true = Body =:= Expected,
        #{chunks => maps:get(<<"chunks">>, Expected), hash => hash(Expected)}
    end);
plan(<<"transcribe">>, Body, #{<<"parts">> := Parts}) ->
    checked(fun() ->
        true = allowed(Body, [<<"chunks">>, <<"overlap_rule">>]),
        <<"none">> = maps:get(<<"overlap_rule">>, Body),
        Chunks = maps:get(<<"chunks">>, Body),
        true = is_list(Chunks) andalso Chunks =/= [] andalso length(Chunks) =< 10000,
        true = is_list(Parts) andalso Parts =/= [] andalso length(Parts) =< 10000,
        ok = audio_chunks(Chunks, Parts, 0, 0),
        #{chunks => Chunks, hash => hash(Body)}
    end);
plan(<<"summarize">>, Body, #{<<"segments">> := Segments}) ->
    checked(fun() ->
        true = allowed(Body, [<<"chunks">>, <<"overlap_rule">>]),
        <<"none">> = maps:get(<<"overlap_rule">>, Body),
        Expected = [
            #{
                <<"seq">> => 0,
                <<"source_segment_ids">> =>
                    [maps:get(<<"id">>, S) || S <- Segments],
                <<"source_hash">> => hash(Segments)
            }
        ],
        true = maps:get(<<"chunks">>, Body) =:= Expected andalso Segments =/= [],
        #{chunks => Expected, hash => hash(Body)}
    end);
plan(_, _, _) ->
    {error, client_schema_invalid}.

audio_chunks([], [], _, 0) ->
    ok;
audio_chunks([Chunk | Chunks], [Part | Parts], Seq, Start) ->
    [Range] = maps:get(<<"source_ranges">>, Chunk),
    End = maps:get(<<"end_ms">>, Range),
    SourceDuration = maps:get(<<"duration_ms">>, Part),
    true = is_integer(End) andalso is_integer(SourceDuration) andalso End =< SourceDuration,
    Duration = End - Start,
    Bytes = maps:get(<<"byte_size">>, Chunk),
    true = is_integer(Duration) andalso Duration > 0 andalso Duration =< 500000,
    true = is_integer(Bytes) andalso Bytes > 0 andalso Bytes =< 50000000,
    true =
        Start =/= 0 orelse End =/= SourceDuration orelse Bytes =:= maps:get(<<"byte_size">>, Part),
    Expected = #{
        <<"seq">> => Seq,
        <<"source_part_manifest_hash">> => hash(Part),
        <<"source_ranges">> => [
            #{
                <<"part_id">> => maps:get(<<"part_id">>, Part),
                <<"sha256">> => maps:get(<<"sha256">>, Part),
                <<"start_ms">> => Start,
                <<"end_ms">> => End
            }
        ],
        <<"offset_ms">> => maps:get(<<"offset_ms">>, Part) + Start,
        <<"duration_ms">> => Duration,
        <<"byte_size">> => Bytes
    },
    true = Chunk =:= Expected,
    case End =:= SourceDuration of
        true -> audio_chunks(Chunks, Parts, Seq + 1, 0);
        false -> audio_chunks(Chunks, [Part | Parts], Seq + 1, End)
    end.

result(Kind, Body, Chunk, Snapshot) ->
    checked(fun() ->
        true = allowed(Body, [<<"result">>, <<"result_hash">>, <<"provider_request_id">>]),
        R = maps:get(<<"result">>, Body),
        true = byte_size(jsone:encode(R)) =< 1048576,
        H = hash(R),
        H = maps:get(<<"result_hash">>, Body),
        Trace = maps:get(<<"provider_request_id">>, Body, <<>>),
        true = is_binary(Trace) andalso byte_size(Trace) =< 128,
        true = re:run(Trace, <<"^[A-Za-z0-9_.:-]*$">>, [{capture, none}]) =:= match,
        bounded_labels(Kind, R),
        Context = context(Kind, Chunk, Snapshot),
        Model = result_model(Kind, Snapshot),
        {ok, Valid} = talk_record_processor_result:validate(
            Kind, R, #{model_version => Model}, Context
        ),
        #{result => Valid, wire_result => R, hash => H, provider_request_id => Trace}
    end).

result_model(<<"transcribe">>, #{<<"source_kind">> := <<"assembly">>}) -> <<"volc.seedasr.auc">>;
result_model(<<"transcribe">>, _) -> <<"asr-1.0">>;
result_model(<<"summarize">>, _) -> <<"glm-5.3-flash">>.

context(<<"transcribe">>, Chunk, _) ->
    #{duration_ms => maps:get(<<"duration_ms">>, Chunk)};
context(<<"summarize">>, _, #{<<"segments">> := Segments}) ->
    #{segment_ids => [maps:get(<<"id">>, S) || S <- Segments]}.

hash(Value) ->
    {ok, Bytes} = enterprise_cursor_v2:canonical_json(Value),
    binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).

allowed(Map, Keys) -> is_map(Map) andalso map_size(maps:without(Keys, Map)) =:= 0.

constant_equal(A, B) when is_binary(A), is_binary(B), byte_size(A) =:= byte_size(B) ->
    0 =:= constant_xor(A, B, 0);
constant_equal(_, _) ->
    false.
constant_xor(<<>>, <<>>, Acc) -> Acc;
constant_xor(<<A, As/binary>>, <<B, Bs/binary>>, Acc) -> constant_xor(As, Bs, Acc bor (A bxor B)).

checked(Fun) ->
    try
        {ok, Fun()}
    catch
        _:_ -> {error, client_schema_invalid}
    end.

bounded_labels(<<"transcribe">>, R) ->
    Language = maps:get(<<"language">>, R, <<>>),
    true = is_binary(Language) andalso byte_size(Language) =< 32,
    true = re:run(Language, <<"^[A-Za-z0-9_-]*$">>, [{capture, none}]) =:= match,
    lists:foreach(
        fun(S) ->
            Label = maps:get(<<"label">>, S),
            true = is_binary(Label) andalso byte_size(Label) > 0 andalso byte_size(Label) =< 48,
            true = re:run(Label, <<"^[A-Za-z0-9_-]+$">>, [{capture, none}]) =:= match
        end,
        maps:get(<<"speakers">>, R)
    );
bounded_labels(<<"summarize">>, _) ->
    ok.
