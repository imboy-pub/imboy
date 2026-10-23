-module(talk_record_client_audio_ds).
-moduledoc "Assembly source capability domain backed by durable provider-task authorization.".
-export([issue/3, metadata/1, fold/4]).

issue(Context, Ids, Headers) ->
    protect(fun() ->
        Config = config(),
        Authority = require(
            talk_record_client_ds:tx(fun(C) -> load(C, Context, Ids, Headers, Config) end)
        ),
        B = require(talk_record_client_audio_contract:authorize(Authority, now_seconds())),
        {ok, Ticket, Expiry} = talk_record_client_audio_ticket:issue(
            B, maps:get(signing_key, Config), now_seconds()
        ),
        URL = require(
            talk_record_client_audio_contract:build_url(maps:get(source_origin, Config), Ticket)
        ),
        %% The caller must deliver this bearer only to the authenticated App.
        %% Never put the result in logs, audit metadata or GET recovery payloads.
        {ok, #{url => URL, expires_at => Expiry, metadata => projection(B)}}
    end).

metadata(Ticket) ->
    protect(fun() -> {ok, projection(authorize_ticket(Ticket))} end).

fold(Ticket, Range, Fun, Acc) when is_function(Fun, 2) ->
    protect(fun() ->
        B = authorize_ticket(Ticket),
        talk_record_client_audio_stream:fold(
            maps:get(<<"attachment_id">>, B),
            Range,
            fun() -> {ok, authorize_ticket(Ticket)} end,
            Fun,
            Acc
        )
    end);
fold(_, _, _, _) ->
    {error, audio_source_denied}.

authorize_ticket(Ticket) ->
    Config = config(),
    B = require(
        talk_record_client_audio_ticket:verify(Ticket, maps:get(signing_key, Config), now_seconds())
    ),
    true = maps:get(<<"deployment_id">>, B) =:= maps:get(deployment_id, Config),
    Ids = #{
        rid => maps:get(<<"rid">>, B),
        jid => maps:get(<<"jid">>, B),
        run_id => maps:get(<<"run_id">>, B),
        seq => 0
    },
    Authority = require(
        talk_record_client_ds:tx(fun(C) ->
            Identity = require(
                talk_record_client_audio_repo:run_identity_tx(
                    C,
                    maps:get(rid, Ids),
                    maps:get(jid, Ids),
                    maps:get(run_id, Ids)
                )
            ),
            Uid = maps:get(<<"uid">>, B),
            true = Uid =:= maps:get(<<"actor_user_id">>, Identity),
            Context = #{uid => Uid, did => maps:get(<<"device_session_id">>, Identity)},
            load(C, Context, Ids, read, Config)
        end)
    ),
    Current = require(talk_record_client_audio_contract:authorize(Authority, now_seconds())),
    true = Current =:= B,
    Current.

load(
    C,
    #{uid := Uid, did := Did} = Context,
    #{rid := Rid, jid := Jid, run_id := RunId, seq := 0},
    Headers,
    Config
) ->
    true = user_device_ds:is_active(Uid, Did),
    Epoch = require(auth_session_ds:current_epoch(Uid)),
    R = talk_record_client_gate:record_tx(C, Context, Rid, write),
    J = require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
    _ = talk_record_client_gate:pins_tx(C, R, J),
    Run = require(talk_record_client_repo:lock_run_tx(C, Rid, Jid, RunId)),
    Uid = maps:get(<<"actor_user_id">>, Run),
    Did = maps:get(<<"device_session_id">>, Run),
    validate_run(C, Context, J, Run, Headers),
    Parts = require(talk_record_client_repo:parts_tx(C, RunId)),
    A = require(talk_record_client_audio_repo:assembly_tx(C, Rid)),
    Artifact = require(
        talk_record_client_audio_repo:artifact_tx(C, Rid, maps:get(<<"audio_attachment_id">>, A))
    ),
    Task = require(
        talk_record_client_audio_repo:source_authority_tx(C, #{
            rid => Rid, jid => Jid, run_id => RunId, seq => 0
        })
    ),
    {ok, #{
        provider_task => Task,
        record => R,
        job => J,
        run => Run,
        parts => Parts,
        assembly => A,
        artifact => Artifact,
        config => Config,
        auth_epoch => Epoch
    }}.

validate_run(C, Context, J, Run, #{token := Token, if_match := ETag}) ->
    talk_record_client_gate:run_tx(C, Context, J, Run, Token),
    talk_record_client_ds:revision(Run, ETag);
validate_run(C, #{uid := Uid}, J, Run, read) ->
    %% The signed source ticket replaces only the source-read bearer, never JWT
    %% issuance or the general execution token. Current actor eligibility remains checked.
    _ = require(talk_record_client_audio_repo:consent_tx(C, Uid, Run, maps:get(<<"kind">>, J))),
    ok.

config() ->
    Config =
        case talk_record_client_policy:get() of
            {ok, C} -> C;
            _ -> throw(audio_source_not_ready)
        end,
    case talk_record_client_audio_contract:enabled(Config) of
        true -> ok;
        false -> throw(audio_source_not_ready)
    end,
    Origin = config_ds:env(base_url, <<>>),
    Deployment = require(talk_record_client_audio_contract:origin(Origin)),
    Key = maps:get(signing_key, Config),
    true = is_binary(Key) andalso byte_size(Key) >= 32,
    Config#{deployment_id => Deployment, source_origin => Origin}.

projection(B) ->
    #{
        attachment_id => maps:get(<<"attachment_id">>, B),
        size => maps:get(<<"size">>, B),
        mime => <<"audio/aac">>,
        headers => #{
            <<"cache-control">> => <<"private, no-store">>,
            <<"content-type">> => <<"audio/aac">>,
            <<"x-content-type-options">> => <<"nosniff">>,
            <<"accept-ranges">> => <<"bytes">>
        }
    }.

now_seconds() -> erlang:system_time(second).
require({ok, V}) -> V;
require({error, audio_source_not_ready}) -> throw({talk_client_abort, audio_source_not_ready});
require(_) -> throw(audio_source_denied).
protect(F) ->
    try F() of
        {ok, _} = Ok -> Ok;
        _ -> {error, audio_source_denied}
    catch
        throw:audio_source_not_ready -> {error, audio_source_not_ready};
        throw:{talk_client_abort, audio_source_not_ready} -> {error, audio_source_not_ready};
        _:_ -> {error, audio_source_denied}
    end.
