-module(talk_record_assembly_unknown_tests).
-include_lib("eunit/include/eunit.hrl").

unknown_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(upload_unknown_persistence_failure()),
            ?_test(copy_unknown_persistence_failure()),
            ?_test(upload_unknown_is_returned_when_recorded()),
            ?_test(unregistered_staging_never_uploaded()),
            ?_test(snapshot_survives_global_config_change())
        ]
    end}.

setup() ->
    Mods = [attachment_derived_artifact, attachment_worker_port],
    meck:new(Mods, [passthrough, no_link]),
    Prev = application:get_env(imboy, garage),
    application:set_env(imboy, garage, #{
        endpoint => <<"fixture">>, access_key => <<"test">>, secret_key => <<"test">>
    }),
    meck:expect(attachment_derived_artifact, begin_write, fun(1, <<"fixture-key">>) ->
        {ok, #{
            driver => garage,
            namespace => <<"fixture">>,
            container => <<"fixture">>,
            endpoint => <<"snapshot-endpoint">>,
            garage_config => #{
                endpoint => <<"snapshot-endpoint">>,
                access_key => <<"snapshot-ak">>,
                secret_key => <<"snapshot-sk">>
            },
            mime => <<"application/json">>
        }}
    end),
    {Mods, Prev}.
cleanup({Mods, Prev}) ->
    meck:unload(Mods),
    case Prev of
        undefined -> application:unset_env(imboy, garage);
        {ok, X} -> application:set_env(imboy, garage, X)
    end.

write() ->
    talk_record_assembly_artifact:write(
        #{artifacts => [#{attachment_id => 1}]},
        #{manifest_staging => <<"fixture-key">>, manifest_file => "/tmp/fixture"}
    ).

upload_unknown_persistence_failure() ->
    meck:expect(attachment_worker_port, upload_file, fun(_) -> {error, timeout} end),
    meck:expect(attachment_derived_artifact, finish_write, fun(1, {error, timeout}) ->
        {error, artifact_write_quarantined}
    end),
    ?assertEqual({error, artifact_write_quarantined}, write()).

copy_unknown_persistence_failure() ->
    meck:expect(attachment_worker_port, upload_file, fun(_) -> {ok, uploaded} end),
    meck:expect(attachment_derived_artifact, copy_staging, fun(_, _) -> {error, timeout} end),
    meck:expect(attachment_derived_artifact, finish_write, fun(1, {error, timeout}) ->
        {error, artifact_write_quarantined}
    end),
    ?assertEqual({error, artifact_write_quarantined}, write()).

upload_unknown_is_returned_when_recorded() ->
    meck:expect(attachment_worker_port, upload_file, fun(_) -> {error, timeout} end),
    meck:expect(attachment_derived_artifact, finish_write, fun(1, {error, timeout}) ->
        {error, timeout}
    end),
    ?assertEqual({error, artifact_write_unknown}, write()).

unregistered_staging_never_uploaded() ->
    meck:reset(attachment_worker_port),
    meck:expect(attachment_derived_artifact, begin_write, fun(1, <<"fixture-key">>) ->
        {error, registration_failed}
    end),
    ?assertEqual({error, registration_failed}, write()),
    ?assertEqual(0, meck:num_calls(attachment_worker_port, upload_file, '_')).

snapshot_survives_global_config_change() ->
    meck:expect(attachment_derived_artifact, begin_write, fun(1, <<"fixture-key">>) ->
        {ok, #{
            driver => garage,
            namespace => <<"fixture">>,
            container => <<"fixture">>,
            endpoint => <<"snapshot-endpoint">>,
            garage_config => #{
                endpoint => <<"snapshot-endpoint">>,
                access_key => <<"snapshot-ak">>,
                secret_key => <<"snapshot-sk">>
            }
        }}
    end),
    application:set_env(imboy, garage, #{
        endpoint => <<"changed-endpoint">>,
        access_key => <<"changed-ak">>,
        secret_key => <<"changed-sk">>
    }),
    meck:expect(attachment_worker_port, upload_file, fun(Task) ->
        put(captured_task, Task),
        {error, timeout}
    end),
    meck:expect(attachment_derived_artifact, finish_write, fun(1, {error, timeout}) ->
        {error, timeout}
    end),
    ?assertEqual({error, artifact_write_unknown}, write()),
    Task = get(captured_task),
    ?assertEqual(<<"snapshot-endpoint">>, maps:get(endpoint, Task)),
    ?assertEqual(<<"snapshot-ak">>, maps:get(ak, Task)),
    ?assertEqual(<<"snapshot-sk">>, maps:get(sk, Task)),
    erase(captured_task).
