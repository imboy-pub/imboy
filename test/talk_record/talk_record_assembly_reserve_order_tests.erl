-module(talk_record_assembly_reserve_order_tests).
-include_lib("eunit/include/eunit.hrl").

order_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            ?_test(reserve_failure_sends_no_upload()),
            ?_test(reserved_artifact_written_once())
        ]
    end}.

setup() ->
    Mods = [
        elib_pg,
        talk_record_audio_validate,
        attachment_storage,
        attachment_storage_garage,
        attachment_worker_port,
        config_ds,
        talk_record_assembly_artifact
    ],
    meck:new(Mods, [passthrough, no_link]),
    meck:expect(talk_record_audio_validate, limits, fun() -> #{max_bytes => 100} end),
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {ok, #{driver => garage}} end),
    meck:expect(attachment_storage_garage, presign_snapshot, fun(_, _) -> {ok, <<"signed">>} end),
    meck:expect(attachment_worker_port, validate_audio, fun(_) -> {ok, #{}} end),
    meck:expect(attachment_worker_port, upload_file, fun(_) -> {ok, sent} end),
    meck:expect(config_ds, env, fun(imboy, talk_record_render_tmp_dir, _) -> "/tmp" end),
    Mods.
cleanup(Mods) -> meck:unload(Mods).

prepared() ->
    Row = #{
        <<"size">> => 5, <<"file_hash256">> => binary:copy(<<"a">>, 64), <<"path">> => <<"input">>
    },
    #{final_audio => #{<<"_row">> => Row}, parts => [], manifest => #{}}.

reserve_failure_sends_no_upload() ->
    put(tx_count, 0),
    meck:reset(attachment_worker_port),
    meck:expect(elib_pg, with_tx, fun(_, _) ->
        case get(tx_count) of
            0 ->
                put(tx_count, 1),
                {ok, prepared()};
            1 ->
                put(tx_count, 2),
                {error, physical_key_busy}
        end
    end),
    ?assertEqual({error, physical_key_busy}, talk_record_assembly_job:run(#{<<"id">> => 1})),
    ?assertEqual(0, meck:num_calls(attachment_worker_port, upload_file, '_')),
    ?assertEqual(0, meck:num_calls(talk_record_assembly_artifact, write, '_')).

reserved_artifact_written_once() ->
    put(tx_count, 0),
    meck:reset(attachment_worker_port),
    meck:expect(elib_pg, with_tx, fun(_, _) ->
        case get(tx_count) of
            0 ->
                put(tx_count, 1),
                {ok, prepared()};
            1 ->
                put(tx_count, 2),
                {ok, reserved};
            2 ->
                put(tx_count, 3),
                {ok, published}
        end
    end),
    meck:expect(talk_record_assembly_artifact, write, fun(reserved, _) -> ok end),
    ?assertEqual({ok, published}, talk_record_assembly_job:run(#{<<"id">> => 2})),
    ?assertEqual(1, meck:num_calls(talk_record_assembly_artifact, write, '_')),
    ?assertEqual(0, meck:num_calls(attachment_worker_port, upload_file, '_')).
