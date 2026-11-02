-module(talk_record_assembly_job).
-moduledoc "真实音频组装：短事务准备、受控IO、重验与原子发布。".
-export([run/1, prepare_tx/2, reserve_tx/4, publish_tx/3, fail/2]).

run(Job) ->
    %% no-backend-ffmpeg V1 三段式（无 FFmpeg 渲染段）：
    %% prepare（tx 权益 + 冻结 final 输入的附件事实预检）→ 事务外 I/O
    %% （成品对象一次有界下载 → 分段范围 SHA + ADTS + 样本轴校验——
    %% 服务器读字节，不信客户端声明；小 JSON manifest 写私有临时文件 +
    %% Worker staging 上传）→ tx reserve（manifest 真值建行）→ 服务端
    %% Copy + finish → tx publish（绑定真实客户端成品附件，无派生副本）。
    case tx(fun(C) -> prepare_tx(C, Job) end) of
        {ok, P} ->
            case validate_final(P) of
                {ok, Final} ->
                    run_manifest(P#{final_audio => Final}, Job);
                {error, _} = E ->
                    E
            end;
        Error ->
            Error
    end.

validate_final(P) ->
    Final = maps:get(final_audio, P),
    Row = maps:get(<<"_row">>, Final),
    Parts = maps:get(parts, P),
    Max = maps:get(max_bytes, talk_record_audio_validate:limits()),
    attachment_artifact_reader:with_source(Row, Max, fun(Source) ->
        case talk_record_audio_validate:validate(Source, Parts, Final) of
            {ok, _Summary} -> {ok, Final};
            {error, _} = E -> E
        end
    end).

run_manifest(P, Job) ->
    Bytes = jsone:encode(maps:get(manifest, P)),
    Base = config_ds:env(imboy, talk_record_render_tmp_dir, "/tmp"),
    Name =
        "assembly-manifest-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(8), lowercase)),
    Dir = filename:join(Base, Name),
    ok = file:make_dir(Dir),
    ok = file:change_mode(Dir, 8#700),
    ManifestFile = filename:join(Dir, "manifest.json"),
    {ok, F} = file:open(ManifestFile, [write, binary, exclusive]),
    ok = file:change_mode(ManifestFile, 8#600),
    ok = file:write(F, Bytes),
    ok = file:close(F),
    ManifestStaging = staging_key(Job, <<"manifest">>),
    try
        case staging_upload(ManifestFile, <<"application/json">>, ManifestStaging) of
            {ok, _} ->
                case tx(fun(C) -> reserve_tx(C, Job, P, Bytes) end) of
                    {ok, Ready} ->
                        %% worker 合同 {ok, published}：publish 成功统一收敛
                        %% 为 tagged 结果（publish_tx 的内部视图不透传）。
                        case
                            talk_record_assembly_artifact:write(Ready, #{
                                manifest_staging => ManifestStaging,
                                manifest_file => ManifestFile
                            })
                        of
                            ok ->
                                case tx(fun(C) -> publish_tx(C, Job, Ready) end) of
                                    {ok, _} -> {ok, published};
                                    E -> E
                                end;
                            Error ->
                                Error
                        end;
                    Error ->
                        Error
                end;
            {error, _} = E ->
                E
        end
    after
        case {file:delete(ManifestFile), file:del_dir(Dir)} of
            {ok, ok} -> ok;
            _ -> throw(manifest_cleanup_unconfirmed)
        end
    end.

staging_key(Job, Role) ->
    Jid = maps:get(<<"id">>, Job),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"assembly-staging/", (integer_to_binary(Jid))/binary, "/", Role/binary, "/", Nonce/binary>>.

staging_upload(Path, Mime, StagingKey) ->
    Cfg = application:get_env(imboy, garage, #{}),
    Task = #{
        src_path => Path,
        endpoint => maps:get(endpoint, Cfg),
        ak => maps:get(access_key, Cfg),
        sk => maps:get(secret_key, Cfg),
        bucket => maps:get(bucket, Cfg, <<"imboy">>),
        region => maps:get(region, Cfg, <<"garage">>),
        object_key => StagingKey,
        mime => Mime
    },
    attachment_worker_port:upload_file(Task).

prepare_tx(C, Job) ->
    ok = gate(C, Job),
    require(talk_record_assembly_logic:prepare_tx(C, Job)).

reserve_tx(C, Job, P, Bytes) ->
    %% no-backend-ffmpeg V1：仅 manifest 派生附件建行（成品不再 reserve——
    %% 直接绑定客户端直传的 CONFIRMED 附件）。
    ok = gate(C, Job),
    {ok, talk_record_assembly_artifact:reserve_tx(C, Job, P, Bytes)}.

publish_tx(C, Job, P) ->
    ok = gate(C, Job),
    require(talk_record_assembly_logic:finish_tx(C, Job, P)).

gate(C, Job) ->
    Rid = maps:get(<<"record_id">>, Job),
    case talk_record_export_repo:lock_record_tx(C, Rid) of
        {ok, Record} ->
            {ok, Locked} =
                case talk_record_export_repo:lock_job_tx(C, Job) of
                    {error, not_found} -> abort(lease_lost);
                    V -> require(V)
                end,
            Keys = [
                <<"id">>,
                <<"record_id">>,
                <<"kind">>,
                <<"attempt">>,
                <<"lease_token">>,
                <<"input_manifest_hash">>,
                <<"input_content_revision">>
            ],
            case
                maps:with(Keys, Locked) =:= maps:with(Keys, Job) andalso
                    maps:get(<<"kind">>, Job) =:= <<"assemble">> andalso
                    maps:get(<<"content_revision">>, Record) =:=
                        maps:get(<<"input_content_revision">>, Job)
            of
                true -> check_manifest(C, Rid, Job);
                false -> abort(input_stale)
            end;
        {error, not_found} ->
            abort(record_deleted);
        {error, R} ->
            abort(R)
    end.

check_manifest(C, Rid, Job) ->
    {ok, A} = require(talk_record_assembly_logic:find_tx(C, Rid)),
    case maps:get(<<"manifest_hash">>, A) =:= maps:get(<<"input_manifest_hash">>, Job) of
        true -> ok;
        false -> abort(input_stale)
    end.

%% Always settle this current leased job; only its still-current live record
%% input may receive a failed media projection. A newer input is left intact.
fail(Job, Code) ->
    tx(fun(C) ->
        Rid = maps:get(<<"record_id">>, Job),
        {ok, Rows} = require(
            elib_pg:query(
                C,
                <<
                    "SELECT deleted_at,content_revision FROM talk_record"
                    " WHERE id=$1 FOR UPDATE"
                >>,
                [Rid]
            )
        ),
        {ok, Locked} =
            case talk_record_export_repo:lock_job_tx(C, Job) of
                {error, not_found} -> abort(lease_lost);
                V -> require(V)
            end,
        Keys = [<<"id">>, <<"record_id">>, <<"attempt">>, <<"lease_token">>, <<"kind">>],
        case maps:with(Keys, Locked) =:= maps:with(Keys, Job) of
            true -> ok;
            false -> abort(lease_lost)
        end,
        ok = maybe_failed_media(C, Rid, Rows, Locked),
        ok = require(
            talk_record_job_repo:finish_failed_tx(
                C,
                maps:get(<<"id">>, Job),
                maps:get(<<"lease_token">>, Job),
                Code
            )
        ),
        {ok, failed}
    end).

maybe_failed_media(C, Rid, [#{<<"deleted_at">> := null, <<"content_revision">> := Rev}], Job) ->
    case talk_record_assembly_logic:find_tx(C, Rid) of
        {ok, A} ->
            Current =
                maps:get(<<"input_content_revision">>, Job) =:= Rev andalso
                    maps:get(<<"input_manifest_hash">>, Job) =:= maps:get(<<"manifest_hash">>, A),
            case Current of
                true ->
                    {ok, 1} = require(
                        elib_pg:execute(
                            C,
                            <<
                                "UPDATE talk_record SET"
                                " media_status='retryable_failed',updated_at=now() WHERE id=$1"
                            >>,
                            [Rid]
                        )
                    ),
                    ok;
                false ->
                    ok
            end;
        {error, not_found} ->
            ok;
        {error, R} ->
            abort(R)
    end;
maybe_failed_media(_, _, _, _) ->
    ok.

tx(Fun) ->
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {rollback, {talk_assembly_abort, R}} -> {error, R};
        V -> V
    end.
require({error, R}) -> abort(R);
require(V) -> V.
abort(R) -> throw({talk_assembly_abort, R}).
