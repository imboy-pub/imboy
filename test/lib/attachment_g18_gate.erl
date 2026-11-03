-module(attachment_g18_gate).
-moduledoc "V3-18 G18：分片直传 presigned PUT→confirm→assembly(真 fork Worker)→播放 302/206/416/HEAD/错票→导出面→边缘承载 真 Garage 全链".
%%%
%%% 独立 app VM（talk_record_g18_gate.sh，eunit 之外 fork 正常）。
%%% 每段落一行 G18_* 标记；全链 PASS → halt(0)，任一断言失败 → 异常退出非零。
%%% 退出码由 gate 脚本透传。合成段：uid 700002（record/capture/part/job/attachment）。
%%%

-export([main/2]).

-define(UID, 700002).
-define(TOKEN, <<"v3run1-edge-internal-token">>).
main(AppPort, EdgePort) ->
    false = os:getenv("A0_ENDPOINT") =:= false orelse exit(a0_env_missing),
    Root = os:getenv("G18_TALK_UPLOAD_ROOT", "/tmp/v3w5b-g18-talk-root"),
    filelib:ensure_dir(filename:join(Root, "x")),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Secret = crypto:strong_rand_bytes(32),
    application:set_env(imboy, jwt_key, Secret),
    {ok, _} = application:ensure_all_started(imboy),
    install_garage_registry(),
    try
        {Rid, AudioAid, AudioBytes} = seg_direct_upload_then_assembly(Root),
        seg_playback_http(AppPort, EdgePort, Rid, Secret),
        seg_edge(EdgePort, Rid, Secret, AudioBytes),
        seg_export_surface(AppPort, Rid, Secret),
        io:format("G18 audio_attachment_id=~p bytes=~p~n", [AudioAid, byte_size(AudioBytes)]),
        io:format("G18_ALL_PASS~n"),
        halt(0)
    after
        stop_edge()
    end.

%% --------------------------------------------------------------------------
%% 段B+C：分片直传（真 presign PUT + S3 checksum）→ confirm → assembly
%% （真 fork escript Worker：成品经 artifact_reader 校验 + manifest staging 上传）。
%% 返回 {Rid, AudioAttachmentId, AudioBytes}
%% --------------------------------------------------------------------------
seg_direct_upload_then_assembly(Root) ->
    Bytes = tone(Root),
    Half = byte_size(Bytes) div 2,
    <<P1:Half/binary, P2/binary>> = Bytes,
    {A1, S1, Z1} = direct_part(1, P1),
    {A2, S2, Z2} = direct_part(2, P2),
    %% no-backend-ffmpeg V1：客户端合成成品独立直传（第三 direct 会话），
    %% assembly job 冻结其身份并在发布时绑定（不再服务端产派生副本）。
    {AFinal, SF, ZF} = direct_part(3, Bytes),
    io:format("G18_direct_parts_confirmed: a=~p/~pB b=~p/~pB~n", [A1, Z1, A2, Z2]),
    %% record + capture（logic 真链路创建）
    Body = #{
        <<"client_recording_id">> => elib_uuid:gen_v7(),
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"processing_mode">> => <<"transcribe">>,
        <<"title">> => <<"g18-direct-assembly">>
    },
    {ok, _Mark, #{<<"id">> := Rid}, _} =
        talk_record_logic:create(?UID, Body, <<"g18-key-1">>),
    Cid = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        <<
            "INSERT INTO talk_record_capture(id,record_id,device_session_id,"
            "device_id,started_by,status) VALUES($1,$2,'11111111-2222-4333-8444-555555555555',"
            "'g18-device',$3,'ended')"
        >>,
        [Cid, Rid, ?UID]
    ),
    insert_audio_part(Rid, Cid, 1, A1, S1, Z1),
    insert_audio_part(Rid, Cid, 2, A2, S2, Z2),
    {ok, Parts} = elib_pg:with_tx(fun(C) ->
        talk_record_assembly_logic:confirmed_parts_tx(C, Rid)
    end),
    true = length(Parts) >= 2 orelse exit({confirmed_parts, length(Parts)}),
    Manifest = talk_record_assembly_logic:build_manifest(Cid, Parts),
    {ok, _, _} = elib_pg:with_tx(fun(C) ->
        talk_record_assembly_logic:persist_tx(C, Rid, Cid, Manifest)
    end),
    {ok, [#{<<"path">> := FinalPath}]} = elib_pg:query(
        <<"SELECT path FROM attachment WHERE id=$1">>, [AFinal]
    ),
    Frames = 64,
    Final = #{
        <<"object_key">> => FinalPath,
        <<"client_upload_id">> => elib_uuid:gen_v7(),
        <<"sha256">> => SF,
        <<"byte_size">> => ZF,
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => Frames * 1024,
        <<"duration_ms">> => (Frames * 1024 * 1000 + 8000) div 16000,
        <<"attachment_id">> => AFinal
    },
    {ok, created, _} = elib_pg:with_tx(fun(C) ->
        talk_record_job_repo:enqueue_tx(
            C,
            #{
                record_id => Rid,
                kind => <<"assemble">>,
                input_manifest_hash => maps:get(<<"manifest_hash">>, Manifest),
                input_content_revision => 1,
                dedupe_key => <<"g18-assembly-", (elib_uuid:gen_v7())/binary>>,
                processor_input => jsone:encode(#{<<"final_audio">> => Final})
            }
        )
    end),
    {ok, 1} = elib_pg:execute(
        <<
            "UPDATE talk_record_job SET state='queued',lease_token=NULL,"
            "lease_owner=NULL,lease_until=NULL WHERE record_id=$1 AND kind='assemble'"
        >>,
        [Rid]
    ),
    %% 真 fork Worker 链（不设 attachment_worker_in_process）
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    Row = talk_record_row(Rid),
    case maps:get(<<"media_status">>, Row) of
        <<"available">> -> ok;
        MS -> exit({media_status, MS})
    end,
    {ok, A} = elib_pg:with_tx(fun(C) -> talk_record_assembly_logic:find_tx(C, Rid) end),
    Aid = maps:get(<<"audio_attachment_id">>, A),
    true = is_integer(Aid) orelse exit({audio_attachment_id, Aid}),
    {ok, [#{<<"n">> := NRef}]} = elib_pg:query(
        <<
            "SELECT count(*) n FROM talk_record_attachment_ref"
            " WHERE record_id=$1 AND role IN ('manifest','transcoded_audio')"
            " AND state='bound' AND lease_until IS NULL"
        >>,
        [Rid]
    ),
    true = NRef >= 2 orelse exit({ref_rows, NRef}),
    {ok, AudioBytes} = read_assembly_audio(Aid),
    true = byte_size(AudioBytes) > 0 orelse exit(empty_assembly_audio),
    io:format("G18_assembly_worker_real_fork: succeeded refs=~p audio=~pB~n", [
        NRef, byte_size(AudioBytes)
    ]),
    {Rid, Aid, AudioBytes}.

talk_record_row(Rid) ->
    {ok, [R]} = elib_pg:query(
        <<"SELECT media_status FROM talk_record WHERE id=$1">>, [Rid]
    ),
    R.

direct_part(Seq, Body) ->
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    Part = #{
        <<"part_id">> => Seq,
        <<"codec">> => <<"aac">>,
        <<"sha256">> => Sha,
        <<"byte_size">> => byte_size(Body)
    },
    {ok, #{grant := G}} = talk_record_part_upload:prepare(?UID, 700002001, Part),
    <<"s3_put">> = maps:get(transport, G),
    {ok, 200} = staging_put(G, Body),
    Row = #{<<"part_id">> => Seq, <<"sha256">> => Sha, <<"byte_size">> => byte_size(Body)},
    ok = talk_record_part_upload:verify_upload(?UID, 700002001, Row),
    {ok, #{attachment_id := Aid}} =
        elib_pg:with_tx(fun(C) ->
            talk_record_part_upload:confirmed_asset_tx(C, ?UID, 700002001, Row)
        end),
    {Aid, Sha, byte_size(Body)}.

insert_audio_part(Rid, Cid, Seq, Aid, Sha, Size) ->
    Pid = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        <<
            "INSERT INTO talk_record_audio_part(id,record_id,capture_id,seq,"
            "start_ms,end_ms,attachment_id,sha256,byte_size,codec,upload_status)"
            " VALUES($1,$2,$3,$4,0,1000,$5,$6,$7,'aac','confirmed')"
        >>,
        [Pid, Rid, Cid, Seq, Aid, Sha, Size]
    ),
    ok.

read_assembly_audio(Aid) ->
    %% no-backend-ffmpeg V1：audio_attachment_id = 客户端直传成品附件，
    %% 经冻结位置有界读取（不再读派生副本）。
    {ok, [Row]} = elib_pg:query(
        <<"SELECT path, size, file_hash256 FROM attachment WHERE id=$1">>, [Aid]
    ),
    attachment_artifact_reader:with_source(Row, 134217728, fun(Source) ->
        Size = attachment_upload_spool:size(Source),
        {ok, attachment_upload_spool:read(Source, 0, Size)}
    end).

%% --------------------------------------------------------------------------
%% 段D：播放 HTTP 面（302 边缘壳 + HEAD 元数据 + 416 + 错票）
%% --------------------------------------------------------------------------
seg_playback_http(AppPort, _EdgePort, Rid, Secret) ->
    {ok, #{<<"ticket">> := Ticket}} = talk_record_grant_playback:open(?UID, Rid, undefined),
    Jwt = imboy_jwt:sign(
        #{<<"uid">> => ?UID, <<"sub">> => <<"g18">>, <<"exp">> => erlang:system_time(second) + 600},
        Secret
    ),
    Base =
        "http://127.0.0.1:" ++ integer_to_list(AppPort) ++
            "/api/v1/talk-records/" ++ integer_to_list(Rid) ++ "/playback/stream?ticket=" ++
            binary_to_list(Ticket),
    Auth = [{"authorization", "Bearer " ++ binary_to_list(Jwt)}],
    %% GET → 302 边缘（不跟随），Location 指向 edge 且不泄 S3 签名
    {ok, {{_, 302, _}, H302, <<>>}} = httpc:request(
        get, {Base, Auth}, [{timeout, 15000}, {autoredirect, false}], [{body_format, binary}]
    ),
    {_, Loc} = lists:keyfind("location", 1, lower(H302)),
    LocB = list_to_binary(Loc),
    true =
        string:prefix(LocB, <<"http://127.0.0.1:">>) =/= nomatch orelse
            exit({unexpected_location, Loc}),
    nomatch = binary:match(LocB, [<<"X-Amz-Signature">>, <<"X-Amz-Credential">>]),
    io:format("G18_playback_get_302_edge: ~ts~n", [Loc]),
    %% HEAD → 200 有界元数据（零字节）
    {ok, {{_, 200, _}, HH, <<>>}} = httpc:request(
        head, {Base, Auth}, [{timeout, 15000}], [{body_format, binary}]
    ),
    true = lists:keymember("content-type", 1, lower(HH)),
    io:format("G18_playback_head_200_meta: ok~n"),
    %% Range 超界 → 416（语义不变）
    {ok, {{_, 416, _}, _, _}} = httpc:request(
        get, {Base, [{"range", "bytes=999999999-"}] ++ Auth}, [{timeout, 15000}], [
            {body_format, binary}
        ]
    ),
    io:format("G18_playback_range_416: ok~n"),
    %% 错票 → 40x（隐藏）
    Bad =
        "http://127.0.0.1:" ++ integer_to_list(AppPort) ++
            "/api/v1/talk-records/" ++ integer_to_list(Rid) ++ "/playback/stream?ticket=INVALID",
    {ok, {{_, SBad, _}, _, _}} = httpc:request(
        get, {Bad, Auth}, [{timeout, 15000}], [{body_format, binary}]
    ),
    true = SBad >= 400 orelse exit({bad_ticket_status, SBad}),
    io:format("G18_playback_bad_ticket_~p: ok~n", [SBad]),
    ok.

%% --------------------------------------------------------------------------
%% 段E：边缘承载（302 Location → edge GET 200 / Range 206 / 坏票 404）
%% --------------------------------------------------------------------------
seg_edge(EdgePort, Rid, _Secret, AudioBytes) ->
    ok = start_edge(EdgePort),
    {ok, #{<<"ticket">> := Ticket}} = talk_record_grant_playback:open(?UID, Rid, undefined),
    Base =
        "http://127.0.0.1:" ++ integer_to_list(EdgePort) ++ "/edge?ticket=" ++
            binary_to_list(Ticket),
    {ok, {{_, 200, _}, H1, R1}} = httpc:request(get, {Base, []}, [{timeout, 20000}], [
        {body_format, binary}
    ]),
    PrefixLen = min(byte_size(R1), byte_size(AudioBytes)),
    <<Pre:PrefixLen/binary, _/binary>> = AudioBytes,
    <<Pre:PrefixLen/binary, _/binary>> = R1,
    io:format("G18_edge_get_200: ~pB prefix_match~n", [byte_size(R1)]),
    {ok, {{_, 206, _}, H2, R2}} = httpc:request(
        get, {Base, [{"range", "bytes=0-7"}]}, [{timeout, 20000}], [{body_format, binary}]
    ),
    8 = byte_size(R2),
    true = lists:keymember("content-range", 1, lower(H2)),
    io:format("G18_edge_range_206: ok~n"),
    %% 边缘不泄 S3 签名/原始 Location
    Lowered = lower(H1),
    [] = [K || {K, _} <- Lowered, K =:= "x-amz-signature" orelse K =:= "x-amz-credential"],
    %% 坏票 → 404
    {ok, {{_, 404, _}, _, _}} = httpc:request(
        get, {Base ++ "BAD", []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("G18_edge_bad_ticket_404: ok~n"),
    ok.

%% --------------------------------------------------------------------------
%% 段F：导出面（负例收口：未授权 401；错票 40x。授权导出全链 job 超本
%% gate 预算——302 壳形态与段D同构，由 V3-29 汇总口径覆盖）
%% --------------------------------------------------------------------------
seg_export_surface(AppPort, Rid, Secret) ->
    Jwt = imboy_jwt:sign(
        #{<<"uid">> => ?UID, <<"sub">> => <<"g18">>, <<"exp">> => erlang:system_time(second) + 600},
        Secret
    ),
    Exports =
        "http://127.0.0.1:" ++ integer_to_list(AppPort) ++
            "/api/v1/talk-records/" ++ integer_to_list(Rid) ++ "/exports",
    {ok, {{_, 401, _}, _, _}} = httpc:request(
        get, {Exports, []}, [{timeout, 15000}], [{body_format, binary}]
    ),
    io:format("G18_export_unauthorized_401: ok~n"),
    BadDl =
        "http://127.0.0.1:" ++ integer_to_list(AppPort) ++
            "/api/v1/talk-records/" ++ integer_to_list(Rid) ++ "/exports/1/download?ticket=INVALID",
    {ok, {{_, SDl, _}, _, _}} = httpc:request(
        get,
        {BadDl, [{"authorization", "Bearer " ++ binary_to_list(Jwt)}]},
        [{timeout, 15000}],
        [{body_format, binary}]
    ),
    true = SDl >= 400 orelse exit({export_bad_ticket, SDl}),
    io:format("G18_export_bad_ticket_~p: ok~n", [SDl]),
    ok.

%% --------------------------------------------------------------------------
%% 基建：garage registry / tone / edge 进程
%% --------------------------------------------------------------------------
install_garage_registry() ->
    Ep = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    application:set_env(imboy, garage, #{
        endpoint => Ep,
        bucket => <<"imboy">>,
        region => <<"garage">>,
        access_key => Ak,
        secret_key => Sk
    }),
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{
            <<"g18-garage">> => #{
                driver => <<"garage">>,
                namespace => <<"g18-garage">>,
                endpoint => Ep,
                container => <<"imboy">>,
                access_key => Ak,
                secret_key => Sk,
                region => <<"garage">>
            }
        },
        upload_namespaces =>
            (maps:get(upload_namespaces, Base, #{}))#{<<"talk_record">> => <<"g18-garage">>}
    }),
    ok.

%% no-backend-ffmpeg V1：固定合法 ADTS 字节（16kHz 单声道 39 字节帧 ×64，
%% 与 talk_record_audio_validate 同构），替换旧 ?FFMPEG 动态生成——
%% 无 FFmpeg 环境仍走真实完成链。Root 参数保留以兼容调用点。
tone(_Root) ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    Frame = <<16#FF, 16#F1, 16#60, B3, B4, B5, 16#00, 0:256>>,
    iolist_to_binary(lists:duplicate(64, Frame)).

staging_put(#{url := Url, required_headers := Headers}, Body) ->
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {_, Ct} = lists:keyfind("content-type", 1, lower(Hs)),
    case
        httpc:request(
            put,
            {binary_to_list(Url), Hs, Ct, Body},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, S, _}, _, _}} when S >= 200, S < 300 -> {ok, 200};
        {ok, {{_, S, _}, _, B}} -> exit({put_rejected, S, B});
        {error, R} -> exit({put_error, R})
    end.

start_edge(EdgePort) ->
    TestLibDir = filename:dirname(code:which(attachment_g18_gate)),
    Backend = filename:dirname(filename:dirname(TestLibDir)),
    Deps = filename:join(Backend, "deps"),
    CodePaths = lists:join(":", [D || D <- filelib:wildcard(filename:join(Deps, "*/ebin"))]),
    Script = filename:join([Backend, "priv", "workers", "download_edge.escript"]),
    Es = os:find_executable("escript"),
    AppPort = app_port(),
    Cmd = lists:flatten(
        io_lib:format(
            "EDGE_PORT=~p EDGE_AUTHORIZE_URL=\"http://127.0.0.1:~p/api/v1/attachment/edge/authorize\" "
            "EDGE_INTERNAL_TOKEN=\"~s\" EDGE_EBIN=\"~s\" EDGE_CODE_PATHS=\"~s\" \"~s\" \"~s\" 2>>/tmp/v3-g18-edge.log &",
            [EdgePort, AppPort, ?TOKEN, filename:join(Backend, "ebin"), CodePaths, Es, Script]
        )
    ),
    os:cmd(Cmd),
    wait_ready(EdgePort, 30).

app_port() ->
    case application:get_env(imboy, http_port) of
        {ok, P} when is_integer(P) -> P;
        _ -> 19881
    end.

wait_ready(_, 0) ->
    exit(edge_not_ready);
wait_ready(Port, N) ->
    case gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}], 500) of
        {ok, S} ->
            gen_tcp:close(S),
            timer:sleep(300),
            ok;
        _ ->
            timer:sleep(500),
            wait_ready(Port, N - 1)
    end.

stop_edge() ->
    os:cmd("pkill -f download_edge.escript || true").

lower(Hs) -> [{string:lowercase(K), V} || {K, V} <- Hs].
