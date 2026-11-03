%%% @doc T12 talk_record job/assembly eunit 支撑（test-only，不进 release）。
%%%
%%% 复用 talk_record_t07_support（→t05_support）的池/环境/tsid meck/夹具，
%%% 叠加：
%%%   * 池指向 T12 隔离库 127.0.0.1:55435/talk_record_t12（env T12_PG_* 可
%%%     覆写；经 os:setenv 注入 t05_support 读取的 T05_PG_* 变量）；
%%%   * 完整录音夹具快捷方式：record + capture + start 事件 + 分段注册
%%%     （可乱序）+ confirm + end 事件（complete 就绪态）；
%%%   * complete/job/引用行直查 SQL。
-module(talk_record_t12_support).

-export([
    setup_all/0,
    setup_audio_fixture/0,
    cleanup_audio_fixture/1,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    sha/1,
    complete_build_body/4,
    %% 夹具快捷方式
    ev/6,
    part/8,
    confirm_part/4,
    ready_record/5,
    complete/4,
    complete_body/5,
    job_row/2,
    media_status/2,
    lease_refs/3,
    permanent_refs/2
]).

-define(DS1, <<"10000000-0000-0000-0000-000000000001">>).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    %% T12 隔离库：55435/talk_record_t12（cluster 在 run root pg-t12/）
    os:putenv("T05_PG_HOST", os:getenv("T12_PG_HOST", "127.0.0.1")),
    os:putenv("T05_PG_PORT", os:getenv("T12_PG_PORT", "55435")),
    os:putenv("T05_PG_USER", os:getenv("T12_PG_USER", "t12admin")),
    os:putenv("T05_PG_PASSWORD", os:getenv("T12_PG_PASSWORD", "")),
    os:putenv("T05_PG_DB", os:getenv("T12_PG_DB", "talk_record_t12")),
    State = talk_record_t07_support:setup_all(),
    try
        State#{audio_fixture => setup_audio_fixture()}
    catch
        Class:Reason:Stack ->
            talk_record_t07_support:teardown_all(State),
            erlang:raise(Class, Reason, Stack)
    end.

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    try
        cleanup_audio_fixture(maps:get(audio_fixture, State))
    after
        talk_record_t07_support:teardown_all(State)
    end.

-spec seed(map()) -> ok.
seed(State) ->
    ok = talk_record_t07_support:seed(State),
    talk_record_t07_support:sql(maps:get(conn, State), "DELETE FROM talk_record_assembly").

-spec ids() -> map().
ids() ->
    talk_record_t07_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, Sql) ->
    talk_record_t07_support:sql(Conn, Sql).

-spec one(pid() | map(), iolist()) -> map().
one(Conn, Sql) ->
    talk_record_t07_support:one(Conn, Sql).

-spec sha(integer()) -> binary().
sha(N) ->
    talk_record_t07_support:sha(N).

%%%===================================================================
%%% 夹具快捷方式
%%%===================================================================

%% @doc 采集事件（seq/client_event_id 由 Seq 派生，DS1 会话）。
-spec ev(integer(), integer(), integer(), integer(), binary(), integer()) ->
    {ok, map()} | {error, term()}.
ev(Uid, Rid, Cid, Seq, Event, Position) ->
    talk_record_t07_support:send_event(
        Uid,
        Rid,
        Cid,
        #{
            <<"client_event_id">> => uuid(<<"40000000">>, Seq),
            <<"device_session_id">> => ?DS1,
            <<"seq">> => Seq,
            <<"event">> => Event,
            <<"position_ms">> => Position
        },
        #{}
    ).

%% @doc 注册分段（指定 sha/byte_size）。
-spec part(integer(), integer(), integer(), integer(), integer(), integer(), binary(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
part(Uid, Rid, Cid, Seq, StartMs, EndMs, IdemKey, Sha) ->
    Body = #{
        <<"seq">> => Seq,
        <<"start_ms">> => StartMs,
        <<"end_ms">> => EndMs,
        <<"capture_offset_ms">> => StartMs,
        <<"sha256">> => Sha,
        <<"byte_size">> => byte_size(audio_bytes()),
        <<"codec">> => <<"aac">>
    },
    talk_record_t07_support:register_part(Uid, Rid, Cid, Body, IdemKey).

%% @doc confirm 分段（空回执——以注册事实比对）。
-spec confirm_part(integer(), integer(), integer(), integer()) ->
    {ok, confirmed | verifying | replay, map()} | {error, term()}.
confirm_part(Uid, Rid, Cid, PartId) ->
    talk_record_ds:confirm_part(Uid, Rid, Cid, PartId, #{}).

%% @doc complete 就绪夹具：record(record_only|transcribe) + capture + start
%% + N 段注册并 confirm（注册顺序按 Seqs 原序，可乱序传入）+ end。
%% 返回 {Rid, Cid, PartIds}。
-spec ready_record(integer(), binary(), binary(), [integer()], binary()) ->
    {ok, {integer(), integer(), [integer()]}} | {error, term()}.
ready_record(Uid, ClientRecordingId, Mode, Seqs, IdemPrefix) ->
    Body = #{
        <<"client_recording_id">> => ClientRecordingId,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"title">> => <<"t12"/utf8>>,
        <<"processing_mode">> => Mode
    },
    case talk_record_logic:create(Uid, Body, <<IdemPrefix/binary, "-rec">>) of
        {ok, _Mark, Payload, _ETag} ->
            Rid = maps:get(<<"id">>, Payload),
            ready_capture(Uid, Rid, ClientRecordingId, Seqs, IdemPrefix);
        {error, Reason} ->
            {error, Reason}
    end.

ready_capture(Uid, Rid, _ClientRecordingId, Seqs, IdemPrefix) ->
    case talk_record_t07_support:create_capture(Uid, Rid, ?DS1, <<IdemPrefix/binary, "-cap">>) of
        {ok, Cid} ->
            {ok, _} = ev(Uid, Rid, Cid, 1, <<"start">>, 0),
            ready_parts(Uid, Rid, Cid, Seqs, IdemPrefix);
        {error, Reason} ->
            {error, Reason}
    end.

ready_parts(Uid, Rid, Cid, Seqs, IdemPrefix) ->
    Parts = [
        begin
            Sha = binary:encode_hex(crypto:hash(sha256, audio_bytes()), lowercase),
            {ok, _Mark, P} = part(
                Uid,
                Rid,
                Cid,
                Seq,
                Seq * 30_000,
                (Seq + 1) * 30_000,
                <<IdemPrefix/binary, "-p-", (integer_to_binary(Seq))/binary>>,
                Sha
            ),
            ok = upload_audio(Uid, P),
            Pid = maps:get(<<"part_id">>, P),
            {ok, confirmed, _} = confirm_part(Uid, Rid, Cid, Pid),
            Pid
        end
     || Seq <- Seqs
    ],
    {ok, _} = ev(Uid, Rid, Cid, 100, <<"end">>, lists:max(Seqs) * 30_000 + 1),
    {ok, {Rid, Cid, Parts}}.

%% @doc complete 调用快捷方式（record_only 无 consent）。
-spec complete(integer(), integer(), integer(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
complete(Uid, Rid, Cid, IdemKey) ->
    complete_body(Uid, Rid, Cid, IdemKey, #{}).

complete_body(Uid, Rid, Cid, IdemKey, Extra) ->
    LastSeq = maps:get(last_seq, Extra, 1),
    Body0 = complete_build_body(Uid, Rid, Cid, LastSeq),
    Body = maps:merge(Body0, maps:remove(last_seq, Extra)),
    talk_record_job_logic:complete(Uid, Rid, <<"\"1\"">>, IdemKey, Body).

%% @doc complete 请求体构造（V1 合同：真实 manifest_hash + final_audio
%% 投影含 attachment_id 准入证明）。供夹具与门禁类用例自定义 body 复用。
complete_build_body(Uid, Rid, Cid, LastSeq) ->
    #{
        <<"captures">> => [
            #{
                <<"capture_id">> => Cid,
                <<"last_seq">> => LastSeq,
                <<"manifest_hash">> => complete_manifest_hash(Rid, Cid)
            }
        ],
        <<"final_audio">> => complete_final_audio(Uid, Rid)
    }.

%% complete 合同（A02/T3）：manifest_hash 须等于服务端 build_manifest 对
%% confirmed parts 的重算——夹具按真实链计算，不再用占位值。
complete_manifest_hash(Rid, Cid) ->
    {ok, Parts} = elib_pg:with_conn(fun(C) ->
        talk_record_assembly_logic:confirmed_parts_tx(C, Rid)
    end),
    maps:get(<<"manifest_hash">>, talk_record_assembly_logic:build_manifest(Cid, Parts)).

%% F1b（2d979bd74 备注债）：complete 的 final_audio 须锚定一个真实
%% CONFIRMED final 直传会话——按 direct 链 register→declare→grant→
%% presign PUT→finalize 收口，返回与冻结身份一致的八字段投影。
%% 幂等：同 (Uid, Rid) 已有 CONFIRMED final 会话则直接复用投影——
%% 同 IdemKey 的 complete 重放 body 必须逐字节一致（幂等 digest）。
complete_final_audio(Uid, Rid) ->
    case existing_final_projection(Uid, Rid) of
        {ok, Final} -> Final;
        none -> create_final_audio(Uid, Rid)
    end.

existing_final_projection(Uid, Rid) ->
    case
        elib_pg:query(
            <<
                "SELECT p.client_upload_id, p.object_key, a.id AS aid,"
                " a.file_hash256, a.size"
                " FROM attach_pending p JOIN attachment a"
                " ON a.id = p.confirmed_attachment_id"
                " WHERE p.scope='talk_record' AND a.scope_ref=$1"
                " AND p.creator_user_id=$2 AND p.session_state='confirmed'"
                " AND p.direct_state='CONFIRMED' AND a.path = p.object_key"
                " AND p.upload_registration->'declaration'->>'file_name'"
                " LIKE 't12-final-%'"
                " ORDER BY p.object_key DESC LIMIT 1"
            >>,
            [integer_to_binary(Rid), Uid]
        )
    of
        {ok, [Row]} ->
            Size = ec_cnv:to_integer(maps:get(<<"size">>, Row)),
            Frames = Size div 39,
            {ok, #{
                <<"object_key">> => maps:get(<<"object_key">>, Row),
                <<"client_upload_id">> => maps:get(<<"client_upload_id">>, Row),
                <<"sha256">> => maps:get(<<"file_hash256">>, Row),
                <<"byte_size">> => Size,
                <<"sample_rate">> => 16000,
                <<"channels">> => 1,
                <<"sample_count">> => Frames * 1024,
                <<"duration_ms">> => Frames * 64,
                <<"attachment_id">> => maps:get(<<"aid">>, Row)
            }};
        _ ->
            none
    end.

create_final_audio(Uid, Rid) ->
    application:ensure_all_started(inets),
    %% validate 合同：成品对象字节 == Σ 段字节（seq 序拼接，逐段 SHA 边界
    %% 对齐）。夹具每段字节即 audio_bytes()，成品按 confirmed parts 拼接。
    {ok, Parts} = elib_pg:with_conn(fun(C) ->
        talk_record_assembly_logic:confirmed_parts_tx(C, Rid)
    end),
    Full = audio_bytes(),
    %% 无 confirmed parts（负向用例未走分段注册）回落单份——complete 的
    %% capture_not_ended 等前置拒绝不依赖成品字节，保持其错误码不变。
    Bytes =
        case Parts of
            [] ->
                Full;
            _ ->
                iolist_to_binary(
                    [
                        binary:part(Full, {0, ec_cnv:to_integer(maps:get(<<"byte_size">>, P))})
                     || P <- Parts
                    ]
                )
        end,
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Size = byte_size(Bytes),
    Frames = Size div 39,
    FinalCid = elib_uuid:gen_v7(),
    Name = <<"t12-final-", FinalCid/binary, ".aac">>,
    {ok, #{object_key := Key}} =
        attach_direct_logic:register(
            Uid, <<"talk_record">>, integer_to_binary(Rid), FinalCid, Name
        ),
    ok = attach_direct_logic:declare(Key, Sha, Size, <<"audio/aac">>),
    {ok, #{grant := G, attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    ok = presign_put(G, Bytes),
    {ok, #{state := <<"CONFIRMED">>}} = attach_direct_logic:finalize(Key, No),
    {ok, [#{<<"id">> := Aid}]} =
        elib_pg:query(<<"SELECT id FROM attachment WHERE path=$1">>, [Key]),
    #{
        <<"object_key">> => Key,
        <<"client_upload_id">> => FinalCid,
        <<"sha256">> => Sha,
        <<"byte_size">> => Size,
        <<"sample_rate">> => 16000,
        <<"channels">> => 1,
        <<"sample_count">> => Frames * 1024,
        <<"duration_ms">> => Frames * 64,
        <<"attachment_id">> => Aid
    }.

presign_put(#{url := Url, required_headers := Hs0}, Bytes) ->
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs0],
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(
            put,
            {binary_to_list(Url), Hs, "audio/aac", Bytes},
            [],
            [{body_format, binary}]
        ),
    ok.

%%%===================================================================
%%% 直查
%%%===================================================================

-spec job_row(pid() | map(), integer()) -> map().
job_row(Conn, JobId) ->
    one(Conn, "SELECT * FROM talk_record_job WHERE id = " ++ integer_to_binary(JobId)).

-spec media_status(pid() | map(), integer()) -> binary().
media_status(Conn, Rid) ->
    maps:get(
        <<"media_status">>,
        one(
            Conn,
            "SELECT media_status FROM talk_record WHERE id = " ++ integer_to_binary(Rid)
        )
    ).

%% job processing lease 引用行（assemble_job 键域）。
-spec lease_refs(pid() | map(), integer(), bound | released) -> [map()].
lease_refs(Conn, JobId, State) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT id, attachment_id, state, lease_until FROM talk_record_attachment_ref",
                " WHERE business_key LIKE 'assemble_job:", (integer_to_binary(JobId))/binary,
                ":%' AND state = $1 ORDER BY id">>,
            [atom_to_binary(State)]
        )
    of
        {ok, Rows} -> Rows;
        {error, _} -> []
    end.

%% 段永久业务引用（audio_part:<part_id> 键域）。
-spec permanent_refs(pid() | map(), integer()) -> [map()].
permanent_refs(Conn, Rid) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT id, attachment_id, business_key, state, lease_until",
                " FROM talk_record_attachment_ref",
                " WHERE record_id = $1 AND business_key LIKE 'audio_part:%'", " ORDER BY id">>,
            [Rid]
        )
    of
        {ok, Rows} -> Rows;
        {error, _} -> []
    end.

%%%===================================================================
%%% 内部
%%%===================================================================

-spec uuid(binary(), integer()) -> binary().
uuid(Prefix, Seq) ->
    Hex = integer_to_binary(Seq, 16),
    PadLen = max(0, 12 - byte_size(Hex)),
    Pad = <<<<$0>> || _ <- lists:seq(1, PadLen)>>,
    <<Prefix/binary, "-0000-0000-0000-", Pad/binary, Hex/binary>>.

%% Synthetic 30-second AAC matches the unchanged declared segment timeline.
%% Bytes go through the real V5 upload/pin/confirm path; no asset rows are forged.
setup_audio_fixture() ->
    Root = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    true = is_list(Root) andalso filename:pathtype(Root) =:= absolute,
    Previous = #{
        bytes => application:get_env(imboy, talk_record_t12_audio_bytes),
        render => application:get_env(imboy, talk_record_render_tmp_dir),
        tmp => os:getenv("TMPDIR")
    },
    Bytes = synthetic_audio(),
    ok = application:set_env(imboy, talk_record_t12_audio_bytes, Bytes),
    ok = application:set_env(imboy, talk_record_render_tmp_dir, Root),
    true = os:putenv("TMPDIR", Root),
    Previous.

cleanup_audio_fixture(Previous) ->
    restore_audio_env(talk_record_t12_audio_bytes, maps:get(bytes, Previous)),
    restore_audio_env(talk_record_render_tmp_dir, maps:get(render, Previous)),
    case maps:get(tmp, Previous) of
        false -> os:unsetenv("TMPDIR");
        Value -> os:putenv("TMPDIR", Value)
    end,
    ok.

restore_audio_env(Key, undefined) -> application:unset_env(imboy, Key);
restore_audio_env(Key, {ok, Value}) -> application:set_env(imboy, Key, Value).

audio_bytes() ->
    {ok, Bytes} = application:get_env(imboy, talk_record_t12_audio_bytes),
    Bytes.

%% S3-only direct 合同：按 register 返回的 grant 描述符直 PUT 成品分段
%% 字节（逐字头部含 checksum），再由 confirm 面收口 CONFIRMED。
upload_audio(_Uid, Part) ->
    application:ensure_all_started(inets),
    Ref = pick(Part, upload_ref, <<"upload_ref">>),
    Grant = pick(Ref, grant, <<"grant">>),
    Url = pick(Grant, url, <<"url">>),
    Hs0 = pick(Grant, required_headers, <<"required_headers">>),
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs0],
    Bytes = audio_bytes(),
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(
            put,
            {binary_to_list(Url), Hs, "audio/aac", Bytes},
            [],
            [{body_format, binary}]
        ),
    ok.

pick(Map, A, B) ->
    case maps:is_key(A, Map) of
        true -> maps:get(A, Map);
        false -> maps:get(B, Map)
    end.

%% no-backend-ffmpeg V1：固定合法 ADTS 字节（16kHz 单声道 39 字节帧 ×469
%% ≈ 30 秒，与 talk_record_audio_validate 同构），替换旧 ffmpeg 动态生成
%% ——分段 byte_size 声明取 byte_size(audio_bytes()) 自动自洽。
synthetic_audio() ->
    Length = 39,
    B3 = 16#40 bor (Length bsr 11),
    B4 = (Length bsr 3) band 16#FF,
    B5 = (Length band 7) bsl 5,
    Frame = <<16#FF, 16#F1, 16#60, B3, B4, B5, 16#00, 0:256>>,
    iolist_to_binary(lists:duplicate(469, Frame)).
