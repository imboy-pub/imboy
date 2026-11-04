%%% @doc T07 talk_record 采集/marks eunit 支撑（test-only，不进 release）。
%%%
%%% 复用 talk_record_t05_support 的池/环境/tsid meck/夹具，叠加：
%%%   * 记录/capture/事件/分段构造快捷方式（DS 直调，绕开 handler 传输）；
%%%   * 采集状态直查 SQL（断言 lease_epoch/status/duration 投影）；
%%%   * 确定性 sha256 夹具（64 位十六进制，非真实字节）。
-module(talk_record_t07_support).

-export([
    setup_all/0,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    create_record/3,
    create_capture/4,
    create_capture/5,
    send_event/5,
    register_part/5,
    sha/1
]).

%%%===================================================================
%%% Setup / Teardown（复用 T05 支撑，行为一致）
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    State = talk_record_t05_support:setup_all(),
    try
        ok = talk_record_upload_test_support:configure(),
        %% confirm 链经 garage copy_dispatch（httpc），每用例确保 inets 在 VM。
        {ok, _} = application:ensure_all_started(inets),
        ok = persistent_test_ids(maps:get(conn, State)),
        _ = retire_direct_residue(maps:get(conn, State)),
        State
    catch
        Class:Reason:Stack ->
            talk_record_t05_support:teardown_all(State),
            erlang:raise(Class, Reason, Stack)
    end.

%% direct-v1 每用户 4 活会话配额按非终态+deadline>now 计活（00000202
%% fn_dv1_quota_check）。恢复类用例有意留非终态残留，会把后续用例推过
%% 配额墙；本支撑在每用例 setup 经产品内置真实生命周期终态函数清退本库
%% 残留（不删行、不改 session_deadline——触发器已令其不可变）：
%%   GRANTED/FINALIZING → fn_dv1_cancel → CANCELLED（00000202）；
%%   DECLARED → fn_dv1_grant（本地 presign，零网络 I/O）→ fn_dv1_cancel；
%%   RESERVED → fn_dv1_declare（冻结新 expected）→ fn_dv1_grant → cancel。
%% attempt 身份约束 ck_dv1_attempt_identity 仅允许 driver='garage'。
%% 单行失败仅报告不中断：残留若未清退，配额墙会在该用例如实显形。
retire_direct_residue(Conn) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT object_key, direct_state FROM attach_pending"
        " WHERE direct_state IN ('RESERVED','DECLARED','GRANTED','FINALIZING')"
        " AND (session_deadline IS NULL OR session_deadline > now())",
        []
    ),
    lists:foreach(
        fun(Row) ->
            try
                retire_one(Conn, Row)
            catch
                Class:Reason ->
                    io:format("T07_direct_residue_retire_failed: ~0p ~0p~n", [
                        Row, {Class, Reason}
                    ])
            end
        end,
        Rows
    ),
    ok.

retire_one(Conn, {Key, State}) when State =:= <<"GRANTED">>; State =:= <<"FINALIZING">> ->
    retire_cancel(Conn, Key);
retire_one(Conn, {Key, <<"DECLARED">>}) ->
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_dv1_grant($1,'eunit-retire-staging','eunit-retire-target',"
        "'garage','talk-record-relay','talk-record-relay','eunit-retire')",
        [Key]
    ),
    retire_cancel(Conn, Key);
retire_one(Conn, {Key, <<"RESERVED">>}) ->
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_dv1_declare($1,$2,$3,$4)",
        [Key, retire_sha(), 1024, <<"audio/aac">>]
    ),
    retire_one(Conn, {Key, <<"DECLARED">>});
retire_one(_Conn, Row) ->
    erlang:error({retire_unknown_state, Row}).

retire_cancel(Conn, Key) ->
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_dv1_cancel($1,'eunit-teardown')", [Key]
    ),
    ok.

retire_sha() ->
    binary:encode_hex(crypto:hash(sha256, <<"eunit-direct-retire">>), lowercase).

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    talk_record_t05_support:teardown_all(State).

-spec seed(map()) -> ok.
seed(State) ->
    ok = talk_record_t05_support:seed_fixtures(maps:get(conn, State)),
    %% V5 assets, sessions, derived-write ledgers and deletion tombstones persist.
    %% They may only be retired through their real lifecycle, never seed DELETE.
    %% Current-record assertions and durable IDs isolate each synthetic case.
    ok.

-spec ids() -> map().
ids() ->
    talk_record_t05_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, Sql) ->
    talk_record_t05_support:sql(Conn, Sql).

-spec one(pid() | map(), iolist()) -> map().
one(Conn, Sql) ->
    talk_record_t05_support:one(Conn, Sql).

%%%===================================================================
%%% 构造快捷方式
%%%===================================================================

%% @doc 建一条 personal 记录，返回 {ok, Id}。
-spec create_record(integer(), binary(), binary()) -> {ok, integer()} | {error, term()}.
create_record(Uid, IdemKey, ClientRecordingId) ->
    Body = #{
        <<"client_recording_id">> => ClientRecordingId,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"title">> => <<"t07"/utf8>>
    },
    case talk_record_logic:create(Uid, Body, IdemKey) of
        {ok, _Mark, Payload, _ETag} -> {ok, maps:get(<<"id">>, Payload)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 建 capture（If-Match=RecordRevision 恒 1；T05 记录创建后 revision=1
%% 且采集路径不推进 record.revision），返回 {ok, CaptureId}。
-spec create_capture(integer(), integer(), binary(), binary()) -> {ok, integer()} | {error, term()}.
create_capture(Uid, RecordId, DeviceSessionId, IdemKey) ->
    create_capture(Uid, RecordId, DeviceSessionId, IdemKey, <<"t07-device-1">>).

-spec create_capture(integer(), integer(), binary(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
create_capture(Uid, RecordId, DeviceSessionId, IdemKey, DeviceId) ->
    Body = #{<<"device_session_id">> => DeviceSessionId, <<"device_id">> => DeviceId},
    Resource = <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/captures">>,
    {ok, Digest} = talk_record_logic:request_digest(<<"POST">>, Resource, Body),
    case talk_record_logic:parse_capture_create(Body) of
        {ok, Validated} ->
            case talk_record_ds:create_capture(Uid, RecordId, 1, Validated, IdemKey, Digest) of
                {ok, _Mark, Payload} -> {ok, maps:get(<<"capture_id">>, Payload)};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 发采集事件（DS 直调）。
-spec send_event(integer(), integer(), integer(), map(), map()) -> {ok, map()} | {error, term()}.
send_event(Uid, RecordId, CaptureId, Base, Overrides) ->
    Body = maps:merge(Base, Overrides),
    case talk_record_logic:parse_event(Body) of
        {ok, Validated} -> talk_record_ds:append_event(Uid, RecordId, CaptureId, Validated);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 注册分段（DS 直调；IdemKey 唯一即可），返回 {ok, Mark, Payload}。
-spec register_part(integer(), integer(), integer(), map(), binary()) ->
    {ok, created | replay, map()} | {error, term()}.
register_part(Uid, RecordId, CaptureId, Body, IdemKey) ->
    Resource =
        <<"/api/v1/talk-records/", (integer_to_binary(RecordId))/binary, "/captures/",
            (integer_to_binary(CaptureId))/binary, "/audio-parts">>,
    case talk_record_logic:parse_audio_part(Body) of
        {ok, Validated} ->
            case talk_record_logic:request_digest(<<"POST">>, Resource, Body) of
                {ok, Digest} ->
                    talk_record_ds:register_part(
                        Uid, RecordId, CaptureId, Validated, IdemKey, Digest
                    );
                {error, non_canonical} ->
                    {error, invalid_body}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 确定性实际字节 sha256 夹具（4096 字节的合成音频载荷）。
-spec sha(integer()) -> binary().
sha(N) ->
    binary:encode_hex(crypto:hash(sha256, talk_record_upload_test_support:bytes(N)), lowercase).

%% meck replaces generate/1 VM-wide; the runner must give this fixture exclusive
%% ownership of its BEAM VM and configured test database window.
%% Dedicated test-only sequence survives case/VM restarts and nextval is atomic.
%% Keep IDs in a separate positive bigint band, not the reset VM counter band.
persistent_test_ids(Conn) ->
    Expected = list_to_binary(os:getenv("T05_PG_DB", "talk_record_t05")),
    {ok, _, [{Expected}]} = epgsql:squery(Conn, "SELECT current_database()"),
    ok = authorize_id_database(Conn, Expected),
    {ok, _, _} = epgsql:squery(
        Conn,
        "CREATE SEQUENCE IF NOT EXISTS talk_record_test_entity_id"
        " AS bigint START WITH 900000000000000000 NO CYCLE"
    ),
    meck:expect(elib_tsid, generate, fun(_) ->
        {ok, _, [{RawId}]} = epgsql:squery(
            Conn,
            "SELECT nextval('talk_record_test_entity_id')"
        ),
        %% Simple-query values are text; parameterized bigint writes need integers.
        Id = binary_to_integer(RawId),
        true = Id > 0 andalso Id =< 9223372036854775807,
        Id
    end).

%% Legacy dedicated DBs retain their original guard and environment contract.
%% Owned children additionally require wrapper-issued proof plus live identity.
authorize_id_database(Conn, Expected) ->
    case binary:match(Expected, <<"talk_record_">>) of
        {0, 12} -> ok;
        _ -> talk_record_owned_eunit_fixture:assert_owned_database(Conn, Expected)
    end.
