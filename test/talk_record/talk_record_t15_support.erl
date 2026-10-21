%%% @doc T15 talk_record 内容域（原文/发言人/摘要）eunit 支撑（test-only）。
%%%
%%% 复用 talk_record_t05_support 的池/环境/tsid meck/夹具，叠加：
%%%   * 已转写记录构造器：真实走 talk_record_logic:create 建记录后，
%%%     以 SQL 直插 transcript/speakers/segments/summary/participant
%%%     （模拟 T13 ASR 产物形状——不冒充真实转写质量，仅提供 API 输入）；
%%%   * 审计行直查（断言 action/metadata 白名单，不含正文）。
-module(talk_record_t15_support).

-export([
    setup_all/0,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    create_record/3,
    create_org_record/4,
    seed_transcript/3,
    seed_speaker/4,
    seed_segment/7,
    seed_summary/4,
    add_participant/4,
    add_grant/5,
    audit_count/3,
    audit_row/3,
    rec_uuid/1,
    ds_uuid/1
]).

%%%===================================================================
%%% Setup / Teardown（复用 T05 支撑，行为一致）
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    talk_record_t05_support:setup_all().

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    talk_record_t05_support:teardown_all(State).

-spec seed(map()) -> ok.
seed(State) ->
    Conn = maps:get(conn, State),
    %% The isolated parent-table fixture lacked this existing production table.
    %% Only columns used by the real mutual-contact predicate are needed here.
    sql(
        Conn,
        "CREATE TABLE IF NOT EXISTS user_friend (id bigint PRIMARY KEY,"
        " from_user_id bigint NOT NULL,to_user_id bigint NOT NULL,status smallint NOT NULL DEFAULT 1)"
    ),
    sql(Conn, "DELETE FROM user_friend"),
    ok = talk_record_t05_support:seed_fixtures(Conn),
    sql(
        Conn,
        "INSERT INTO user_friend(id,from_user_id,to_user_id,status) VALUES"
        " (960001,700001,700002,1),(960002,700002,700001,1),"
        " (960003,700001,700003,1),(960004,700003,700001,1)"
    ),
    %% Content-domain fixtures do not own attachment/derived-write evidence.
    %% Deleting its parent rows now violates the derived-write lifecycle FK.
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
%%% 夹具构造
%%%===================================================================

-define(REC_BASE, 5100000).

-spec rec_uuid(integer()) -> binary().
rec_uuid(N) ->
    S = integer_to_binary(?REC_BASE + N),
    <<"00000000-0000-0000-0000-", (binary:copy(<<"0">>, 12 - byte_size(S)))/binary, S/binary>>.

-spec ds_uuid(integer()) -> binary().
ds_uuid(N) ->
    S = integer_to_binary(?REC_BASE + N),
    <<"10000000-0000-0000-0000-", (binary:copy(<<"0">>, 12 - byte_size(S)))/binary, S/binary>>.

%% @doc 建 personal 记录（logic 真链路），返回 {ok, RecordId}。
-spec create_record(integer(), integer(), binary()) -> {ok, integer()} | {error, term()}.
create_record(Uid, N, Title) ->
    Body = #{
        <<"client_recording_id">> => rec_uuid(N),
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"processing_mode">> => <<"transcribe">>,
        <<"title">> => Title
    },
    case talk_record_logic:create(Uid, Body, <<"t15-key-", (integer_to_binary(N))/binary>>) of
        {ok, _Mark, Payload, _ETag} -> {ok, maps:get(<<"id">>, Payload)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 建 organization 记录（logic 真链路，含 org/ws 组合校验）。
%% WsId = 0 表示不绑定工作区（workspace_id 可选）。
-spec create_org_record(integer(), integer(), integer(), integer()) ->
    {ok, integer()} | {error, term()}.
create_org_record(Uid, OrgId, WsId, N) ->
    WsField =
        case WsId of
            0 -> #{};
            _ -> #{<<"workspace_id">> => WsId}
        end,
    Body = maps:merge(
        #{
            <<"client_recording_id">> => rec_uuid(N),
            <<"notice_ack">> => true,
            <<"policy_version">> => <<"talk-record-notice-v1">>,
            <<"scope">> => <<"organization">>,
            <<"organization_id">> => OrgId,
            <<"processing_mode">> => <<"transcribe">>,
            <<"title">> => <<"t15-org"/utf8>>
        },
        WsField
    ),
    case talk_record_logic:create(Uid, Body, <<"t15-org-key-", (integer_to_binary(N))/binary>>) of
        {ok, _Mark, Payload, _ETag} -> {ok, maps:get(<<"id">>, Payload)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 发布转写版本：插 transcript 行（version/ready）+ 推进记录投影
%% （transcript_status=ready、current_transcript_id、media_status=available）。
%% 返回 TranscriptId。
-spec seed_transcript(pid() | map(), integer(), integer()) -> integer().
seed_transcript(Conn, RecordId, Version) ->
    Tid = elib_tsid:generate(talk_record),
    sql(Conn, [
        "INSERT INTO talk_record_transcript(id,record_id,version,source_manifest_hash,",
        "source_revision,language,status,model_version) VALUES (",
        integer_to_binary(Tid),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(Version),
        ", '",
        binary:copy(<<"a">>, 64),
        "', 1, 'zh-CN', 'ready', 'asr-fixture-v0')"
    ]),
    sql(Conn, [
        "UPDATE talk_record SET transcript_status='ready', current_transcript_id=",
        integer_to_binary(Tid),
        ", media_status='available', summary_status='ready' WHERE id=",
        integer_to_binary(RecordId)
    ]),
    Tid.

%% @doc 插说话人（默认 anonymous），返回 SpeakerId。
-spec seed_speaker(pid() | map(), integer(), binary(), map()) -> integer().
seed_speaker(Conn, RecordId, LocalLabel, Opts) ->
    Sid = elib_tsid:generate(talk_record),
    Matched = maps:get(matched_user_id, Opts, null),
    Source = maps:get(identity_source, Opts, <<"anonymous">>),
    ConfirmedBy = maps:get(confirmed_by, Opts, null),
    MatchedSql = value_sql(Matched),
    ConfirmedSql = value_sql(ConfirmedBy),
    sql(Conn, [
        "INSERT INTO talk_record_speaker(id,record_id,local_label,display_name,",
        "matched_user_id,identity_source,confirmed_by,confidence) VALUES (",
        integer_to_binary(Sid),
        ", ",
        integer_to_binary(RecordId),
        ", '",
        LocalLabel,
        "', '",
        maps:get(display_name, Opts, ""),
        "', ",
        MatchedSql,
        ", '",
        Source,
        "', ",
        ConfirmedSql,
        ", 0.8700)"
    ]),
    Sid.

%% @doc 插片段（当前版本），返回 SegmentId。
-spec seed_segment(
    pid() | map(),
    integer(),
    integer(),
    integer(),
    integer() | null,
    integer(),
    binary()
) -> integer().
seed_segment(Conn, RecordId, Version, Seq, SpeakerId, StartMs, Text) ->
    Gid = elib_tsid:generate(talk_record),
    SpeakerSql = value_sql(SpeakerId),
    %% text 单引号转义（夹具文本可控，仅防御）
    Escaped = binary:replace(Text, <<"'">>, <<"''">>, [global]),
    sql(Conn, [
        "INSERT INTO talk_record_segment(id,record_id,transcript_version,seq,speaker_id,",
        "start_ms,end_ms,text,confidence,is_final,origin) VALUES (",
        integer_to_binary(Gid),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(Version),
        ", ",
        integer_to_binary(Seq),
        ", ",
        SpeakerSql,
        ", ",
        integer_to_binary(StartMs),
        ", ",
        integer_to_binary(StartMs + 5000),
        ", '",
        Escaped,
        "', 0.9500, true, 'model')"
    ]),
    Gid.

%% @doc 插摘要（draft），返回 SummaryId。Decisions/Actions 为 Erlang term
%% （jsone:encode 后落库）。
-spec seed_summary(pid() | map(), integer(), integer(), map()) -> integer().
seed_summary(Conn, RecordId, Version, Opts) ->
    Uid = elib_tsid:generate(talk_record),
    Decisions = maps:get(decisions, Opts, []),
    Actions = maps:get(actions, Opts, []),
    SourceContentRev = maps:get(source_content_revision, Opts, 1),
    Status = maps:get(status, Opts, <<"draft">>),
    sql(Conn, [
        "INSERT INTO talk_record_summary(id,record_id,source_transcript_version,",
        "source_content_revision,status,overview,decisions,actions,model_version) VALUES (",
        integer_to_binary(Uid),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(Version),
        ", ",
        integer_to_binary(SourceContentRev),
        ", '",
        Status,
        "', '",
        maps:get(overview, Opts, "fixture overview"),
        "', '",
        binary:replace(jsone:encode(Decisions), <<"'">>, <<"''">>, [global]),
        "', '",
        binary:replace(jsone:encode(Actions), <<"'">>, <<"''">>, [global]),
        "', 'sum-fixture-v0')"
    ]),
    Uid.

%% @doc 参与者名单（§4 claim 前置；参与者≠授权≠认证）。
-spec add_participant(pid() | map(), integer(), integer(), binary()) -> ok.
add_participant(Conn, RecordId, Uid, DisplayName) ->
    sql(Conn, [
        "INSERT INTO talk_record_participant(id,record_id,user_id,display_name) VALUES (",
        integer_to_binary(elib_tsid:generate(talk_record)),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(Uid),
        ", '",
        DisplayName,
        "')"
    ]).

%% @doc 显式授权（viewer/editor；T17 grants API 未交付，直插等价行）。
-spec add_grant(pid() | map(), integer(), integer(), integer(), binary()) -> ok.
add_grant(Conn, RecordId, GranteeUid, GrantorUid, Role) ->
    sql(Conn, [
        "INSERT INTO talk_record_grant(id,record_id,grantee_user_id,role,allow_export,granted_by)",
        " VALUES (",
        integer_to_binary(elib_tsid:generate(talk_record)),
        ", ",
        integer_to_binary(RecordId),
        ", ",
        integer_to_binary(GranteeUid),
        ", '",
        Role,
        "', ",
        "false",
        ", ",
        integer_to_binary(GrantorUid),
        ")"
    ]).

%%%===================================================================
%%% 审计直查
%%%===================================================================

%% @doc 记录+动作的审计行数。
-spec audit_count(pid() | map(), integer(), binary()) -> non_neg_integer().
audit_count(Conn, RecordId, Action) ->
    Row = one(Conn, [
        "SELECT count(*) AS n FROM talk_record_audit WHERE record_id=",
        integer_to_binary(RecordId),
        " AND action='",
        Action,
        "'"
    ]),
    maps:get(<<"n">>, Row, 0).

%% @doc 记录+动作最新审计行（含 metadata_allowlist）。
-spec audit_row(pid() | map(), integer(), binary()) -> map().
audit_row(Conn, RecordId, Action) ->
    Row = one(Conn, [
        "SELECT id, actor_id, action, target_id, metadata_allowlist FROM talk_record_audit",
        " WHERE record_id=",
        integer_to_binary(RecordId),
        " AND action='",
        Action,
        "'",
        " ORDER BY created_at DESC, id DESC LIMIT 1"
    ]),
    case maps:get(<<"metadata_allowlist">>, Row, null) of
        Meta when is_binary(Meta) -> Row#{<<"metadata_allowlist">> => jsone:decode(Meta)};
        _ -> Row
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

-spec value_sql(integer() | null) -> binary() | string().
value_sql(null) ->
    "NULL";
value_sql(V) when is_integer(V) ->
    integer_to_binary(V).
