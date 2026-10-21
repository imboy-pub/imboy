%%% @doc T19 talk_record 导出/责任移交域 eunit 支撑（test-only）。
%%%
%%% 复用 T17 支撑（池/环境/jwt_key 注入/用户目录列），叠加：
%%%   * 导出行直查（state/content/expires_at/job 绑定）；
%%%   * job 行直查（export kind 终态断言）；
%%%   * 移交后 owner/custody/grant 快照读取；
%%%   * org 成员资格两态切换（custody 投影用例；用后恢复）。
-module(talk_record_t19_support).

-export([
    setup_all/0,
    teardown_all/1,
    seed/1,
    ids/0,
    sql/2,
    one/2,
    conn_of/1,
    rec_uuid/1,
    export_row/2,
    export_count/2,
    export_job/2,
    export_content/2,
    owner_and_custody/2,
    set_org_member_status/4,
    with_env/3
]).

%%%===================================================================
%%% Setup / Teardown（复用 T17 支撑：jwt_key 已注入）
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    talk_record_t17_support:setup_all().

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    talk_record_t17_support:teardown_all(State).

-spec seed(map()) -> ok.
seed(State) ->
    sql(maps:get(conn, State), "DELETE FROM attachment_derived_write"),
    ok = talk_record_upload_test_support:configure(),
    ok = talk_record_t17_support:seed(State),
    %% Preserve earlier attachment evidence while avoiding reused synthetic TSIDs.
    {ok, [#{<<"base">> := Base}]} = elib_pg:query(
        maps:get(conn, State),
        <<"SELECT greatest(coalesce(max(id),0),9000000000000) base FROM attachment">>,
        []
    ),
    meck:expect(elib_tsid, generate, fun(_) ->
        Base + erlang:unique_integer([positive, monotonic])
    end).

-spec ids() -> map().
ids() ->
    talk_record_t17_support:ids().

-spec sql(pid() | map(), iolist()) -> ok.
sql(Conn, SqlIolist) ->
    talk_record_t17_support:sql(Conn, SqlIolist).

-spec one(pid() | map(), iolist()) -> map().
one(Conn, SqlIolist) ->
    talk_record_t17_support:one(Conn, SqlIolist).

-spec conn_of(map()) -> pid().
conn_of(State) ->
    maps:get(conn, State).

-spec rec_uuid(integer()) -> binary().
rec_uuid(N) ->
    talk_record_t17_support:rec_uuid(N).

%% @doc 临时覆写 app env（用后恢复）。
-spec with_env(atom(), term(), fun(() -> term())) -> term().
with_env(Key, Value, Fun) ->
    talk_record_t17_support:with_env(Key, Value, Fun).

%%%===================================================================
%%% 导出行直查
%%%===================================================================

%% @doc 导出行（含 content/state/expires_at/requester_id/job_id/format）。
-spec export_row(pid() | map(), integer()) -> map().
export_row(Conn, ExportId) ->
    Row = one(Conn, [
        "SELECT id,record_id,requester_id,format,include_audio,attachment_id,job_id,",
        "state,expires_at FROM talk_record_export WHERE id=",
        integer_to_binary(ExportId)
    ]),
    Content =
        case maps:get(<<"state">>, Row) of
            <<"ready">> ->
                case talk_record_export_artifact:read(Row) of
                    {ok, B} -> B;
                    _ -> null
                end;
            _ ->
                null
        end,
    Row#{<<"content">> => Content}.

%% @doc 记录的导出行数。
-spec export_count(pid() | map(), integer()) -> non_neg_integer().
export_count(Conn, RecordId) ->
    Row = one(Conn, [
        "SELECT count(*) AS n FROM talk_record_export WHERE record_id=",
        integer_to_binary(RecordId)
    ]),
    maps:get(<<"n">>, Row, 0).

%% @doc 记录的 export kind job 行。
-spec export_job(pid() | map(), integer()) -> map().
export_job(Conn, RecordId) ->
    one(Conn, [
        "SELECT id, kind, state, error_code, finished_at FROM talk_record_job",
        " WHERE record_id=",
        integer_to_binary(RecordId),
        " AND kind='export'",
        " ORDER BY created_at DESC, id DESC LIMIT 1"
    ]).

-spec export_content(pid() | map(), integer()) -> binary() | null.
export_content(Conn, ExportId) ->
    maps:get(<<"content">>, export_row(Conn, ExportId), null).

%%%===================================================================
%%% 移交快照
%%%===================================================================

%% @doc owner/custody/created_by/scope/三版本域快照（§9/§11.2 断言用）。
-spec owner_and_custody(pid() | map(), integer()) -> map().
owner_and_custody(Conn, RecordId) ->
    one(Conn, [
        "SELECT owner_user_id, created_by, scope, custody_status,",
        " revision, content_revision, acl_revision FROM talk_record WHERE id=",
        integer_to_binary(RecordId)
    ]).

%% @doc org 成员资格两态切换（custody 投影用例；用后恢复 active）。
-spec set_org_member_status(pid() | map(), integer(), integer(), binary()) -> ok.
set_org_member_status(Conn, OrgId, Uid, Status) ->
    sql(Conn, [
        "UPDATE organization_member SET status='",
        Status,
        "' WHERE organization_id=",
        integer_to_binary(OrgId),
        " AND user_id=",
        integer_to_binary(Uid)
    ]).
