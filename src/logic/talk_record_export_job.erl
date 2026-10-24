-module(talk_record_export_job).
-moduledoc "文字导出任务：有界快照、附件产物与租约原子发布。".
-export([run/1, execute_tx/2, prepare_tx/2, fail/2]).

run(Job) ->
    case transaction(fun(C) -> prepare_tx(C, Job) end) of
        {ok, Prepared} -> write_and_publish(Job, Prepared);
        Error -> Error
    end.

prepare_tx(Conn, Job) ->
    Rid = maps:get(<<"record_id">>, Job),
    {ok, _} = require(talk_record_export_repo:lock_record_tx(Conn, Rid)),
    {ok, _} = require_live_job(Conn, Job),
    {ok, Export} = require(talk_record_export_repo:find_by_job_tx(Conn, maps:get(<<"id">>, Job))),
    {ok, Record} = require(
        talk_record_export_logic:authorize_tx(
            Conn, maps:get(<<"requester_id">>, Export), Rid
        )
    ),
    ok = check_input(Record, Job),
    {ok, Generated} = require(talk_record_export_logic:snapshot_tx(Conn, Export)),
    {ok, Artifact} = require(talk_record_export_artifact:prepare_tx(Conn, Job, Export, Generated)),
    {ok, #{export => Export, artifact => Artifact, generated => Generated}}.

write_and_publish(Job, #{artifact := Artifact, generated := Generated} = Prepared) ->
    case talk_record_export_artifact:write(Artifact, maps:get(<<"content">>, Generated)) of
        ok -> transaction(fun(C) -> publish_tx(C, Job, Prepared) end);
        Error -> Error
    end.

%% 兼容事务调用只允许已写好的产物，禁止事务中做存储 I/O。
execute_tx(_Conn, _Job) -> {error, export_requires_worker}.

publish_tx(Conn, Job, #{export := Export, artifact := Artifact}) ->
    Rid = maps:get(<<"record_id">>, Job),
    {ok, _} = require(talk_record_export_repo:lock_record_tx(Conn, Rid)),
    {ok, _} = require_live_job(Conn, Job),
    {ok, Record} = require(
        talk_record_export_logic:authorize_tx(
            Conn, maps:get(<<"requester_id">>, Export), Rid
        )
    ),
    ok = check_input(Record, Job),
    ok = require(talk_record_export_artifact:publish_tx(Conn, Artifact)),
    {ok, Ready} = require(
        talk_record_export_repo:mark_ready_tx(
            Conn,
            maps:get(<<"id">>, Export),
            maps:get(attachment_id, Artifact),
            talk_record_export_logic:export_ttl_seconds()
        )
    ),
    %% CAS 失败抛回滚，不能返回普通 error 而提交 ready/引用。
    ok = require(
        talk_record_job_repo:finish_success_tx(
            Conn,
            maps:get(<<"id">>, Job),
            maps:get(<<"lease_token">>, Job)
        )
    ),
    {ok, Ready}.

require_live_job(Conn, Job) ->
    case talk_record_export_repo:lock_job_tx(Conn, Job) of
        {error, not_found} ->
            abort(lease_lost);
        {ok, Locked} = Result ->
            Fields = [
                <<"id">>,
                <<"record_id">>,
                <<"kind">>,
                <<"input_content_revision">>,
                <<"lease_token">>
            ],
            case maps:with(Fields, Locked) =:= maps:with(Fields, Job) of
                true -> Result;
                false -> abort(input_stale)
            end;
        {error, Reason} ->
            abort(Reason)
    end.

check_input(Record, Job) ->
    case maps:get(<<"content_revision">>, Record) =:= maps:get(<<"input_content_revision">>, Job) of
        true -> ok;
        false -> abort(input_stale)
    end.

transaction(Fun) ->
    case elib_pg:with_tx(Fun, [{reraise, false}]) of
        {rollback, {talk_export_abort, Reason}} -> {error, Reason};
        Other -> Other
    end.

require({error, Reason}) -> abort(Reason);
require(Value) -> Value.
abort(Reason) -> throw({talk_export_abort, Reason}).

fail(Job, ErrorCode) ->
    transaction(fun(C) ->
        {ok, _} = require(
            talk_record_export_repo:lock_record_tx(C, maps:get(<<"record_id">>, Job))
        ),
        {ok, _} = require_live_job(C, Job),
        case talk_record_export_repo:find_by_job_tx(C, maps:get(<<"id">>, Job)) of
            {ok, Export} ->
                ok = require(talk_record_export_repo:mark_failed_tx(C, maps:get(<<"id">>, Export)));
            {error, not_found} ->
                ok;
            {error, Reason} ->
                abort(Reason)
        end,
        require(
            talk_record_job_repo:finish_failed_tx(
                C,
                maps:get(<<"id">>, Job),
                maps:get(<<"lease_token">>, Job),
                ErrorCode
            )
        )
    end).
