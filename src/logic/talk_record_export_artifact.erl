-module(talk_record_export_artifact).
-moduledoc "文字导出复用附件派生服务；业务引用与到期清理协调。".
-export([prepare_tx/4, write/2, publish_tx/2, read/1, sweep/0]).

prepare_tx(Conn, Job, Export, Generated) ->
    Rid = maps:get(<<"record_id">>, Export),
    Eid = maps:get(<<"id">>, Export),
    Bytes = maps:get(<<"content">>, Generated),
    Ext = maps:get(<<"format">>, Export),
    Nonce = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    Key = <<"talk-record/export/", Nonce/binary, ".", Ext/binary>>,
    Meta = #{
        <<"path">> => Key,
        <<"url">> => <<>>,
        <<"name">> => <<"talk-record.", Ext/binary>>,
        <<"size">> => byte_size(Bytes),
        <<"mime_type">> => mime(Ext),
        <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
        <<"scope">> => <<"talk_record">>,
        <<"scope_ref">> => integer_to_binary(Rid),
        <<"origin">> => <<"server_export">>
    },
    Source = #{
        job_id => maps:get(<<"id">>, Job),
        attempt => maps:get(<<"attempt">>, Job),
        actor_id => maps:get(<<"requester_id">>, Export)
    },
    case attachment_derived_artifact:reserve_tx(Conn, Source, Meta) of
        {ok, Aid} -> bind_prepared_tx(Conn, Rid, Eid, Aid);
        Error -> Error
    end.

bind_prepared_tx(Conn, Rid, Eid, Aid) ->
    Key = business_key(Eid, Aid),
    case
        talk_record_attachment_adapter:grant_processing_lease_tx(
            Conn, Rid, Aid, <<"export">>, Key, talk_record_export_logic:export_ttl_seconds()
        )
    of
        {ok, _, _} ->
            case talk_record_export_repo:set_artifact_tx(Conn, Eid, Aid) of
                ok ->
                    {ok, #{
                        attachment_id => Aid,
                        record_id => Rid,
                        export_id => Eid,
                        business_key => Key
                    }};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

write(#{attachment_id := Aid}, Bytes) -> attachment_derived_artifact:write(Aid, Bytes).

publish_tx(Conn, #{attachment_id := Aid, record_id := Rid, business_key := Key}) ->
    case attachment_derived_artifact:complete_tx(Conn, Aid) of
        ok ->
            case
                talk_record_attachment_adapter:grant_processing_lease_tx(
                    Conn, Rid, Aid, <<"export">>, Key, talk_record_export_logic:export_ttl_seconds()
                )
            of
                {ok, _, _} -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end.

read(#{<<"attachment_id">> := Aid}) when is_integer(Aid) -> attachment_derived_artifact:read(Aid);
read(_) -> {error, artifact_unavailable}.

%% Existing job-worker tick invokes this coordinator, including exports never read.
%% Retention/hold/other-reference decisions remain in the attachment lifecycle.
sweep() ->
    case attachment_derived_artifact:reconcile() of
        {ok, _} -> sweep_due();
        Error -> Error
    end.

sweep_due() ->
    case elib_pg:with_conn(fun talk_record_export_repo:due_records_tx/1) of
        {ok, Rows} -> sweep_records(Rows, 0);
        Error -> Error
    end.

sweep_records([], N) ->
    {ok, N};
sweep_records([#{<<"record_id">> := Rid} | Rest], N) ->
    case elib_pg:with_tx(fun(C) -> expire_record_tx(C, Rid) end, [{reraise, false}]) of
        {ok, Count} -> sweep_records(Rest, N + Count);
        Error -> Error
    end.

expire_record_tx(Conn, Rid) ->
    %% Parent first, then refs/attachment locks (same as delete/publish).
    case elib_pg:query(Conn, <<"SELECT id FROM talk_record WHERE id=$1 FOR UPDATE">>, [Rid]) of
        {ok, [_]} ->
            {ok, _} = elib_pg:execute(
                Conn,
                <<
                    "UPDATE talk_record_export SET state='expired',updated_at=now()"
                    " WHERE record_id=$1 AND expires_at<=now() AND state<>'expired'"
                >>,
                [Rid]
            ),
            case talk_record_attachment_adapter:evaluate_release_blockers_tx(Conn, Rid) of
                {ok, none} -> release_expired_tx(Conn, Rid);
                {ok, _} -> {ok, 0};
                Error -> Error
            end;
        Error ->
            Error
    end.

release_expired_tx(Conn, Rid) ->
    case talk_record_export_repo:expired_artifacts_tx(Conn, Rid) of
        {ok, Rows} -> release_rows_tx(Conn, Rid, Rows, 0);
        Error -> Error
    end.

release_rows_tx(_Conn, _Rid, [], N) ->
    {ok, N};
release_rows_tx(
    Conn, Rid, [#{<<"id">> := Eid, <<"attachment_id">> := Aid, <<"path">> := Path} | Rest], N
) ->
    {ok, _} = talk_record_attachment_adapter:release_tx(
        Conn, Rid, <<"export">>, business_key(Eid, Aid)
    ),
    case attachment_repo:refresh_gc_verified_tx(Conn, Path) of
        {ok, true} ->
            case
                attachment_repo:intent_ordinary_delete_tx(
                    Conn, Path, <<"talk_record_export_expired">>
                )
            of
                ok -> release_rows_tx(Conn, Rid, Rest, N + 1);
                {error, intent_conflict} -> release_rows_tx(Conn, Rid, Rest, N);
                {error, tombstone} -> release_rows_tx(Conn, Rid, Rest, N);
                Error -> Error
            end;
        {ok, false} ->
            release_rows_tx(Conn, Rid, Rest, N);
        Error ->
            Error
    end.

mime(<<"md">>) -> <<"text/markdown">>;
mime(_) -> <<"text/plain">>.
business_key(Eid, Aid) ->
    <<"export:", (integer_to_binary(Eid))/binary, ":", (integer_to_binary(Aid))/binary>>.
