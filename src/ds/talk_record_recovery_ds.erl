-module(talk_record_recovery_ds).
-moduledoc "当前owner精确恢复；不以旧幂等快照代替当前权限。".
-export([lookup/2, replay/4, authorize_tx/4, notice_tx/4]).

lookup(Uid, Context) ->
    elib_pg:with_tx(
        fun(C) ->
            case talk_record_repo:find_recovery_tx(C, Uid, Context) of
                {ok, Row} -> {ok, projection(Row)};
                Error -> Error
            end
        end,
        [{reraise, false}]
    ).

replay(Uid, Context, Key, Digest) ->
    case
        talk_record_idempotency_repo:find(
            Uid,
            <<"POST">>,
            <<"/api/v1/talk-records">>,
            Key
        )
    of
        {ok, #{<<"payload_hash">> := Digest, <<"result_ref">> := Snap}} when
            is_binary(Snap)
        ->
            case lookup(Uid, Context) of
                {ok, #{<<"id">> := Id}} ->
                    #{<<"payload">> := P, <<"etag">> := E} = jsone:decode(Snap),
                    case maps:get(<<"id">>, P, undefined) =:= Id of
                        true -> {ok, replay, P, E};
                        false -> {error, not_found}
                    end;
                Error ->
                    Error
            end;
        {ok, _} ->
            miss;
        {error, not_found} ->
            miss;
        {error, Reason} ->
            {error, Reason}
    end.

authorize_tx(C, Uid, Context, Id) ->
    case talk_record_repo:find_recovery_tx(C, Uid, Context) of
        {ok, #{<<"id">> := Id} = Row} -> {ok, Row};
        {ok, _} -> {error, not_found};
        Error -> Error
    end.

%% 仅新建时登记本次实际提交的告知事实；不回填历史录音时刻。
notice_tx(C, Uid, Rid, V) ->
    Params = [
        elib_tsid:generate(talk_record),
        Rid,
        Uid,
        maps:get(policy_version, V),
        maps:get(processing_mode, V)
    ],
    case talk_record_recovery_repo:insert_notice_tx(C, Params) of
        {ok, 1} -> ok;
        _ -> throw({talk_record_rollback, {error, db_error}})
    end.

projection(Row) ->
    maps:with(
        [
            <<"id">>,
            <<"owner_user_id">>,
            <<"client_recording_id">>,
            <<"scope">>,
            <<"organization_id">>,
            <<"workspace_id">>,
            <<"revision">>,
            <<"capture_status">>,
            <<"media_status">>
        ],
        Row
    ).
