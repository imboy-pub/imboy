-module(talk_record_participant_repo).
-moduledoc "面聊参与者仓储；父记录先锁定，复用统一可见/企业工作区ACL。".
-export([record_tx/4, list_tx/2, editable_tx/3, display_name_tx/2, replace_tx/3]).

-spec record_tx(any(), integer(), integer(), read | write) -> {ok, map()} | {error, term()}.
record_tx(C, Uid, Rid, Mode) ->
    Lock =
        case Mode of
            read -> <<" FOR SHARE">>;
            write -> <<" FOR UPDATE">>
        end,
    case elib_pg:query(C, <<"SELECT id FROM talk_record WHERE id=$1", Lock/binary>>, [Rid]) of
        {ok, [_]} -> visible_tx(C, Uid, Rid);
        {ok, []} -> {error, not_found};
        Error -> Error
    end.
visible_tx(C, Uid, Rid) ->
    Sql =
        <<"SELECT ", (talk_record_repo:detail_columns())/binary,
            " FROM talk_record tr WHERE tr.id=$2 AND (",
            (talk_record_repo:visible_access_sql(all))/binary, ")">>,
    case elib_pg:query(C, Sql, [Uid, Rid]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

-spec editable_tx(any(), integer(), map()) -> {ok, boolean()} | {error, term()}.
editable_tx(_C, Uid, #{<<"owner_user_id">> := Uid}) ->
    {ok, true};
editable_tx(C, Uid, #{<<"id">> := Rid}) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT 1 FROM talk_record_grant WHERE record_id=$1"
                " AND grantee_user_id=$2 AND role='editor' AND revoked_at IS NULL"
                " AND (expires_at IS NULL OR expires_at>clock_timestamp())"
            >>,
            [Rid, Uid]
        )
    of
        {ok, [_]} -> {ok, true};
        {ok, []} -> {ok, false};
        Error -> Error
    end.

-spec list_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
list_tx(C, Rid) ->
    elib_pg:query(
        C,
        <<
            "SELECT id,user_id,display_name,attendance_source"
            " FROM talk_record_participant WHERE record_id=$1 ORDER BY id"
        >>,
        [Rid]
    ).

-spec display_name_tx(any(), integer()) -> {ok, binary()} | {error, term()}.
display_name_tx(C, Uid) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT nickname FROM \"user\" WHERE id=$1"
                " AND status=1 AND account_type=0"
            >>,
            [Uid]
        )
    of
        {ok, [#{<<"nickname">> := Name}]} when is_binary(Name) -> {ok, Name};
        {ok, [#{<<"nickname">> := null}]} -> {ok, <<>>};
        {ok, []} -> {error, target_not_in_directory};
        Error -> Error
    end.

-spec replace_tx(any(), integer(), [map()]) -> {ok, pos_integer()} | {error, term()}.
replace_tx(C, Rid, Entries) ->
    case elib_pg:execute(C, <<"DELETE FROM talk_record_participant WHERE record_id=$1">>, [Rid]) of
        {ok, _} -> insert_all(C, Rid, Entries);
        Error -> Error
    end.
insert_all(C, Rid, []) ->
    case
        elib_pg:query(
            C,
            <<
                "UPDATE talk_record SET revision=revision+1,updated_at=now()"
                " WHERE id=$1 AND deleted_at IS NULL RETURNING revision"
            >>,
            [Rid]
        )
    of
        {ok, [#{<<"revision">> := Rev}]} -> {ok, Rev};
        {ok, []} -> {error, not_found};
        Error -> Error
    end;
insert_all(C, Rid, [#{user_id := Uid, display_name := Name, attendance_source := Source} | Rest]) ->
    Id = elib_tsid:generate(talk_record),
    case
        elib_pg:execute(
            C,
            <<
                "INSERT INTO talk_record_participant"
                " (id,record_id,user_id,display_name,attendance_source) VALUES($1,$2,$3,$4,$5)"
            >>,
            [Id, Rid, Uid, Name, Source]
        )
    of
        {ok, 1} -> insert_all(C, Rid, Rest);
        Error -> Error
    end.
