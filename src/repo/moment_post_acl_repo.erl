-module(moment_post_acl_repo).
%%%
% moment_post_acl repository
%%%

-export([tablename/0]).
-export([add/1, add/2]).
-export([replace_for_post/4]).
-export([list_by_post/1]).
-export([list_uids_by_post/2]).
-export([delete_by_post/1, delete_by_post/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"moment_post_acl">>).

-spec add(map()) -> {ok, integer(), map()} | {error, term()}.
add(Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Tb, Data, <<"RETURNING id">>)).

-spec add(any(), map()) -> {ok, integer(), map()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    elib_pg_sql:parse_result(elib_pg:insert(Conn, Tb, Data, <<"RETURNING id">>)).

-spec replace_for_post(any(), integer(), [integer()], [integer()]) -> ok | {error, any()}.
replace_for_post(Conn, PostId, AllowUids0, DenyUids0) ->
    case delete_by_post(Conn, PostId) of
        {ok, _} ->
            AllowUids = normalize_uids(AllowUids0),
            DenyUids = normalize_uids(DenyUids0),
            case insert_acl_batch(Conn, PostId, AllowUids, 1) of
                ok ->
                    insert_acl_batch(Conn, PostId, DenyUids, 2);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_by_post(integer()) -> {ok, [map()]} | {error, any()}.
list_by_post(PostId) ->
    Tb = tablename(),
    Sql = <<"SELECT post_id, uid, acl_type, created_at"
            " FROM ", Tb/binary,
            " WHERE post_id = $1"
            " ORDER BY id ASC">>,
    elib_pg:query(Sql, [PostId]).

-spec list_uids_by_post(integer(), integer()) -> [integer()].
list_uids_by_post(PostId, AclType) ->
    Tb = tablename(),
    Sql = <<"SELECT uid FROM ", Tb/binary,
            " WHERE post_id = $1 AND acl_type = $2">>,
    case elib_pg:query(Sql, [PostId, AclType]) of
        {ok, Rows} ->
            [Uid || #{<<"uid">> := Uid} <- Rows, is_integer(Uid), Uid > 0];
        _ ->
            []
    end.

-spec delete_by_post(integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_post(PostId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1">>,
    elib_pg:execute(Sql, [PostId]).

-spec delete_by_post(any(), integer()) -> {ok, non_neg_integer()} | {error, any()}.
delete_by_post(Conn, PostId) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " WHERE post_id = $1">>,
    elib_pg:execute(Conn, Sql, [PostId]).

-spec normalize_uids([term()]) -> [integer()].
normalize_uids(Uids) ->
    lists:usort([Uid || Uid <- Uids, is_integer(Uid), Uid > 0]).

-spec insert_acl_batch(any(), integer(), [integer()], integer()) -> ok | {error, any()}.
insert_acl_batch(_Conn, _PostId, [], _AclType) ->
    ok;
insert_acl_batch(Conn, PostId, Uids, AclType) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    Rows = [[PostId, Uid, AclType, Now] || Uid <- Uids],
    {Sql0, Params} = elib_pg_sql:insert_batch(
        Tb,
        [post_id, uid, acl_type, created_at],
        Rows
    ),
    Sql = [Sql0, <<" ON CONFLICT (post_id, uid, acl_type) DO NOTHING">>],
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
