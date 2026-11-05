%% Recheck durable tombstones: a late PUT may appear after a successful DELETE.
-module(attachment_direct_cleanup_worker).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-export([start_link/0, run_batch/0, run_batch/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(INTERVAL, 30000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init(_) ->
    erlang:send_after(?INTERVAL, self(), tick),
    {ok, #{}}.
handle_info(tick, State) ->
    case safe_run_batch() of
        {ok, _} -> ok;
        {error, Reason} -> logger:warning("Direct cleanup failed: ~p", [Reason])
    end,
    erlang:send_after(?INTERVAL, self(), tick),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.
handle_call(_, _, State) -> {reply, ok, State}.
handle_cast(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

safe_run_batch() ->
    try
        run_batch()
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

run_batch() ->
    case worker_options() of
        {ok, Opts} ->
            case epgsql:connect(Opts) of
                {ok, Conn} ->
                    try
                        case worker_role(Conn) of
                            ok -> run_batch(Conn, fun delete/1);
                            {error, _} = Error -> Error
                        end
                    after
                        epgsql:close(Conn)
                    end;
                {error, _} ->
                    {error, worker_connection_failed}
            end;
        {error, _} = Error ->
            Error
    end.

worker_options() ->
    case
        {
            os:getenv("IMBOY_DIRECT_CLEANUP_PG_USER"),
            worker_password(),
            application:get_env(imboy, pg_conf)
        }
    of
        {User, {ok, Password}, {ok, #{start_mfa := {epgsql, connect, [Opts]}}}} when
            is_list(User), User =/= [], is_map(Opts)
        ->
            {ok, Opts#{username => User, password => Password}};
        _ ->
            {error, worker_credentials_missing}
    end.

worker_password() ->
    case
        {
            os:getenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD"),
            os:getenv("IMBOY_DIRECT_CLEANUP_PG_PASSWORD_FILE")
        }
    of
        {Password, false} when is_list(Password), Password =/= [] -> {ok, Password};
        {false, File} when is_list(File), File =/= [] ->
            case file:read_file_info(File) of
                {ok, #file_info{type = regular, mode = Mode}} when Mode band 8#077 =:= 0 ->
                    case file:read_file(File) of
                        {ok, Bytes} ->
                            case string:trim(binary_to_list(Bytes)) of
                                [] -> {error, missing};
                                Value -> {ok, Value}
                            end;
                        _ ->
                            {error, unreadable}
                    end;
                _ ->
                    {error, unsafe_file}
            end;
        _ ->
            {error, missing}
    end.

worker_role(Conn) ->
    Sql = <<
        "SELECT NOT (rolsuper OR rolreplication OR rolbypassrls) AND "
        "NOT EXISTS (SELECT 1 FROM pg_roles r WHERE r.rolname IN "
        "('v5_app_runtime','v5_proof','eb_direct_proof_worker') "
        "AND pg_has_role(session_user,r.rolname,'MEMBER')) AND "
        "NOT EXISTS (SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace "
        "WHERE n.nspname='public' AND c.relowner=(SELECT oid FROM pg_roles WHERE rolname=session_user)) AS safe "
        "FROM pg_roles WHERE rolname=session_user"
    >>,
    case elib_pg:query(Conn, Sql, []) of
        {ok, [#{<<"safe">> := true}]} ->
            case epgsql:squery(Conn, "SET ROLE v5_worker") of
                {ok, [], []} -> ok;
                _ -> {error, worker_role_unavailable}
            end;
        _ ->
            {error, unsafe_worker_identity}
    end.

run_batch(Conn, Delete) when is_function(Delete, 1) ->
    case loop(Conn, Delete, 100, 0) of
        {ok, Count} ->
            case attachment_cross_rescan:run_batch(Conn, Delete) of
                {ok, Scanned} -> {ok, Count + Scanned};
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.
loop(_, _, 0, Count) ->
    {ok, Count};
loop(Conn, Delete, Left, Count) ->
    Token = token(),
    case elib_pg:query(Conn, <<"SELECT * FROM fn_dv1_tombstone_claim($1)">>, [Token]) of
        {ok, []} ->
            {ok, Count};
        {ok, [Row]} ->
            case run_one(Conn, Delete, Row, Token) of
                ok -> loop(Conn, Delete, Left - 1, Count + 1);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
run_one(Conn, Delete, Row, Token) ->
    Key = maps:get(<<"out_physical_key">>, Row),
    Frozen = #{
        <<"storage_driver">> => maps:get(<<"out_driver">>, Row),
        <<"storage_namespace">> => maps:get(<<"out_namespace">>, Row),
        <<"storage_container">> => maps:get(<<"out_container">>, Row),
        <<"storage_locator_sha256">> => maps:get(<<"out_locator">>, Row),
        <<"storage_physical_key">> => Key
    },
    Result =
        case attachment_storage:frozen_location(Key, Frozen) of
            {ok, Location} when is_map(Location) -> Delete(Location);
            _ -> {error, location_unverified}
        end,
    Ok = Result =:= ok orelse Result =:= {error, not_found},
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_dv1_tombstone_ack($1,$2,$3) AS state">>,
            [maps:get(<<"out_id">>, Row), Token, Ok]
        )
    of
        {ok, [#{<<"state">> := State}]} when State =:= <<"done">>; State =:= <<"retry_wait">> -> ok;
        Other -> {error, {ack_failed, Other}}
    end.
delete(Location) -> attachment_storage:delete_snapshot(Location).
token() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.
