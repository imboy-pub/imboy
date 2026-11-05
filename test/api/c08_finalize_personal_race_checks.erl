-module(c08_finalize_personal_race_checks).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(C) ->
    Uid = 995011,
    G1 = 886000001,
    G2 = 886000002,
    seed(C, G1, 886001001, Uid),
    seed(C, G2, 886001002, Uid),
    finalize_first(C, G1, Uid),
    leave_first(C, G2, Uid),
    io:format(
        "PASS: personal group direct finalize serializes with member removal in both orders~n"
    ).

seed(C, G, M, U) ->
    {ok, 1} = epgsql:equery(
        C,
        "INSERT INTO \"group\" (id,type,join_limit,owner_uid,creator_uid,member_max,member_count,title,status,scope,workspace_id,created_at,updated_at) VALUES ($1,2,3,$2,$2,500,1,'c08-personal',1,'personal',NULL,now(),now())",
        [G, U]
    ),
    {ok, 1} = epgsql:equery(
        C,
        "INSERT INTO group_member (id,group_id,user_id,role,is_join,status,created_at,updated_at) VALUES ($1,$2,$3,1,true,1,now(),now())",
        [M, G, U]
    ),
    ok.

stage(G, U, Name) ->
    Bytes = <<"synthetic personal group race">>,
    Sha = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        U, <<"group">>, integer_to_binary(G), elib_uuid:gen_v7(), Name
    ),
    ok = attach_direct_logic:declare(Key, Sha, byte_size(Bytes), <<"text/plain">>),
    {ok, #{grant := #{url := Url, required_headers := Hs}, attempt_id := Attempt}} = attach_direct_logic:grant(
        Key, Sha
    ),
    Headers = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs],
    {ok, {{_, Status, _}, _, _}} = httpc:request(
        put,
        {binary_to_list(Url), Headers, "text/plain", Bytes},
        [{timeout, 30000}, {autoredirect, false}],
        [{body_format, binary}]
    ),
    ?assert(Status >= 200 andalso Status < 300),
    {Key, Attempt}.

finalize_first(C, G, U) ->
    {Key, Attempt} = stage(G, U, <<"finalize-first.txt">>),
    Parent = self(),
    ok = meck:new(attachment_repo, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_repo, authorize_upload_scope_tx_retryable, fun(
            Conn, Kind, Id, Uid
        ) ->
            R = meck:passthrough([Conn, Kind, Id, Uid]),
            case get(c08_personal_after) of
                true ->
                    true = R,
                    {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                    Parent ! {guard_acquired, Pid},
                    receive
                        guard_continue -> ok
                    after 15000 -> error(guard_timeout)
                    end;
                _ ->
                    ok
            end,
            R
        end),
        Final = spawn(fun() ->
            put(c08_personal_after, true),
            Parent ! {final_result, attach_direct_logic:finalize(Key, Attempt)}
        end),
        FinalPid =
            receive
                {guard_acquired, P} -> P
            after 15000 -> error(guard_pid_timeout)
            end,
        spawn(fun() ->
            R = elib_pg:with_tx(fun(Conn) ->
                {ok, _, [{LeaveBackend}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                Parent ! {leave_pid, LeaveBackend},
                epgsql:equery(Conn, "DELETE FROM group_member WHERE group_id=$1 AND user_id=$2", [
                    G, U
                ])
            end),
            Parent ! {leave_result, R}
        end),
        LeavePid =
            receive
                {leave_pid, P2} -> P2
            after 15000 -> error(leave_pid_timeout)
            end,
        ok = await_block(C, LeavePid, FinalPid, 200),
        Final ! guard_continue,
        receive
            {final_result, Result} -> ?assertMatch({ok, #{state := <<"CONFIRMED">>}}, Result)
        after 15000 -> error(final_timeout)
        end,
        receive
            {leave_result, Result2} -> ?assertMatch({ok, 1}, Result2)
        after 15000 -> error(leave_timeout)
        end,
        #{<<"direct_state">> := <<"CONFIRMED">>} = intbe02_http_support:one(
            C, <<"SELECT direct_state FROM attach_pending WHERE object_key=$1">>, [Key]
        ),
        ok
    after
        meck:unload(attachment_repo)
    end.

leave_first(C, G, U) ->
    {Key, Attempt} = stage(G, U, <<"leave-first.txt">>),
    Parent = self(),
    ok = meck:new(attachment_repo, [passthrough, no_link]),
    try
        ok = meck:expect(attachment_repo, authorize_upload_scope_tx_retryable, fun(
            Conn, Kind, Id, Uid
        ) ->
            case get(c08_personal_before) of
                true ->
                    {ok, _, [{Pid}]} = epgsql:equery(Conn, "SELECT pg_backend_pid()", []),
                    Parent ! {before_guard, Pid};
                _ ->
                    ok
            end,
            meck:passthrough([Conn, Kind, Id, Uid])
        end),
        {ok, _, [{Locker}]} = epgsql:equery(C, "SELECT pg_backend_pid()", []),
        ok = intbe02_http_support:sql_exec(C, <<"BEGIN">>),
        try
            {ok, 1} = epgsql:equery(
                C, "DELETE FROM group_member WHERE group_id=$1 AND user_id=$2", [G, U]
            ),
            spawn(fun() ->
                put(c08_personal_before, true),
                Parent ! {denied_result, attach_direct_logic:finalize(Key, Attempt)}
            end),
            Waiter =
                receive
                    {before_guard, P} -> P
                after 15000 -> error(before_guard_timeout)
                end,
            ok = await_block(C, Waiter, Locker, 200),
            ok = intbe02_http_support:sql_exec(C, <<"COMMIT">>),
            receive
                {denied_result, Result} -> ?assertEqual({error, forbidden}, Result)
            after 15000 -> error(denied_timeout)
            end,
            #{<<"n">> := 0} = intbe02_http_support:one(
                C, <<"SELECT count(*) AS n FROM attachment WHERE path=$1">>, [Key]
            ),
            #{<<"direct_state">> := State} = intbe02_http_support:one(
                C, <<"SELECT direct_state FROM attach_pending WHERE object_key=$1">>, [Key]
            ),
            ?assertNotEqual(<<"CONFIRMED">>, State),
            ok
        after
            catch epgsql:squery(C, "ROLLBACK")
        end
    after
        meck:unload(attachment_repo)
    end.

await_block(_C, _Waiter, _Locker, 0) ->
    error(personal_group_lock_timeout);
await_block(C, Waiter, Locker, N) ->
    case epgsql:equery(C, "SELECT $2::int=ANY(pg_blocking_pids($1::int))", [Waiter, Locker]) of
        {ok, _, [{true}]} ->
            ok;
        _ ->
            timer:sleep(50),
            await_block(C, Waiter, Locker, N - 1)
    end.
