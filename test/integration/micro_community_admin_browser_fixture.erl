-module(micro_community_admin_browser_fixture).
-export([run/0]).
-import(micro_community_owned_http_fixture, [assert/2, rows/3, exec/3]).
-include_lib("kernel/include/file.hrl").

-define(BASE, 9007199254744000).
-define(ADMIN, (?BASE + 5)).
-define(READONLY, (?BASE + 6)).
-define(GROUP, (?BASE + 10)).
-define(C1, (?BASE + 20)).
-define(C2, (?BASE + 21)).
-define(MESSAGE, (?BASE + 30)).
-define(PRIVATE, "/Users/leeyi/.codex/micro-community-runs/mc-20261008T083109Z/private").
-define(CONTEXT, ?PRIVATE "/admin-browser-context-r1.json").
-define(STOP, ?PRIVATE "/admin-browser-stop-r1").

%% Root-only finite fixture lease. It issues real production sessions, not UI login.
run() -> micro_community_owned_http_fixture:run(fun hold/1).

hold(Ctx = #{conn := Conn, port := Port, credentials := Credentials}) ->
    private_preflight(),
    exec(Conn, <<"UPDATE public.adm_user SET role_id=ARRAY[4] WHERE id=$1">>, [?READONLY]),
    adm_acl:flush_cache(?READONLY),
    Dispatch = cowboy_router:compile([{'_', browser_routes()}]),
    ok = cowboy:set_env(mc_owned_http_listener, dispatch, Dispatch),
    Data = #{
        port => Port,
        admin_cookie => cookie(Credentials, ?ADMIN),
        readonly_cookie => cookie(Credentials, ?READONLY),
        channel_id => integer_to_binary(?C1),
        conflict_channel_id => integer_to_binary(?C2),
        group_id => integer_to_binary(?GROUP),
        message_id => integer_to_binary(?MESSAGE),
        database => <<"imboy_test_group">>,
        authentication_scope => <<"production signed session; UI login not tested">>
    },
    {ok, File} = file:open(?CONTEXT, [write, exclusive, binary, raw]),
    try
        ok = file:change_mode(?CONTEXT, 8#600),
        ok = file:write(File, jsone:encode(Data)),
        io:format("MC_ADMIN_BROWSER_LEASE_READY~n"),
        wait_stop(erlang:monotonic_time(millisecond) + 600000),
        final_oracles(Ctx)
    after
        file:close(File),
        file:delete(?CONTEXT),
        file:delete(?STOP)
    end.

private_preflight() ->
    {ok, #file_info{type = directory, mode = Mode}} = file:read_link_info(?PRIVATE),
    assert(Mode band 8#077 =:= 0, browser_private_directory_permissions),
    assert(file:read_link_info(?CONTEXT) =:= {error, enoent}, browser_context_preexisting),
    assert(file:read_link_info(?STOP) =:= {error, enoent}, browser_stop_preexisting).

cookie(Credentials, Id) ->
    list_to_binary(proplists:get_value("cookie", maps:get({admin, Id}, Credentials))).

browser_routes() ->
    Wanted = [
        "/api/adm/current",
        "/api/adm/rbac/me",
        "/api/adm/admin/config/features",
        "/api/adm/admin/config/policy",
        "/api/adm/admin/config/sidebar",
        "/api/adm/channel/detail/:channel_id",
        "/api/adm/channel/:channel_id/stats",
        "/api/adm/channel/:channel_id/messages",
        "/api/adm/channel/:channel_id/discussion",
        "/api/adm/channel/:channel_id/discussion/candidates",
        "/api/adm/channel/:channel_id/message/:message_id/essence"
    ],
    Routes = [
        R
     || {_, Rs} <- imboy_router:get_routes(),
        R = {Path, _, _} <- Rs,
        lists:member(Path, Wanted)
    ],
    assert(length(Routes) =:= length(Wanted), browser_route_whitelist_mismatch),
    Routes.

wait_stop(Deadline) ->
    assert(erlang:monotonic_time(millisecond) < Deadline, browser_lease_timeout),
    case file:read_link_info(?STOP) of
        {ok, #file_info{type = regular}} ->
            assert(file:read_file(?STOP) =:= {ok, <<"stop">>}, browser_stop_invalid);
        {error, enoent} ->
            receive
            after 250 -> wait_stop(Deadline)
            end;
        _ ->
            assert(false, browser_stop_invalid)
    end.

final_oracles(#{conn := Conn}) ->
    assert(
        rows(
            Conn,
            <<
                "SELECT id,discussion_group_id FROM public.channel "
                "WHERE id=ANY($1::bigint[]) ORDER BY id"
            >>,
            [[?C1, ?C2]]
        ) =:=
            [{?C1, ?GROUP}, {?C2, null}],
        browser_binding_final_state
    ),
    assert(
        rows(
            Conn,
            <<"SELECT is_essence,is_pinned FROM public.channel_message WHERE id=$1">>,
            [?MESSAGE]
        ) =:= [{false, true}],
        browser_essence_final_state
    ),
    assert(
        rows(
            Conn,
            <<
                "SELECT count(*) FROM public.group_log WHERE group_id=$1 "
                "AND option_uid=$2 AND type=302"
            >>,
            [?GROUP, ?ADMIN]
        ) =:= [{1}],
        browser_binding_audit_count
    ),
    io:format(
        "MC_ADMIN_BROWSER_DB_SUBORACLES_PASS binding=1 conflict=0 essence=false pin=true audit=1~n"
    ),
    ok.
