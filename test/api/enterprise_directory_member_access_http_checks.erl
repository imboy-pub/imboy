-module(enterprise_directory_member_access_http_checks).
-export([setup/3, check/2]).
-include_lib("eunit/include/eunit.hrl").

%% Shares only the suite-owned marker DB. Human tokens are device-bound and
%% signed through production APIs; no auth/repo/transport decision is mocked.
setup(#{conn := C} = S, Org, User) ->
    exec(
        C,
        <<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES(995102,$1,'member','active'),(995102,995001,'member','active')">>,
        [User]
    ),
    [workspace(C, Id, O, User) || {Id, O} <- [{995800, Org}, {995801, null}, {995802, 995102}]],
    [group(C, G, W, User) || {G, W} <- [{995810, 995800}, {995811, null}, {995812, 995802}]],
    exec(
        C,
        <<"INSERT INTO project(id,workspace_id,name,owner_id,status) VALUES(995820,995800,'synthetic-access-project',$1,'active')">>,
        [User]
    ),
    exec(
        C,
        <<"INSERT INTO project_task(id,project_id,title,creator_id) VALUES(995821,995820,'synthetic-access-task',$1)">>,
        [User]
    ),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>, <<"oa-access">>, <<"synthetic.oa.access">>, <<"synthetic-oa-access-key">>
    ),
    enterprise_directory_channel_access_checks:setup(C, User),
    Did = <<"oa-access-device">>,
    {ok, Epoch} = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = enterprise_oa_login_repo:device_tx(Conn, User, Did, #{}),
        {ok, #{<<"epoch">> := E}} = enterprise_oa_login_repo:epoch_tx(Conn, User),
        {ok, E}
    end),
    {ok, _} = elib_pg:with_tx(fun(Conn) ->
        workspace_invite_repo:add_tx(Conn, 995800, <<"AAZZ2233">>, User, <<"2099-01-01T00:00:00Z">>)
    end),
    Token = maps:get(<<"token">>, token_ds:issue_device_session(User, Did, Epoch)),
    H = #{
        <<"authorization">> => <<"Bearer ", Token/binary>>,
        <<"cos">> => <<"synthetic">>,
        <<"vsn">> => <<"oa-access">>,
        <<"pkg">> => <<"synthetic.oa.access">>,
        <<"did">> => Did,
        <<"method">> => <<"sha256">>,
        <<"sign">> => elib_hasher:hmac_sha256(
            <<"oa-access-device|oa-access|synthetic|synthetic.oa.access">>,
            <<"synthetic-oa-access-key">>
        )
    },
    S#{human_headers => H, human_uid => User}.

%% This isolated single-node fixture has no cluster coverage service. Exercise
%% real retained caches without enabling distributed sensitive-cache admission.
check(S, Allowed) ->
    Saved = application:get_env(imboy, dsync_enabled),
    application:set_env(imboy, dsync_enabled, false),
    try
        check_current(S, Allowed)
    after
        case Saved of
            {ok, Value} -> application:set_env(imboy, dsync_enabled, Value);
            undefined -> application:unset_env(imboy, dsync_enabled)
        end
    end.

check_current(#{conn := C, human_uid := U} = S, Allowed) ->
    Expected =
        case Allowed of
            true -> 0;
            false -> 403
        end,
    [
        assert_http(S, P, Expected)
     || P <- [
            <<"/api/v1/workspaces/995800">>,
            <<"/api/v1/workspaces/995800/overview">>,
            <<"/api/v1/workspaces/995800/branding">>
        ]
    ],
    [
        assert_http(S, P, 0)
     || P <- [<<"/api/v1/workspaces/995801">>, <<"/api/v1/workspaces/995802">>]
    ],
    ?assertEqual(Allowed, workspace_logic:ensure_can_create_resource(995800, U) =:= ok),
    ?assertEqual(Allowed, is_success(project_task_logic:detail(U, 995821))),
    ?assertEqual(Allowed, is_success(project_member_logic:ensure_can_write(U, 995820))),
    {ok, Mine} = workspace_repo:page_by_member(U, 1, 100, <<"w.id">>),
    Ids = [maps:get(<<"id">>, W) || W <- maps:get(list, Mine)],
    ?assertEqual(Allowed, lists:member(995800, Ids)),
    ?assert(lists:member(995801, Ids)),
    ?assert(lists:member(995802, Ids)),
    [check_group(C, G, U, A) || {G, A} <- [{995810, Allowed}, {995811, true}, {995812, true}]],
    check_delivery(U, Allowed),
    enterprise_directory_channel_access_checks:check(C, U, Allowed),
    check_rejoin(C, U, Allowed).

check_group(C, G, U, Allowed) ->
    {ok, FromUser} = elib_pg:query(C, msg_c2g_recipient_sql:recipients(), [G, U, 1, 100]),
    ?assertEqual(Allowed, FromUser =/= []),
    {ok, FromPeer} = elib_pg:query(C, msg_c2g_recipient_sql:recipients(), [G, 995001, 1, 100]),
    ?assertEqual(Allowed, lists:any(fun(R) -> maps:get(<<"user_id">>, R) =:= U end, FromPeer)),
    ?assertEqual(Allowed, micro_community_access_repo:delivery_allowed(G, U)),
    case Allowed of
        false -> imboy_cache:invalidate({group, G});
        true -> ok
    end,
    {ok, CachedRecipients} = group_ds:member_uids_strict(G),
    ?assertEqual(Allowed, lists:member(U, CachedRecipients)),
    {ok, RelationUids} = imboy_cache:get({group, G}),
    ?assert(lists:member(U, RelationUids)).

check_delivery(U, Allowed) ->
    Bin =
        <<"{\"type\":\"C2G\",\"to\":995810,\"id\":\"oa-revoke-message\",\"payload\":{\"ciphertext\":\"synthetic\"}}">>,
    State = #{current_uid => U, framing => v2},
    Result = websocket_handler:websocket_info({reply, Bin}, State),
    case Allowed of
        true ->
            ?assertMatch({reply, _, State, hibernate}, Result);
        false ->
            ?assertEqual({ok, State, hibernate}, Result),
            Key = {U, <<"oa-access-device">>, <<"oa-revoke-message">>},
            Ref = make_ref(),
            ack_retry_cache:set(Key, Ref, 10000),
            ?assertEqual(
                {ok, State, hibernate},
                websocket_handler:websocket_info({timeout, Ref, {[1], Key, Bin}}, State)
            ),
            ?assertEqual(undefined, ack_retry_cache:get(Key))
    end.

assert_http(#{port := Port, human_headers := H}, P, Expected) ->
    Reply = intbe02_http_support:http(Port, <<"GET">>, P, <<>>, H),
    ?assertEqual(200, maps:get(status, Reply), {path, P}),
    ?assertMatch(#{<<"code">> := Expected}, jsone:decode(maps:get(body, Reply))).
is_success({ok, _}) -> true;
is_success(_) -> false.
workspace(C, Id, Org, U) ->
    exec(
        C,
        <<"INSERT INTO workspace(id,name,owner_id,status,organization_id) VALUES($1,'synthetic-access-ws',$2,'active',$3)">>,
        [Id, U, Org]
    ),
    exec(
        C,
        <<"INSERT INTO workspace_member(workspace_id,user_id,role,status) VALUES($1,$2,'owner','active'),($1,995001,'member','active')">>,
        [Id, U]
    ).
group(C, G, W, U) ->
    Scope =
        case W of
            null -> <<"personal">>;
            _ -> <<"workspace">>
        end,
    exec(
        C,
        <<"INSERT INTO \"group\"(id,owner_uid,creator_uid,title,type,join_limit,status,scope,workspace_id) VALUES($1,$2,$2,'synthetic-access-group',1,0,1,$3,$4)">>,
        [G, U, Scope, W]
    ),
    exec(
        C,
        <<"INSERT INTO group_member(id,group_id,user_id,role,status) VALUES($1,$2,$3,4,1),($4,$2,995001,1,1)">>,
        [G + 100, G, U, G + 200]
    ).
exec(C, Q, P) -> intbe02_http_support:sql_exec(C, Q, P).

check_rejoin(C, U, Allowed) ->
    {ok, [Before]} = elib_pg:query(
        C, <<"SELECT * FROM workspace_member WHERE workspace_id=995800 AND user_id=$1">>, [U]
    ),
    case Allowed of
        true ->
            ?assertMatch({ok, unchanged, _}, workspace_logic:join_by_code(U, <<"AAZZ2233">>));
        false ->
            ?assertMatch({error, {403, _}}, workspace_logic:join_by_code(U, <<"AAZZ2233">>)),
            ?assertMatch(
                {ok, role_conflict, _},
                elib_pg:with_tx(fun(Conn) ->
                    workspace_member_repo:upsert_active_tx(Conn, 995800, U, <<"member">>, 995001)
                end)
            )
    end,
    {ok, [After]} = elib_pg:query(
        C, <<"SELECT * FROM workspace_member WHERE workspace_id=995800 AND user_id=$1">>, [U]
    ),
    ?assertEqual(Before, After).
