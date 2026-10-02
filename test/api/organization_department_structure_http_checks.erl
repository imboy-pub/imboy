-module(organization_department_structure_http_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").
-define(H, intbe02_http_support).
-define(A, organization_department_app).
run() ->
    S = ?H:setup_all(),
    try
        configure(S),
        journey(S)
    after
        ?H:teardown_all(S),
        inttest_marker_db:release(S)
    end.
configure(S) ->
    application:set_env(imboy, jwt_key, <<"synthetic-department-role-key">>),
    ok = app_version_ds:set_sign_key(
        <<"synthetic">>, <<"seat-test">>, <<"synthetic.seat">>, <<"synthetic-seat-device-key">>
    ),
    C = maps:get(conn, S),
    ok = ?H:sql_exec(
        C,
        <<"UPDATE organization_member SET role='admin' WHERE organization_id=995101 AND user_id=995012">>
    ),
    ok = ?H:sql_exec(C, <<"UPDATE workspace SET owner_id=995011 WHERE id=995201">>),
    ok = ?H:sql_exec(
        C,
        <<"UPDATE workspace_member SET role='owner' WHERE workspace_id=995201 AND user_id=995011">>
    ),
    #{<<"role">> := <<"member">>, <<"status">> := <<"active">>} = ?H:one(
        C,
        <<"SELECT role,status FROM organization_member WHERE organization_id=995101 AND user_id=995011">>
    ),
    #{<<"role">> := <<"owner">>} = ?H:one(
        C,
        <<"SELECT role FROM workspace_member WHERE workspace_id=995201 AND user_id=995011">>
    ),
    ok.

journey(S) ->
    Parent = create(S, 995001, <<"parent">>),
    Dept = create(S, 995001, <<"target">>),
    Id = maps:get(<<"id">>, Dept),
    deny(S, 995013, Id, maps:get(<<"id">>, Parent)),
    deny(S, 995011, Id, maps:get(<<"id">>, Parent)),
    {ok, _} = ?A:add_member(995101, #{
        department_id => Id, user_id => 995013, actor_user_id => 995001
    }),
    {ok, _} = ?A:set_admin(995101, #{
        department_id => Id, user_id => 995013, admin => true, actor_user_id => 995001
    }),
    deny(S, 995013, Id, maps:get(<<"id">>, Parent)),
    {ok, _} = ?A:add_member(995101, #{
        department_id => Id, user_id => 995011, actor_user_id => 995013
    }),
    {error, {actor_not_permitted, 995013}} = ?A:add_member(
        995101,
        #{
            department_id => maps:get(<<"id">>, Parent),
            user_id => 995011,
            actor_user_id => 995013
        }
    ),
    io:format("DEPARTMENT_LOCAL_ADMIN_DELEGATION_PRESERVED=PASS~n"),
    allow(S, 995001, maps:get(<<"id">>, Parent)),
    allow(S, 995012, maps:get(<<"id">>, Parent)),
    io:format("DEPARTMENT_STRUCTURE_REAL_HTTP_RESULT=ok~n").

deny(S, User, Id, Parent) ->
    C = maps:get(conn, S),
    Before = snapshot(C),
    ?assertEqual(0, code(request(S, User, <<"GET">>, path(Id), #{}))),
    Bodies = [
        {<<"POST">>, collection(), #{<<"name">> => <<"forbidden">>}},
        {<<"PATCH">>, path(Id), #{<<"name">> => <<"changed">>, <<"expected_version">> => 1}},
        {<<"POST">>, <<(path(Id))/binary, "/move">>, #{
            <<"parent_id">> => Parent, <<"expected_version">> => 1
        }},
        {<<"POST">>, <<(path(Id))/binary, "/archive">>, #{}}
    ],
    Codes = [code(request(S, User, Method, Path, Body)) || {Method, Path, Body} <- Bodies],
    io:format("DEPARTMENT_STRUCTURE_DENY actor=~B codes=~p~n", [User, Codes]),
    ?assertEqual([403, 403, 403, 403], Codes),
    ?assertEqual(Before, snapshot(C)),
    io:format("DEPARTMENT_STRUCTURE_DENY_UNCHANGED=PASS~n").
allow(S, User, Parent) ->
    Dept = create(S, User, integer_to_binary(User)),
    Id = maps:get(<<"id">>, Dept),
    ?assertEqual(
        0,
        code(
            request(S, User, <<"PATCH">>, path(Id), #{
                <<"name">> => <<"renamed-", (integer_to_binary(User))/binary>>,
                <<"expected_version">> => 1
            })
        )
    ),
    ?assertEqual(
        409,
        code(
            request(S, User, <<"PATCH">>, path(Id), #{
                <<"name">> => <<"stale">>, <<"expected_version">> => 1
            })
        )
    ),
    ?assertEqual(
        0,
        code(
            request(S, User, <<"POST">>, <<(path(Id))/binary, "/move">>, #{
                <<"parent_id">> => Parent, <<"expected_version">> => 2
            })
        )
    ),
    ?assertEqual(0, code(request(S, User, <<"POST">>, <<(path(Id))/binary, "/archive">>, #{}))),
    #{
        <<"name">> := Name,
        <<"status">> := <<"archived">>,
        <<"version">> := 3,
        <<"parent_id">> := Parent
    } = ?H:one(
        maps:get(conn, S),
        <<"SELECT name,status,version,parent_id FROM organization_department WHERE id=$1">>,
        [Id]
    ),
    ?assertEqual(<<"renamed-", (integer_to_binary(User))/binary>>, Name),
    io:format("DEPARTMENT_STRUCTURE_OWNER_ADMIN_POSITIVE_CAS=PASS~n").
create(S, User, Name) ->
    Resp = request(S, User, <<"POST">>, collection(), #{<<"name">> => Name}),
    ?assertEqual(0, code(Resp)),
    maps:get(<<"payload">>, jsone:decode(maps:get(body, Resp))).
request(S, User, Method, Path, Body) ->
    ?H:http(
        maps:get(port, S),
        Method,
        Path,
        Body,
        customer_service_seat_http_checks:headers(#{actor_user_id => User})
    ).
code(Resp) ->
    ?assertEqual(200, maps:get(status, Resp)),
    maps:get(<<"code">>, jsone:decode(maps:get(body, Resp))).
collection() -> <<"/api/v1/organizations/995101/departments">>.
path(Id) -> <<(collection())/binary, "/", (integer_to_binary(Id))/binary>>.
snapshot(C) ->
    {ok, Rows} = elib_pg:query(
        C,
        <<"SELECT id,name,parent_id,status,version FROM organization_department WHERE organization_id=995101 ORDER BY id">>,
        []
    ),
    Rows.
