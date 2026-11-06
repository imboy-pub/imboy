-module(admin_avatar_session_repo_tests).
-include_lib("eunit/include/eunit.hrl").
%% 本模块定义了本地 register/2 辅助函数；OTP 27 会因预 R14 自动导入的
%% BIF register/2 报"ambiguous call"编译错误（OTP 28+ 仅告警）。显式
%% 关闭该自动导入，本地定义不再歧义。
-compile({no_auto_import, [register/2]}).

owner_test_() ->
    case os:getenv("C08_OWNER_DB") of
        false -> [];
        _ -> {timeout, 60, {setup, fun setup/0, fun cleanup/1, fun(_) -> ?_test(run()) end}}
    end.

setup() ->
    Db = os:getenv("C08_OWNER_DB"),
    true = is_list(Db) andalso lists:prefix("c08_owner_binding_", Db),
    logger:set_primary_config(level, warning),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    {ok, _} = application:ensure_all_started(crypto),
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    Endpoint = <<"http://127.0.0.1:49319">>,
    application:set_env(imboy, garage, #{endpoint => Endpoint, bucket => <<"private">>}),
    Root = #{
        driver => <<"garage">>,
        namespace => <<"owner-test">>,
        container => <<"private">>,
        endpoint => Endpoint
    },
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{owner => Root},
        upload_namespaces => #{<<"public">> => <<"owner-test">>}
    }),
    Opts = #{
        host => "127.0.0.1",
        port => 49320,
        username => os:getenv("POSTGRES_USER"),
        password => os:getenv("POSTGRES_PASSWORD"),
        database => os:getenv("C08_OWNER_DB"),
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = pooler:new_pool(#{
        name => pgsql,
        init_count => 2,
        max_count => 4,
        start_mfa => {epgsql, connect, [Opts]}
    }),
    {ok, [#{<<"comment">> := <<"task-owned-c08-avatar-owner-binding">>}]} = elib_pg:query(
        <<"SELECT shobj_description(oid,'pg_database') AS comment FROM pg_database WHERE datname=current_database()">>,
        []
    ),
    ok.

cleanup(_) -> pooler:rm_pool(pgsql).

run() ->
    Cid = uuid(),
    {ok, #{object_key := Key}} = register(Cid, 71),
    {ok, #{object_key := Key}} = register(Cid, 71),
    ?assertEqual({error, not_session_owner}, register(Cid, 72)),
    ?assertEqual(ok, admin_avatar_session_repo:owned(Key, 71)),
    ?assertEqual({error, not_session_owner}, admin_avatar_session_repo:owned(Key, 72)),
    Mixed = <<"AA000000-0000-4000-8000-000000000001">>,
    {ok, #{object_key := MixedKey}} = register(Mixed, 71),
    {ok, #{object_key := MixedKey}} = register(string:lowercase(Mixed), 71),
    {ok, #{object_key := MixedKey}} = attach_direct_logic:register_application_owned(
        Mixed, <<"0">>, <<"public">>, string:lowercase(Mixed), <<"a.png">>, 71
    ),
    ?assertEqual({error, not_session_owner}, register(string:lowercase(Mixed), 72)),
    Failed = uuid(),
    ?assertMatch(
        {error, _},
        attach_direct_logic:register_application_owned(
            Failed, <<"">>, <<"public">>, Failed, <<"a.png">>, 71
        )
    ),
    {ok, [#{<<"n">> := 0}]} = elib_pg:query(
        <<"SELECT count(*) AS n FROM admin_avatar_session_owner WHERE client_upload_id=$1::uuid">>,
        [Failed]
    ),
    Old = uuid(),
    {ok, _} = attach_direct_logic:register_application(
        Old, <<"0">>, <<"public">>, Old, <<"a.png">>
    ),
    ?assertEqual({error, unowned_session}, register(Old, 71)),
    ?assertEqual({error, invalid_upload_id}, register(<<"invalid">>, 71)),
    Race = uuid(),
    Parent = self(),
    [spawn(fun() -> Parent ! {owner_result, register(Race, Id)} end) || Id <- [81, 82]],
    Results = [
        receive
            {owner_result, R} -> R
        after 10000 -> timeout
        end
     || _ <- [1, 2]
    ],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([denied || {error, not_session_owner} <- Results])),
    Same = uuid(),
    [spawn(fun() -> Parent ! {same_result, register(Same, 91)} end) || _ <- [1, 2]],
    SameResults = [
        receive
            {same_result, R2} -> R2
        after 10000 -> timeout
        end
     || _ <- [1, 2]
    ],
    [{ok, #{object_key := SameKey}}, {ok, #{object_key := SameKey}}] = SameResults,
    CidUpper = <<"5F0E9C1E-1111-4222-8333-444455556666">>,
    ok = elib_pg:with_tx(fun(C) ->
        ai_agent_repo:bind_avatar_tx(
            C, <<"app880000001/avatar/fixture">>, CidUpper, 880000001, false, 71
        )
    end),
    {ok, [#{<<"bound_user_id">> := Created}]} = elib_pg:query(
        <<"SELECT bound_user_id FROM attachment_avatar_publication WHERE attachment_id=880000003">>,
        []
    ),
    ok = elib_pg:with_tx(fun(C) ->
        ai_agent_repo:bind_avatar_tx(
            C,
            <<"app5f0e9c1e-1111-4222-8333-444455556668/avatar/fixture">>,
            <<"5F0E9C1E-1111-4222-8333-444455556668">>,
            Created,
            true,
            71
        )
    end),
    {rollback, uppercase_subject_test} = elib_pg:with_tx(
        fun(C) ->
            {ok, _} = elib_pg:query(C, <<"SET LOCAL session_replication_role=replica">>, []),
            {ok, 1} = elib_pg:execute(
                C,
                <<"UPDATE attach_pending SET upload_registration=jsonb_set(upload_registration,'{subject,principal}',to_jsonb($2::text)) WHERE object_key=$1">>,
                [
                    <<"app5f0e9c1e-1111-4222-8333-444455556668/avatar/fixture">>,
                    <<"5F0E9C1E-1111-4222-8333-444455556668">>
                ]
            ),
            ok = ai_agent_repo:bind_avatar_tx(
                C,
                <<"app5f0e9c1e-1111-4222-8333-444455556668/avatar/fixture">>,
                <<"5f0e9c1e-1111-4222-8333-444455556668">>,
                Created,
                true,
                71
            ),
            throw(uppercase_subject_test)
        end,
        [{reraise, false}]
    ),
    ?assertEqual(
        {error, avatar_session_mismatch},
        ai_agent_repo:bind_avatar_tx(undefined, <<"bad">>, binary:copy(<<"x">>, 36), 1, false, 71)
    ),
    ok.

register(Cid, Admin) ->
    attach_direct_logic:register_application_owned(
        Cid, <<"0">>, <<"public">>, Cid, <<"a.png">>, Admin
    ).

uuid() ->
    H = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = H,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.
