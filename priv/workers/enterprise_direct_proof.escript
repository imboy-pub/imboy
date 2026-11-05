#!/usr/bin/env escript
%%! -noshell +Bd
%% Independent service identity: its DB login is a member of
%% eb_direct_proof_worker only; app runtime must not read this environment,
%% storage registry, task directory, or DB credential. Run one asset id per job.
main([Mode]) ->
    try
        Root = filename:dirname(filename:dirname(filename:dirname(escript:script_name()))),
        code:add_patha(filename:join(Root, "ebin")),
        Paths = case os:getenv("EB_PROOF_CODE_PATHS") of
            false -> filelib:wildcard(filename:join([Root,"deps","*","ebin"]));
            Value -> string:split(Value, ":", all)
        end,
        true = Paths =/= [] andalso lists:all(fun filelib:is_dir/1, Paths),
        [code:add_patha(P) || P <- Paths],
        {module, epgsql} = code:ensure_loaded(epgsql),
        {module, attachment_worker_port} = code:ensure_loaded(attachment_worker_port),
        {ok, _} = application:ensure_all_started(inets),
        {ok, _} = application:ensure_all_started(crypto),
        {ok, _} = application:ensure_all_started(ssl),
        put(proof_stage, registry),
        {ok, Namespace, Locator, Container} = configure_registry(),
        WorkerId = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
        put(proof_identity, {Namespace, Locator, WorkerId, Container}),
        put(proof_stage, connect),
        {ok, C} = epgsql:connect(required("EB_PROOF_PG_HOST"), required("EB_PROOF_PG_USER"),
            required("EB_PROOF_PG_PASSWORD"), [{database,required("EB_PROOF_PG_DB")},
                {port,list_to_integer(required("EB_PROOF_PG_PORT"))}, {ssl,ssl_mode()}]),
        put(proof_stage, identity),
        ok = verify_identity(C),
        put(proof_conn, C),
        {ok, [], []} = epgsql:squery(C, "SET ROLE eb_direct_proof_worker"),
        ok = run_mode(C, Mode),
        ok = epgsql:close(C),
        io:format("PROOF_RECORDED~n"),
        halt(0)
    catch _:_ ->
        %% Avoid logging task fields, credentials, connection details or URLs.
        offline_if_possible(),
        io:format(standard_error, "PROOF_FAILED stage=~p~n", [get(proof_stage)]),
        halt(1)
    end;
main(_) -> halt(2).

%% Refuse privileged or shared logins even if they can SET ROLE.
verify_identity(C) ->
    Sql = "SELECT r.rolname='imboy_eb_proof_worker' AND NOT "
          "(r.rolsuper OR r.rolcreatedb OR r.rolcreaterole OR r.rolreplication OR r.rolbypassrls OR r.rolinherit) "
          "AND pg_has_role(session_user,'eb_direct_proof_worker','MEMBER') "
          "AND NOT EXISTS (SELECT 1 FROM pg_roles x WHERE x.rolname IN "
          "('v5_app_runtime','v5_worker','v5_proof') AND pg_has_role(session_user,x.rolname,'MEMBER')) "
          "AND NOT EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles p ON p.oid=m.roleid "
          "WHERE m.member=r.oid AND p.rolname<>'eb_direct_proof_worker') "
          "AND NOT EXISTS (SELECT 1 FROM pg_auth_members m JOIN pg_roles p ON p.oid=m.member "
          "WHERE p.rolname='eb_direct_proof_worker') "
          "AND NOT EXISTS (SELECT 1 FROM pg_roles x WHERE x.rolcanlogin AND NOT x.rolsuper "
          "AND x.rolname<>session_user AND pg_has_role(x.oid,'eb_direct_proof_worker','MEMBER')) "
          "AND NOT EXISTS (SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace "
          "WHERE n.nspname='public' AND c.relowner=r.oid) FROM pg_roles r WHERE r.rolname=session_user",
    case epgsql:equery(C, Sql, []) of {ok, _, [{true}]} -> ok; _ -> error(unsafe_worker_identity) end.

run_mode(C, "--poll") -> poll(C);
run_mode(C, AssetText) ->
    Asset = list_to_integer(AssetText),
    true = Asset > 0,
    run_asset(C, Asset).

poll(C) ->
    {Namespace, Locator, _WorkerId, _Container} = get(proof_identity),
    put(proof_stage, storage_health),
    ok = storage_health(),
    put(proof_stage, heartbeat),
    ok = heartbeat(C),
    {ok, _, Rows} = epgsql:equery(C,
        "SELECT asset_id FROM fn_eb_direct_scan_candidates_for_location($1,$2,$3)",
        [Namespace, Locator, 20]),
    lists:foreach(fun({Asset}) ->
        put(proof_stage, storage_health),
        ok = storage_health(),
        put(proof_stage, heartbeat),
        ok = heartbeat(C),
        try run_asset(C, Asset) catch _:_ ->
            offline_if_possible(),
            io:format(standard_error, "PROOF_JOB_FAILED stage=~p~n", [get(proof_stage)]),
            {ok, _, _} = epgsql:equery(C, "SELECT fn_eb_direct_scan_failed($1)", [Asset])
        end
    end, Rows),
    receive after 5000 -> poll(C) end.

%% Signed HEAD verifies the configured credentials and bucket without fetching bytes.
storage_health() ->
    ok = scanner_ready(),
    {Namespace, _Locator, _WorkerId, Container} = get(proof_identity),
    HealthKey = list_to_binary(required("EB_PROOF_HEALTH_KEY")),
    case attachment_storage_garage:head_bounded(Namespace, Container, HealthKey) of
        {ok, _} -> ok;
        _ -> error(storage_unavailable)
    end.

scanner_ready() ->
    Script = attachment_worker_port:worker_script_path(),
    true = Script =/= false andalso filelib:is_regular(Script),
    {module, attachment_content_receipt} = code:ensure_loaded(attachment_content_receipt),
    {module, eb_asset_content} = code:ensure_loaded(eb_asset_content),
    ok.

heartbeat(C) ->
    {Namespace, Locator, WorkerId, _Container} = get(proof_identity),
    case epgsql:equery(C, "SELECT fn_eb_direct_worker_heartbeat($1,$2,$3)",
                       [Namespace, Locator, WorkerId]) of
        {ok, _, [{true}]} -> ok;
        _ -> error(heartbeat_failed)
    end.

offline_if_possible() ->
    case {get(proof_conn), get(proof_identity)} of
        {C, {Namespace, Locator, WorkerId, _}} when is_pid(C) ->
            try epgsql:equery(C, "SELECT fn_eb_direct_worker_offline($1,$2,$3)",
                              [Namespace, Locator, WorkerId]) of
                _ -> ok
            catch _:_ -> ok
            end;
        _ -> ok
    end.

required(Key) ->
    case os:getenv(Key) of false -> error(missing_config); "" -> error(missing_config); V -> V end.
ssl_mode() ->
    case required("EB_PROOF_PG_SSL") of "true" -> true; "false" -> false; _ -> error(invalid_ssl) end.

configure_registry() ->
    Ns = list_to_binary(required("EB_PROOF_STORAGE_NAMESPACE")),
    Endpoint = list_to_binary(required("EB_PROOF_STORAGE_ENDPOINT")),
    Container = list_to_binary(required("EB_PROOF_STORAGE_CONTAINER")),
    Storage = #{driver => <<"garage">>, namespace => Ns, endpoint => Endpoint,
        container => Container, access_key => list_to_binary(required("EB_PROOF_STORAGE_ACCESS_KEY")),
        secret_key => list_to_binary(required("EB_PROOF_STORAGE_SECRET_KEY")), region => <<"garage">>},
    ok = application:set_env(imboy, garage, Storage#{bucket => Container}),
    ok = application:set_env(imboy, attachment_storage_registry,
        #{version => 1, roots => #{Ns => Storage}, upload_namespaces => #{}}),
    {ok, Root} = attachment_storage:resolve(Ns),
    {ok, Ns, attachment_storage:locator_digest(Ns, Root), maps:get(container, Root)}.

run_asset(C, Asset) ->
    put(proof_stage, projection),
    {ok, _, [{Org, Ws, Asset, _Subject, Fence0}]} = epgsql:equery(C,
        "SELECT organization_id,workspace_id,asset_id,subject::text,fence::text FROM fn_eb_direct_scan_projection($1)", [Asset]),
    true = Org > 0 andalso Ws > 0,
    Fence = jsone:decode(Fence0),
    put(proof_stage, location),
    Key = maps:get(<<"object_key">>, Fence),
    {ok, Location = #{garage_config := Cfg}} = attachment_storage:frozen_location(Key, Fence),
    Physical = maps:get(key, Location),
    Secret = crypto:strong_rand_bytes(32),
    Attempt = {Key,maps:get(<<"attempt_no">>, Fence),maps:get(<<"dispatch_cas_uuid">>, Fence)},
    AttemptKey = binary:encode_hex(crypto:hash(sha256,term_to_binary(Attempt)),lowercase),
    Mime = maps:get(<<"mime_type">>, Fence),
    Task = #{endpoint => maps:get(endpoint,Cfg), ak => maps:get(access_key,Cfg), sk => maps:get(secret_key,Cfg),
        bucket => maps:get(container,Location), region => maps:get(region,Cfg,<<"garage">>),
        object_key => Physical, attempt_key => AttemptKey, mime => Mime, secret => Secret, deadline_ms => 60000},
    put(proof_stage, scan),
    {ok, #{verified := true,sha256 := Sha,size := Size,receipt := Receipt}} = attachment_worker_port:scan_external(Task),
    put(proof_stage, verification),
    Sha = maps:get(<<"file_hash256">>, Fence),
    Size = maps:get(<<"size">>, Fence),
    Ruleset = attachment_content_receipt:ruleset_digest(),
    Expected = #{attempt_key => AttemptKey,object_key => Physical,sha256 => Sha,size => Size,
                 mime => Mime,ruleset_digest => Ruleset},
    {ok, Expiry} = attachment_content_receipt:verify_with_expiry(Receipt,Secret,Expected),
    ReceiptSha = binary:encode_hex(crypto:hash(sha256,Receipt),lowercase),
    put(proof_stage, record),
    {ok, _, [{true}]} = epgsql:equery(C,
        "SELECT fn_eb_direct_record_scan($1,$2::jsonb,$3,$4,$5)",
        [Asset,jsone:encode(Fence),ReceiptSha,string:lowercase(Ruleset),Expiry]),
    ok.
