%% Independent backend acceptance admission. Execution stays in the existing full runner.
%% No legacy task gate, recovery authorization or budget is inferred by this schema.
-module(imboy_quality_backend).
-include_lib("kernel/include/file.hrl").
-export([selection/2, is_state/1, reserve/3, reserve_directory/1, read_state/1,
    authorize/2, resource_root/2, snapshot/1, context/1, verify_expected/4,
    verify_result/3, expected/1, require_build/2, save/2, preview/2, verify_pg_exit/1, result_exit/1,
    begin_resources/1, end_resources/0, enter_run/1, track_launch/3, track_pg/1, track_prepare/1, clear_prepare/0,
    provision_digest/4, verify_provision/1]).

-define(SCHEMA, <<"IMBOY_BACKEND_ACCEPTANCE_V1">>).

selection(false, ["backend-acceptance"]) -> error(backend_selector_required);
selection(false, _) -> legacy;
selection("1", ["backend-acceptance"]) -> backend;
selection("1", ["build-check"]) -> backend_build;
selection("1", ["build-seal"]) -> backend_build;
selection("1", _) -> error(backend_requires_explicit_operation);
selection(_, _) -> error(invalid_backend_selector).

is_state(#{<<"schema">> := ?SCHEMA}) -> true;
is_state(_) -> false.

%% A fresh directory is the atomic, single-use budget reservation. Never adopt an
%% existing run, even if its previous controller died before it wrote STATE.
reserve_directory(Ctrl) ->
    case file:make_dir(Ctrl) of
        ok -> ok = file:change_mode(Ctrl, 8#700);
        {error,eexist} -> error({run_already_reserved,Ctrl});
        Other -> error({run_reservation_failed,Other})
    end.

reserve(Repo, Identity, Workers) ->
    Root = required("IMBOY_BACKEND_RUN_ROOT"),
    absolute = filename:pathtype(Root),
    Root = filename:absname(Root),
    false = lists:prefix(filename:absname(Repo) ++ "/", Root ++ "/"),
    ok = safe_directory(Root),
    Ctrl = filename:join(Root,"control"),
    ok = reserve_directory(Ctrl),
    ok = file:make_dir(filename:join(Ctrl,"evidence")),
    ResourceRoot = filename:join(Root,"resources"),
    ok = safe_directory(ResourceRoot),
    {ok,#file_info{uid=Uid}} = file:read_link_info(Ctrl),
    {ok,#file_info{type=directory,uid=Uid}} = file:read_link_info(ResourceRoot),
    State0 = initial_state(Repo,Identity,Workers,Root,Ctrl,ResourceRoot,Uid),
    State = State0#{<<"frozen_inputs">>=>snapshot(Repo)},
    ok = save(Ctrl,State),
    %% Expected obligations and context must exist before any resource is started.
    E = expected(State),
    C = context(State),
    #{schema:=backend_expected_v1,roots:=Roots,zero_leaf_roots:=Zero,leaf_manifest:=L} = E,
    Roots = inventory_roots(Repo),
    true = Zero =:= lists:usort(Zero),
    true = ordsets:is_subset(Zero,Roots),
    #{kind:=expected,context:=C,roots:=NonZero} = L,
    NonZero = ordsets:subtract(Roots,Zero),
    true = NonZero =/= [],
    ok = imboy_quality_leaf_identity:validate_expected(L),
    {Ctrl,State}.

initial_state(Repo,Identity,Workers,Root,Ctrl,ResourceRoot,Uid) ->
    Reason = unicode:characters_to_binary(required("IMBOY_BACKEND_RUN_REASON")),
    Config = config_path(Repo),
    {ok,[Terms]} = file:consult(Config),
    Imboy = proplists:get_value(imboy,Terms),
    #{start_mfa := {epgsql,connect,[PGArgs]}} = proplists:get_value(pg_conf,Imboy),
    <<"127.0.0.1">> = iolist_to_binary(maps:get(host,PGArgs)),
    Port = maps:get(port,PGArgs),
    true = is_integer(Port) andalso Port > 0 andalso Port < 65536,
    PGExec = filename:join(ResourceRoot,"postgres-toolchain/bin/postgres"),
    Data = filename:join(ResourceRoot,"pgdata"),
    ok = safe_directory(Data),
    SystemId = unicode:characters_to_binary(required("IMBOY_BACKEND_PG_SYSTEM_IDENTIFIER")),
    true = byte_size(SystemId)>0,
    true = lists:all(fun(C)-> C >= $0 andalso C =< $9 end,binary_to_list(SystemId)),
    ExpectedPath = filename:absname(required("IMBOY_BACKEND_EXPECTED")),
    false = lists:prefix(filename:absname(Repo) ++ "/", ExpectedPath),
    {ok,[_]} = file:consult(ExpectedPath),
    RunId = "backend-" ++ binary_to_list(string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16)))),
    Marker = filename:join(ResourceRoot,".imboy-quality-owned"),
    MarkerBytes = term_to_binary({backend_acceptance,RunId,Identity}),
    ok = exclusive_write(Marker,MarkerBytes),
    #{<<"schema">>=>?SCHEMA,<<"worktree">>=>b(Repo),
        <<"control_dir">>=>b(Ctrl),<<"run_id">>=>b(RunId),<<"run_reason">>=>Reason,
        <<"owner">>=>Identity,<<"checkpoint_sequence">>=>0,
        <<"config">>=>b(Config),<<"expected_path">>=>b(ExpectedPath),
        <<"expected_sha256">>=>file_hash(ExpectedPath),<<"workers">>=>Workers,
        <<"owned_resources">>=>[
            #{<<"kind">>=><<"OWNED_DIRECTORY">>,<<"path">>=>b(ResourceRoot),
                <<"ownership_marker">>=>b(Marker),<<"ownership_marker_sha256">>=>digest(MarkerBytes),
                <<"filesystem_identity">>=>identity(ResourceRoot),<<"control_identity">>=>identity(Ctrl),
                <<"parent_identity">>=>identity(Root),<<"uid">>=>Uid,
                <<"owner_start_identity">>=>maps:get(<<"start_identity">>,Identity)},
            #{<<"kind">>=><<"ISOLATED_POSTGRESQL">>,<<"status">>=><<"PREPARED">>,
                <<"host">>=><<"127.0.0.1">>,<<"port">>=>Port,<<"data_dir">>=>b(Data),
                <<"executable">>=>b(PGExec),<<"executable_sha256">>=>file_hash(PGExec),
                <<"system_identifier">>=>SystemId}],
        <<"provision">>=>provision_digest(Config,ResourceRoot,SystemId,Workers)}.

save(Ctrl,State) ->
    {ok,Sha} = imboy_quality_state:write(filename:join(Ctrl,"STATE.json"),State),
    {ok,Sha} = imboy_quality_state:write(filename:join(Ctrl,"checkpoint.json"),State),
    Hash = string:lowercase(binary:encode_hex(Sha)),
    lists:foreach(fun(Name)->ok=file:write_file(filename:join(Ctrl,Name),Hash,[sync]) end,
        ["STATE.json.sha256","checkpoint.json.sha256"]),
    ok.

read_state(Ctrl) ->
    {ok,State,Sha} = imboy_quality_state:read(filename:join(Ctrl,"STATE.json")),
    {ok,Hash} = file:read_file(filename:join(Ctrl,"STATE.json.sha256")),
    true = string:lowercase(binary:encode_hex(Sha)) =:= string:trim(Hash),
    true = is_state(State),
    State.

%% Cleanup still needs live owner/directory proof, but source drift must never
%% prevent the owner from stopping its own PostgreSQL process.
authorize(State, Action) ->
    try
        true = is_state(State),
        %% Fail before owner/Port startup when provisioning changed after freeze.
        case Action of start_pg -> ok=verify_provision(State); _ -> ok end,
        {ok,Repo}=file:get_cwd(),
        RepoBin=b(Repo), RepoBin = maps:get(<<"worktree">>,State),
        "1" = os:getenv("IMBOY_BACKEND_ACCEPTANCE"),
        Ctrl = binary_to_list(maps:get(<<"control_dir">>,State)),
        Ctrl = os:getenv("IMBOY_QUALITY_CTRL"),
        Token = b(required("IMBOY_QUALITY_OWNER_TOKEN")),
        true = imboy_quality_task_owner:verify(State,Token,b(Repo)),
        _ = resource_root(Ctrl,State),
        case Action of
            cleanup -> ok;
            build ->
                Frozen = maps:get(<<"frozen_inputs">>,State),
                Frozen = snapshot(Repo), ok;
            start_pg ->
                Frozen = maps:get(<<"frozen_inputs">>,State),
                Frozen = snapshot(Repo),
                ok = require_build(Repo,maps:get(<<"build_key">>,Frozen));
            _ -> error(invalid_backend_action)
        end
    catch Class:Reason -> {error,{backend_authorization_failed,Class,Reason}} end.

resource_root(Ctrl,State) ->
    true = is_state(State),
    CtrlBin=b(Ctrl), CtrlBin = maps:get(<<"control_dir">>,State),
    [D] = [R || #{<<"kind">>:=<<"OWNED_DIRECTORY">>}=R <- maps:get(<<"owned_resources">>,State)],
    Root = filename:join(filename:dirname(Ctrl),"resources"),
    RootBin=b(Root), RootBin = maps:get(<<"path">>,D),
    ok = safe_directory(Ctrl), ok = safe_directory(Root),
    Id = identity(Root), Id = maps:get(<<"filesystem_identity">>,D),
    CId = identity(Ctrl), CId = maps:get(<<"control_identity">>,D),
    PId = identity(filename:dirname(Ctrl)), PId = maps:get(<<"parent_identity">>,D),
    {ok,#file_info{uid=Uid}}=file:read_link_info(Ctrl),
    Uid = maps:get(<<"uid">>,D),
    {ok,#file_info{uid=Uid}}=file:read_link_info(Root),
    Marker = filename:join(Root,".imboy-quality-owned"),
    MarkerBin=b(Marker), MarkerBin = maps:get(<<"ownership_marker">>,D),
    Hash=file_hash(Marker), Hash=maps:get(<<"ownership_marker_sha256">>,D),
    Start = maps:get(<<"start_identity">>,maps:get(<<"owner">>,State)),
    Start = maps:get(<<"owner_start_identity">>,D),
    {ok,MarkerBytes}=file:read_file(Marker),
    {backend_acceptance,RunId,Owner}=binary_to_term(MarkerBytes,[safe]),
    RunBin=b(RunId), RunBin=maps:get(<<"run_id">>,State), Owner=maps:get(<<"owner">>,State),
    Root.

snapshot(Repo) ->
    {0,Head0}=imboy_quality_guard:git(Repo,["rev-parse","HEAD"]),
    {0,Status}=imboy_quality_guard:git(Repo,["status","--porcelain=v1","--untracked-files=all"]),
    {0,Tracked}=imboy_quality_guard:git(Repo,["ls-files","-z"]),
    {0,Untracked}=imboy_quality_guard:git(Repo,["ls-files","--others","--exclude-standard","-z"]),
    Files=lists:usort(binary:split(<<Tracked/binary,Untracked/binary>>,<<0>>,[global])),
    Wip=[{P,input_hash(filename:join(Repo,binary_to_list(P)))} || P <- Files,P=/=<<>>],
    #{key:=Build}=imboy_quality_build:build_key(Repo,imboy_quality_full_budget:build_profile()),
    #{<<"head">>=>string:trim(Head0),<<"status_sha256">>=>digest(Status),
        <<"worktree_sha256">>=>digest(term_to_binary(Wip)),
        <<"inputs">>=>imboy_quality_state:candidate_input_fingerprint(Repo),
        <<"build_key">>=>hex(Build),<<"roots">>=>[atom_to_binary(R) || R <- inventory_roots(Repo)]}.

context(State) ->
    F=maps:get(<<"frozen_inputs">>,State),
    #{candidate=>digest(term_to_binary(F)),build=>maps:get(<<"build_key">>,F),
        provision=>maps:get(<<"provision">>,State),otp=>b(erlang:system_info(otp_release)),
        erts=>b(erlang:system_info(version)),root_contract=>digest(term_to_binary(
            {maps:get(<<"roots">>,F),file_hash("tools/imboy_quality/imboy_quality_full.erl"),
                file_hash("scripts/quality.escript")}))}.

expected(State) ->
    Path=binary_to_list(maps:get(<<"expected_path">>,State)),
    Hash=file_hash(Path), Hash=maps:get(<<"expected_sha256">>,State),
    {ok,[E]}=file:consult(Path), E.

require_build(Repo,HexKey) ->
    Key=binary:decode_hex(HexKey),
    Path=os:getenv("IMBOY_QUALITY_BUILD_RECEIPT", ".erlang.mk/imboy.quality-test-build.receipt"),
    {hit,Key}=imboy_quality_build:receipt_verify(Path,Repo,Key),
    imboy_quality_full_budget:require_test_build(Repo), ok.

verify_expected(E,C,Roots0,PR) ->
    try
        Roots=lists:usort(Roots0), true=length(Roots0)=:=length(Roots),
        #{schema:=backend_expected_v1,roots:=Roots,zero_leaf_roots:=Zero,leaf_manifest:=L}=E,
        true=Zero=:=lists:usort(Zero), true=ordsets:is_subset(Zero,Roots),
        Roots=lists:sort(maps:keys(PR)),
        NonZero=ordsets:subtract(Roots,Zero), true=NonZero=/=[],
        lists:foreach(fun(R)-> ok=zero_root(maps:get(R,PR)) end,Zero),
        {ok,O}=imboy_quality_leaf_identity:observed_manifest(C,NonZero,maps:with(NonZero,PR)),
        ok=imboy_quality_leaf_identity:compare(L,O), ok
    catch Class:Reason -> {error,{backend_obligations_failed,Class,Reason}} end.

zero_root(#{exit:=ok,counts:=#{pass:=0,fail:=0,skip:=0,cancel:=0}=C,events:=Events}) ->
    %% Empty groups are permitted; even a declared empty root needs closed evidence.
    true=lists:all(fun({group_end,_,_})->true; ({result,{ok,_}})->true; (_)->false end,Events),
    true=lists:any(fun({group_end,[],_})->true; (_)->false end,Events),
    {result,{ok,Summary}}=lists:last(Events),
    true=lists:sort(Summary)=:=lists:sort(maps:to_list(C)), ok.

verify_result(State,OutDir,Result) ->
    try
        ok=verify_provision(State),
        Repo=binary_to_list(maps:get(<<"worktree">>,State)),
        Frozen=maps:get(<<"frozen_inputs">>,State), Frozen=snapshot(Repo),
        ok=require_build(Repo,maps:get(<<"build_key">>,Frozen)),
        ok = authorize(read_state(binary_to_list(maps:get(<<"control_dir">>,State))),cleanup),
        ok = verify_pg_exit(read_state(binary_to_list(maps:get(<<"control_dir">>,State)))),
        {ok,PRBytes}=file:read_file(filename:join(OutDir,"per_root.etf")),
        PR0=binary_to_term(PRBytes,[safe]),
        PR=maps:filter(fun(K,_)->is_atom(K) end,PR0),
        ok=verify_expected(expected(State),context(State),inventory_roots(Repo),PR),
        {ok,Result#{<<"input_binding">>=><<"VERIFIED">>,<<"roots_verified">>=>map_size(PR)}}
    catch Class:Reason ->
        {error,Result#{<<"input_binding">>=><<"NOT_VERIFIED">>,
            <<"binding_error">>=>b(io_lib:format("~tp:~tp",[Class,Reason]))}}
    end.

%% Public preparation descriptor: no execution, no reservation, no observed leaves.
%% The caller reviews the independent obligation tree against this context.
preview(Repo,Workers) ->
    Root=required("IMBOY_BACKEND_RUN_ROOT"),
    ResourceRoot=filename:join(Root,"resources"),
    Config=config_path(Repo),
    SystemId=b(required("IMBOY_BACKEND_PG_SYSTEM_IDENTIFIER")),
    Provision=provision_digest(Config,ResourceRoot,SystemId,Workers),
    State=#{<<"frozen_inputs">>=>snapshot(Repo),<<"provision">>=>Provision},
    #{context=>context(State),roots=>inventory_roots(Repo)}.

provision_digest(Config,Root,SystemId,Workers) ->
    digest(term_to_binary({file_hash(Config),
        file_hash(filename:join(Root,"postgres-toolchain/bin/postgres")),
        file_hash(filename:join(Root,"postgres-toolchain/bin/pg_ctl")),
        file_hash(filename:join(Root,"pgdata/PG_VERSION")),SystemId,Workers})).

verify_provision(State) ->
    try
        Config=binary_to_list(maps:get(<<"config">>,State)),
        [#{<<"path">>:=RootBin}] = [D || #{<<"kind">>:=<<"OWNED_DIRECTORY">>}=D <- maps:get(<<"owned_resources">>,State)],
        [#{<<"system_identifier">>:=SystemId}] = [P || #{<<"kind">>:=<<"ISOLATED_POSTGRESQL">>}=P <- maps:get(<<"owned_resources">>,State)],
        Expected=maps:get(<<"provision">>,State),
        Expected=provision_digest(Config,binary_to_list(RootBin),SystemId,maps:get(<<"workers">>,State)),
        ok
    catch C:R -> {error,{provision_drift,C,R}} end.

verify_pg_exit(State) ->
    try
        [#{<<"status">>:=<<"STOPPED">>,<<"postgres_exit">>:=0,
            <<"pg_ctl_exit">>:=0,<<"log_ok">>:=true}] =
            [P || #{<<"kind">>:=<<"ISOLATED_POSTGRESQL">>}=P <- maps:get(<<"owned_resources">>,State)],
        ok
    catch _:_ -> {error,postgres_exit_not_proven} end.

%% A signal can arrive while preparation is still creating resources. Publish
%% the original owner handles and confirmed transaction items before proceeding;
%% the signal server suspends the controller to prevent new work racing cleanup.
begin_resources(Ctrl) ->
    ?MODULE=ets:new(?MODULE,[named_table,public,set]),
    true=ets:insert(?MODULE,[{controller,self()},{ctrl,Ctrl},{stage,prepare}]),
    %% The foreground operation owns exactly one handler for its whole lifetime.
    []=[H || H<-gen_event:which_handlers(erl_signal_server),H=:=imboy_quality_signal],
    ok=imboy_quality_signal:install(fun signal_cleanup/0),
    [imboy_quality_signal]=[H || H<-gen_event:which_handlers(erl_signal_server),H=:=imboy_quality_signal],
    ok.
end_resources() ->
    case ets:whereis(?MODULE) of undefined->ok; _->ets:delete(?MODULE),ok end.
enter_run(Release) when is_function(Release,0) ->
    [{controller,Controller}]=ets:lookup(?MODULE,controller),
    true=Controller=:=self(),
    [imboy_quality_signal]=[H || H<-gen_event:which_handlers(erl_signal_server),H=:=imboy_quality_signal],
    true=ets:insert(?MODULE,{stage,{run,Release}}),
    ok.
track_launch(Owner,Ref,Nonce) -> track(launch,{Owner,Ref,Nonce}).
track_pg(H) -> track(pg,H).
track_prepare(Tx) -> track(prepare,Tx).
%% Keep the last confirmed transaction through the prepare -> run handoff.
clear_prepare() -> ok.
track(K,V) ->
    case ets:whereis(?MODULE) of undefined->ok; _->true=ets:insert(?MODULE,{K,V}),ok end.
lookup(K) -> case ets:lookup(?MODULE,K) of [{K,V}]->V; []->undefined end.

signal_cleanup() ->
    Main=lookup(controller),
    true=erlang:suspend_process(Main),
    H=lookup(pg),
    case H of undefined->ok; _->put(imboy_pg_owner,H) end,
    {Phase,Resources}=case lookup(stage) of
        {run,Release} -> {run,safe_cleanup(Release)};
        prepare -> {prepare,signal_prepare_cleanup()}
    end,
    PGResult=safe_cleanup(fun()->signal_postgres_cleanup(H) end),
    ConfigResult=safe_cleanup(fun imboy_quality_resources:cleanup_config_receipts/0),
    Ctrl=lookup(ctrl),
    %% Never translate partial/ambiguous signal cleanup into a successful full run.
    exclusive_write(filename:join(Ctrl,"evidence/backend-signal-cleanup.term"),
        iolist_to_binary(io_lib:format("~p.~n",[#{exit=>143,phase=>Phase,
            resources=>Resources,postgres=>PGResult,config_cleanup=>ConfigResult}]))).

signal_prepare_cleanup() ->
    case lookup(prepare) of
        undefined -> #{pending=>unknown,results=>[]};
        #{cleanup:=Cleanup,tracked:=Tracked,pending:=Pending}->
            #{pending=>maps:keys(Pending),results=>[
                {Type,Name,safe_cleanup(fun()->Cleanup(Type,R) end)}
                || {Type,Name,R} <- Tracked]}
    end.

signal_postgres_cleanup(undefined) ->
    case lookup(launch) of
        undefined -> not_started;
        {Owner,Ref,Nonce}->imboy_quality_pg_owner:cancel_start(Owner,Ref,Nonce)
    end;
signal_postgres_cleanup(H) ->
    case maps:find(port_handle,H) of
        {ok,P} when is_port(P) ->
            case erlang:port_info(P) of
                undefined -> safe_cleanup(fun()->verify_pg_exit(read_state(lookup(ctrl))) end);
                _ -> imboy_quality_pg_owner:stop(H,fun imboy_quality_pg_register:record_exit/3)
            end;
        _ -> {error,postgres_handle_unverified}
    end.

safe_cleanup(Fun) -> try Fun() catch C:R -> {error,{C,R}} end.

result_exit(#{<<"scope">>:=<<"BACKEND_ACCEPTANCE">>,<<"run_id">>:=Id,
    <<"cleanup">>:=#{<<"ok">>:=true},<<"input_binding">>:=<<"VERIFIED">>,
    <<"roots_missing">>:=0,<<"roots_failed">>:=0,<<"roots_total">>:=N,
    <<"roots_verified">>:=N,<<"pass">>:=P,<<"fail">>:=0,<<"skip">>:=0,<<"cancel">>:=0})
    when is_binary(Id),byte_size(Id)>0,is_integer(N),N>0,is_integer(P),P>0 -> 0;
result_exit(_) -> 2.

inventory_roots(Repo) ->
    #{ordinary:=O,excluded:=E}=imboy_quality_full:inventory(Repo), lists:usort(O++E).
config_path(Repo) ->
    Base=os:getenv("EUNIT_CONFIG","config/sys.local"),
    false = filename:extension(Base) =:= ".config",
    Path=case filename:extension(Base) of ".config"->Base; _->Base++".config" end,
    filename:absname(Path,Repo).
required(Name) -> case os:getenv(Name) of V when is_list(V),V=/=[] -> V; _->error({required_environment,Name}) end.
b(X) -> unicode:characters_to_binary(X).
hex(B) -> string:lowercase(binary:encode_hex(B)).
digest(B) -> hex(crypto:hash(sha256,B)).
file_hash(Path) ->
    {ok,#file_info{type=regular,links=1}}=file:read_link_info(Path),
    {ok,Bytes}=file:read_file(Path), digest(Bytes).
input_hash(Path) ->
    case file:read_link_info(Path) of
        {ok,#file_info{type=regular}} -> {ok,B}=file:read_file(Path),digest(B);
        {ok,#file_info{type=symlink}} -> {ok,Target}=file:read_link(Path),{symlink,Target};
        {ok,#file_info{type=directory}} -> directory;
        {error,enoent} -> deleted
    end.
identity(Path) ->
    {ok,#file_info{type=directory,inode=I,major_device=A,minor_device=B}}=file:read_link_info(Path),
    #{<<"major_device">>=>A,<<"minor_device">>=>B,<<"inode">>=>I}.
safe_directory("/") -> ok;
safe_directory(Path) ->
    absolute=filename:pathtype(Path),
    {ok,#file_info{type=directory}}=file:read_link_info(Path),
    safe_directory(filename:dirname(Path)).
exclusive_write(Path,Bytes) ->
    {ok,F}=file:open(Path,[write,binary,raw,exclusive]),
    try ok=file:change_mode(Path,8#600),ok=file:write(F,Bytes),ok=file:sync(F)
    after file:close(F) end.
