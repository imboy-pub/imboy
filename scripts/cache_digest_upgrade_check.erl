%% Isolated VM regression: real code replacement must invalidate digest reuse,
%% and caller-local memo entries must not prevent purging old module code.
-module(cache_digest_upgrade_check).
-export([run/0]).

run() ->
    {ok, _} = application:ensure_all_started(crypto),
    Mod = imboy_cache_policy,
    {module, Mod} = code:ensure_loaded(Mod),
    {Mod, OriginalBeam, OriginalFile} = code:get_object_code(Mod),
    application:set_env(imboy, cache_replication, #{}),
    OriginalHash = Mod:config_hash(),
    Slot = {Mod, digest, config},
    {_, BeforeToken, _} = get(Slot),
    true = is_binary(BeforeToken),
    {ok, Source} = file:read_file("src/lib/imboy_cache_policy.erl"),
    Changed = binary:replace(Source, <<"{n_max, N},">>, <<"{n_max, N + 1},">>),
    true = Changed =/= Source,
    Dir = filename:join("/tmp", "cache_code_upgrade_" ++ os:getpid()),
    ok = file:make_dir(Dir),
    Temp = filename:join(Dir, "imboy_cache_policy.erl"),
    try
        ok = file:write_file(Temp, Changed),
        {ok, Mod, ChangedBeam} = compile:file(Temp,
            [binary, {i, "include"}, {i, "src/lib"}, report_errors]),
        {module, Mod} = code:load_binary(Mod, Temp, ChangedBeam),
        UpdatedHash = Mod:config_hash(),
        true = UpdatedHash =/= OriginalHash,
        {_, AfterToken, _} = get(Slot),
        true = AfterToken =/= BeforeToken,
        true = code:soft_purge(Mod),
        {module, Mod} = code:load_binary(Mod, OriginalFile, OriginalBeam),
        OriginalHash = Mod:config_hash(),
        true = code:soft_purge(Mod),
        io:format("CACHE_DIGEST_UPGRADE passed: new hash, new code token, soft purge, rollback~n")
    after
        code:soft_purge(Mod),
        code:load_binary(Mod, OriginalFile, OriginalBeam),
        file:delete(Temp),
        file:del_dir(Dir)
    end,
    halt(0).
