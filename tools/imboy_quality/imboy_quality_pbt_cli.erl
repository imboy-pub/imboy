%% PBT receipts are diagnostic until the complete input identity is verified.
-module(imboy_quality_pbt_cli).
-export([prepare/3, run/2, replay/1]).

prepare(Ctrl, Erlc, NeedsCompile) ->
    Proper = filename:absname("deps/proper/ebin"),
    true = filelib:is_dir(Proper),
    Beam = filename:join(Proper, "proper.beam"),
    true = filelib:is_regular(Beam),
    true = code:add_patha(Proper),
    {module, proper} = code:ensure_loaded(proper),
    Beam = code:which(proper),
    BuildDir = filename:join(Ctrl, "prep/build"),
    Files = filelib:wildcard("test/pbt/*.erl"),
    true = Files =/= [],
    lists:foreach(
        fun(Src) ->
            Out = filename:join(BuildDir, filename:rootname(filename:basename(Src)) ++ ".beam"),
            case NeedsCompile(Src, Out) of
                true -> 0 = Erlc(Src, BuildDir);
                false -> ok
            end
        end,
        Files
    ).

run(Profile, Ctrl) when Profile =:= quick; Profile =:= nightly ->
    Repo = prepare_paths(),
    {ok, Manifest} = imboy_quality_pbt:load_manifest(manifest(Repo)),
    Before = head(Repo),
    {ok, Summary} = imboy_quality_pbt:run(Manifest, Profile, #{}),
    After = head(Repo),
    %% D6-Step1（2026-11-10）：binding/3 额外带回 admission 的 dynamic_U，
    %% 使 pbt 收据原生携带 u_fingerprint（此前只有 candidate_sha/
    %% input_fingerprint，2/3；u_fingerprint 连 token 都不出现）。
    {Binding, Fingerprint, UFp} = binding(Ctrl, Repo, After),
    RunId = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(16))),
    Result = #{
        <<"op">> => <<"pbt">>,
        <<"profile">> => atom_to_binary(Profile),
        <<"run_id">> => RunId,
        <<"candidate_before">> => Before,
        <<"candidate_sha">> => After,
        <<"input_fingerprint">> => Fingerprint,
        <<"u_fingerprint">> => UFp,
        <<"input_binding">> => Binding,
        <<"verdict">> => verdict(Before, After, Summary, Binding),
        <<"total">> => maps:get(total, Summary),
        <<"passed">> => maps:get(passed, Summary),
        <<"results">> => [format_result(R) || R <- maps:get(results, Summary)]
    },
    Path = filename:join([
        Ctrl,
        "evidence",
        "pbt-" ++ atom_to_list(Profile) ++ "-" ++
            binary_to_list(RunId) ++ ".json"
    ]),
    ok = write_receipt(Path, Result),
    {Result, Path}.

replay(Ctrl) ->
    Repo = prepare_paths(),
    {ok, #{properties := Props}} = imboy_quality_pbt:load_manifest(manifest(Repo)),
    MFAs = lists:usort(lists:append([[maps:get(mfa, P) | maps:get(witnesses, P)] || P <- Props])),
    lists:foreach(
        fun({M, F, 0}) ->
            {module, M} = code:ensure_loaded(M),
            true = erlang:function_exported(M, F, 0)
        end,
        MFAs
    ),
    {ok, #{path := Path, replay := Replay, entry := Entry}} =
        imboy_quality_pbt:replay_next(Ctrl, [maps:get(mfa, P) || P <- Props]),
    {
        #{
            <<"op">> => <<"replay">>,
            <<"verdict">> => <<"input_unverified">>,
            <<"path">> => unicode:characters_to_binary(Path),
            <<"id">> => atom_to_binary(maps:get(id, Entry)),
            <<"replay">> => unicode:characters_to_binary(io_lib:format("~0p", [Replay]))
        },
        undefined
    }.

prepare_paths() ->
    {ok, Repo} = file:get_cwd(),
    Ebin = filename:join(Repo, "ebin"),
    true = filelib:is_dir(Ebin),
    true = code:add_patha(Ebin),
    lists:foreach(
        fun(D) -> true = code:add_patha(D) end,
        lists:sort(filelib:wildcard(filename:join([Repo, "deps", "*", "ebin"])))
    ),
    {module, proper} = code:ensure_loaded(proper),
    Repo.

manifest(Repo) -> filename:join([Repo, "test", "pbt", "pbt-manifest.term"]).
head(Repo) ->
    {0, Output} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
    iolist_to_binary(string:trim(Output)).

verdict(Before, After, _, _) when Before =/= After -> <<"candidate_drift">>;
verdict(_, _, #{total := Total, passed := Total}, <<"VERIFIED">>) when Total > 0 ->
    <<"all_passed">>;
verdict(_, _, #{total := Total, passed := Total}, _) when Total > 0 -> <<"input_unverified">>;
verdict(_, _, _, _) ->
    <<"properties_failed">>.

%% 诊断级与 gate 级的分界：候选身份实测绑定。STATE 准入指纹与工作树实时
%% 指纹一致、且 admission candidate_sha 与 HEAD 一致才发 VERIFIED；否则
%% 保持 NOT_VERIFIED 并把 verdict 降级为 input_unverified（不伪造 all_passed）。
%% D6-Step1（2026-11-10）：返回 {Binding, Fingerprint, UFingerprint} 三元组，
%% 额外带回 admission 的 dynamic_U 供收据原生携带 u_fingerprint。
binding(Ctrl, Repo, Head) ->
    case file:read_file(filename:join(Ctrl, "STATE.json")) of
        {ok, Bytes} ->
            State = json:decode(Bytes),
            Adm = maps:get(<<"current_admission">>, State, #{}),
            Stored = maps:get(<<"input_fingerprint">>, Adm, <<>>),
            StoredSha = maps:get(<<"candidate_sha">>, Adm, <<>>),
            UFp = maps:get(<<"dynamic_U">>, Adm, <<>>),
            Live = imboy_quality_state:candidate_input_fingerprint(Repo),
            HeadBin = unicode:characters_to_binary(Head),
            case Stored =:= Live andalso StoredSha =:= HeadBin of
                true -> {<<"VERIFIED">>, Live, UFp};
                false -> {<<"NOT_VERIFIED">>, Live, UFp}
            end;
        _ ->
            {<<"NOT_VERIFIED">>, <<>>, <<>>}
    end.

format_result(R) ->
    #{
        <<"id">> => atom_to_binary(maps:get(id, R)),
        <<"verdict">> => atom_to_binary(maps:get(verdict, R)),
        <<"completion_events">> => maps:get(completion_events, R, null),
        <<"performed">> => maps:get(performed, R, null),
        <<"requested">> => maps:get(requested, R, null),
        <<"random_started">> => maps:get(random_started, R, null),
        <<"witnesses_passed_count">> => maps:get(witnesses_passed_count, R, null)
    }.

write_receipt(Path, Result) ->
    Temp = Path ++ ".tmp",
    {ok, Handle} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        try
            ok = file:write(Handle, json:encode(Result)),
            ok = file:sync(Handle)
        after
            ok = file:close(Handle)
        end,
        ok = file:make_link(Temp, Path),
        ok = file:delete(Temp)
    catch
        Class:Reason:Stack ->
            _ = file:delete(Temp),
            erlang:raise(Class, Reason, Stack)
    end.
