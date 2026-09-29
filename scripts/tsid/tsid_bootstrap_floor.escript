#!/usr/bin/env escript
%%! -pa ebin
%% DEPRECATED (step 6 retirement): this script is NO LONGER an implementation.
%% It is now a thin CLI shell around the single authoritative implementation
%% src/lib/elib_tsid_scan.erl (bootstrap_floor/1). Cutover decisions MUST be
%% based on the library contract, never on this file.
%%
%% Semantics (authoritative, see elib_tsid_scan):
%%   floor_safe_before = max(per TSID table max id -> id_to_slot) + 1;
%%   empty database -> 0; read-only REPEATABLE READ snapshot.
%%
%% What it does now:
%%   load compiled beams (run `make compile` first) ->
%%   call elib_tsid_scan:bootstrap_floor(#{}) ->
%%   success: write a small JSON report and print floor_safe_before
%%            (full 64-bit readable output: decimal + hex), exit 0;
%%   failure: print the reason, exit 2.
%%
%% Connection: this script never had CLI connection parameters, so none are
%% preserved. It stays on the DEFAULT connection path of elib_tsid_scan
%% (elib_pg read-only transaction via the app-env pool). Point the app env
%% at the scratch/clone database before running. (The scan contract Opts
%% also accept conn_fun for custom connections; a CLI shell without conn
%% args intentionally keeps the default path.)
%%
%% Historical note: before this retirement the script was an in-memory
%% cutover harness (fresh store + persist floor + guard + 5000 ids,
%% scenarios A/B, output bootstrap-floor.json). That evidence lives on in
%% the TSID-09/10 harness suite and docs/architecture/tsid-cutover-runbook.md.
%% The CLI form `tsid_bootstrap_floor.escript <out.json>` is preserved.
%%
%% Usage: escript tsid_bootstrap_floor.escript <out.json>
main([OutJson]) ->
    ensure_paths(),
    try elib_tsid_scan:bootstrap_floor(#{}) of
        {ok, Floor} when is_integer(Floor), Floor >= 0 ->
            ok = write_report(OutJson, Floor),
            %% 64-bit readable output: full decimal + zero-padded hex.
            io:format(
                "BOOTSTRAP_FLOOR_OK floor_safe_before=~p hex=~s~n",
                [Floor, io_lib:format("16#~16.16.0b", [Floor])]
            ),
            halt(0);
        {ok, Other} ->
            fail({invalid_floor_value, Other});
        {error, Reason} ->
            fail(Reason)
    catch
        C:R ->
            fail({bootstrap_floor_exception, C, R})
    end;
main(_) ->
    io:format("usage: tsid_bootstrap_floor.escript <out.json>~n"),
    halt(2).

fail(Reason) ->
    io:format(
        "BOOTSTRAP_FLOOR_FAILED reason=~tp~n"
        "note: authoritative semantics live in src/lib/elib_tsid_scan.erl;~n"
        "      this script is only a CLI shell (step-6 retirement).~n",
        [Reason]
    ),
    halt(2).

write_report(OutJson, Floor) ->
    Report = #{
        status => <<"OK">>,
        floor_safe_before => Floor,
        floor_hex => iolist_to_binary(io_lib:format("16#~16.16.0b", [Floor])),
        source => <<"elib_tsid_scan:bootstrap_floor/1">>
    },
    ok = filelib:ensure_dir(OutJson),
    file:write_file(OutJson, [jenc(Report), $\n]).

%% Beams live in repo-root ebin (make compile). Resolve relative to the
%% caller's working directory and to this script's own location so both
%% invocation styles keep working.
ensure_paths() ->
    add_path("ebin"),
    add_path("deps/epgsql/ebin"),
    ScriptDir = filename:dirname(escript:script_name()),
    add_path(filename:join(ScriptDir, "../../ebin")),
    add_path(filename:join(ScriptDir, "../../deps/epgsql/ebin")),
    ok.

add_path(Path) ->
    case filelib:is_dir(Path) of
        true -> code:add_pathz(Path);
        false -> ok
    end.

jenc(M) when is_map(M) ->
    [${, join([[jkey(K), $:, jenc(V)] || {K, V} <- lists:sort(maps:to_list(M))]), $}];
jenc(L) when is_list(L) -> [$[, join([jenc(V) || V <- L]), $]];
jenc(B) when is_boolean(B) -> atom_to_list(B);
jenc(I) when is_integer(I) -> integer_to_list(I);
jenc(A) when is_atom(A) -> [$", atom_to_binary(A, utf8), $"];
jenc(B) when is_binary(B) -> [$", B, $"].

jkey(K) when is_atom(K) -> [$", atom_to_binary(K, utf8), $"];
jkey(K) when is_binary(K) -> [$", K, $"].

join([]) -> [];
join([X]) -> [X];
join([X | Rest]) -> [X, $, | join(Rest)].
