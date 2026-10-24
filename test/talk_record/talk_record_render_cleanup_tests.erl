-module(talk_record_render_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").
-export([adding_handler/1, removing_handler/1, log/2]).

cleanup_test_() ->
    [
        {atom_to_list(Mode), fun() -> twice(Mode) end}
     || Mode <- [committed, lease_lost, exception, clean]
    ].

twice(Mode) ->
    [once(Mode) || _ <- [1, 2]],
    ok.

once(Mode) ->
    Base = filename:join(
        os:getenv("HOME"), "owned-" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    ok = file:make_dir(Base),
    Handler = list_to_atom(
        "cleanup_probe_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    ok = logger:add_handler(Handler, ?MODULE, #{level => all, config => #{owner => self()}}),
    try
        Residual = filename:join(Base, "synthetic-leftover"),
        case Mode of
            clean ->
                ok;
            _ ->
                ok = file:make_dir(Residual),
                ok = file:write_file(filename:join(Residual, "fixture"), <<"synthetic">>)
        end,
        Fun = fun() ->
            case Mode of
                committed ->
                    put(synthetic_committed, true),
                    {ok, published};
                lease_lost ->
                    {error, lease_lost};
                exception ->
                    throw(synthetic_original_failure);
                clean ->
                    {ok, normal}
            end
        end,
        case Mode of
            exception ->
                ?assertThrow(
                    synthetic_original_failure, talk_record_audio_render:with_cleanup(Base, Fun)
                );
            committed ->
                ?assertEqual({ok, published}, talk_record_audio_render:with_cleanup(Base, Fun)),
                ?assertEqual(true, get(synthetic_committed));
            lease_lost ->
                ?assertEqual({error, lease_lost}, talk_record_audio_render:with_cleanup(Base, Fun));
            clean ->
                ?assertEqual({ok, normal}, talk_record_audio_render:with_cleanup(Base, Fun))
        end,
        case Mode of
            clean ->
                receive
                    {cleanup_log, _} -> error(unexpected_log)
                after 0 -> ok
                end,
                ?assertEqual(false, filelib:is_dir(Base));
            _ ->
                receive
                    {cleanup_log,
                        #{
                            event := talk_record_audio_render_cleanup_failed,
                            directory_sha256 := Hash,
                            cleanup_confirmed := false
                        } = Report} ->
                        ?assertEqual(3, map_size(Report)),
                        ?assertEqual(64, byte_size(Hash)),
                        ?assertEqual(
                            nomatch, binary:match(term_to_binary(Report), list_to_binary(Base))
                        )
                after 1000 -> error(missing_cleanup_signal)
                end,
                ?assert(filelib:is_dir(Residual))
        end
    after
        logger:remove_handler(Handler),
        erase(synthetic_committed),
        remove_owned(Base),
        ?assertEqual(false, filelib:is_dir(Base))
    end.

remove_owned(Path) ->
    case file:list_dir(Path) of
        {ok, Names} ->
            lists:foreach(
                fun(Name) ->
                    P = filename:join(Path, Name),
                    case filelib:is_dir(P) of
                        true -> remove_owned(P);
                        false -> ok = file:delete(P)
                    end
                end,
                Names
            ),
            ok = file:del_dir(Path);
        {error, enoent} ->
            ok
    end.
adding_handler(Config) -> {ok, Config}.
removing_handler(_) -> ok.
log(#{msg := {report, Report}}, #{config := #{owner := Owner}}) ->
    Owner ! {cleanup_log, Report},
    ok;
log(_, _) ->
    ok.

private_mode_cleanup_test_() ->
    [
        {atom_to_list(Mode), fun() -> private_mode_cleanup(Mode) end}
     || Mode <- [mode_failure_removed, mode_failure_leftover]
    ].

private_mode_cleanup(Mode) ->
    Base = filename:join(
        os:getenv("HOME"),
        "owned-mode-" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    ok = file:make_dir(Base),
    Handler = list_to_atom(
        "mode_probe_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))
    ),
    ok = logger:add_handler(Handler, ?MODULE, #{level => all, config => #{owner => self()}}),
    try
        case Mode of
            mode_failure_removed ->
                ok;
            mode_failure_leftover ->
                ok = file:write_file(filename:join(Base, "synthetic"), <<"owned fixture">>)
        end,
        %% Inject only the completed chmod result; actual deletion is real file I/O.
        %% Never mock the VM's core file module.
        ?assertEqual(
            {error, eperm}, talk_record_audio_render:finish_private_dir(Base, {error, eperm})
        ),
        case Mode of
            mode_failure_removed ->
                ?assertEqual(false, filelib:is_dir(Base)),
                receive
                    {cleanup_log, _} -> error(unexpected_log)
                after 0 -> ok
                end;
            mode_failure_leftover ->
                ?assert(filelib:is_dir(Base)),
                receive
                    {cleanup_log,
                        #{
                            event := talk_record_audio_render_cleanup_failed,
                            directory_sha256 := Hash,
                            cleanup_confirmed := false
                        } = Report} ->
                        ?assertEqual(3, map_size(Report)),
                        ?assertEqual(
                            binary:encode_hex(
                                crypto:hash(sha256, unicode:characters_to_binary(Base)), lowercase
                            ),
                            Hash
                        ),
                        ?assertEqual(
                            nomatch, binary:match(term_to_binary(Report), list_to_binary(Base))
                        )
                after 1000 -> error(missing_mode_cleanup_signal)
                end
        end
    after
        logger:remove_handler(Handler),
        remove_owned(Base),
        ?assertEqual(false, filelib:is_dir(Base))
    end.
