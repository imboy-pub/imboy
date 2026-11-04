-module(imboy_quality_task_owner_tests).
-include_lib("eunit/include/eunit.hrl").

owner_authentication_and_replay_test() ->
    Token = token(),
    Repo = <<"/synthetic/task/worktree">>,
    {ok, Keeper, Owner} = imboy_quality_task_owner:start(Token, Repo),
    State = #{<<"owner">> => Owner, <<"worktree">> => Repo},
    try
        ?assert(imboy_quality_task_owner:verify(State, Token, Repo)),
        lists:foreach(
            fun(Bad) ->
                ?assertNot(
                    imboy_quality_task_owner:verify(
                        State#{<<"owner">> := Owner#{<<"token">> := Bad}}, Bad, Repo
                    )
                )
            end,
            [
                <<0, (binary:copy(<<"a">>, 63))/binary>>,
                binary:copy(<<"A">>, 64),
                binary:copy(<<"g">>, 64)
            ]
        ),
        ?assertNot(imboy_quality_task_owner:verify(State, token(), Repo)),
        ?assertNot(imboy_quality_task_owner:verify(State, Token, <<"/other">>)),
        ?assertNot(
            imboy_quality_task_owner:verify(
                State#{<<"owner">> := Owner#{<<"pid">> := 1}}, Token, Repo
            )
        ),
        ?assertNot(
            imboy_quality_task_owner:verify(
                State#{
                    <<"owner">> := Owner#{
                        <<"start_identity">> := <<"other-identity-0000000000000000000000000000">>
                    }
                },
                Token,
                Repo
            )
        ),
        ?assertNot(
            imboy_quality_task_owner:verify(
                State#{<<"owner">> := maps:remove(<<"owner_port">>, Owner)}, Token, Repo
            )
        ),
        Nonce = crypto:strong_rand_bytes(32),
        ?assert(imboy_quality_task_owner:challenge(Owner, Repo, Nonce)),
        ?assertNot(imboy_quality_task_owner:challenge(Owner, Repo, Nonce))
    after
        ok = imboy_quality_task_owner:stop(Keeper)
    end,
    ?assertNot(imboy_quality_task_owner:verify(State, Token, Repo)).

invalid_keeper_input_test() ->
    ?assertMatch({error, _}, imboy_quality_task_owner:start(<<"short">>, <<"/repo">>)),
    ?assertMatch({error, _}, imboy_quality_task_owner:start(token(), <<>>)),
    lists:foreach(
        fun(Bad) -> ?assertMatch({error, _}, imboy_quality_task_owner:start(Bad, <<"/repo">>)) end,
        [
            <<0, (binary:copy(<<"a">>, 63))/binary>>,
            binary:copy(<<"A">>, 64),
            binary:copy(<<"g">>, 64)
        ]
    ).

authenticated_shutdown_test() ->
    Token = token(),
    Repo = <<"/synthetic/task/worktree">>,
    {ok, Keeper, Owner} = imboy_quality_task_owner:start(Token, Repo),
    Ref = monitor(process, Keeper),
    ?assertNot(
        imboy_quality_task_owner:shutdown(Owner#{<<"token">> := token()}, Repo)
    ),
    ?assert(
        imboy_quality_task_owner:verify(
            #{<<"owner">> => Owner, <<"worktree">> => Repo}, Token, Repo
        )
    ),
    ?assert(imboy_quality_task_owner:shutdown(Owner, Repo)),
    receive
        {'DOWN', Ref, process, Keeper, _} -> ok
    after 3000 -> error(shutdown_timeout)
    end.

keeper_exits_with_parent_test() ->
    Test = self(),
    Parent = spawn(fun() ->
        {ok, Keeper, _} = imboy_quality_task_owner:start(token(), <<"/repo">>),
        Test ! {keeper, Keeper},
        receive
            hold -> ok
        end
    end),
    Keeper =
        receive
            {keeper, K} -> K
        after 3000 -> error(no_keeper)
        end,
    Ref = monitor(process, Keeper),
    exit(Parent, kill),
    receive
        {'DOWN', Ref, process, Keeper, _} -> ok
    after 3000 -> error(orphan_keeper)
    end.

json_environment_authentication_in_separate_vm_test() ->
    Token = token(),
    {ok, Cwd} = file:get_cwd(),
    Repo = unicode:characters_to_binary(Cwd),
    {ok, Keeper, Owner} = imboy_quality_task_owner:start(Token, Repo),
    State = #{<<"owner">> => Owner, <<"worktree">> => Repo},
    Path = filename:join(
        "/private/tmp",
        "quality-owner-json-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    try
        {ok, File} = file:open(Path, [write, binary, exclusive]),
        try
            ok = file:change_mode(Path, 8#600),
            ok = file:write(File, json:encode(State)),
            ok = file:sync(File)
        after
            ok = file:close(File)
        end,
        BeamDir = filename:dirname(code:which(imboy_quality_task_owner)),
        Eval = lists:flatten(
            io_lib:format(
                "{ok,B}=file:read_file(~tp),S=json:decode(B),true=imboy_quality_full_budget:owner_verified(S),halt(0).",
                [Path]
            )
        ),
        Port = open_port(
            {spawn_executable, os:find_executable("erl")},
            [
                binary,
                exit_status,
                stderr_to_stdout,
                {env, [{"IMBOY_QUALITY_OWNER_TOKEN", binary_to_list(Token)}]},
                {args, [
                    "+S",
                    "1:1",
                    "+SDcpu",
                    "1",
                    "+SDio",
                    "1",
                    "+A",
                    "1",
                    "-noshell",
                    "-pa",
                    BeamDir,
                    "-eval",
                    Eval
                ]}
            ]
        ),
        ?assertEqual(0, child_exit(Port))
    after
        file:delete(Path),
        ok = imboy_quality_task_owner:stop(Keeper)
    end.

child_exit(Port) ->
    receive
        {Port, {data, _}} -> child_exit(Port);
        {Port, {exit_status, Code}} -> Code
    after 4000 ->
        port_close(Port),
        timeout
    end.

token() -> string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(32))).
