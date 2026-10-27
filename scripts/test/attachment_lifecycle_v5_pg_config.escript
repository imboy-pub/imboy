#!/usr/bin/env escript
%% Retain test configuration while replacing every PostgreSQL pool target.
main([Input, Output, PasswordFile, Host, PortText, Database]) ->
    try
        {ok, [Config]} = file:consult(Input),
        Env = proplists:get_value(imboy, Config),
        true = is_list(Env),
        {ok, Password} = file:read_file(PasswordFile),
        true = byte_size(Password) > 0,
        Pg0 = proplists:get_value(pg_conf, Env),
        {epgsql, connect, [_]} = maps:get(start_mfa, Pg0),
        Connect = #{host => Host, port => list_to_integer(PortText),
            username => "imboy_user", password => string:trim(binary_to_list(Password)),
            database => Database},
        Pg = Pg0#{start_mfa => {epgsql, connect, [Connect]}},
        NewEnv = lists:keystore(pg_conf, 1, Env, {pg_conf, Pg}),
        NewConfig = lists:keystore(imboy, 1, Config, {imboy, NewEnv}),
        ok = file:write_file(Output, io_lib:format("~tp.~n", [NewConfig])),
        ok = file:change_mode(Output, 8#600)
    catch _:_ ->
        io:format(standard_error, "CONTRACT_CONFLICT: isolated PG config unavailable~n", []),
        halt(20)
    end;
main(_) -> halt(20).
