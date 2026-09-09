#!/usr/bin/env escript
%% 签发冒烟 JWT（教学 API 打点用）。
%% 用法: escript sign_jwt.escript <UID> [sys.config 路径，默认 config/sys.local.config]
%% jwt_key 运行时从 config 读取（脱敏：绝不硬编码）。
%% claims 形态照 src/ds/token_ds.erl do_encrypt_token/4：#{sub=><<"tk">>,exp,uid}。
main([UidStr]) -> main([UidStr, "config/sys.local.config"]);
main([UidStr, ConfigPath]) ->
    Uid = list_to_integer(UidStr),
    Key = find_jwt_key(ConfigPath),
    Exp = erlang:system_time(second) + 7200,
    Token = jwerl:sign(#{sub => <<"tk">>, exp => Exp, uid => Uid}, hs256, Key),
    io:format("~s~n", [Token]).
find_jwt_key(Path) ->
    {ok, [Terms]} = file:consult(Path),
    Apps = hd(Terms),
    {imboy, Env} = lists:keyfind(imboy, 1, Apps),
    {jwt_key, Key} = lists:keyfind(jwt_key, 1, Env),
    Key.
