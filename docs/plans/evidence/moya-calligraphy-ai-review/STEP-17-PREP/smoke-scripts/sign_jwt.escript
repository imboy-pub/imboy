#!/usr/bin/env escript
%%! -pa /Users/leeyi/project/imboy.pub/imboy/deps/jwerl/ebin /Users/leeyi/project/imboy.pub/imboy/deps/jsx/ebin
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
    %% 回植修复（2026-09-10 续跑轮 Agent E，Coordinator 批准；详见同轮 agents/E/integration-run.md 附录 A）：
    %% 原版 {ok,[Terms]} + hd(Terms) 在标准单层 [{App,Env}] 结构的 sys.local.config 上
    %% 取到元组 → lists:keyfind(imboy,1,元组) badarg，脚本从未真正可运行（R9 只静态固化未执行）。
    %% lists:flatten 同时兼容标准单层与嵌套 [[{App,Env}]] 两种 sys.config 形态。
    {ok, Terms} = file:consult(Path),
    Apps = lists:flatten(Terms),
    {imboy, Env} = lists:keyfind(imboy, 1, Apps),
    {jwt_key, Key} = lists:keyfind(jwt_key, 1, Env),
    Key.
