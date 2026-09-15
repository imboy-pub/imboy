#!/usr/bin/env escript
%%! -noshell
%% dev_verify_oss_config.escript — 校验配置里的 garage 块。
%%
%% 用法： escript dev_verify_oss_config.escript <config> [期望的 key_prefix]
%% 退出码：0 = 合格；非 0 = 有问题（供 dev_use_public_bucket.sh 决定是否回滚）
%%
%% 为什么需要：切换脚本是**文本改写**，纯语法校验（file:consult）抓不到
%% 「键名写错 / 前缀没写进去 / 密钥忘了走 env」这类语义问题，而它们会让
%% 节点起不来或静默地继续用内网端点。

main([Path]) -> main([Path, ""]);
main([Path, ExpectPrefix]) ->
    case file:consult(Path) of
        {error, Reason} ->
            io:format("语法错误: ~p~n", [Reason]),
            halt(2);
        {ok, [Apps]} ->
            Imboy = proplists:get_value(imboy, Apps, []),
            case proplists:get_value(garage, Imboy, undefined) of
                undefined ->
                    io:format("缺少 garage 配置块~n"),
                    halt(3);
                G ->
                    Problems =
                        check_present(G, [endpoint, public_endpoint, bucket])
                        ++ check_https(G)
                        ++ check_prefix(G, ExpectPrefix)
                        ++ check_no_plaintext_secret(G),
                    lists:foreach(fun(P) -> io:format("  - ~ts~n", [P]) end, Problems),
                    case Problems of
                        [] ->
                            io:format("OSS_CONFIG_OK~n"),
                            halt(0);
                        _ ->
                            io:format("OSS_CONFIG_BAD~n"),
                            halt(1)
                    end
            end
    end.

check_present(G, Keys) ->
    [io_lib:format("缺少键 ~p", [K]) || K <- Keys, not maps:is_key(K, G)].

check_https(G) ->
    Ep = maps:get(public_endpoint, G, <<>>),
    case Ep of
        <<"https://", _/binary>> -> [];
        _ -> [io_lib:format("public_endpoint 不是 https（~p）→ 模型侧抓不到", [Ep])]
    end.

check_prefix(_G, <<>>) ->
    [];
check_prefix(_G, "") ->
    [];
check_prefix(G, Expect) ->
    case maps:get(key_prefix, G, undefined) of
        undefined ->
            ["未写入 key_prefix"];
        V when is_binary(V) ->
            case unicode:characters_to_list(V) of
                Expect -> [];
                Other ->
                    [io_lib:format("key_prefix=~s，期望 ~s", [Other, Expect])]
            end;
        V ->
            [io_lib:format("key_prefix 不是 binary（~p）", [V])]
    end.

%% 密钥必须走 {env, VAR}：明文落进配置文件是本次要纠正的问题本身
check_no_plaintext_secret(G) ->
    [io_lib:format("~p 是明文，应改成 {env, <<\"VAR\">>}", [K])
     || K <- [access_key, secret_key],
        is_binary(maps:get(K, G, undefined)),
        byte_size(maps:get(K, G)) > 0].
