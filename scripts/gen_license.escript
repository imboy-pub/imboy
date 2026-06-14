#!/usr/bin/env escript
%%! -noshell
%%%===================================================================
%%% gen_license.escript — Vendor 侧 License 签发工具
%%%
%%% 用 Vendor RSA 私钥对 license payload 签名，生成 license.key 文件。
%%% 部署端 imboy_license 用对应 Vendor 公钥(priv/vendor_pubkey*.pem)验签。
%%%
%%% ⚠️ Vendor 私钥务必离线保管，绝不进 git / release。
%%%
%%% 用法:
%%%   escript gen_license.escript <priv_key.pem> <out.key> <edition> \
%%%           <max_users> <max_nodes> <days> <licensee> [domain...]
%%%
%%%   max_users / max_nodes = 0 表示不限量。
%%%
%%% 示例:
%%%   escript scripts/gen_license.escript \
%%%     ~/imboy-private/license_vendor_priv_dev.pem /tmp/lic.key \
%%%     professional 500 0 365 "某企业有限公司" api.example.com
%%% @end
%%%===================================================================
-include_lib("public_key/include/public_key.hrl").

main([PrivKeyPath, OutPath, Edition, MaxUsersS, MaxNodesS, DaysS, Licensee | Domains]) ->
    _ = application:ensure_all_started(crypto),
    _ = application:ensure_all_started(public_key),
    Now = erlang:system_time(millisecond),
    Expires = Now + list_to_integer(DaysS) * 86400000,
    Payload = iolist_to_binary([
        "{",
        "\"edition\":\"", Edition, "\",",
        "\"max_users\":", MaxUsersS, ",",
        "\"max_nodes\":", MaxNodesS, ",",
        "\"domains\":", domains_json(Domains), ",",
        "\"licensee\":\"", unicode:characters_to_binary(Licensee), "\",",
        "\"issued_at\":", integer_to_binary(Now), ",",
        "\"expires_at\":", integer_to_binary(Expires),
        "}"
    ]),
    PrivKey = read_priv_key(PrivKeyPath),
    Sig = public_key:sign(Payload, sha256, PrivKey),
    License = <<(base64:encode(Payload))/binary, ".", (base64:encode(Sig))/binary>>,
    ok = file:write_file(OutPath, License),
    %% 自检：用私钥导出的公钥验签，证明签名/验签 roundtrip 正确
    PubKey = pubkey_from_priv(PrivKey),
    SelfCheck = public_key:verify(Payload, sha256, Sig, PubKey),
    io:format("✓ license 已生成: ~s~n", [OutPath]),
    io:format("  edition=~s max_users=~s max_nodes=~s 有效~s天~n",
              [Edition, MaxUsersS, MaxNodesS, DaysS]),
    io:format("  payload=~s~n", [Payload]),
    io:format("  自检验签(应为 true): ~p~n", [SelfCheck]),
    case SelfCheck of
        true -> halt(0);
        false -> halt(2)
    end;
main(_) ->
    io:format(
        "用法: escript gen_license.escript <priv_key.pem> <out.key> "
        "<edition> <max_users> <max_nodes> <days> <licensee> [domain...]~n"
    ),
    halt(1).

domains_json([]) ->
    "[]";
domains_json(Domains) ->
    ["[", lists:join(",", [["\"", D, "\""] || D <- Domains]), "]"].

read_priv_key(Path) ->
    {ok, Pem} = file:read_file(Path),
    [Entry | _] = public_key:pem_decode(Pem),
    public_key:pem_entry_decode(Entry).

pubkey_from_priv(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E}.
