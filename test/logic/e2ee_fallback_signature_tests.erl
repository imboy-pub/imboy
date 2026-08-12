%%% E2EE-062 残留 6：**fallback prekey 的服务端验签**。
%%%
%%% == 缺口 ==
%%%
%%% playbook E2EE-025 验收标准：
%%%   「OTK 耗尽只使用协议允许且**身份验证通过的 signed fallback prekey**，或拒发。」
%%%
%%% 现状 `olm_identity_logic:report_fallback_key/4` 只校验 key_id / key_base64 非空，
%%% **没有任何签名**。
%%%
%%% == 威胁（这不是理论问题）==
%%%
%%% E2EE-013 用 token 绑定设备所有权：只有持 device D 的 token 才能写 D 的密钥。
%%% 但 **token 会在网络上传输，identity 私钥不会**——盗取 token 远比盗取设备
%%% ed25519 私钥容易。持有被盗 token 的攻击者今天可以给 D 上传**自己控制的**
%%% fallback prekey；此后凡是 D 的 OTK 耗尽、对端回退 fallback 的会话，
%%% 用的都是攻击者的预密钥。
%%%
%%% 让 fallback key 由**设备已注册的 ed25519 身份键**签名，就把它绑到了
%%% 一个 token 窃取者拿不到的秘密上。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. 【对照组】未带签名（旧客户端）→ 仍照常落库。改前改后都必须绿；
%%% 2. 【正向可用性】带**有效**签名 → 照常落库。
%%%    一个「一律拒绝」的实现在"能拦下伪造"上恒得满分，必须被这条否掉；
%%% 3. 带**无效**签名 → 拒绝，且**不得落库**（fail-closed）；
%%% 4. 签名必须覆盖 key_base64：换一把 key 复用同一签名必须失效；
%%% 5. 设备尚未注册 identity → 无从验证 → 拒绝（不得"验不了就放行"）；
%%% 6. 未带签名的上传必须被**计数**，让"这道防护还没铺开"这件事可见。
%%%
%%% ⚠️ 本文件用 `crypto:generate_key(eddsa, ed25519)` 生成**真实密钥对**并真实签名，
%%% 不 mock 任何密码学函数。
-module(e2ee_fallback_signature_tests).

-include_lib("eunit/include/eunit.hrl").

-define(UID, 6001).
-define(DID, <<"dev-fb-A">>).
-define(KID, <<"fbkey-1">>).
-define(KB64, <<"ZmFsbGJhY2sta2V5LWJ5dGVz">>).

%% 记录 upsert 与指标调用
-define(SK, {?MODULE, sink}).

sink(Item) ->
    Prev =
        case persistent_term:get(?SK, undefined) of
            undefined -> [];
            L -> L
        end,
    persistent_term:put(?SK, Prev ++ [Item]).

sunk() ->
    case persistent_term:get(?SK, undefined) of
        undefined -> [];
        L -> L
    end.

upserts() -> [X || {upsert, X} <- sunk()].
metrics() -> [X || {metric, X} <- sunk()].

%% canonical 签名载荷：`key=value\n`，ASCII 字典序，末字段无尾随换行。
%% 与 e2ee_trust_logic:canonical_payload/1 同一方案（项目既有、双语言对齐）。
canonical(UserId, DeviceId, KeyId, KeyB64) ->
    <<"device_id=", DeviceId/binary, "\n", "key_base64=", KeyB64/binary, "\n", "key_id=",
        KeyId/binary, "\n", "user_id=", (integer_to_binary(UserId))/binary>>.

setup() ->
    persistent_term:erase(?SK),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    persistent_term:put({?MODULE, keys}, {Pub, Priv}),
    meck:new(olm_identity_ds, [passthrough, no_link]),
    meck:new(elib_metric, [passthrough, no_link]),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) ->
        {ok, #{<<"ed25519_key">> => base64:encode(Pub)}}
    end),
    meck:expect(olm_identity_ds, upsert_fallback_key, fun(U, D, K, B) ->
        sink({upsert, {U, D, K, B}}),
        {ok, 1}
    end),
    meck:expect(elib_metric, increment, fun(Name) ->
        sink({metric, Name}),
        ok
    end),
    ok.

cleanup(_) ->
    _ = (catch meck:unload(elib_metric)),
    _ = (catch meck:unload(olm_identity_ds)),
    persistent_term:erase(?SK),
    persistent_term:erase({?MODULE, keys}),
    ok.

sign(Bin) ->
    {_Pub, Priv} = persistent_term:get({?MODULE, keys}),
    base64:encode(crypto:sign(eddsa, none, Bin, [Priv, ed25519])).

valid_sig() ->
    sign(canonical(?UID, ?DID, ?KID, ?KB64)).

fb_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"对照组：未带签名的旧客户端仍照常落库", fun unsigned_still_accepted/0},
            {"正向可用性：有效签名照常落库", fun valid_signature_accepted/0},
            {"兼容 vodozemac 无填充 base64 的身份键和签名", fun unpadded_base64_accepted/0},
            {"无效签名 → 拒绝且不落库", fun invalid_signature_rejected/0},
            {"签名覆盖 key_base64：换 key 复用签名必须失效", fun signature_binds_key/0},
            {"设备未注册 identity → 无从验证即拒绝", fun unregistered_device_rejected/0},
            {"未签名上传必须被计数（缺口可见）", fun unsigned_is_counted/0},
            {"有效签名不得被误计成未签名", fun signed_not_counted_as_unsigned/0},
            {"canonical golden vector（跨语言钉死）", fun canonical_golden_vector/0}
        ]
    end}.

reset() -> persistent_term:erase(?SK).

%% ===================================================================

unsigned_still_accepted() ->
    reset(),
    ?assertEqual(ok, olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64)),
    ?assertEqual(
        [{?UID, ?DID, ?KID, ?KB64}],
        upserts(),
        "旧客户端不发签名；此刻拒绝它们等于所有设备都发布不了 fallback key"
    ).

valid_signature_accepted() ->
    reset(),
    ?assertEqual(
        ok,
        olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, valid_sig()),
        "「一律拒绝」的实现在拦伪造上恒满分，必须被这条否掉"
    ),
    ?assertEqual([{?UID, ?DID, ?KID, ?KB64}], upserts()).

unpadded_base64_accepted() ->
    reset(),
    {Pub, _} = persistent_term:get({?MODULE, keys}),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) ->
        {ok, #{<<"ed25519_key">> => unpad(base64:encode(Pub))}}
    end),
    ?assertEqual(
        ok,
        olm_identity_logic:report_fallback_key(
            ?UID, ?DID, ?KID, ?KB64, unpad(valid_sig())
        )
    ),
    ?assertEqual([{?UID, ?DID, ?KID, ?KB64}], upserts()).

invalid_signature_rejected() ->
    reset(),
    Bad = sign(<<"something-else-entirely">>),
    ?assertEqual(
        {error, <<"invalid_signature">>},
        olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, Bad)
    ),
    ?assertEqual([], upserts(), "验签失败必须 fail-closed：一行都不许落库").

%% 防「拿到某次合法签名后，换一把自己控制的 key 复用它」
signature_binds_key() ->
    reset(),
    Sig = valid_sig(),
    Other = <<"YXR0YWNrZXItY29udHJvbGxlZC1rZXk=">>,
    ?assertEqual(
        {error, <<"invalid_signature">>},
        olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, Other, Sig)
    ),
    ?assertEqual([], upserts()).

unregistered_device_rejected() ->
    reset(),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) -> {ok, not_found} end),
    R = olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, valid_sig()),
    %% 恢复，避免影响后续用例
    {Pub, _} = persistent_term:get({?MODULE, keys}),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) ->
        {ok, #{<<"ed25519_key">> => base64:encode(Pub)}}
    end),
    ?assertEqual(
        {error, <<"device_not_registered">>},
        R,
        "验不了就放行 = 攻击者只需先删/绕过 identity 即可绕开整道验签"
    ),
    ?assertEqual([], upserts()).

%% 走**生产实际路径**：handler 统一调 /5，未带签名时传 <<>>（见 olm_handler
%% do_report_fallback1）。直接调 /4 不是生产入口——/4 也被 /5 验签成功后内部
%% 复用，若把计数打在 /4 上，签名合法的上传会被误计成"未签名"。
unsigned_is_counted() ->
    reset(),
    ok = olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, <<>>),
    ?assert(
        lists:member(olm_fallback_unsigned_total, metrics()),
        "未签名上传仍被接受（旧客户端兼容），但必须计数——"
        "否则「这道防护还没铺开」这件事在运维侧完全不可见"
    ).

%% /5 验签成功后内部复用 /4；若计数打在 /4 上，合法签名的上传会被误计成"未签名"，
%% 指标随即失去意义。
signed_not_counted_as_unsigned() ->
    reset(),
    ok = olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, valid_sig()),
    ?assertNot(lists:member(olm_fallback_unsigned_total, metrics())).

unpad(<<>>) ->
    <<>>;
unpad(Bin) ->
    case binary:last(Bin) of
        $= -> unpad(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

%% ⚠️ 跨语言 golden vector。客户端（imboyapp `fallbackKeyCanonical`）必须产出
%% **逐字节相同**的载荷，否则服务端验签必然失败 → 该设备发布不了 fallback key
%% → 每次 OTK 耗尽都变成 `no_prekey_available`，是一次**生产可用性事故**。
%% 两侧各自把同一条字面量钉死，是本项目在没有联调环境时能做的最强一致性检查。
%% 对侧断言：imboyapp `test/service/e2ee/fallback_key_signature_test.dart`。
canonical_golden_vector() ->
    reset(),
    Expected =
        <<
            "device_id=dev-fb-A\n"
            "key_base64=ZmFsbGJhY2sta2V5LWJ5dGVz\n"
            "key_id=fbkey-1\n"
            "user_id=6001"
        >>,
    %% 用「按 Expected 签名 → 服务端必须接受」间接钉死服务端 canonical：
    %% 若服务端构造的字节与 Expected 不同，验签必失败。
    Sig = sign(Expected),
    ?assertEqual(
        ok,
        olm_identity_logic:report_fallback_key(?UID, ?DID, ?KID, ?KB64, Sig),
        "服务端 canonical 必须与 golden vector 逐字节一致"
    ),
    ?assertEqual(
        82,
        byte_size(Expected),
        "长度也是向量的一部分：长度对不上说明编码规则理解错了，"
        "此时再比签名只会得到无信息量的『验签失败』"
    ).
