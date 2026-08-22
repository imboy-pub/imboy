-module(alipay_openapi).
%%%===================================================================
%%% @doc 支付宝开放平台 OpenAPI 客户端（APP 支付宝登录服务端对接）
%%%
%%% 能力边界：只封装「公共参数构造 + RSA2 签名 + POST gateway.do + 响应
%%% 解析」的传输能力，面向登录场景：
%%%   - oauth_token/2     : alipay.system.oauth.token，授权码换访问令牌
%%%   - user_info_share/2 : alipay.user.info.share，取用户公开信息
%%% 支付场景（下单/退款/回调验签）在 erlang_pay(epay_alipay)，勿重复造。
%%%
%%% 加签方式两种（支付宝后台二选一，本模块按 Cfg 是否带 SN 自适应）：
%%%   - 普通公钥模式：Cfg 只给 app_id/private_key
%%%   - 证书模式    ：Cfg 另给 app_cert_sn/alipay_root_cert_sn
%%%     （SN 由 cert_sn/1、root_cert_sn/1 从证书 PEM 算出，部署时注入）
%%%
%%% 同步响应不验签（HTTPS 信道 + 请求签名已防伪），与 erlang_pay 现状一致；
%%% 异步通知验签是支付侧职责，见 payment_sign。
%%% @end
%%%===================================================================

-export([oauth_token/2, user_info_share/2]).
-export([authinfo/2]).
-export([cert_sn/1, root_cert_sn/1]).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_GATEWAY, "https://openapi.alipay.com/gateway.do").
-define(NO_CREDENTIAL, <<"支付宝登录未配置凭据"/utf8>>).
%% 出站 HTTP 超时：网关挂起时避免无限阻塞 cowboy 进程（对齐 elib_oss 10000ms 惯例）
-define(HTTP_TIMEOUT, 10000).

%% X.500 属性 OID（本地定义，避免依赖 public_key.hrl 宏名稳定性）
-define(OID_CN, {2, 5, 4, 3}).
-define(OID_C, {2, 5, 4, 6}).
-define(OID_O, {2, 5, 4, 10}).
-define(OID_OU, {2, 5, 4, 11}).

-type cfg() :: #{
    app_id := binary(),
    private_key := binary(),
    pid => binary(),
    aes_key => binary(),
    gateway_url => string(),
    app_cert_sn => binary(),
    alipay_root_cert_sn => binary()
}.
-export_type([cfg/0]).

%%%===================================================================
%%% 业务接口
%%%===================================================================

%% @doc 授权码换访问令牌。返回归一 map：access_token/user_id/refresh_token/expires_in。
%% user_id 回退链 user_id → alipay_user_id → open_id：2023 后新建应用走 OpenID
%% 体系，网关只返回 open_id（生产实证 2026-08-22），无 2088 老会员号。
%% 注意：oauth.token 是前 biz_content 时代接口，grant_type/code 必须平铺顶层
%% （塞 biz_content JSON 会被网关前置层报「grant_type参数不正确」，生产探针实证）。
-spec oauth_token(cfg(), binary()) -> {ok, map()} | {error, binary()}.
oauth_token(Cfg, AuthCode) ->
    Extra = #{<<"grant_type">> => <<"authorization_code">>, <<"code">> => AuthCode},
    request(
        Cfg,
        <<"alipay.system.oauth.token">>,
        Extra,
        <<"alipay_system_oauth_token_response">>,
        fun norm_token/1
    ).

%% @doc 访问令牌取用户公开信息（user_id/avatar/nick_name/gender/province/city）。
%% 响应字段原样透出（binary 键 map），字段有无取决于用户授权范围。
%% auth_token 同样平铺顶层（与 oauth.token 同代接口）。
-spec user_info_share(cfg(), binary()) -> {ok, map()} | {error, binary()}.
user_info_share(Cfg, AccessToken) ->
    request(
        Cfg,
        <<"alipay.user.info.share">>,
        #{<<"auth_token">> => AccessToken},
        <<"alipay_user_info_share_response">>,
        fun(Resp) -> Resp end
    ).

%% @doc 客户端唤起支付宝 SDK 的 authInfo 签名串（alipay.open.auth.sdk.code.get）。
%% 私钥不出服务端：客户端拿本串直接喂 SDK，授权结果（auth_code）回传服务端。
%% TargetId：商户侧生成的唯一标识（防重放），每次调用新生成。
-spec authinfo(cfg(), binary()) -> {ok, binary()} | {error, binary()}.
authinfo(Cfg, TargetId) ->
    case cfg_ok(Cfg) andalso maps:get(pid, Cfg, <<>>) =/= <<>> of
        false ->
            {error, ?NO_CREDENTIAL};
        true ->
            Params = maybe_put(
                <<"app_cert_sn">>,
                maps:get(app_cert_sn, Cfg, <<>>),
                maybe_put(
                    <<"alipay_root_cert_sn">>,
                    maps:get(alipay_root_cert_sn, Cfg, <<>>),
                    #{
                        <<"apiname">> => <<"com.alipay.account.auth">>,
                        <<"app_id">> => maps:get(app_id, Cfg),
                        <<"app_name">> => <<"mc">>,
                        <<"auth_type">> => <<"AUTHACCOUNT">>,
                        <<"biz_type">> => <<"openservice">>,
                        <<"method">> => <<"alipay.open.auth.sdk.code.get">>,
                        <<"pid">> => maps:get(pid, Cfg),
                        <<"product_id">> => <<"APP_FAST_LOGIN">>,
                        <<"scope">> => <<"kuaijie">>,
                        <<"target_id">> => TargetId,
                        <<"sign_type">> => <<"RSA2">>
                    }
                )
            ),
            %% 签名内容 = 全部参数按 key 字典序 k=v& 原值拼接（与 sign_content 同规则）
            %% 公钥证书模式下 app_cert_sn / alipay_root_cert_sn 必须参与签名：
            %% 网关以 app_cert_sn 是否存在判定签名模式，缺失即按公钥模式验签 → 必败
            Content = sign_content(Params),
            case epay_crypto:rsa_sign_sha256(Content, maps:get(private_key, Cfg)) of
                {ok, Sig} ->
                    {ok, <<Content/binary, "&sign=", (urlencode(base64:encode(Sig)))/binary>>};
                {error, Reason} ->
                    {error, <<"支付宝签名失败:"/utf8, (atom_to_binary(Reason, utf8))/binary>>}
            end
    end.

-spec norm_token(map()) -> map().
norm_token(Resp) ->
    #{
        access_token => maps:get(<<"access_token">>, Resp, <<>>),
        user_id => maps:get(
            <<"user_id">>,
            Resp,
            maps:get(
                <<"alipay_user_id">>,
                Resp,
                maps:get(<<"open_id">>, Resp, <<>>)
            )
        ),
        refresh_token => maps:get(<<"refresh_token">>, Resp, <<>>),
        expires_in => maps:get(<<"expires_in">>, Resp, 0)
    }.

%%%===================================================================
%%% 传输：参数构造 + RSA2 签名 + POST + 响应解析
%%%===================================================================

-spec request(cfg(), binary(), map(), binary(), fun((map()) -> map())) ->
    {ok, map()} | {error, binary()}.
request(Cfg, Method, Extra, RespKey, OkFun) ->
    case cfg_ok(Cfg) of
        false ->
            {error, ?NO_CREDENTIAL};
        true ->
            do_request(Cfg, Method, Extra, RespKey, OkFun)
    end.

-spec cfg_ok(cfg()) -> boolean().
cfg_ok(Cfg) ->
    maps:get(app_id, Cfg, <<>>) =/= <<>> andalso maps:get(private_key, Cfg, <<>>) =/= <<>>.

%% Extra：接口业务参数，平铺进顶层（登录链两个老接口均无 biz_content）
-spec do_request(cfg(), binary(), map(), binary(), fun((map()) -> map())) ->
    {ok, map()} | {error, binary()}.
do_request(Cfg, Method, Extra, RespKey, OkFun) ->
    Params0 = maps:merge(
        Extra,
        #{
            <<"app_id">> => maps:get(app_id, Cfg),
            <<"method">> => Method,
            <<"format">> => <<"JSON">>,
            <<"charset">> => <<"utf-8">>,
            <<"sign_type">> => <<"RSA2">>,
            <<"timestamp">> => now_beijing(),
            <<"version">> => <<"1.0">>
        }
    ),
    Params = maybe_put(
        <<"app_cert_sn">>,
        maps:get(app_cert_sn, Cfg, <<>>),
        maybe_put(
            <<"alipay_root_cert_sn">>,
            maps:get(alipay_root_cert_sn, Cfg, <<>>),
            Params0
        )
    ),
    case epay_crypto:rsa_sign_sha256(sign_content(Params), maps:get(private_key, Cfg)) of
        {ok, Sig} ->
            Signed = Params#{<<"sign">> => base64:encode(Sig)},
            Url = maps:get(gateway_url, Cfg, ?DEFAULT_GATEWAY),
            post(Url, form_encode(Signed), RespKey, OkFun, Cfg);
        {error, Reason} ->
            {error, <<"支付宝签名失败:"/utf8, (atom_to_binary(Reason, utf8))/binary>>}
    end.

-spec post(string(), binary(), binary(), fun((map()) -> map()), cfg()) ->
    {ok, map()} | {error, binary()}.
post(Url, Body, RespKey, OkFun, Cfg) ->
    case
        httpc:request(
            post,
            {Url, [], "application/x-www-form-urlencoded", Body},
            [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_TIMEOUT}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, 200, _}, _Hdrs, RespBody}} ->
            parse(RespBody, RespKey, OkFun, Cfg);
        {ok, {{_, Status, _}, _, _}} ->
            {error, <<"支付宝接口 HTTP "/utf8, (integer_to_binary(Status))/binary>>};
        {error, Reason} ->
            ?LOG_ERROR("alipay_openapi post error ~p", [Reason]),
            {error, <<"支付宝接口请求失败"/utf8>>}
    end.

-spec parse(binary(), binary(), fun((map()) -> map()), cfg()) -> {ok, map()} | {error, binary()}.
parse(Body, RespKey, OkFun, Cfg) ->
    %% TODO(debug): 定位「未获取到用户标识」网关原始响应，排查后移除
    ?LOG_INFO("alipay_openapi resp body=~tp", [Body]),
    try jsone:decode(Body) of
        #{RespKey := Resp} when is_map(Resp) ->
            check_code(Resp, OkFun);
        #{RespKey := Cipher} when is_binary(Cipher) ->
            %% 控制台开启 AES 后 response 节点为密文 string：解密后再按明文走
            case aes_decrypt_b64(Cipher, maps:get(aes_key, Cfg, <<>>)) of
                {ok, Plain} ->
                    decode_biz(Plain, OkFun);
                error ->
                    {error, <<"支付宝响应解析失败"/utf8>>}
            end;
        %% 业务错误的真实载体（签名错/SN错/code失效等）：error_response 节点
        %% 本身就是错误标识，不走成功判定；透出 sub_msg 便于定位，
        %% 勿掩盖成「解析失败」（真机排障曾因此多绕一轮）
        #{<<"error_response">> := ErrResp} when is_map(ErrResp) ->
            ?LOG_ERROR("alipay_openapi error_response ~p", [ErrResp]),
            {error, biz_err_msg(ErrResp)};
        _ ->
            ?LOG_ERROR("alipay_openapi parse unrecognized body=~p", [Body]),
            {error, <<"支付宝响应解析失败"/utf8>>}
    catch
        _:_ ->
            ?LOG_ERROR("alipay_openapi parse crash body=~p", [Body]),
            {error, <<"支付宝响应解析失败"/utf8>>}
    end.

-spec decode_biz(binary(), fun((map()) -> map())) -> {ok, map()} | {error, binary()}.
decode_biz(Plain, OkFun) ->
    try jsone:decode(Plain) of
        Resp when is_map(Resp) ->
            check_code(Resp, OkFun);
        _ ->
            ?LOG_ERROR("alipay_openapi decode_biz unrecognized plain=~p", [Plain]),
            {error, <<"支付宝响应解析失败"/utf8>>}
    catch
        _:_ ->
            ?LOG_ERROR("alipay_openapi decode_biz crash plain=~p", [Plain]),
            {error, <<"支付宝响应解析失败"/utf8>>}
    end.

%% 成功判定对标官方 SDK 语义：无错误标识即成功。
%% oauth.token / user.info.share 是前 biz_content 时代老接口，成功响应可
%% 直出业务字段、无 code/msg（2026-08-19 真机「接口失败」根因：旧实现缺
%% code 即误判失败）；有 sub_code/sub_msg 或非 10000 code 才是错误。
-spec check_code(map(), fun((map()) -> map())) -> {ok, map()} | {error, binary()}.
check_code(Resp, OkFun) ->
    HasSub =
        maps:get(<<"sub_code">>, Resp, <<>>) =/= <<>> orelse
            maps:get(<<"sub_msg">>, Resp, <<>>) =/= <<>>,
    case maps:get(<<"code">>, Resp, <<"10000">>) of
        Code when
            not HasSub andalso (Code =:= <<"10000">> orelse Code =:= 10000 orelse Code =:= <<>>)
        ->
            {ok, OkFun(Resp)};
        _ ->
            ?LOG_ERROR("alipay_openapi biz error resp=~p", [Resp]),
            {error, biz_err_msg(Resp)}
    end.

%% 错误文案提取顺序：sub_msg（用户可读中文）→ msg → sub_code → code → 兜底。
%% 网关部分错误响应 charset=GBK（2026-08-22 真机 crash：GBK sub_msg 透传到
%% 响应层 jsone:encode badarg → 500），非 UTF-8 候选直接跳过，回退 ASCII sub_code。
-spec biz_err_msg(map()) -> binary().
biz_err_msg(Resp) ->
    Candidates = [
        maps:get(<<"sub_msg">>, Resp, <<>>),
        maps:get(<<"msg">>, Resp, <<>>),
        maps:get(<<"sub_code">>, Resp, <<>>),
        code_bin(maps:get(<<"code">>, Resp, <<>>))
    ],
    case [C || C <- Candidates, C =/= <<>>, is_utf8(C)] of
        [First | _] -> First;
        [] -> <<"接口失败"/utf8>>
    end.

-spec is_utf8(binary()) -> boolean().
is_utf8(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        B when is_binary(B) -> true;
        _ -> false
    end.

-spec code_bin(term()) -> binary().
code_bin(Code) when is_integer(Code) -> integer_to_binary(Code);
code_bin(Code) when is_binary(Code) -> Code;
code_bin(_) -> <<>>.

%%%===================================================================
%%% AES 内容加密（控制台「接口内容加密方式」）
%%% 当前仅响应侧：AES-128-CBC + PKCS7 解密，IV 为 16 字节 0，密文 base64。
%%% 登录链两个接口均为平铺参数（无 biz_content），请求侧无可加密对象；
%%% 未来对接 biz_content 时代接口（如获取手机号）时恢复请求侧 encode_biz
%%%（见 git 历史 4764871e）。
%%%===================================================================

-spec aes_decrypt_b64(binary(), binary()) -> {ok, binary()} | error.
aes_decrypt_b64(_CipherB64, <<>>) ->
    error;
aes_decrypt_b64(CipherB64, KeyB64) ->
    try
        Key = base64:decode(KeyB64),
        Plain = crypto:crypto_one_time(aes_128_cbc, Key, aes_iv(), base64:decode(CipherB64), false),
        {ok, pkcs7_unpad(Plain)}
    catch
        _:_ -> error
    end.

-spec aes_iv() -> binary().
aes_iv() ->
    <<0:128>>.

-spec pkcs7_unpad(binary()) -> binary().
pkcs7_unpad(Data) ->
    N = binary:last(Data),
    binary:part(Data, 0, byte_size(Data) - N).

%% 签名串：排除 sign 与空值（保留 sign_type），key 字典序，原值 k=v& 连接
-spec sign_content(map()) -> binary().
sign_content(Params) ->
    Pairs = [
        {K, V}
     || K <- lists:sort(maps:keys(Params)),
        K =/= <<"sign">>,
        V <- [maps:get(K, Params)],
        V =/= <<>>
    ],
    iolist_to_binary(lists:join(<<"&">>, [<<K/binary, "=", V/binary>> || {K, V} <- Pairs])).

%% RFC3986 form 编码：非保留字符 A-Z a-z 0-9 - _ . ~ 原样，其余 %HH（空格 %20）
-spec form_encode(map()) -> binary().
form_encode(Params) ->
    Pairs = [{K, maps:get(K, Params)} || K <- lists:sort(maps:keys(Params))],
    iolist_to_binary(
        lists:join(<<"&">>, [<<K/binary, "=", (urlencode(V))/binary>> || {K, V} <- Pairs])
    ).

-spec urlencode(binary()) -> binary().
urlencode(V) ->
    <<<<(enc_char(C))/binary>> || <<C>> <= V>>.

-spec enc_char(byte()) -> binary().
enc_char(C) when
    (C >= $A andalso C =< $Z) orelse
        (C >= $a andalso C =< $z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $- orelse C =:= $_ orelse C =:= $. orelse C =:= $~
->
    <<C>>;
enc_char(C) ->
    list_to_binary(io_lib:format("%~2.16.0B", [C])).

-spec maybe_put(binary(), binary(), map()) -> map().
maybe_put(_K, <<>>, M) -> M;
maybe_put(K, V, M) -> M#{K => V}.

-spec now_beijing() -> binary().
now_beijing() ->
    Secs = erlang:system_time(second) + 8 * 3600,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Secs, second),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, Mo, D, H, Mi, S])
    ).

%%%===================================================================
%%% 证书 SN（证书模式必传 app_cert_sn / alipay_root_cert_sn）
%%% 算法对标支付宝官方 SDK：md5("CN=..,OU=..,O=..,C=.." + 十进制 serial)
%%%===================================================================

%% @doc 应用公钥证书 SN（appCertPublicKey.crt 内容 → 32 位 md5 hex）
-spec cert_sn(binary()) -> binary().
cert_sn(Pem) ->
    [{'Certificate', Der, not_encrypted} | _] = public_key:pem_decode(Pem),
    Cert = public_key:pkix_decode_cert(Der, otp),
    sn_from_cert(Cert).

%% @doc 支付宝根证书 SN：链上每张 RSA 家族签名算法的证书各算一个 SN，
%% 按出现顺序用 "_" 拼接；解析失败的证书块跳过（对标官方 SDK 行为）。
-spec root_cert_sn(binary()) -> binary().
root_cert_sn(Pem) ->
    Ders = [Der || {'Certificate', Der, _} <- public_key:pem_decode(Pem)],
    Sns = lists:filtermap(fun der_sn_rsa/1, Ders),
    iolist_to_binary(lists:join(<<"_">>, Sns)).

-spec der_sn_rsa(binary()) -> {true, binary()} | false.
der_sn_rsa(Der) ->
    try public_key:pkix_decode_cert(Der, otp) of
        Cert ->
            Tbs = Cert#'OTPCertificate'.tbsCertificate,
            Alg = (Tbs#'OTPTBSCertificate'.signature)#'SignatureAlgorithm'.algorithm,
            case is_rsa_family(Alg) of
                true -> {true, sn_from_cert(Cert)};
                false -> false
            end
    catch
        _:_ ->
            false
    end.

%% RSA 家族签名算法 OID：1.2.840.113549.1.1.{1,2,4,5,11,12,13}
-spec is_rsa_family(tuple() | term()) -> boolean().
is_rsa_family({1, 2, 840, 113549, 1, 1, N}) -> lists:member(N, [1, 2, 4, 5, 11, 12, 13]);
is_rsa_family(_) -> false.

-spec sn_from_cert(term()) -> binary().
sn_from_cert(Cert) ->
    Tbs = Cert#'OTPCertificate'.tbsCertificate,
    Issuer = issuer_str(Tbs#'OTPTBSCertificate'.issuer),
    Serial = integer_to_binary(Tbs#'OTPTBSCertificate'.serialNumber),
    epay_crypto:lower_hex(crypto:hash(md5, <<Issuer/binary, Serial/binary>>)).

%% issuer DN 固定按 "CN=..,OU=..,O=..,C=.." 序拼接（官方 SDK 格式，与
%% 证书内 RDN 原始顺序无关）
-spec issuer_str(tuple() | term()) -> binary().
issuer_str({rdnSequence, RDNs}) ->
    Attrs = lists:flatten(RDNs),
    CN = rdn_value(?OID_CN, Attrs),
    OU = rdn_value(?OID_OU, Attrs),
    O = rdn_value(?OID_O, Attrs),
    C = rdn_value(?OID_C, Attrs),
    <<"CN=", CN/binary, ",OU=", OU/binary, ",O=", O/binary, ",C=", C/binary>>;
issuer_str(_) ->
    <<>>.

-spec rdn_value(tuple(), [term()]) -> binary().
rdn_value(_Oid, []) ->
    <<>>;
rdn_value(Oid, [#'AttributeTypeAndValue'{type = Oid, value = V} | _]) ->
    dir_string(V);
rdn_value(Oid, [_ | T]) ->
    rdn_value(Oid, T).

-spec dir_string(term()) -> binary().
dir_string({utf8String, B}) when is_binary(B) -> B;
dir_string({printableString, S}) -> dir_string(S);
dir_string({teletexString, S}) -> dir_string(S);
dir_string(B) when is_binary(B) -> B;
dir_string(S) when is_list(S) -> list_to_binary(S);
dir_string(_) -> <<>>.
