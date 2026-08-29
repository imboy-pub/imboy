-module(elib_password).

%%% @doc 密码哈希和验证模块
%%% 使用 HMAC-SHA512 算法生成密码哈希，支持密码验证
%%%
%%% 前端密码预哈希格式：
%%%   现行协议：前端发送 md5(plaintext)（hex），后端验证 hmac_sha512(md5(plaintext), salt)。
%%%   sha256 切换（暂缓）：须先落地 rehash-on-login（验证成功后升级存储）再切前端——
%%%   存量 default_md5 行从 sha256 值数学上不可反推验证，先切前端 = 存量用户全部锁死
%%%   （2026-08-28 发布审查 C-1：verify 曾缺直比较分支导致 generate/verify 往返必假）。
%%% 兼容性：verify/2 依次尝试 hmac 直值 / sha256 预哈希 / md5 预哈希，最后回退 default_md5 旧行。
%%%
%%% Pwd = elib_password:generate(<<"admin888">>).
%%% elib_password:verify(<<"admin888">>, Pwd).
%%%
-include("log.hrl").
-include("common.hrl").

-export([generate/1, generate/2]).
-export([verify/2]).

%% @doc 生成密码哈希（使用默认 HMAC-SHA512 算法）
%% @param Plaintext 明文密码
%% @returns Base64 编码的密码哈希
-spec generate(iodata()) -> binary().
generate(Plaintext) ->
    generate(Plaintext, hmac_sha512).

%% @doc 生成密码哈希（指定算法）
%% @param Plaintext 明文密码
%% @param hmac_sha512 算法类型（目前仅支持 hmac_sha512）
%% @returns Base64 编码的密码哈希
-spec generate(iodata(), hmac_sha512) -> binary().
generate(Plaintext, hmac_sha512) ->
    Salt2 = base64:encode(crypto:strong_rand_bytes(16)),
    % CI-00 修复：对齐 verify_hmac_sha512/3 的存储格式。2026-08-26 SHA-256 预哈希
    % 迁移只改了 verify 侧、遗漏 generate 侧——原实现直发 hmac(Plaintext, salt)，
    % 其产物在 verify 中新旧格式分支均不命中，generate↔verify 不闭环（新注册
    % 用户必然无法登录）。现按新协议存储 hmac(sha256(plaintext), salt)。
    Ciphertext = elib_hasher:hmac_sha512(crypto:hash(sha256, Plaintext), Salt2),
    base64:encode(<<Salt2/binary, ":hmac_sha512:", Ciphertext/binary>>).

%% @doc 验证密码
%% @param Plaintext 明文密码
%% @param Ciphertext 密码哈希（Base64 编码）
%% @returns {ok, []} | {error, <<"errorPassword">>}
%% @example
%% Pwd = elib_password:generate(<<"admin888">>),
%% elib_password:verify(<<"admin888">>, Pwd).
-spec verify(iodata(), iodata()) -> {ok, []} | {error, binary()}.
verify(Plaintext, Ciphertext) ->
    % 统一收敛为 binary：verify_hmac_sha512 的 md5 回退路径用 binary_to_list，
    % 列表输入会 badarg；spec 声明 iodata，两态皆须可用
    PlainBin = iolist_to_binary(Plaintext),
    % 首先尝试解码为 hmac_sha512 格式
    Decoded = try_decode_hmac_sha512(Ciphertext),
    case Decoded of
        {ok, Salt, Ciphertext3} ->
            % 尝试新格式（SHA-256 预哈希，2026-08-26 迁移）
            case verify_hmac_sha512(PlainBin, Salt, Ciphertext3) of
                {ok, _} = Ok ->
                    Ok;
                _ ->
                    % 回退旧格式（MD5 预哈希，兼容存量密码；verify/4 内部自行计算 md5）
                    verify(PlainBin, default_md5, config_ds:env(password_salt, <<>>), Ciphertext)
            end;
        _ ->
            % 回退到旧的 md5 格式（仅用于存量旧密码，password_salt 从 sys.config 读取）
            verify(PlainBin, default_md5, config_ds:env(password_salt, <<>>), Ciphertext)
    end.

%% @private 尝试解码为 hmac_sha512 格式
-spec try_decode_hmac_sha512(iodata()) -> {ok, binary(), binary()} | error.
try_decode_hmac_sha512(Ciphertext) ->
    try
        Decoded = base64:decode(Ciphertext, #{padding => false}),
        case binary:split(Decoded, <<$:>>, [global]) of
            [Salt, <<"hmac_sha512">>, Ciphertext3] ->
                {ok, Salt, Ciphertext3};
            _ ->
                error
        end
    catch
        _:_:_ -> error
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("eunit_setup.hrl").

md5_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "abc",
        Ciphertext = generate(Plaintext),
        Resp = verify(Plaintext, Ciphertext),

        ?assert(Resp =:= {ok, []}),
        ?DEBUG_LOG(Resp)
    end).

hmac_sha512_test_() ->
    ?TEST_WITH_APP(fun() ->
        Plaintext = "abc",
        Ciphertext = generate(Plaintext),
        Resp = verify(Plaintext, Ciphertext),
        ?assert(Resp =:= {ok, []}),
        ?DEBUG_LOG(Resp)
    end).

-endif.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec verify(iodata(), atom(), binary(), binary()) -> {ok, []} | {error, binary()}.
verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Plaintext) ->
    verify(list_to_binary(Plaintext), default_md5, Salt, Ciphertext);
verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Ciphertext) ->
    verify(Plaintext, default_md5, Salt, list_to_binary(Ciphertext));
verify(Plaintext, default_md5, Salt, Ciphertext) ->
    Plaintext2 = <<Plaintext/binary, Salt/binary>>,
    % ?DEBUG_LOG(Plaintext2),
    Ciphertext2 = elib_hasher:md5(binary_to_list(Plaintext2)),
    % ?DEBUG_LOG([default_md5, Ciphertext2, Ciphertext]),
    eq(Ciphertext, Ciphertext2);
verify(Plaintext, hmac_sha512, Salt, Ciphertext) ->
    Ciphertext2 = elib_hasher:hmac_sha512(Plaintext, Salt),
    eq(Ciphertext, Ciphertext2).

%% @private 验证 HMAC-SHA512 格式密码（按预哈希变体依次尝试，全部走 eq/2 常数时间比较）
%% ① 直值：与 generate/1 存储的 hmac(P, Salt) 成对，必须最先尝试——
%%    缺此分支则 generate/verify 往返必假（2026-08-28 发布审查 C-1）
%% ② SHA-256 预哈希：为前端 sha256 切换预留
%% ③ MD5 预哈希：兼容旧协议 hmac 行
-spec verify_hmac_sha512(iodata(), binary(), binary()) -> {ok, []} | {error, binary()}.
verify_hmac_sha512(Plaintext, Salt, Ciphertext) ->
    PlainBin = iolist_to_binary(Plaintext),
    Candidates = [
        PlainBin,
        crypto:hash(sha256, PlainBin),
        elib_hasher:md5(binary_to_list(PlainBin))
    ],
    verify_candidates(Candidates, Salt, Ciphertext).

-spec verify_candidates([binary()], binary(), binary()) -> {ok, []} | {error, binary()}.
verify_candidates([], _Salt, _Ciphertext) ->
    {error, <<"errorPassword">>};
verify_candidates([P | Rest], Salt, Ciphertext) ->
    case eq(Ciphertext, elib_hasher:hmac_sha512(P, Salt)) of
        {ok, _} = Ok -> Ok;
        _ -> verify_candidates(Rest, Salt, Ciphertext)
    end.

-spec eq(binary(), binary()) -> {ok, []} | {error, binary()}.
eq(Ciphertext, Ciphertext2) ->
    % 常数时间比较，避免逐字节比对泄露时序信息（原用 =:= 直接比较存在时序攻击风险）
    case crypto:hash_equals(crypto:hash(sha256, Ciphertext), crypto:hash(sha256, Ciphertext2)) of
        true -> {ok, []};
        false -> {error, <<"errorPassword">>}
    end.
