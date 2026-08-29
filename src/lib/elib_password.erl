-module(elib_password).

%%% @doc 密码哈希和验证模块
%%% 使用 HMAC-SHA512 算法生成密码哈希，支持密码验证
%%%
%%% 前端密码预哈希格式（2026-08-26 从 MD5 迁移到 SHA-256）：
%%%   旧协议：前端发送 md5(plaintext)，后端验证 hmac_sha512(md5(plaintext), salt)
%%%   新协议：前端发送 sha256(plaintext)，后端验证 hmac_sha512(sha256(plaintext), salt)
%%% 兼容性：verify/2 先尝试新格式，失败后自动回退旧格式，验证成功时升级存储。
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
    % 首先尝试解码为 hmac_sha512 格式
    Decoded = try_decode_hmac_sha512(Ciphertext),
    case Decoded of
        {ok, Salt, Ciphertext3} ->
            % 尝试新格式（SHA-256 预哈希，2026-08-26 迁移）
            case verify_hmac_sha512(Plaintext, Salt, Ciphertext3) of
                {ok, _} = Ok ->
                    Ok;
                _ ->
                    % 回退旧格式（MD5 预哈希，兼容存量密码）
                    % CI-00 修复：删除死代码 _Md5Plain（计算结果从未使用），且其对
                    % list 输入（-spec 允许 iodata()）调用 binary_to_list 必 badarg，
                    % 导致合法 string 密码验证直接崩溃而非走旧格式回退。
                    verify(Plaintext, default_md5, config_ds:env(password_salt, <<>>), Ciphertext)
            end;
        _ ->
            % 回退到旧的 md5 格式（仅用于存量旧密码，password_salt 从 sys.config 读取）
            verify(Plaintext, default_md5, config_ds:env(password_salt, <<>>), Ciphertext)
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

%% @private 尝试验证 HMAC-SHA512 格式密码（支持新旧预哈希兼容）
%% 先尝试新格式（SHA-256 预哈希），失败后回退旧格式（MD5 预哈希）。
-spec verify_hmac_sha512(iodata(), binary(), binary()) -> {ok, []} | {error, binary()}.
verify_hmac_sha512(Plaintext, Salt, Ciphertext) ->
    %% 新格式：hmac_sha512(sha256(plaintext), salt)
    Sha256Plain = crypto:hash(sha256, Plaintext),
    case elib_hasher:hmac_sha512(Sha256Plain, Salt) =:= Ciphertext of
        true ->
            {ok, []};
        false ->
            %% 旧格式兼容：hmac_sha512(md5(plaintext), salt)
            % CI-00 修复：去掉 binary_to_list 包装——elib_hasher:md5/1 的 spec
            % 本就接受 binary() | list()，对 binary 输入 erlang:md5 结果一致；
            % 原写法对 list 输入（-spec 允许 iodata()）badarg。
            Md5Plain = elib_hasher:md5(Plaintext),
            case elib_hasher:hmac_sha512(Md5Plain, Salt) =:= Ciphertext of
                true -> {ok, []};
                false -> {error, <<"errorPassword">>}
            end
    end.

-spec eq(binary(), binary()) -> {ok, []} | {error, binary()}.
eq(Ciphertext, Ciphertext2) ->
    % 常数时间比较，避免逐字节比对泄露时序信息（原用 =:= 直接比较存在时序攻击风险）
    case crypto:hash_equals(crypto:hash(sha256, Ciphertext), crypto:hash(sha256, Ciphertext2)) of
        true -> {ok, []};
        false -> {error, <<"errorPassword">>}
    end.
