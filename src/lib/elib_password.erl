-module(elib_password).

%%% @doc 密码哈希和验证模块
%%% 使用 HMAC-SHA512 算法生成密码哈希，支持密码验证

%%%
% Pwd = elib_password:generate(<<"admin888">>).
% elib_password:verify(<<"admin888">>, Pwd).
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
    Salt1 = elib_cipher:num_random(40),
    Salt2 = integer_to_binary(Salt1),
    Ciphertext = elib_hasher:hmac_sha512(Plaintext, Salt2),
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
    % 首先尝试解码为新的 hmac_sha512 格式
    Decoded = try_decode_hmac_sha512(Ciphertext),
    case Decoded of
        {ok, Salt, Ciphertext3} ->
            verify(Plaintext, hmac_sha512, Salt, Ciphertext3);
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

-spec eq(binary(), binary()) -> {ok, []} | {error, binary()}.
eq(Ciphertext, Ciphertext2) ->
    % ?DEBUG_LOG([admin_pwd, Ciphertext2, Ciphertext]),
    case Ciphertext2 == Ciphertext of
        true ->
            {ok, []};
        _ ->
            % errorPassword 为APP端的多语言吗
            {error, <<"errorPassword">>}
    end.
