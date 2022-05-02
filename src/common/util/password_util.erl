-module (password_util).
%%%
% Pwd = password_util:generate("admin888").
% password_util:verify("admin888", Pwd).
%%%
-export ([generate/1, generate/2]).
-export ([verify/2]).

-include("common.hrl").

-spec generate(Plaintext::list()) -> Ciphertext::binary().
generate(Plaintext) ->
    generate(Plaintext, hmac_sha512).

generate(Plaintext, hmac_sha512) ->
    Salt1 = func:num_random(40),
    Salt2 = list_to_binary(integer_to_list(Salt1)),
    Ciphertext = hash_util:hmac_sha512(Plaintext, Salt2),
    base64:encode(<<Salt2/binary, ":hmac_sha512:", Ciphertext/binary>>).

-spec verify(Plaintext::list(), Ciphertext::list()) -> {ok, any()} | {error, Msg::list()}.
verify(Plaintext, Ciphertext) ->
    % ?LOG([Plaintext, Ciphertext]),
    try
        Ciphertext2 = base64:decode(Ciphertext),
        binary:split(Ciphertext2, <<$:>>, [global, trim])
    of
        [Salt, <<"hmac_sha512">>, Ciphertext3] ->
            verify(Plaintext, hmac_sha512, Salt, Ciphertext3);
        _Msg ->
            % ?LOG(Msg),
            verify(Plaintext, default_md5, ?MD5_SALT, Ciphertext)
    catch _:_ ->
        % ?LOG([default_md5, Plaintext, Ciphertext]),
        verify(Plaintext, default_md5, ?MD5_SALT, Ciphertext)
    end.

-ifdef(TEST).
    md5_test() ->
        Resp1 = password_util:verify("admin888", "299e0c8fbc9cf877bcc46bcee2ca5987"),
        ?LOG(Resp1),

        Plaintext = "abc",
        Ciphertext = generate(Plaintext),
        Resp = verify(Plaintext, Ciphertext),
        ?LOG(Resp).

    hmac_sha512_test() ->
        Plaintext = "abc",
        Ciphertext = generate(Plaintext),
        Resp = verify(Plaintext, Ciphertext),
        ?LOG(Resp).

-endif.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Plaintext) ->
    verify(list_to_binary(Plaintext), default_md5, Salt, Ciphertext);
verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Ciphertext) ->
    verify(Plaintext, default_md5, Salt, list_to_binary(Ciphertext));
verify(Plaintext, default_md5, Salt, Ciphertext) ->
    Plaintext2 = <<Plaintext/binary, Salt/binary>>,
    ?LOG(Plaintext2),
    Ciphertext2 = hash_util:md5(binary_to_list(Plaintext2)),
    ?LOG([default_md5, Ciphertext2, Ciphertext]),
    if
        Ciphertext2 == Ciphertext ->
            {ok, []};
        true ->
            {error, "密码有误"}
    end;

verify(Plaintext, hmac_sha512, Salt, Ciphertext) ->
    Ciphertext2 = hash_util:hmac_sha512(Plaintext, Salt),
    ?LOG([Plaintext, Salt, Ciphertext, Ciphertext2]),
    if
        Ciphertext2 == Ciphertext ->
            {ok, []};
        true ->
            {error, "密码有误"}
    end.

