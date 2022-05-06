-module(util_password).
%%%
% Pwd = util_password:generate("admin888").
% util_password:verify("admin888", Pwd).
%%%
-export([generate/1, generate/2]).
-export([verify/2]).

-include("common.hrl").


-spec generate(Plaintext :: list()) -> Ciphertext :: binary().
generate(Plaintext) ->
    generate(Plaintext, hmac_sha512).


generate(Plaintext, hmac_sha512) ->
    Salt1 = util_func:num_random(40),
    Salt2 = list_to_binary(integer_to_list(Salt1)),
    Ciphertext = util_hash:hmac_sha512(Plaintext, Salt2),
    base64:encode(<<Salt2/binary, ":hmac_sha512:", Ciphertext/binary>>).


-spec verify(Plaintext :: list(), Ciphertext :: list()) ->
          {ok, any()} | {error, Msg :: list()}.
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
    catch
        _:_ ->
            % ?LOG([default_md5, Plaintext, Ciphertext]),
            verify(Plaintext, default_md5, ?MD5_SALT, Ciphertext)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


md5_test() ->
    Resp1 = util_password:verify("admin888",
                                 "299e0c8fbc9cf877bcc46bcee2ca5987"),
    ?LOG(Resp1),
    Plaintext = "abc",
    Ciphertext = generate(Plaintext),
    Resp = verify(Plaintext, Ciphertext),

    ?assert(Resp =:= {ok, []}),
    ?LOG(Resp).


hmac_sha512_test() ->
    Plaintext = "abc",
    Ciphertext = generate(Plaintext),
    Resp = verify(Plaintext, Ciphertext),
    ?assert(Resp =:= {ok, []}),
    ?LOG(Resp).

-endif.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

verify(Plaintext, default_md5, Salt, Ciphertext)
  when is_list(Plaintext) ->
    verify(list_to_binary(Plaintext), default_md5, Salt, Ciphertext);
verify(Plaintext, default_md5, Salt, Ciphertext)
  when is_list(Ciphertext) ->
    verify(Plaintext, default_md5, Salt, list_to_binary(Ciphertext));
verify(Plaintext, default_md5, Salt, Ciphertext) ->
    Plaintext2 = <<Plaintext/binary, Salt/binary>>,
    % ?LOG(Plaintext2),
    Ciphertext2 = util_hash:md5(binary_to_list(Plaintext2)),
    % ?LOG([default_md5, Ciphertext2, Ciphertext]),
    eq(Ciphertext, Ciphertext2);
verify(Plaintext, hmac_sha512, Salt, Ciphertext) ->
    Ciphertext2 = util_hash:hmac_sha512(Plaintext, Salt),
    % ?LOG([Plaintext, Salt, Ciphertext, Ciphertext2]),
    eq(Ciphertext, Ciphertext2).


eq(Ciphertext, Ciphertext2) ->
    case Ciphertext2 == Ciphertext of
        true ->
            {ok, []};
        _ ->
            {error, "密码有误"}
    end.
