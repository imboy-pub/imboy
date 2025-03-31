-module(imboy_password).
%%%
% Pwd = imboy_password:generate(imboy_hasher:md5("admin888")).
% imboy_password:verify(imboy_hasher:md5("admin888"), Pwd).
%%%
-include_lib("imlib/include/log.hrl").
-include_lib("imlib/include/common.hrl").

-export([generate/1, generate/2]).
-export([verify/2]).


-spec generate(Plaintext :: list()) -> Ciphertext :: binary().
generate(Plaintext) ->
    generate(Plaintext, hmac_sha512).


% io:format("~s~n", [imboy_password:generate(imboy_hasher:md5("admin888"))]).
generate(Plaintext, hmac_sha512) ->
    Salt1 = imboy_func:num_random(40),
    Salt2 = list_to_binary(integer_to_list(Salt1)),
    Ciphertext = imboy_hasher:hmac_sha512(Plaintext, Salt2),
    % io:format("~s~n", [Ciphertext]),
    base64:encode(<<Salt2/binary, ":hmac_sha512:", Ciphertext/binary>>).


-spec verify(list(), list()) -> {ok, any()} | {error, Msg :: list()}.
verify(Plaintext, Ciphertext) ->
    % ?LOG([Plaintext, base64:decode(Plaintext), Ciphertext, base64:decode(Ciphertext)]),
    try
        Ciphertext2 = base64:decode(Ciphertext),
        binary:split(Ciphertext2, <<$:>>, [global, trim])
    of
        [Salt, <<"hmac_sha512">>, Ciphertext3] ->
            verify(Plaintext, hmac_sha512, Salt, Ciphertext3);
        _Msg ->
            % ?LOG(Msg),
            verify(Plaintext, default_md5, config_ds:get(password_salt), Ciphertext)
    catch
        _:_ ->
            % ?LOG([default_md5, Plaintext, Ciphertext]),
            verify(Plaintext, default_md5, config_ds:get(password_salt), Ciphertext)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


md5_test() ->
    Resp1 = imboy_password:verify("admin888", "299e0c8fbc9cf877bcc46bcee2ca5987"),
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

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Plaintext) ->
    verify(list_to_binary(Plaintext), default_md5, Salt, Ciphertext);
verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Ciphertext) ->
    verify(Plaintext, default_md5, Salt, list_to_binary(Ciphertext));
verify(Plaintext, default_md5, Salt, Ciphertext) ->
    Plaintext2 = <<Plaintext/binary, Salt/binary>>,
    % ?LOG(Plaintext2),
    Ciphertext2 = imboy_hasher:md5(binary_to_list(Plaintext2)),
    % ?LOG([default_md5, Ciphertext2, Ciphertext]),
    eq(Ciphertext, Ciphertext2);
verify(Plaintext, hmac_sha512, Salt, Ciphertext) ->
    Ciphertext2 = imboy_hasher:hmac_sha512(Plaintext, Salt),
    % io:format("~p~n", [Plaintext]),
    % io:format("~p~n", [Ciphertext2]),
    % ?LOG([Plaintext, Salt, Ciphertext, Ciphertext2]),
    eq(Ciphertext, Ciphertext2).


eq(Ciphertext, Ciphertext2) ->
    case Ciphertext2 == Ciphertext of
        true ->
            {ok, []};
        _ ->
            % error_password 为APP端的多语言吗
            {error, "error_password"}
    end.
