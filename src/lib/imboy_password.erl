-module(imboy_password).

%%%
% Pwd = imboy_password:generate(imboy_hasher:md5("admin888")).
% imboy_password:verify(imboy_hasher:md5("admin888"), Pwd).
%%%
-include("log.hrl").
-include("common.hrl").

-export([generate/1, generate/2]).
-export([verify/2]).

-spec generate(iodata()) -> binary().
generate(Plaintext) ->
    generate(Plaintext, hmac_sha512).

% io:format("~s~n", [imboy_password:generate(imboy_hasher:md5("admin888"))]).
-spec generate(iodata(), hmac_sha512) -> binary().
generate(Plaintext, hmac_sha512) ->
    Salt1 = imboy_func:num_random(40),
    Salt2 = integer_to_binary(Salt1),
    Ciphertext = imboy_hasher:hmac_sha512(Plaintext, Salt2),
    % io:format("~s~n", [Ciphertext]),
    base64:encode(<<Salt2/binary, ":hmac_sha512:", Ciphertext/binary>>).

-spec verify(iodata(), iodata()) -> {ok, []} | {error, binary()}.
verify(Plaintext, Ciphertext) ->
    % ?DEBUG_LOG([Plaintext, base64:decode(Plaintext), Ciphertext, base64:decode(Ciphertext)]),
    try Ciphertext2 = base64:decode(Ciphertext),
        binary:split(Ciphertext2, <<$:>>, [global, trim])
    of
        [Salt, <<"hmac_sha512">>, Ciphertext3] ->
            verify(Plaintext, hmac_sha512, Salt, Ciphertext3);
        _Msg ->
            % ?DEBUG_LOG(Msg),
            verify(Plaintext, default_md5, config_ds:get(<<"password_salt">>), Ciphertext)
    catch
        _:_ ->
            % ?DEBUG_LOG([default_md5, Plaintext, Ciphertext]),
            verify(Plaintext, default_md5, config_ds:get(<<"password_salt">>), Ciphertext)
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

verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Plaintext) ->
    verify(list_to_binary(Plaintext), default_md5, Salt, Ciphertext);
verify(Plaintext, default_md5, Salt, Ciphertext) when is_list(Ciphertext) ->
    verify(Plaintext, default_md5, Salt, list_to_binary(Ciphertext));
verify(Plaintext, default_md5, Salt, Ciphertext) ->
    Plaintext2 = <<Plaintext/binary, Salt/binary>>,
    % ?DEBUG_LOG(Plaintext2),
    Ciphertext2 = imboy_hasher:md5(binary_to_list(Plaintext2)),
    % ?DEBUG_LOG([default_md5, Ciphertext2, Ciphertext]),
    eq(Ciphertext, Ciphertext2);
verify(Plaintext, hmac_sha512, Salt, Ciphertext) ->
    Ciphertext2 = imboy_hasher:hmac_sha512(Plaintext, Salt),
    % io:format("~p~n", [Plaintext]),
    % io:format("~p~n", [Ciphertext2]),
    % ?DEBUG_LOG([hmac_sha512, Plaintext, Salt, Ciphertext, Ciphertext2]),
    eq(Ciphertext, Ciphertext2).

eq(Ciphertext, Ciphertext2) ->
    % ?DEBUG_LOG([admin_pwd, Ciphertext2, Ciphertext]),
    case Ciphertext2 == Ciphertext of
        true ->
            {ok, []};
        _ ->
            % errorPassword 为APP端的多语言吗
            {error, <<"errorPassword">>}
    end.
