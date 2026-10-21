-module(enterprise_internal_signature).
-moduledoc "Internal request HMAC signing. Canonical bytes bind the target, body and retry key.".
-export([present/1, parse/2, canonical/5, sign/6, verify/6]).

-define(WINDOW, 300).

present(Headers) ->
    lists:any(
        fun(Key) -> maps:is_key(Key, Headers) end,
        [
            <<"x-imboy-credential">>,
            <<"x-imboy-signature">>,
            <<"x-imboy-timestamp">>,
            <<"x-imboy-nonce">>
        ]
    ).

%% Nonces are opaque single-use values. Wall-clock validation is repeated in
%% the database when reserving a nonce; parsing alone never authenticates.
parse(Headers, Now) when is_map(Headers), is_integer(Now) ->
    try
        Prefix = maps:get(<<"x-imboy-credential">>, Headers),
        <<"ib_int_", Id/binary>> = Prefix,
        true = ascii(Id, 1, 24, fun digit/1),
        Timestamp = maps:get(<<"x-imboy-timestamp">>, Headers),
        true = ascii(Timestamp, 1, 12, fun digit/1),
        Seconds = binary_to_integer(Timestamp),
        true = integer_to_binary(Seconds) =:= Timestamp,
        true = abs(Now - Seconds) =< ?WINDOW,
        Nonce = maps:get(<<"x-imboy-nonce">>, Headers),
        true = ascii(Nonce, 16, 128, fun nonce_char/1),
        {Algorithm, Hex} = algorithm(maps:get(<<"x-imboy-signature">>, Headers)),
        {ok, #{
            prefix => Prefix,
            timestamp => Timestamp,
            seconds => Seconds,
            nonce => Nonce,
            algorithm => Algorithm,
            signature => Hex
        }}
    catch
        _:_ -> {error, invalid_credential}
    end;
parse(_, _) ->
    {error, invalid_credential}.

%% Each field occupies one line. Path and query are the exact wire bytes;
%% the query is neither sorted nor decoded, and an empty query stays empty.
canonical(Method, Path, Query, Body, #{headers := Headers, algorithm := sha256} = Signed) ->
    iolist_to_binary(
        lists:join(<<"\n">>, [
            <<"IMBOY-INTERNAL-V1">>,
            <<"sha256">>,
            maps:get(prefix, Signed),
            maps:get(<<"x-imboy-organization">>, Headers, <<>>),
            Method,
            Path,
            Query,
            maps:get(<<"content-type">>, Headers, <<>>),
            maps:get(<<"idempotency-key">>, Headers, <<>>),
            binary:encode_hex(crypto:hash(sha256, Body), lowercase),
            maps:get(timestamp, Signed),
            maps:get(nonce, Signed)
        ])
    ).

%% Secret is the secret component, excluding the ib_int_ locator.
sign(Secret, Method, Path, Query, Body, #{algorithm := sha256} = Signed) ->
    binary:encode_hex(
        crypto:mac(
            hmac,
            sha256,
            Secret,
            canonical(Method, Path, Query, Body, Signed)
        ),
        lowercase
    ).

verify(Secret, Method, Path, Query, Body, Signed) ->
    Expected = sign(Secret, Method, Path, Query, Body, Signed),
    Actual = maps:get(signature, Signed),
    byte_size(Expected) =:= byte_size(Actual) andalso crypto:hash_equals(Expected, Actual).

algorithm(<<"sha256=", Hex:64/binary>>) ->
    true = ascii(Hex, 64, 64, fun hex/1),
    {sha256, Hex}.

ascii(Value, Min, Max, Predicate) when is_binary(Value) ->
    byte_size(Value) >= Min andalso byte_size(Value) =< Max andalso
        lists:all(Predicate, binary_to_list(Value)).
digit(C) -> C >= $0 andalso C =< $9.
hex(C) -> digit(C) orelse (C >= $a andalso C =< $f).
nonce_char(C) ->
    digit(C) orelse (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse C =:= $_ orelse C =:= $-.
