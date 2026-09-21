-module(rest_fixture).

%% Direct factory functions for REST Common Test suites (no fixture
%% framework: plan §3.2 — extract shared helpers only once real duplication
%% exists across three or more suites).

-export([
    create_user/1,
    unique_id/1,
    signed_headers/2,
    login/2,
    auth_header/1,
    session/1,
    sign_key/0,
    store_session/2,
    sanitize_user/2,
    await_device_active/2,
    ensure_login_throttle_capacity/0,
    ensure_ct_priv_alias/0,
    ensure_sign_key/0,
    flip_signature_bit/1
]).

%% Must run first in init_per_suite. The alias subtree
%% .ct/appalias/imboy/{ebin,priv} (built by the runner as VM startup -pa)
%% makes code:priv_dir(imboy) resolvable in a differently named worktree;
%% this helper now only forces the test/common build of eunit_runner
%% (the app's ebin carries an unrelated src/lib eunit_runner that would
%% otherwise shadow it and lack ct_suite_setup/1).
-spec ensure_ct_priv_alias() -> ok.
ensure_ct_priv_alias() ->
    case os:getenv("REST_PROJECT_ROOT") of
        false ->
            ok;
        Root ->
            TestBeam = filename:join([Root, "test", "eunit_runner"]),
            code:purge(eunit_runner),
            code:delete(eunit_runner),
            {module, eunit_runner} = code:load_abs(TestBeam, eunit_runner),
            %% test/common ships STUB copies of product modules (a 35-line
            %% config_ds whose get/2 always returns the default). CT can put
            %% the test dir ahead of the alias subtree regardless of -pa
            %% ordering, so path order alone is not trustworthy: load the
            %% real ebin build by absolute path. cowboy_req_h is test-only
            %% and has no ebin build; skip it.
            lists:foreach(
                fun(M) ->
                    code:purge(M),
                    code:delete(M),
                    Beam = filename:join([Root, "ebin", atom_to_list(M)]),
                    {module, M} = code:load_abs(Beam, M)
                end,
                [config_ds]
            ),
            ok
    end.

-spec create_user(map()) -> map().
create_user(Overrides) ->
    Password = maps:get(password, Overrides, <<"RestLogin123!">>),
    Suffix = unique_id(<<"u">>),
    Defaults = #{
        account => <<"rest-", Suffix/binary>>,
        email => <<"rest-", Suffix/binary, "@example.invalid">>,
        mobile => <<>>,
        nickname => <<"REST Fixture ", Suffix/binary>>,
        password => elib_password:generate(Password)
    },
    Data = maps:merge(Defaults, maps:remove(password, Overrides)),
    {ok, Uid} = user_repo:create(Data),
    Data#{uid => Uid, plain_password => Password}.

%% Run/case-scoped unique lowercase id (used for accounts, dids, group names
%% ...) so parallel or repeated runs never collide (plan G6 isolation).
-spec unique_id(binary()) -> binary().
unique_id(Prefix) ->
    Raw = os:getenv("REST_RUN_ID", "manual"),
    Sanitized = list_to_binary(lists:filter(fun alnum/1, Raw)),
    Padded = <<Sanitized/binary, "00000000000000000000">>,
    <<S:12/binary, _/binary>> = Padded,
    Rand = binary:encode_hex(crypto:strong_rand_bytes(4)),
    string:lowercase(<<Prefix/binary, S/binary, Rand/binary>>).

alnum(C) when C >= $a, C =< $z -> true;
alnum(C) when C >= $A, C =< $Z -> true;
alnum(C) when C >= $0, C =< $9 -> true;
alnum(_) -> false.

%% Deterministically flip a signature-real bit in the final base64url
%% character of a JWT: the character's 6-bit index is XORed with 16, which
%% is a real bit in the last 2-byte group of a 32-byte HS256 signature
%% (only bit weights 2 and 1 are padding there). XOR keeps the index inside
%% 0..63 for EVERY input character and keeps padding bits zero, so the
%% tampered token still decodes and the server answers signature-invalid
%% (706/401) instead of failing to decode — independent of what the last
%% character happens to be.
-spec flip_signature_bit(binary()) -> binary().
flip_signature_bit(Token) when byte_size(Token) > 1 ->
    Size = byte_size(Token) - 1,
    <<Head:Size/binary, Last>> = Token,
    Flipped = b64url_char(b64url_index(Last) bxor 16),
    <<Head/binary, Flipped:8>>.

b64url_index(C) when C >= $A, C =< $Z -> C - $A;
b64url_index(C) when C >= $a, C =< $z -> C - $a + 26;
b64url_index(C) when C >= $0, C =< $9 -> C - $0 + 52;
b64url_index($-) -> 62;
b64url_index($_) -> 63.

b64url_char(I) when I < 26 -> I + $A;
b64url_char(I) when I < 52 -> I - 26 + $a;
b64url_char(I) when I < 62 -> I - 52 + $0;
b64url_char(62) -> $-;
b64url_char(63) -> $_.

%% Device signature headers following auth_ds:verify_sign/2 exactly.
%% The signing key must be the one registered for these test headers via
%% app_version_ds:set_sign_key/4 (see api_v1_login_SUITE init_per_suite).
-define(REST_COS, <<"android">>).
-define(REST_VSN, <<"rest-golden">>).
-define(REST_PKG, <<"pub.imboy.rest">>).

-spec signed_headers(binary(), binary()) -> map().
signed_headers(Did, SignKey) ->
    Sign = elib_hasher:hmac_sha256(sign_plain(Did), SignKey),
    Base = base_headers(Did),
    Base#{<<"sign">> => Sign, <<"method">> => <<"sha256">>}.

-spec sign_plain(binary()) -> binary().
sign_plain(Did) ->
    <<Did/binary, "|", ?REST_VSN/binary, "|", ?REST_COS/binary, "|", ?REST_PKG/binary>>.

-spec base_headers(binary()) -> map().
base_headers(Did) ->
    #{
        <<"cos">> => ?REST_COS,
        <<"did">> => Did,
        <<"dname">> => <<"REST Golden Suite">>,
        <<"vsn">> => ?REST_VSN,
        <<"pkg">> => ?REST_PKG
    }.

%% Register a fresh per-run signing key once per suite (idempotent) and
%% return it, so domain suites do not roll their own key management.
%%
%% The config row is written with an explicit parameterized statement
%% mirroring config_ds:do_aes_encrypt/2 exactly (same pgcrypto SQL, key
%% passed as a bound parameter). config_ds:save/2 cannot be used here: its
%% update branch reuses the "$1" placeholder for both the first SET field
%% and the WHERE clause, so the row never matches and the write is
%% silently dropped. Reading stays on the production path
%% (config_ds:get -> pluck_decrypted_value), as do signing and
%% auth_ds:verify_sign/2.
-spec ensure_sign_key() -> binary().
ensure_sign_key() ->
    Key = <<"rest-", (binary:encode_hex(crypto:strong_rand_bytes(16)))/binary>>,
    ConfigKey =
        <<?REST_PKG/binary, "_", ?REST_COS/binary, "_", ?REST_VSN/binary>>,
    AesKey =
        case application:get_env(imboy, postgre_aes_key, undefined) of
            undefined -> erlang:error(postgre_aes_key_missing);
            AK when is_binary(AK) -> AK
        end,
    Plain = jsone:encode(Key, [native_utf8]),
    SeedSql =
        <<
            "INSERT INTO config (tab, key, value, title, remark) "
            "VALUES ('sys', $1, 'seed', '', '') "
            "ON CONFLICT (key) DO NOTHING"
        >>,
    case elib_pg:execute(SeedSql, [ConfigKey]) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sign_key_seed_failed, ConfigKey, Reason})
    end,
    UpdateSql =
        <<
            "UPDATE config SET value = 'aes_cbc_' || encode(encrypt(encode($1, "
            "'base64')::bytea, $2, 'aes-cbc/pad:pkcs'), 'base64') WHERE "
            "key = $3"
        >>,
    case elib_pg:execute(UpdateSql, [Plain, AesKey, ConfigKey]) of
        {ok, _} -> ok;
        {error, Reason2} -> erlang:error({sign_key_encrypt_failed, ConfigKey, Reason2})
    end,
    _ = imboy_cache:flush({config5, ConfigKey}),
    ok = imboy_cache:set({config5, ConfigKey}, Key, 864000),
    depcache:flush_process_dict(),
    ReadBack = app_version_ds:sign_key(?REST_COS, ?REST_VSN, ?REST_PKG),
    %% Fail INIT loudly rather than letting every signed request 902 later:
    %% an empty readback means the product config read chain (config_ds:get
    %% -> pluck_decrypted_value) cannot serve the freshly written key.
    case ReadBack of
        Key -> ok;
        _ -> erlang:error({sign_key_readback_mismatch, ConfigKey})
    end,
    persistent_term:put({?MODULE, sign_key}, Key),
    Key.

%% Log a fixture user in through the real POST /api/v1/passport/login and
%% return the map extended with token/refreshtoken/authorization headers.
-spec login(map(), binary()) -> map().
login(User, SignKey) ->
    Did = unique_id(<<"d">>),
    Body = #{
        <<"type">> => <<"account">>,
        <<"account">> => maps:get(account, User),
        <<"pwd">> => maps:get(plain_password, User),
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => Did
    },
    Headers = signed_headers(Did, SignKey),
    Resp = rest_client:post(login_port(), <<"/api/v1/passport/login">>, Body, Headers),
    #{<<"code">> := 0, <<"payload">> := Payload} = maps:get(body, Resp),
    Token = maps:get(<<"token">>, Payload),
    User#{
        did => Did,
        token => Token,
        refreshtoken => maps:get(<<"refreshtoken">>, Payload),
        authorization => <<"Bearer ", Token/binary>>
    }.

%% Bearer authorization header map for an already-logged-in fixture user.
-spec auth_header(map()) -> map().
auth_header(#{session_key := Key}) ->
    auth_header(session(Key));
auth_header(#{authorization := Auth}) ->
    #{<<"authorization">> => Auth}.

%% ---------------------------------------------------------------------------
%% Session store (review P1 fix): Common Test writes init_per_suite's return
%% value into every suite log page, so suite config must never carry
%% credential fields. Suites store the full login map here and place only
%% sanitize_user/2 output into the config; auth_header/1 rehydrates the
%% credentials through the session_key handle.
%% ---------------------------------------------------------------------------

-define(SESSION_SENSITIVE, [password, plain_password, token, refreshtoken, authorization]).

-spec store_session(atom(), map()) -> ok.
store_session(Key, UserMap) ->
    persistent_term:put({?MODULE, session, Key}, UserMap),
    ok.

-spec session(atom()) -> map().
session(Key) ->
    persistent_term:get({?MODULE, session, Key}).

%% Per-run device-sign key accessor (review round 2): the key lives in the
%% session store, NOT in suite config — CT logs init_per_suite's return
%% value on every suite log page, so the config must stay key-free.
-spec sign_key() -> binary().
sign_key() ->
    persistent_term:get({?MODULE, sign_key}).

%% Strip credential fields and attach the session handle used for
%% rehydration; the sanitized map is safe for CT to log as suite config.
-spec sanitize_user(map(), atom()) -> map().
sanitize_user(UserMap, Key) ->
    (maps:without(?SESSION_SENSITIVE, UserMap))#{session_key => Key}.

%% The login success path writes the user_device row through
%% gen_server:cast (user_server {login_success, ...}) only after the HTTP
%% answer is already on the wire, while both the refresh handler and the
%% JWT gate reject tokens whose device row is not active yet. Waiting on
%% the production predicate user_device_logic:is_active/2 synchronizes
%% the fixture without touching shared code. Cap: 100 x 50 ms = 5 s.
-spec await_device_active(integer(), binary()) -> ok.
await_device_active(Uid, Did) ->
    await_device_active(Uid, Did, 100).

await_device_active(_Uid, _Did, 0) ->
    erlang:error(device_row_not_visible);
await_device_active(Uid, Did, Attempts) ->
    case user_device_logic:is_active(Uid, Did) of
        true ->
            ok;
        false ->
            timer:sleep(50),
            await_device_active(Uid, Did, Attempts - 1)
    end.

%% Test-environment CAPACITY configuration, not a bypass of any behavior
%% under test (rate-limit semantics are not in this batch's tested
%% contract list): throttle_middleware gates POST /api/v1/passport/*
%% through the passport_per_ip scope via throttle:check/2 on every
%% request. The rule itself is registered once at app boot
%% (imboy_app:init_throttle_rates/0, default 10/min; the {throttle, rates}
%% env override is read only at boot), and the per-request check reads the
%% registered rule, not the env — so a post-boot application:set_env has
%% no effect on the live ceiling. Re-registering the scope with the same
%% primitive the product uses (throttle:setup/3; throttle_sup is
%% simple_one_for_one and the fresh child re-initializes the driver
%% limit) raises it. Domain suites that log in more than 6 fixture users
%% per run call this from init_per_suite so the shared 127.0.0.1 passport
%% bucket does not hand 429s to fixture logins.
%% Node-scope note: the raised ceiling lives on the shared ct_imboy VM for
%% its remaining lifetime — later suites or runs on the same node inherit it
%% until that VM restarts. Rate-limit semantics are not under test here.
-spec ensure_login_throttle_capacity() -> ok.
ensure_login_throttle_capacity() ->
    ok = throttle:setup(passport_per_ip, 300, per_minute).

login_port() ->
    ranch:get_port(imboy_listener).
