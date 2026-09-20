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
    ensure_ct_priv_alias/0,
    ensure_sign_key/0
]).

%% Must run first in init_per_suite: code:priv_dir(imboy) resolves by
%% matching a code-path segment named "<...>/imboy/ebin". The shared main
%% tree satisfies this because its checkout directory is named `imboy`; a
%% differently named worktree does not, so app start crashes with
%% {terminology_priv_dir_error, bad_name}. Build an alias subtree
%% .ct/appalias/imboy/{ebin,priv} (symlinks, untracked, runtime-only) and
%% add it to the front of the code path so the pattern matches again.
-spec ensure_ct_priv_alias() -> ok.
ensure_ct_priv_alias() ->
    case os:getenv("REST_PROJECT_ROOT") of
        false ->
            ok;
        Root ->
            CtDir = filename:join(Root, ".ct"),
            Alias = filename:join(filename:join(CtDir, "appalias"), "imboy"),
            Ebin = filename:join(Alias, "ebin"),
            Priv = filename:join(Alias, "priv"),
            ok = filelib:ensure_dir(filename:join(Alias, "placeholder")),
            ok = ensure_symlink(Ebin, filename:join(Root, "ebin")),
            ok = ensure_symlink(Priv, filename:join(Root, "priv")),
            true = code:add_patha(Ebin),
            %% erlang.mk flattens test builds: test/common/*.erl produces
            %% test/*.beam (test/common itself holds no beams). Ensure the
            %% test dir is on the path, then reload.
            true = code:add_patha(filename:join(Root, "test")),
            code:purge(eunit_runner),
            code:delete(eunit_runner),
            {module, eunit_runner} = code:ensure_loaded(eunit_runner),
            {module, inttest_marker_db} = code:ensure_loaded(inttest_marker_db),
            ok
    end.

ensure_symlink(Link, Target) ->
    case file:read_link_info(Link) of
        {ok, _} ->
            ok;
        _ ->
            %% file:make_symlink(Target, Link): the link is created at the
            %% second argument pointing at the first (verified on OTP 29).
            ok = file:make_symlink(Target, Link)
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
    %% depcache memoizes query results (including `undefined`) in the
    %% calling process dictionary; a stale undefined memo would shadow the
    %% fresh value. Clear it, then verify the production read path
    %% (app_version_ds:sign_key/3 -> config_ds:get -> depcache/DB) really
    %% serves this key before any case runs; fail INIT loudly otherwise.
    _ = imboy_cache:flush({config5, ConfigKey}),
    ok = imboy_cache:set({config5, ConfigKey}, Key, 864000),
    depcache:flush_process_dict(),
    ReadBack = app_version_ds:sign_key(?REST_COS, ?REST_VSN, ?REST_PKG),
    ct:pal(
        "SIGNKEY readback_len=~p (0 means the product config_ds read "
        "chain fails to serve the freshly written key in this node; see "
        "FINAL report finding F-RTF-READCHAIN)",
        [byte_size(ReadBack)]
    ),
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
auth_header(#{authorization := Auth}) ->
    #{<<"authorization">> => Auth}.

login_port() ->
    ranch:get_port(imboy_listener).
