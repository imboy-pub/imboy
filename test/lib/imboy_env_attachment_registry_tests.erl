-module(imboy_env_attachment_registry_tests).

%%%===================================================================
%%% @doc F13：IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE 显式派生附件存储根。
%%%
%%% 容器镜像的 sys.config 来自 sys.config.example（roots 为空、fail-closed），
%%% garage 又被 IMBOY_GARAGE_* 覆盖——静态样例根无法匹配运行时 endpoint，
%%% 全新部署的附件上传一律 503 upload_root_unavailable。派生只在“开关为真
%%% 且 registry 无根”时发生，显式 registry 永不被覆盖。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

-define(FLAG, "IMBOY_ATTACHMENT_REGISTRY_FROM_GARAGE").

derive_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun flag_unset_leaves_registry_untouched/0,
        fun flag_false_leaves_registry_untouched/0,
        fun derives_private_and_public_roots_when_empty/0,
        fun derived_roots_resolve_for_presign/0,
        fun explicit_registry_is_never_overwritten/0,
        fun garage_without_bucket_derives_nothing/0,
        fun unresolved_env_placeholder_derives_nothing/0,
        fun public_bucket_equal_to_bucket_yields_single_root/0,
        fun invalid_flag_value_is_rejected/0
    ]}.

setup() ->
    Saved = {
        application:get_env(imboy, attachment_storage_registry),
        application:get_env(imboy, garage)
    },
    os:unsetenv(?FLAG),
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{}, upload_namespaces => #{}
    }),
    application:set_env(imboy, garage, #{
        endpoint => <<"http://garage:3900">>,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy-public">>
    }),
    Saved.

cleanup({Registry, Garage}) ->
    os:unsetenv(?FLAG),
    restore(attachment_storage_registry, Registry),
    restore(garage, Garage).

restore(Key, {ok, Value}) -> application:set_env(imboy, Key, Value);
restore(Key, undefined) -> application:unset_env(imboy, Key).

registry() ->
    {ok, R} = application:get_env(imboy, attachment_storage_registry),
    R.

flag_unset_leaves_registry_untouched() ->
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(#{}, maps:get(roots, registry())).

flag_false_leaves_registry_untouched() ->
    os:putenv(?FLAG, "false"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(#{}, maps:get(roots, registry())).

derives_private_and_public_roots_when_empty() ->
    os:putenv(?FLAG, "true"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(
        #{
            version => 1,
            upload_namespaces => #{},
            roots => #{
                <<"garage-primary-private">> => #{
                    driver => <<"garage">>,
                    namespace => <<"garage-primary-private">>,
                    endpoint => <<"http://garage:3900">>,
                    container => <<"imboy">>
                },
                <<"garage-primary-public">> => #{
                    driver => <<"garage">>,
                    namespace => <<"garage-primary-public">>,
                    endpoint => <<"http://garage:3900">>,
                    container => <<"imboy-public">>
                }
            }
        },
        registry()
    ).

%% 派生出的根必须能被 presign 选根逻辑命中——这才是 F13 的实际修复目标。
derived_roots_resolve_for_presign() ->
    os:putenv(?FLAG, "1"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertMatch(
        {ok, #{namespace := <<"garage-primary-private">>, container := <<"imboy">>}},
        attachment_storage:registration_location(<<"private">>, <<"imboy">>, <<"k">>)
    ),
    ?assertMatch(
        {ok, #{namespace := <<"garage-primary-public">>, container := <<"imboy-public">>}},
        attachment_storage:registration_location(<<"public">>, <<"imboy-public">>, <<"k">>)
    ),
    ?assertEqual(ok, attachment_storage:validate_registry()).

explicit_registry_is_never_overwritten() ->
    Explicit = #{
        version => 1,
        upload_namespaces => #{},
        roots => #{
            <<"ops-root">> => #{
                driver => <<"garage">>,
                namespace => <<"ops-location-2026">>,
                endpoint => <<"http://other:3900">>,
                container => <<"ops">>
            }
        }
    },
    application:set_env(imboy, attachment_storage_registry, Explicit),
    os:putenv(?FLAG, "true"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(Explicit, registry()).

garage_without_bucket_derives_nothing() ->
    application:set_env(imboy, garage, #{endpoint => <<"http://garage:3900">>}),
    os:putenv(?FLAG, "true"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(#{}, maps:get(roots, registry())),
    ?assertEqual({error, empty_roots}, attachment_storage:validate_registry()).

%% `{env, Var}` 占位尚未解析成 binary 时绝不能派生：派生出的根永远匹配不上
%% elib_oss:endpoint/0，却会让 roots 非空、把启动 ERROR 吞掉。
unresolved_env_placeholder_derives_nothing() ->
    application:set_env(imboy, garage, #{
        endpoint => {env, <<"IMBOY_GARAGE_ENDPOINT">>},
        bucket => <<"imboy">>,
        public_bucket => <<"imboy-public">>
    }),
    os:putenv(?FLAG, "true"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual(#{}, maps:get(roots, registry())),
    ?assertEqual({error, empty_roots}, attachment_storage:validate_registry()).

public_bucket_equal_to_bucket_yields_single_root() ->
    application:set_env(imboy, garage, #{
        endpoint => <<"http://garage:3900">>, bucket => <<"imboy">>, public_bucket => <<"imboy">>
    }),
    os:putenv(?FLAG, "true"),
    ok = imboy_env:override_attachment_storage_registry(),
    ?assertEqual([<<"garage-primary-private">>], maps:keys(maps:get(roots, registry()))).

invalid_flag_value_is_rejected() ->
    os:putenv(?FLAG, "maybe"),
    ?assertError({invalid_env, ?FLAG, "maybe"}, imboy_env:override_attachment_storage_registry()),
    ?assertEqual(#{}, maps:get(roots, registry())).
