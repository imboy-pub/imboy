%% attachment registry 启动校验单测（2026-10-20 prod 事故加固）：
%% strict 环境空 registry/endpoint 不一致 → 拒启；attachment_registry_optional
%% 显式豁免；非 strict 环境维持告警不阻断（F13 原义）。
-module(imboy_app_registry_validate_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Saved =
        {
            os:getenv("IMBOYENV"),
            application:get_env(imboy, attachment_storage_registry),
            application:get_env(imboy, garage),
            application:get_env(imboy, attachment_registry_optional)
        },
    Saved.

cleanup({EnvVar, Registry, Garage, Optional}) ->
    restore_env_var("IMBOYENV", EnvVar),
    restore_app_env(imboy, attachment_storage_registry, Registry),
    restore_app_env(imboy, garage, Garage),
    restore_app_env(imboy, attachment_registry_optional, Optional).

restore_env_var(_Name, false) ->
    ok;
restore_env_var(Name, Value) ->
    os:putenv(Name, Value).

restore_app_env(App, Key, undefined) ->
    application:unset_env(App, Key);
restore_app_env(App, Key, {ok, Value}) ->
    application:set_env(App, Key, Value).

registry_test_() ->
    {
        foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun strict_empty_roots_refuses_to_start/0,
            fun strict_empty_roots_optional_waiver_starts/0,
            fun dev_empty_roots_warns_only/0,
            fun strict_consistent_registry_passes/0,
            fun strict_endpoint_mismatch_refuses_to_start/0,
            fun dev_endpoint_mismatch_warns_only/0
        ]
    }.

strict_empty_roots_refuses_to_start() ->
    os:putenv("IMBOYENV", "prod"),
    application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
    ?assertError(attachment_registry_empty, imboy_app:validate_attachment_registry()).

strict_empty_roots_optional_waiver_starts() ->
    os:putenv("IMBOYENV", "prod"),
    application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
    application:set_env(imboy, attachment_registry_optional, true),
    ?assertEqual(ok, imboy_app:validate_attachment_registry()).

dev_empty_roots_warns_only() ->
    os:putenv("IMBOYENV", "dev"),
    application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
    ?assertEqual(ok, imboy_app:validate_attachment_registry()).

strict_consistent_registry_passes() ->
    os:putenv("IMBOYENV", "prod"),
    application:set_env(imboy, garage, #{endpoint => <<"http://garage:3900">>}),
    application:set_env(
        imboy,
        attachment_storage_registry,
        #{
            version => 1,
            roots => #{
                <<"prod-main">> => #{
                    driver => <<"garage">>,
                    namespace => <<"prod-main">>,
                    endpoint => <<"http://garage:3900">>,
                    container => <<"imboy">>
                }
            },
            upload_namespaces => #{}
        }
    ),
    ?assertEqual(ok, imboy_app:validate_attachment_registry()).

strict_endpoint_mismatch_refuses_to_start() ->
    os:putenv("IMBOYENV", "prod"),
    application:set_env(imboy, garage, #{endpoint => <<"http://garage:3900">>}),
    application:set_env(
        imboy,
        attachment_storage_registry,
        #{
            version => 1,
            roots => #{
                <<"prod-main">> => #{
                    driver => <<"garage">>,
                    namespace => <<"prod-main">>,
                    %% 与 garage 配置不一致：历史附件 location_changed +
                    %% 新上传选根失败，必须在启动期暴露
                    endpoint => <<"http://wrong-host:3900">>,
                    container => <<"imboy">>
                }
            }
        }
    ),
    ?assertError(
        {attachment_registry_endpoint_mismatch, [<<"prod-main">>]},
        imboy_app:validate_attachment_registry()
    ).

dev_endpoint_mismatch_warns_only() ->
    os:putenv("IMBOYENV", "dev"),
    application:set_env(imboy, garage, #{endpoint => <<"http://garage:3900">>}),
    application:set_env(
        imboy,
        attachment_storage_registry,
        #{
            version => 1,
            roots => #{
                <<"prod-main">> => #{
                    driver => <<"garage">>,
                    namespace => <<"prod-main">>,
                    endpoint => <<"http://wrong-host:3900">>,
                    container => <<"imboy">>
                }
            }
        }
    ),
    ?assertEqual(ok, imboy_app:validate_attachment_registry()).
