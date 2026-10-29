-module(attachment_storage_validate_registry_tests).

%%%===================================================================
%%% @doc F13：启动期注册表校验。空根 = 所有新上传 503，必须在启动日志里可见。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

validate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun missing_registry_is_empty/0,
        fun empty_roots_is_empty/0,
        fun wrong_version_is_treated_as_empty/0,
        fun any_root_is_ok/0,
        fun duplicate_namespace_is_ambiguous/0,
        fun duplicate_physical_root_is_rejected/0,
        fun unsupported_driver_is_rejected/0,
        fun incomplete_root_is_rejected/0
    ]}.

setup() ->
    Saved = application:get_env(imboy, attachment_storage_registry),
    application:unset_env(imboy, attachment_storage_registry),
    Saved.

cleanup({ok, Value}) -> application:set_env(imboy, attachment_storage_registry, Value);
cleanup(undefined) -> application:unset_env(imboy, attachment_storage_registry).

missing_registry_is_empty() ->
    ?assertEqual({error, empty_roots}, attachment_storage:validate_registry()).

empty_roots_is_empty() ->
    application:set_env(imboy, attachment_storage_registry, #{version => 1, roots => #{}}),
    ?assertEqual({error, empty_roots}, attachment_storage:validate_registry()).

%% 版本不匹配时 registry/0 归一为空 registry（fail-closed），校验必须同样报空。
wrong_version_is_treated_as_empty() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 99, roots => #{<<"r">> => #{driver => <<"garage">>}}
    }),
    ?assertEqual({error, empty_roots}, attachment_storage:validate_registry()).

any_root_is_ok() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"r">> => #{
                driver => <<"garage">>,
                namespace => <<"r">>,
                endpoint => <<"http://garage:3900">>,
                container => <<"imboy">>
            }
        }
    }),
    ?assertEqual(ok, attachment_storage:validate_registry()).

root(Driver, Ns, Endpoint, Container) ->
    #{driver => Driver, namespace => Ns, endpoint => Endpoint, container => Container}.

%% 同一 namespace/alias 属于两个根 = 未证明分离（namespace_contract）。
duplicate_namespace_is_ambiguous() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"a">> => root(<<"disk">>, <<"shared">>, <<"file:///tmp/a">>, <<"c">>),
            <<"b">> => (root(<<"disk">>, <<"other">>, <<"file:///tmp/b">>, <<"c">>))#{
                aliases => [<<"shared">>]
            }
        }
    }),
    ?assertEqual(
        {error, {ambiguous_namespace, <<"shared">>}},
        attachment_storage:validate_registry()
    ).

%% 两个根指向同一物理 locator = 同一对象两个 digest/锁域。
duplicate_physical_root_is_rejected() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"a">> => root(<<"garage">>, <<"ns-a">>, <<"http://garage:3900">>, <<"imboy">>),
            <<"b">> => root(<<"garage">>, <<"ns-b">>, <<"http://garage:3900">>, <<"imboy">>)
        }
    }),
    ?assertEqual(
        {error, {duplicate_physical_root, [<<"a">>, <<"b">>]}},
        attachment_storage:validate_registry()
    ).

unsupported_driver_is_rejected() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{
            <<"cos-root">> => root(<<"cos">>, <<"ns">>, <<"https://cos">>, <<"b">>)
        }
    }),
    ?assertEqual(
        {error, {unsupported_driver, <<"cos-root">>}},
        attachment_storage:validate_registry()
    ).

incomplete_root_is_rejected() ->
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        roots => #{<<"half">> => #{driver => <<"disk">>, namespace => <<"ns">>}}
    }),
    ?assertEqual(
        {error, {incomplete_root, <<"half">>}},
        attachment_storage:validate_registry()
    ).
