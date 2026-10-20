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
        fun any_root_is_ok/0
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
