-module(imboy_quality_cloud_phase0_tests).
-include_lib("eunit/include/eunit.hrl").

package_lock_override_is_explicit_and_fail_closed_test() ->
    Name = "IMBOY_QUALITY_PACKAGE_LOCK",
    Old = os:getenv(Name),
    try
        true = os:unsetenv(Name),
        ?assertEqual(
            "/Users/leeyi/project/imboy.pub/_task_packages/IMBOY_ERLANG_QUALITY/.package.lock",
            imboy_quality_full_budget:package_lock_path()
        ),
        Absolute = filename:join("/tmp", "imboy-quality-phase0-package.lock"),
        true = os:putenv(Name, Absolute),
        ?assertEqual(Absolute, imboy_quality_full_budget:package_lock_path()),
        true = os:putenv(Name, "relative-package.lock"),
        ?assertException(error, _, imboy_quality_full_budget:package_lock_path()),
        true = os:putenv(Name, ""),
        ?assertException(error, _, imboy_quality_full_budget:package_lock_path())
    after
        restore_env(Name, Old)
    end.

garage_binary_override_requires_executable_absolute_path_test() ->
    Name = "IMBOY_QUALITY_GARAGE_BIN",
    Old = os:getenv(Name),
    try
        true = os:putenv(Name, "/bin/sh"),
        ?assertEqual("/bin/sh", imboy_quality_resources:garage_executable()),
        true = os:putenv(Name, "garage"),
        ?assertException(error, _, imboy_quality_resources:garage_executable()),
        true = os:putenv(Name, "/no/such/quality/garage"),
        ?assertException(error, _, imboy_quality_resources:garage_executable()),
        true = os:putenv(Name, ""),
        ?assertException(error, _, imboy_quality_resources:garage_executable())
    after
        restore_env(Name, Old)
    end.

restore_env(Name, false) ->
    os:unsetenv(Name);
restore_env(Name, Value) ->
    os:putenv(Name, Value).
