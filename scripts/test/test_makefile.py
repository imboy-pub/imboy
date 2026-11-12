import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tarfile
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
MAKE = shutil.which("gmake") or shutil.which("make")


class MakefileTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="imboy-make-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        for name in ("Makefile", "erlang.mk", "include/deps.mk", "include/tpl.mk", "include/cli.mk", "include/quality.mk"):
            self.write(name, (ROOT / name).read_text())
        self.write("VERSION", "0.0.0\n")
        (self.root / ".erlang.mk").mkdir()
        self.write("config/sys.config.example", "[].\n")
        self.write("relx.config", '{release, {imboy, "0.0.0"}, [imboy]}.\n{extended_start_script, true}.\n')
        self.write("scripts/mod_check.sh", 'echo "audit:$1"\nexit "${AUDIT_EXIT:-0}"\n')
        self.write("scripts/quality.escript", '''#!/usr/bin/env escript
main(Args) ->
    {ok, F} = file:open("quality.calls", [append]),
    io:format(F, "~p~n", [Args]), file:close(F), halt(0).
''')
        # The quality-full Make recipe invokes this wrapper in its temporary repo.
        self.write("scripts/quality_timed.sh", (ROOT / "scripts/quality_timed.sh").read_text())
        self.write("probe.mk", '''.PHONY: probe
probe:
	@printf 'route=[%s] relx=[%s] sys=[%s] default=[%s] mods=[%s]\\n' '$(IMBOY_EUNIT_ROUTE)' '$(RELX_CONFIG)' '$(_SYS_RUNTIME_SRC)' '$(.DEFAULT_GOAL)' '$(EUNIT_MODS)'
''')
        self.environment = {
            name: value for name, value in os.environ.items()
            if name in ("PATH", "HOME", "TMPDIR", "LANG")
        }
        self.options = ["SKIP_DEPS=1", "DEPS=", "BUILD_DEPS=relx", "TEST_DEPS=", "DOC_DEPS=",
                        "LOCAL_DEPS=", "ERLC_COMPILE_OPTS=", "ERLC_OPTS=+debug_info",
                        "TEST_ERLC_OPTS=+debug_info -DTEST=1", "PROJECT_MOD="]

    def write(self, name, content):
        destination = self.root / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(content)
        return destination

    def make(self, *arguments, success=True, executable=MAKE):
        result = subprocess.run(
            [executable, "--no-print-directory", "-f", "Makefile", "-f", "probe.mk", *self.options, *arguments],
            cwd=self.root, env=self.environment, text=True, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, timeout=60,
        )
        if success:
            self.assertEqual(result.returncode, 0, result.stdout)
            self.assertNotIn("Runtime terminating", result.stdout)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
        return result

    def pure_tests(self):
        self.write("test/make_probe_tests.erl", '''-module(make_probe_tests).
-include_lib("eunit/include/eunit.hrl").
passes_test() -> ?assertEqual(2, 1 + 1).
generator_test_() -> ?_assertEqual(ok, ok).
''')
        self.write("test/talk_record_owned_eunit_fixture.erl", '''-module(talk_record_owned_eunit_fixture).
-export([entry/1]).
entry(Module) -> Module.
''')

    def release_probe(self):
        self.write("src/make_probe.erl", "-module(make_probe).\n-export([value/0]).\nvalue() -> ok.\n")
        with (self.root / "probe.mk").open("a") as stream:
            stream.write('''
define relx_release.erl
    {ok, _} = file:read_file("config/sys.runtime.config"),
    true = filelib:is_file("ebin/imboy.app"),
    ok = file:write_file("release.called", <<"ok">>), halt(0).
endef
define relx_tar.erl
    halt(0).
endef
''')
        executable = self.write("_rel/imboy/bin/imboy", '''#!/usr/bin/env python3
import json, os
from pathlib import Path
Path("run.env").write_text(json.dumps({name: os.environ.get(name) for name in
    ["BIGMODEL_API_KEY", "ARK_API_KEY", "BAILIAN_API_KEY"]}))
''')
        executable.chmod(0o755)

    def test_configuration_selection(self):
        self.write("config/sys..config", "unused when IMBOYENV is unset\n")
        for environment in ("", "local", "dev", "pro"):
            with self.subTest(environment=environment):
                result = self.make("probe", "IMBOYENV=" + environment)
                self.assertIn("relx=[" + str(self.root.resolve() / "relx.config") + "]", result.stdout)
                self.assertIn("sys=[config/sys.config.example]", result.stdout)
                if environment:
                    relx = self.write("relx" + environment + ".config", "[].\n")
                    config = self.write("config/sys." + environment + ".config", "[].\n")
                    result = self.make("probe", "IMBOYENV=" + environment)
                    self.assertIn("/relx" + environment + ".config]", result.stdout)
                    self.assertIn("sys=[config/sys." + environment + ".config]", result.stdout)
                    relx.unlink()
                    config.unlink()
        self.write("config/sys.config", "[].\n")
        self.assertIn("sys=[config/sys.config]", self.make("probe").stdout)
        self.assertIn("default=[test-build]", self.make("probe").stdout)

    def hex_fixture(self):
        # Exercise erlang.mk's actual eval-generated fetch/extract recipe offline.
        # Only the Hex transport is replaced; no shared deps/cache or network I/O.
        self.write(".erlang.mk/dep_built/hex_core", "offline transport fixture\n")
        self.write("fixtures/hex_core.erl", '''-module(hex_core).
-export([default_config/0]).
default_config() -> #{offline_fixture => true}.
''')
        self.write("fixtures/hex_repo.erl", '''-module(hex_repo).
-export([get_tarball/3]).
get_tarball(#{offline_fixture := true} = Config, <<"fixture">>, <<"1.0.0">>) ->
    ok = file:write_file("hex.request", <<"fixture 1.0.0">>),
    case maps:get(response_status, Config, 200) of
        200 ->
            {ok, Tarball} = file:read_file("fixtures/package.tar"),
            {ok, {200, [], Tarball}};
        Status -> {ok, {Status, [], <<"offline fixture failure">>}}
    end.
''')
        subprocess.run(
            ["erlc", "-Werror", "-o", "fixtures", "fixtures/hex_core.erl", "fixtures/hex_repo.erl"],
            cwd=self.root, env=self.environment, check=True, capture_output=True, timeout=20,
        )
        payload = b"verified local Hex payload\n"
        contents = io.BytesIO()
        with tarfile.open(fileobj=contents, mode="w:gz") as archive:
            member = tarfile.TarInfo("payload.txt")
            member.size = len(payload)
            archive.addfile(member, io.BytesIO(payload))
        with tarfile.open(self.root / "fixtures/package.tar", mode="w") as archive:
            member = tarfile.TarInfo("contents.tar.gz")
            member.size = len(contents.getvalue())
            archive.addfile(member, io.BytesIO(contents.getvalue()))
        return (
            str(self.root / "deps/fixture"), "DEPS=fixture", "dep_fixture=hex 1.0.0",
            "NO_AUTOPATCH=fixture", "ERL=erl -noshell -pa fixtures",
            "CACHE_DIR=" + str(self.root / "cache"),
        ), payload

    def test_hex_fetch_runs_before_extraction_with_and_without_cache(self):
        arguments, payload = self.hex_fixture()
        for cached in (0, 1):
            with self.subTest(cached=cached):
                shutil.rmtree(self.root / "deps/fixture", ignore_errors=True)
                self.make(*arguments, "CACHE_DEPS=" + str(cached))
                self.assertEqual((self.root / "hex.request").read_text(), "fixture 1.0.0")
                self.assertEqual((self.root / "deps/fixture/payload.txt").read_bytes(), payload)
                (self.root / "hex.request").unlink()
                shutil.rmtree(self.root / "deps/fixture")
                if cached:
                    # A populated cache must work when the transport would fail.
                    self.make(*arguments, "CACHE_DEPS=1", "HEX_CONFIG=#{response_status => 503}")
                    self.assertFalse((self.root / "hex.request").exists())
                    self.assertEqual((self.root / "deps/fixture/payload.txt").read_bytes(), payload)

    def test_hex_download_and_archive_failures_remain_failures(self):
        arguments, _ = self.hex_fixture()
        for cached in (0, 1):
            for failure in ("transport", "archive"):
                with self.subTest(cached=cached, failure=failure):
                    if failure == "archive":
                        (self.root / "fixtures/package.tar").write_bytes(b"invalid archive")
                    status = 503 if failure == "transport" else 200
                    result = self.make(
                        *arguments, "CACHE_DEPS=" + str(cached),
                        "HEX_CONFIG=#{response_status => " + str(status) + "}", success=False,
                    )
                    self.assertEqual((self.root / "hex.request").read_text(), "fixture 1.0.0")
                    if failure == "transport":
                        self.assertIn("Error 503", result.stdout)
                    self.assertFalse((self.root / "deps/fixture/payload.txt").exists())
                    (self.root / "hex.request").unlink()
                    shutil.rmtree(self.root / "deps/fixture")

    def test_audit_and_help_have_no_release_side_effects(self):
        self.write("relxlocal.config", "this is invalid Erlang\n")
        self.write(".env.local", "BIGMODEL_API_KEY=$(touch SECRET_READ)\n")
        erlang_probe = self.write("bin/erl-probe", '#!/bin/sh\nprintf called >> erlang.calls\nexit 93\n')
        erlang_probe.chmod(0o755)
        for executable in dict.fromkeys([MAKE, shutil.which("make")]):
            for arguments in (("help",), ("mod-check",), ("-j4", "help", "mod-check")):
                with self.subTest(make=executable, arguments=arguments):
                    self.make(*arguments, "IMBOYENV=local", "ERL=bin/erl-probe", executable=executable)
                    self.assertFalse((self.root / "config/sys.runtime.config").exists())
                    self.assertFalse((self.root / "deps").exists())
                    self.assertFalse((self.root / "SECRET_READ").exists())
                    self.assertFalse((self.root / "erlang.calls").exists())
        result = self.make("mod-check", "_RELX_SRC=$(if true, relxlocal.config)")
        self.assertIn("audit:check", result.stdout)
        self.environment["AUDIT_EXIT"] = "7"
        self.assertIn("Error 7", self.make("mod-check", success=False).stdout)

    def test_feature_smoke_preserves_arguments_without_evaluation_or_secret_output(self):
        self.write("scripts/run_feature_flag_smoke.sh", r'''for argument; do printf '%s\0' "$argument"; done > feature.args
printf '%s\0' "$IMBOY_FEATURE_BASE_URL" "$IMBOY_FEATURE_PUBLIC_PATH" \
    "$IMBOY_FEATURE_ADMIN_PATH" "$IMBOY_FEATURE_TIMEOUT" > feature.env
''')
        values = {
            "FEATURE_SMOKE_BASE_URL": "http://127.0.0.1:9800",
            "FEATURE_SMOKE_PUBLIC_PATH": "/custom/features",
            "FEATURE_SMOKE_ADMIN_PATH": "/custom/admin/features",
            "FEATURE_SMOKE_TIMEOUT": "7",
            "FEATURE_SMOKE_ADMIN_HEADER": '''authorization: Bearer a'b"c;$(touch COMMAND_EXECUTED)''',
            "FEATURE_SMOKE_FORBIDDEN_HEADER": "authorization: Bearer limited token",
            "FEATURE_SMOKE_EXPECTS": "core=true moment=false",
            "FEATURE_SMOKE_INSECURE": "1",
            "FEATURE_SMOKE_SHOW_BODY": "1",
        }
        arguments = [name + "=" + value.replace("$", "$$") for name, value in values.items()]
        result = self.make("feature-smoke", *arguments)
        expected = ["--insecure", "--show-body", "--admin-header", values["FEATURE_SMOKE_ADMIN_HEADER"],
                    "--forbidden-header", values["FEATURE_SMOKE_FORBIDDEN_HEADER"],
                    "--expect", "core=true", "--expect", "moment=false"]
        self.assertEqual((self.root / "feature.args").read_bytes().split(b"\0")[:-1],
                         [value.encode() for value in expected])
        self.assertEqual((self.root / "feature.env").read_bytes().split(b"\0")[:-1],
                         [values[name].encode() for name in list(values)[:4]])
        self.assertFalse((self.root / "COMMAND_EXECUTED").exists())
        self.assertNotIn("authorization:", result.stdout)

    def test_feature_smoke_defaults_missing_url_and_failure_propagation(self):
        self.write("scripts/run_feature_flag_smoke.sh", (ROOT / "scripts/run_feature_flag_smoke.sh").read_text())
        for base_url in ("", " \t "):
            with self.subTest(base_url=base_url):
                result = self.make("feature-smoke", "FEATURE_SMOKE_BASE_URL=" + base_url, success=False)
                self.assertIn("FEATURE_SMOKE_BASE_URL is required.", result.stdout)
        self.write("scripts/run_feature_flag_smoke.sh", r'''for argument; do printf '%s\0' "$argument"; done > feature.args
printf '%s\0' "$IMBOY_FEATURE_PUBLIC_PATH" "$IMBOY_FEATURE_ADMIN_PATH" \
    "$IMBOY_FEATURE_TIMEOUT" > feature.env
exit "${SMOKE_EXIT:-0}"
''')
        arguments = ("feature-smoke", "FEATURE_SMOKE_BASE_URL=http://127.0.0.1:9800")
        self.make(*arguments)
        self.assertEqual((self.root / "feature.args").read_bytes(), b"")
        self.assertEqual((self.root / "feature.env").read_bytes(),
                         b"/v1/app/features\0/adm/admin/config/features\x0015\0")
        self.environment["SMOKE_EXIT"] = "7"
        self.assertIn("Error 7", self.make(*arguments, success=False).stdout)

    def test_release_rejects_missing_invalid_and_whitespace_paths(self):
        for target in ("rel", "run", "relx-rel", "relup", "relx-relup"):
            with self.subTest(target=target):
                result = self.make(target, "RELX_CONFIG=missing.config", success=False)
                self.assertIn("Missing relx configuration", result.stdout)
        self.write("relxlocal.config", "[].\n")
        self.assertIn("Invalid relx", self.make("rel", "IMBOYENV=local", success=False).stdout)
        result = self.make("rel", "_RELX_SRC=$(if true, relxlocal.config)", success=False)
        self.assertIn("/ relxlocal.config]", result.stdout)
        self.write("relx.config", "invalid Erlang\n")
        self.make("rel", success=False)

    def test_release_prerequisites_and_failure_propagation(self):
        self.release_probe()
        self.make("-j4", "help", "rel", "RELX_TAR=0")
        self.assertEqual((self.root / "config/sys.runtime.config").read_text(), "[].\n")
        self.assertTrue((self.root / "release.called").exists())
        (self.root / "release.called").unlink()
        (self.root / "config/sys.config.example").unlink()
        self.make("rel", success=False)
        self.assertFalse((self.root / "release.called").exists())
        self.write("config/sys.config.example", "[].\n")
        self.write("src/broken.erl", "invalid Erlang\n")
        self.make("rel", success=False)
        self.assertFalse((self.root / "release.called").exists())

    def test_run_loads_literal_llm_values_with_existing_precedence(self):
        self.release_probe()
        literal = "fake$cash#hash with spaces"
        self.environment["BIGMODEL_API_KEY"] = "inherited"
        self.environment["ARK_API_KEY"] = literal
        self.write(".env.local", "BIGMODEL_API_KEY=" + literal + "\nBAILIAN_API_KEY=\n")
        self.write(".env", "BIGMODEL_API_KEY=fallback\nBAILIAN_API_KEY=" + literal + "\n")
        self.make("run")
        values = json.loads((self.root / "run.env").read_text())
        self.assertEqual(list(values.values()), [literal, literal, literal])
        self.make("run", "BIGMODEL_API_KEY=command$$literal# with spaces")
        self.assertEqual(json.loads((self.root / "run.env").read_text())["BIGMODEL_API_KEY"],
                         "command$literal# with spaces")

    def test_routes_distinguish_undefined_empty_and_inner(self):
        for environment in ("", "local", "dev", "pro"):
            for variable in (None, "t", "EUNIT_CONFIG", "EUNIT_MODS", "EUNIT_USE_RELAY"):
                for inner in ("", "1"):
                    with self.subTest(environment=environment, variable=variable, inner=inner):
                        arguments = ["probe", "IMBOYENV=" + environment, "IMBOY_EUNIT_INNER=" + inner]
                        if variable:
                            arguments.append(variable + "=")
                        route = "native" if inner else "legacy" if variable else "quality_full"
                        self.assertIn("route=[" + route + "]", self.make(*arguments).stdout)

    def test_quality_aliases_do_not_run_native_tests(self):
        self.write("src/broken.erl", "invalid Erlang\n")
        for target in ("eunit", "eunit-local", "eunit-all", "quality-full", "quality-group"):
            with self.subTest(target=target):
                self.make(target, "IMBOYENV=local", "g=make_probe_tests")
                self.assertFalse((self.root / "ebin/broken.beam").exists())
        calls = (self.root / "quality.calls").read_text()
        self.assertEqual(calls.count('["eunit","--full"]'), 4)
        self.assertEqual(calls.count('["eunit","--group=make_probe_tests"]'), 1)
        self.make("-j4", "eunit", "eunit-local", "eunit-all", "quality-full", "IMBOYENV=local")
        self.assertEqual((self.root / "quality.calls").read_text().count('["eunit","--full"]'), 5)

    def test_native_module_function_and_test_failure(self):
        self.pure_tests()
        for environment in ("", "local"):
            for selection in ("make_probe_tests", "make_probe_tests:generator_test_"):
                self.assertIn("passed", self.make("eunit", "IMBOYENV=" + environment, "t=" + selection).stdout)
        self.environment["EUNIT_MODS"] = "make_probe_tests"
        self.assertIn("mods=[make_probe_tests]", self.make("probe").stdout)
        self.make("eunit", "IMBOY_EUNIT_INNER=1", "EUNIT_MODS=make_probe_tests")
        self.make("eunit", "EUNIT_MODS=", success=False)
        self.write("test/make_probe_tests.erl", '''-module(make_probe_tests).
-include_lib("eunit/include/eunit.hrl").
fails_test() -> ?assert(false).
''')
        self.make("eunit", "t=make_probe_tests", success=False)

    def test_legacy_aliases_forward_parameters_and_missing_config_fails(self):
        self.pure_tests()
        self.write("config/sys.local.config", "[].\n")
        for target in ("eunit-local", "eunit-legacy"):
            self.make(target, "EUNIT_USE_RELAY=0", "EUNIT_MODS=make_probe_tests")
            self.assertFalse(list((self.root / "config").glob("sys.eunit-relay.*.config")))
        (self.root / "config/sys.local.config").unlink()
        self.make("eunit-local", "EUNIT_USE_RELAY=0", success=False)

    def test_beam_recovery_test_mode_and_recursive_exclusions(self):
        self.write("src/make_probe.erl", '''-module(make_probe).
-export([mode/0]).
-ifdef(TEST).
mode() -> test.
-else.
mode() -> app.
-endif.
''')
        self.write("src/deep/nested/make_feature.erl", "-module(make_feature).\n-export([value/0]).\nvalue() -> ok.\n")
        for target, expected in (("compile", "app"), ("test-build", "test"), ("compile", "app")):
            self.make(target)
            result = subprocess.run(
                ["erl", "-noshell", "-pa", "ebin", "-eval",
                 "case make_probe:mode() of " + expected + " -> halt(0); _ -> halt(1) end."],
                cwd=self.root, env=self.environment, capture_output=True, timeout=10,
            )
            self.assertEqual(result.returncode, 0, result.stdout)
        self.make("imboy-prune-excluded-beams", "ERLC_EXCLUDE=make_feature")
        self.assertFalse((self.root / "ebin/make_feature.beam").exists())
        self.assertNotIn("make_feature", (self.root / "ebin/imboy.app").read_text())
        self.make("test-build", "ERLC_EXCLUDE=make_feature")
        self.assertTrue((self.root / ".erlang.mk/feature-test-ebin/make_feature.beam").exists())
        self.assertFalse((self.root / "ebin/make_feature.beam").exists())
        self.make("test-build")
        self.assertTrue((self.root / "ebin/make_feature.beam").exists())
        (self.root / "ebin/make_probe.beam").unlink()
        self.assertIn("beam missing", self.make("test-build").stdout)
        self.assertTrue((self.root / "ebin/make_probe.beam").exists())

    def test_batch_compilation_failure_does_not_seal_stamp(self):
        self.write("test/broken_tests.erl", "invalid Erlang\n")
        result = self.make("test-build", success=False)
        self.assertIn("Error", result.stdout)
        self.assertFalse((self.root / ".erlang.mk/imboy.last-testdir-build").exists())

    def test_retired_commands_and_unused_variables_are_absent(self):
        database = self.make("-np", "probe").stdout
        self.assertNotRegex(database, r"(?m)^(?:efmt|clean-beam|clear_beam):")
        self.assertNotRegex(database, r"(?m)^(?:APP_VERSION|DIALYZER_WARNINGS)\s*[:?+]?=")

    def test_proto_generation_preserves_native_format_and_propagates_failure(self):
        self.write("fixtures/gpb_compile.erl", '''-module(gpb_compile).
-export([file/2]).
file(Source, Options) ->
    case file:read_file(Source) of
        {ok, <<"invalid">>} -> {error, invalid_proto};
        {ok, _} ->
            Name = filename:basename(Source, ".proto"),
            ExpectedPackages = Name =:= "textguard",
            ExpectedPackages = lists:member(use_packages, Options),
            file:write_file("src/" ++ Name ++ "_pb.erl",
                ["%% -*- coding: utf-8 -*-\\n-module(", Name, "_pb).\\n",
                 "-export([value/0]).\\nvalue()->native.\\n"])
    end.
''')
        subprocess.run(["erlc", "-Werror", "-o", "fixtures", "fixtures/gpb_compile.erl"],
                       cwd=self.root, env=self.environment, check=True, capture_output=True)
        self.write("src/imboy.proto", "valid")
        self.write("src/textguard.proto", "valid")
        self.write("probe.mk", '''.PHONY: proto-probe
proto-probe:
	@$(call erlang,$(call compile_proto.erl,src/imboy.proto src/textguard.proto))
''')
        arguments = ("proto-probe", "ERL=erl -noshell -pa fixtures")
        self.make(*arguments)
        first = {}
        for name in ("imboy", "textguard"):
            output = self.root / "src" / (name + "_pb.erl")
            first[name] = output.read_bytes()
            self.assertTrue(first[name].startswith(b"%% @noformat\n%% -*- coding: utf-8 -*-\n"))
            self.assertIn(b"value()->native.", first[name])
        self.make(*arguments)
        for name, expected in first.items():
            self.assertEqual((self.root / "src" / (name + "_pb.erl")).read_bytes(), expected)
        self.write("src/textguard.proto", "invalid")
        result = self.make(*arguments, success=False)
        self.assertIn("invalid_proto", result.stdout)

    def test_native_clean_preserves_dependency_beams(self):
        self.write("src/make_probe.erl", "-module(make_probe).\n")
        products = ("ebin/make_probe.beam", "test/make_probe_tests.beam",
                    ".erlang.mk/beam-cache/imboy/ebin-app/make_probe.beam")
        for name in products:
            self.write(name, "synthetic build product\n")
        dependency = self.write("deps/fixture/ebin/dependency.beam", "synthetic dependency\n")
        self.make("clean")
        for name in products:
            self.assertFalse((self.root / name).exists(), name)
        self.assertEqual(dependency.read_text(), "synthetic dependency\n")

    def test_quality_include_invalidates_native_build_stamps(self):
        self.write("src/make_probe.erl", "-module(make_probe).\n-export([value/0]).\nvalue() -> ok.\n")
        self.pure_tests()
        database = self.make("-np", "probe").stdout
        for stamp in ("last-makefile-change", "imboy.last-testdir-build"):
            with self.subTest(stamp=stamp):
                rule = next(line for line in database.splitlines()
                            if line.partition(":")[0].endswith(".erlang.mk/" + stamp))
                self.assertIn("include/quality.mk", rule)

    def test_eqwalize_all_preserves_tool_failures_and_diagnostics(self):
        executable = self.write("bin/elp", '''#!/bin/sh
printf '%s\\n' "$ELP_OUTPUT"
exit "$ELP_EXIT"
''')
        executable.chmod(0o755)
        for exit_code, output, success in ((0, "checked", True),
                                          (0, "error: invalid type", False),
                                          (7, "tool crashed", False)):
            with self.subTest(exit_code=exit_code, output=output):
                self.environment.update(ELP_EXIT=str(exit_code), ELP_OUTPUT=output)
                result = self.make("eqwalize-all", "ELP=bin/elp", success=success)
                self.assertIn(output, result.stdout)
                self.assertEqual((self.root / ".elp/eqwalize-all.log").read_text(), output + "\n")


if __name__ == "__main__":
    unittest.main()
