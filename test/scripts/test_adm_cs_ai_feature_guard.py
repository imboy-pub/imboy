import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "src/adm/adm_cs_ai_handler.erl"


class AdminAiFeatureGuardTest(unittest.TestCase):
    def check_profile(self, enabled):
        with tempfile.TemporaryDirectory() as directory:
            include = Path(directory)
            (include / "generated").mkdir()
            (include / "generated/imboy_product_features.hrl").write_text(
                "-define(IMBOY_FEATURE_CUSTOMER_SERVICE, true).\n" if enabled else ""
            )
            expression = (
                f"{{ok, Forms}} = epp:parse_file({json.dumps(str(SOURCE))},"
                f"[{json.dumps(str(include))}, \"include\"], []),"
                "[] = [Error || {error, Error} <- Forms],"
                "Names = [Name || {function, _, Name, _, _} <- Forms],"
                f"{str(enabled).lower()} = lists:member(config_view_action, Names),"
                f"{str(enabled).lower()} = lists:member(config_set_params, Names),"
                "{ok, adm_cs_ai_handler, Beam, _} = compile:forms(Forms,"
                "[binary, return_errors, return_warnings]),"
                "{module, adm_cs_ai_handler} = code:load_binary(adm_cs_ai_handler, \"fixture\", Beam),"
                "ok = meck:new(cowboy_req, [non_strict]),"
                "ok = meck:expect(cowboy_req, method, fun(_) -> <<\"GET\">> end),"
                "ok = meck:new(elib_response, [non_strict]),"
                "ok = meck:expect(elib_response, error, fun(_, _, Code) -> {response, Code} end),"
                "ok = meck:new(adm_acl, [non_strict]),"
                "ok = meck:expect(adm_acl, ensure_permission,"
                "fun(_, <<\"customer_service:read\">>, _) -> {error, {response, 403}} end),"
                f"{{ok, {{response, {403 if enabled else 404}}}, #{{}}}} = "
                "adm_cs_ai_handler:init(#{}, #{action => config_view}), halt(0)."
            )
            result = subprocess.run(
                ["erl", "-noshell", "-pa", "ebin", "deps/meck/ebin", "-eval", expression],
                cwd=ROOT, capture_output=True, text=True, timeout=30,
                env={**os.environ, "ERL_CRASH_DUMP": str(include / "erl_crash.dump")}
            )
            self.assertEqual(0, result.returncode, result.stdout + result.stderr)

    def test_selected_profile_compiles_actions(self):
        self.check_profile(True)

    def test_unselected_profile_trims_actions_and_helpers(self):
        self.check_profile(False)


if __name__ == "__main__":
    unittest.main()
