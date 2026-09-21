import importlib.util
import re
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/generate_product_features.py"
SPEC = importlib.util.spec_from_file_location("product_features", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ProductFeatureManifestTest(unittest.TestCase):
    catalog = {
        "features": ["core", "e2ee", "channel", "channel_order"],
        "dependencies": {"channel_order": ["channel"]},
    }

    def manifest(self, selected=None, **changes):
        value = {
            "schema_version": 1,
            "product_id": "imboy",
            "profile": "test",
            "base_ref": "imboy-feature-inventory-v1",
            "selected_features": selected or [],
        }
        value.update(changes)
        return value

    def test_base_only_and_selected(self):
        base = MODULE.validate(self.manifest(), self.catalog)
        full = MODULE.validate(self.manifest(["channel", "channel_order", "e2ee"]), self.catalog)
        self.assertEqual(["core"], base["compiled_features"])
        self.assertEqual(["channel", "channel_order", "core", "e2ee"], full["compiled_features"])

    def test_required_profile_rejects_non_full_test_builds(self):
        MODULE.require_profile(self.manifest(profile="full-selected"), "full-selected")
        with self.assertRaisesRegex(MODULE.ManifestError, "profile must be full-selected"):
            MODULE.require_profile(self.manifest(profile="base-only"), "full-selected")

    def test_unknown_duplicate_missing_dependency_and_base_disable(self):
        cases = [
            self.manifest(["unknown"]),
            self.manifest(["channel", "channel"]),
            self.manifest(["channel_order"]),
            self.manifest(disabled_base_features=["core"]),
        ]
        for case in cases:
            with self.subTest(case=case), self.assertRaises(MODULE.ManifestError):
                MODULE.validate(case, self.catalog)

    def test_invalid_schema_and_cycle(self):
        with self.assertRaises(MODULE.ManifestError):
            MODULE.validate(self.manifest(schema_version=2), self.catalog)
        cyclic = {"features": ["core", "a", "b"], "dependencies": {"a": ["b"], "b": ["a"]}}
        with self.assertRaisesRegex(MODULE.ManifestError, "cyclic"):
            MODULE.validate(self.manifest(["a", "b"]), cyclic)

    def test_duplicate_json_field_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "manifest.json"
            path.write_text('{"schema_version":1,"schema_version":1}')
            with self.assertRaisesRegex(MODULE.ManifestError, "duplicate field"):
                MODULE.read_manifest(path)

    def test_hash_and_render_are_deterministic(self):
        first = MODULE.validate(self.manifest(["e2ee", "channel"]), self.catalog)
        second = MODULE.validate(self.manifest(["channel", "e2ee"]), self.catalog)
        self.assertEqual(first, second)
        self.assertEqual(MODULE.render(first), MODULE.render(second))

    def test_generated_contracts_share_schema_hash_and_features(self):
        contract = MODULE.validate(self.manifest(["channel", "e2ee"]), self.catalog)
        # erlc.mk 是 make 片段（供 ERLC_EXCLUDE 消费），不承载产品契约字段
        outputs = [
            output for path, output in MODULE.render(contract).items()
            if path.suffix != ".mk"
        ]
        for output in outputs:
            self.assertIn(str(contract["schema_version"]), output)
            self.assertIn(contract["manifest_hash"], output)
            for feature in contract["compiled_features"]:
                self.assertIn(f'"{feature}"', output)

    def test_backend_erlc_exclude_follows_compiled_features(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        mk_path = next(
            path for path in base
            if path.name == "imboy_product_features_erlc.mk"
        )
        base_mk = base[mk_path]
        for module in MODULE.FEATURE_BACKEND_MODULES["moment"]:
            self.assertIn(module, base_mk)
        self.assertIn("moment_ds moment_handler", base_mk)

        moment_catalog = {
            "features": [*self.catalog["features"], "moment"],
            "dependencies": self.catalog["dependencies"],
        }
        selected = MODULE.render(
            MODULE.validate(self.manifest(["moment"]), moment_catalog)
        )[mk_path]
        self.assertNotIn("moment_ds", selected)
        # 本测试 catalog 不含 enterprise_business ⇒ EB-10 的企业映射行仍会出现在
        # ERLC_EXCLUDE 里，故「整行为空」不再是 selected 的判据（EB-10 前它靠
        # 「只有 moment 一个 feature 有模块映射」这个假前提成立）。改为：
        # ① selected 的模块逐个不在排除集合里；② 在**完整 catalog**（moment +
        # enterprise_business 都被选中）下，排除行恢复为空 —— 保住原来那条
        # 「无未选中 feature 时排除行为空」的检查。
        self.assertNotIn("moment_ds", selected.splitlines()[-1].split(":=")[1].split())
        # customer_service 自 CS-02 起也有模块映射，完整 catalog 必须同样纳入，
        # 否则 all-selected 场景下 cs 模块仍留在排除行（CS-02 起本断言一直红）。
        # project 自 GZAPP-02/G6 起同样有模块映射（API handler 家族），一并纳入。
        full_catalog = {
            "features": [
                *self.catalog["features"],
                "moment",
                "enterprise_business",
                "customer_service",
                "project",
            ],
            "dependencies": self.catalog["dependencies"],
        }
        all_selected = MODULE.render(
            MODULE.validate(
                self.manifest([
                    "moment",
                    "enterprise_business",
                    "customer_service",
                    "project",
                ]),
                full_catalog,
            )
        )[mk_path]
        self.assertEqual("IMBOY_FEATURE_ERLC_EXCLUDE :=", all_selected.splitlines()[-1])

    def test_backend_per_feature_defines_follow_compiled_features(self):
        hrl_path = next(
            path for path in MODULE.render(MODULE.validate(self.manifest(), self.catalog))
            if path.name == "imboy_product_features.hrl"
        )
        base_hrl = MODULE.render(MODULE.validate(self.manifest(), self.catalog))[hrl_path]
        self.assertNotIn("IMBOY_FEATURE_MOMENT", base_hrl)

        moment_catalog = {
            "features": [*self.catalog["features"], "moment"],
            "dependencies": self.catalog["dependencies"],
        }
        selected_hrl = MODULE.render(
            MODULE.validate(self.manifest(["moment"]), moment_catalog)
        )[hrl_path]
        self.assertIn("-define(IMBOY_FEATURE_MOMENT, true).", selected_hrl)

    def test_flutter_moment_route_import_is_sliced(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        selected = MODULE.render(
            MODULE.validate(self.manifest(["channel", "e2ee"]), self.catalog)
        )
        route_path = next(path for path in base if path.name == "generated_product_feature_routes.dart")
        self.assertNotIn("moment_social", base[route_path])

        moment_catalog = {
            "features": [*self.catalog["features"], "moment"],
            "dependencies": self.catalog["dependencies"],
        }
        moment = MODULE.render(MODULE.validate(self.manifest(["moment"]), moment_catalog))
        self.assertIn("moment_social", moment[route_path])
        self.assertIn("moment_feed", moment[route_path])

    def test_flutter_location_route_imports_are_sliced(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        route_path = next(path for path in base if path.name == "generated_product_feature_routes.dart")
        self.assertNotIn("people_nearby", base[route_path])
        self.assertNotIn("component/location", base[route_path])

        location_catalog = {
            "features": [*self.catalog["features"], "location"],
            "dependencies": self.catalog["dependencies"],
        }
        location = MODULE.render(MODULE.validate(self.manifest(["location"]), location_catalog))
        self.assertIn("people_nearby", location[route_path])
        self.assertIn("map_location_picker", location[route_path])

    def test_flutter_group_feature_route_imports_are_sliced(self):
        route_path = next(
            path for path in MODULE.render(MODULE.validate(self.manifest(), self.catalog))
            if path.name == "generated_product_feature_routes.dart"
        )
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))[route_path]
        self.assertNotIn("group_vote_routes", base)

        group_catalog = {
            "features": [*self.catalog["features"], "group_vote"],
            "dependencies": self.catalog["dependencies"],
        }
        selected = MODULE.render(
            MODULE.validate(self.manifest(["group_vote"]), group_catalog)
        )[route_path]
        self.assertIn("group_vote_routes.dart", selected)
        self.assertIn("groupVoteRoutes()", selected)

    def test_flutter_channel_routes_and_widgets_are_sliced(self):
        base_contract = MODULE.validate(self.manifest(), self.catalog)
        route_path = next(
            path for path in MODULE.render(base_contract)
            if path.name == "generated_product_feature_routes.dart"
        )
        base = MODULE.render(base_contract)[route_path]
        self.assertNotIn("page/channel", base)
        self.assertIn("compiledChannelListPage() => null", base)

        selected = MODULE.render(
            MODULE.validate(self.manifest(["channel"]), self.catalog)
        )[route_path]
        self.assertIn("channel_list_page", selected)
        self.assertIn("channelRoutes(", selected)
        self.assertIn("const ChannelListPage()", selected)

    def test_flutter_channel_subfeature_routes_are_independent(self):
        selected = MODULE.render(
            MODULE.validate(self.manifest(["channel"]), self.catalog)
        )
        route_path = next(path for path in selected if path.name == "generated_product_feature_routes.dart")
        channel_only = selected[route_path]
        self.assertNotIn("channel_order_routes", channel_only)

        with_order = MODULE.render(
            MODULE.validate(self.manifest(["channel", "channel_order"]), self.catalog)
        )[route_path]
        self.assertIn("channel_order_routes.dart", with_order)
        self.assertIn("channelOrderRoutes()", with_order)

    def test_flutter_channel_order_paywall_is_sliced(self):
        channel_only = MODULE.render(
            MODULE.validate(self.manifest(["channel"]), self.catalog)
        )
        widget_path = next(
            path for path in channel_only
            if path.name == "generated_channel_order_widget.dart"
        )
        self.assertNotIn("channel_paywall_view", channel_only[widget_path])
        self.assertIn("=> null", channel_only[widget_path])

        with_order = MODULE.render(
            MODULE.validate(self.manifest(["channel", "channel_order"]), self.catalog)
        )[widget_path]
        self.assertIn("channel_paywall_view", with_order)
        self.assertIn("ChannelPaywallView", with_order)

    def test_admin_moment_module_import_is_sliced(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        composition_path = next(
            path for path in base
            if path.name == "generatedFeatureComposition.tsx"
        )
        self.assertNotIn("modules/moments", base[composition_path])

        moment_catalog = {
            "features": [*self.catalog["features"], "moment"],
            "dependencies": self.catalog["dependencies"],
        }
        selected = MODULE.render(
            MODULE.validate(self.manifest(["moment"]), moment_catalog)
        )[composition_path]
        self.assertIn("modules/moments", selected)
        self.assertIn('path="/moments"', selected)

    def test_admin_optional_route_imports_follow_compiled_features(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        composition_path = next(
            path for path in base
            if path.name == "generatedFeatureComposition.tsx"
        )
        base_output = base[composition_path]
        for symbol in (
            "ChannelListPage", "ChannelInvitationPage", "ChannelOrderPage",
            "GroupVoteManagePage", "GroupScheduleManagePage",
            "GroupTaskManagePage", "ComplianceKeyPage",
        ):
            self.assertNotIn(symbol, base_output)

        admin_catalog = {
            "features": [
                *self.catalog["features"], "channel_invitation",
                "group_vote", "group_schedule", "group_task",
            ],
            "dependencies": {
                **self.catalog["dependencies"],
                "channel_invitation": ["channel"],
            },
        }
        selected = MODULE.render(
            MODULE.validate(
                self.manifest([
                    "channel", "channel_invitation", "channel_order",
                    "group_vote", "group_schedule", "group_task", "e2ee",
                ]),
                admin_catalog,
            )
        )[composition_path]
        for symbol in (
            "ChannelListPage", "ChannelInvitationPage", "ChannelOrderPage",
            "GroupVoteManagePage", "GroupScheduleManagePage",
            "GroupTaskManagePage", "ComplianceKeyPage",
        ):
            self.assertIn(symbol, selected)

    def test_android_location_metadata_follows_compiled_feature(self):
        base = MODULE.render(MODULE.validate(self.manifest(), self.catalog))
        properties_path = next(
            path for path in base if path.name == "product-features.properties"
        )
        self.assertIn("location=false", base[properties_path])

        location_catalog = {
            "features": [*self.catalog["features"], "location"],
            "dependencies": self.catalog["dependencies"],
        }
        selected = MODULE.render(
            MODULE.validate(self.manifest(["location"]), location_catalog)
        )[properties_path]
        self.assertIn("location=true", selected)

        overlay_path = next(
            path for path in base
            if path.name == "AndroidManifest.xml" and path.parent.name == "debug"
        )
        self.assertNotIn("pathPattern", base[overlay_path])
        e2ee = MODULE.render(
            MODULE.validate(self.manifest(["e2ee"]), self.catalog)
        )[overlay_path]
        self.assertIn("pathPattern", e2ee)

    def test_source_catalog_bootstraps_without_beams(self):
        catalog = MODULE.source_catalog(SCRIPT.parents[1])
        self.assertEqual("core", catalog["features"][0])
        self.assertIn("channel", catalog["features"])
        self.assertEqual(["channel"], catalog["dependencies"]["channel_order"])

    # ------------------------------------------------------------------
    # EB-10：enterprise_business / customer_service 入册（catalog + 生成器 + 依赖边）
    # ------------------------------------------------------------------

    def test_source_catalog_bootstraps_builtin_features(self):
        """无 beam 的新 checkout（disposable worktree）里，Builtin 键也必须可见。

        生成器在 `ebin/imboy_feature.beam` 缺失时回退到 source_catalog；它按
        `Builtin = [...]` 解析平台内建键。企业业务/客服正是挂在 Builtin 上，
        漏解析会让三档矩阵在**新树**上被 validate 判成 unknown feature。
        """
        catalog = MODULE.source_catalog(SCRIPT.parents[1])
        for feature in ("bot_webhook", "appeal", "enterprise_business", "customer_service"):
            with self.subTest(feature=feature):
                self.assertIn(feature, catalog["features"])
        self.assertEqual(
            ["enterprise_business"], catalog["dependencies"]["customer_service"]
        )

    def test_enterprise_backend_modules_cover_feature_directory(self):
        """物理裁剪清单必须与目录**逐项相等**（否则漏排的模块会被静默编译）。"""
        feature_dir = SCRIPT.parents[1] / "src/features/enterprise_business"
        modules = set()
        for path in feature_dir.rglob("*.erl"):
            match = re.search(r"^-module\(([a-z][a-z0-9_]*)\)", path.read_text(), re.M)
            self.assertIsNotNone(match, f"no -module attribute in {path}")
            modules.add(match.group(1))
        self.assertTrue(modules, "enterprise feature directory must not be empty")
        self.assertEqual(modules, set(MODULE.FEATURE_BACKEND_MODULES["enterprise_business"]))

    def test_enterprise_selection_drives_erlc_exclude_and_defines(self):
        catalog = MODULE.source_catalog(SCRIPT.parents[1])
        selected = MODULE.render(MODULE.validate(self.manifest(["enterprise_business"]), catalog))
        mk_path = next(
            path for path in selected if path.name == "imboy_product_features_erlc.mk"
        )
        hrl_path = next(
            path for path in selected if path.name == "imboy_product_features.hrl"
        )
        for module in MODULE.FEATURE_BACKEND_MODULES["enterprise_business"]:
            self.assertNotIn(module, selected[mk_path])
        self.assertIn("-define(IMBOY_FEATURE_ENTERPRISE_BUSINESS, true).", selected[hrl_path])

        base = MODULE.render(MODULE.validate(self.manifest(), catalog))
        base_mk = base[mk_path]
        for module in MODULE.FEATURE_BACKEND_MODULES["enterprise_business"]:
            self.assertIn(module, base_mk)
        self.assertNotIn("IMBOY_FEATURE_ENTERPRISE_BUSINESS", base[hrl_path])

    def test_customer_service_requires_enterprise_business(self):
        """EB-10-A03：selected_features 里 customer_service 而无 enterprise_business
        ⇒ 生成期即失败（依赖边来自 imboy_policy_catalog:dependencies/1）。"""
        catalog = MODULE.source_catalog(SCRIPT.parents[1])
        with self.assertRaisesRegex(
            MODULE.ManifestError,
            "missing dependency for customer_service: enterprise_business",
        ):
            MODULE.validate(self.manifest(["customer_service"]), catalog)
        # 正例对照：同一 manifest 加上 enterprise_business 即通过（失败不是环境伪影）
        MODULE.validate(self.manifest(["enterprise_business", "customer_service"]), catalog)

    def test_stale_check(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "generated.txt"
            with self.assertRaisesRegex(MODULE.ManifestError, "stale"):
                MODULE.write_or_check({path: "expected"}, True)
            MODULE.write_or_check({path: "expected"}, False)
            MODULE.write_or_check({path: "expected"}, True)


if __name__ == "__main__":
    unittest.main()
