#!/usr/bin/env python3
"""Validate the canonical product manifest and emit three deterministic contracts."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path

SCHEMA_VERSION = 1
BASE_REF = "imboy-feature-inventory-v1"
BASE_FEATURES = ("core",)
FIELDS = {"schema_version", "product_id", "profile", "base_ref", "selected_features", "disabled_base_features"}


class ManifestError(ValueError):
    pass


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ManifestError(f"duplicate field: {key}")
        result[key] = value
    return result


def read_manifest(path: Path) -> dict:
    try:
        return json.loads(path.read_text(), object_pairs_hook=_unique_object)
    except (OSError, json.JSONDecodeError) as exc:
        raise ManifestError(str(exc)) from exc


def runtime_catalog(repo: Path) -> dict:
    ebin = repo / "ebin"
    if not (ebin / "imboy_feature.beam").exists():
        return source_catalog(repo)
    paths = [ebin, *(path / "ebin" for path in (repo / "deps").glob("*") if (path / "ebin").is_dir())]
    args = ["erl", "-noshell"]
    for path in paths:
        args.extend(("-pa", str(path)))
    expression = (
        "F=imboy_feature:feature_names(),"
        "D=maps:from_list([{X,imboy_policy_catalog:dependencies(X)} || X <- F]),"
        "io:format(\"CATALOG_JSON=~s~n\",[jsx:encode(#{features=>F,dependencies=>D})]),halt()."
    )
    completed = subprocess.run(args + ["-eval", expression], text=True, capture_output=True, check=False)
    marker = "CATALOG_JSON="
    line = next((line for line in completed.stdout.splitlines() if line.startswith(marker)), None)
    if completed.returncode or line is None:
        raise ManifestError(f"cannot load backend feature catalog: {completed.stderr.strip() or completed.stdout.strip()}")
    return json.loads(line[len(marker) :])


def source_catalog(repo: Path) -> dict:
    feature_source = (repo / "src/lib/imboy_feature.erl").read_text()
    registry_source = (repo / "src/lib/imboy_plugin_registry.erl").read_text()
    dependency_source = (repo / "src/lib/imboy_policy_catalog.erl").read_text()
    plugin_features = re.findall(r"feature_keys\s*=>\s*\[([^]]*)\]", registry_source)
    features = {"core", "e2ee"}
    for values in plugin_features:
        features.update(re.findall(r"\b[a-z][a-z0-9_]*\b", values))
    order_match = re.search(r"Ordered\s*=\s*\[(.*?)\],\s*Extra", feature_source, re.S)
    if not order_match or not plugin_features:
        raise ManifestError("cannot bootstrap feature catalog from Erlang sources")
    ordered = [name for name in re.findall(r"\b[a-z][a-z0-9_]*\b", order_match.group(1)) if name in features]
    feature_list = ["core", "e2ee", *dict.fromkeys(ordered), *sorted(features - set(ordered) - {"core", "e2ee"})]
    dependencies = {name: [] for name in feature_list}
    for name, values in re.findall(r"dependencies\((\w+)\)\s*->\s*\[([^]]*)\]", dependency_source):
        if name != "_":
            dependencies[name] = re.findall(r"\b[a-z][a-z0-9_]*\b", values)
    return {"features": feature_list, "dependencies": dependencies}


def validate(manifest: dict, catalog: dict) -> dict:
    unknown_fields = sorted(set(manifest) - FIELDS)
    if unknown_fields:
        raise ManifestError(f"unknown manifest fields: {', '.join(unknown_fields)}")
    if manifest.get("schema_version") != SCHEMA_VERSION:
        raise ManifestError(f"schema_version must be {SCHEMA_VERSION}")
    if manifest.get("product_id") != "imboy" or not isinstance(manifest.get("profile"), str):
        raise ManifestError("product_id must be imboy and profile must be a string")
    if manifest.get("base_ref") != BASE_REF:
        raise ManifestError(f"base_ref must be {BASE_REF}")
    if manifest.get("disabled_base_features", []):
        raise ManifestError("Base features cannot be disabled")
    selected = manifest.get("selected_features")
    if not isinstance(selected, list) or any(not isinstance(item, str) for item in selected):
        raise ManifestError("selected_features must be a string array")
    if len(selected) != len(set(selected)):
        raise ManifestError("duplicate selected feature")
    known = set(catalog["features"])
    unknown = sorted(set(selected) - known)
    if unknown:
        raise ManifestError(f"unknown feature: {', '.join(unknown)}")
    if set(selected) & set(BASE_FEATURES):
        raise ManifestError("Base features must not appear in selected_features")
    dependencies = catalog["dependencies"]
    _validate_catalog_graph(known, dependencies)
    compiled = set(BASE_FEATURES) | set(selected)
    for feature in selected:
        missing = set(dependencies.get(feature, [])) - compiled
        if missing:
            raise ManifestError(f"missing dependency for {feature}: {', '.join(sorted(missing))}")
    canonical = {
        "base_ref": BASE_REF,
        "product_id": "imboy",
        "profile": manifest["profile"],
        "schema_version": SCHEMA_VERSION,
        "selected_features": sorted(selected),
    }
    digest = hashlib.sha256(json.dumps(canonical, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return {**canonical, "manifest_hash": f"sha256:{digest}", "compiled_features": sorted(compiled)}


def _validate_catalog_graph(known: set[str], dependencies: dict) -> None:
    for feature, deps in dependencies.items():
        missing = set(deps) - known
        if feature not in known or missing:
            raise ManifestError(f"catalog has unknown dependency: {feature} -> {sorted(missing)}")
    visiting, visited = set(), set()
    def visit(feature):
        if feature in visiting:
            raise ManifestError(f"cyclic dependency at {feature}")
        if feature in visited:
            return
        visiting.add(feature)
        for dependency in dependencies.get(feature, []):
            visit(dependency)
        visiting.remove(feature)
        visited.add(feature)
    for feature in known:
        visit(feature)


def render(contract: dict) -> dict[Path, str]:
    root = Path(__file__).resolve().parents[1]
    features = contract["compiled_features"]
    header = "Generated by imboy/scripts/generate_product_features.py; do not edit."
    backend = (
        "%% " + header + "\n"
        + "-define(IMBOY_PRODUCT_FEATURE_SCHEMA_VERSION, " + str(SCHEMA_VERSION) + ").\n"
        + "-define(IMBOY_PRODUCT_FEATURE_MANIFEST_HASH, " + _erl_term(contract["manifest_hash"]) + ").\n"
        + "-define(IMBOY_COMPILED_FEATURES, " + _erl_atom_list(features) + ").\n"
        + "-define(IMBOY_PRODUCT_FEATURE_CONTRACT, " + _erl_term(contract) + ").\n"
    )
    dart_items = ",\n  ".join(json.dumps(item) for item in features)
    dart = f"// {header}\nconst productFeatureSchemaVersion = {SCHEMA_VERSION};\nconst productFeatureManifestHash = {json.dumps(contract['manifest_hash'])};\nconst compiledProductFeatures = <String>[\n  {dart_items},\n];\n"
    route_imports = [
        "import 'package:flutter/cupertino.dart';",
        "import 'package:flutter_riverpod/flutter_riverpod.dart';",
        "import 'package:go_router/go_router.dart';",
    ]
    if "moment" in features:
        route_imports.extend([
            "import 'package:imboy/config/routes.dart';",
            "import 'package:imboy/modules/moment_social/public.dart';",
        ])
    if "location" in features:
        route_imports.extend([
            "import 'package:imboy/component/location/widget.dart';",
            "import 'package:imboy/page/contact/people_nearby/people_nearby_page.dart';",
        ])
    if "channel" in features:
        route_imports.extend([
            "import 'package:imboy/page/channel/channel_detail_page.dart';",
            "import 'package:imboy/page/channel/channel_list_page.dart';",
            "import 'package:imboy/page/conversation/subscribed_channel_strip_provider.dart';",
            "import 'package:imboy/page/conversation/widget/subscribed_channel_strip.dart';",
            "import 'routes/channel_routes.dart';",
        ])
        for feature, route_file in (
            ("channel_discover", "channel_discover_routes.dart"),
            ("channel_invitation", "channel_invitation_routes.dart"),
            ("channel_order", "channel_order_routes.dart"),
        ):
            if feature in features:
                route_imports.append(f"import 'routes/{route_file}';")
    for feature, route_file in (
        ("group_vote", "group_vote_routes.dart"),
        ("group_schedule", "group_schedule_routes.dart"),
        ("group_task", "group_task_routes.dart"),
    ):
        if feature in features:
            route_imports.append(f"import 'routes/{route_file}';")
    moment_routes = "" if "moment" not in features else """  GoRoute(
    path: AppRoutes.momentFeed,
    name: 'moment_feed',
    pageBuilder: (context, state) =>
        CupertinoPage(key: state.pageKey, child: const MomentFeedPage()),
  ),
  GoRoute(
    path: AppRoutes.momentCreate,
    name: 'moment_create',
    pageBuilder: (context, state) =>
        CupertinoPage(key: state.pageKey, child: const MomentCreatePage()),
  ),
  GoRoute(
    path: '/moment_notify',
    name: 'moment_notify',
    pageBuilder: (context, state) =>
        CupertinoPage(key: state.pageKey, child: const MomentNotifyPage()),
  ),
  GoRoute(
    path: '${AppRoutes.momentRoot}/:momentId',
    name: 'moment_detail',
    pageBuilder: (context, state) => CupertinoPage(
      key: state.pageKey,
      child: MomentDetailPage(momentId: state.pathParameters['momentId'] ?? ''),
    ),
  ),
"""
    location_routes = "" if "location" not in features else """  GoRoute(
    path: '/contact/people_nearby',
    name: 'people_nearby',
    pageBuilder: (context, state) =>
        CupertinoPage(key: state.pageKey, child: const PeopleNearbyPage()),
  ),
  GoRoute(
    path: '/map_location_picker',
    name: 'map_location_picker',
    pageBuilder: (context, state) {
      final extra = state.extra as Map<String, dynamic>? ?? {};
      return CupertinoPage(
        key: state.pageKey,
        child: MapLocationPicker(
          arguments: {
            'lat': extra['lat'] as double? ?? 39.909187,
            'lng': extra['lng'] as double? ?? 116.397451,
            'citycode': extra['citycode']?.toString() ?? '',
            'isMapImage': extra['isMapImage'] as bool? ?? false,
          },
        ),
      );
    },
  ),
"""
    route_feature_list = (
        f"<String>[{', '.join(json.dumps(item) for item in features)}]"
        if len(features) <= 2 else f"<String>[\n  {dart_items},\n]"
    )
    group_routes = "".join(
        f"  ...{function_name}(),\n"
        for feature, function_name in (
            ("group_vote", "groupVoteRoutes"),
            ("group_schedule", "groupScheduleRoutes"),
            ("group_task", "groupTaskRoutes"),
        )
        if feature in features
    )
    channel_feature_routes = "".join(
        f"      ...{function_name}(),\n"
        for feature, function_name in (
            ("channel_discover", "channelDiscoverRoutes"),
            ("channel_invitation", "channelInvitationRoutes"),
            ("channel_order", "channelOrderRoutes"),
        )
        if feature in features
    )
    channel_routes = ""
    if "channel" in features:
        channel_routes = (
            "  ...channelRoutes(featureRoutes: <RouteBase>[]),\n"
            if not channel_feature_routes else
            "  ...channelRoutes(\n    featureRoutes: <RouteBase>[\n"
            + channel_feature_routes
            + "    ],\n  ),\n"
        )
    feature_routes = moment_routes + location_routes + group_routes + channel_routes
    route_list = "<RouteBase>[]" if not feature_routes else f"<RouteBase>[\n{feature_routes}]"
    channel_widgets = """Widget? compiledChannelListPage() => const ChannelListPage();
Widget? compiledChannelDetailPage(String channelId) =>
    ChannelDetailPage(channelId: channelId, autoLoadStats: false);
Widget? compiledSubscribedChannelStrip() => const SubscribedChannelStrip();
int compiledChannelUnreadCount(WidgetRef ref) =>
    ref
        .watch(subscribedChannelStripProvider)
        .value
        ?.fold<int>(
          0,
          (sum, item) => item.isMuted ? sum : sum + item.unreadCount,
        ) ??
    0;
""" if "channel" in features else """Widget? compiledChannelListPage() => null;
Widget? compiledChannelDetailPage(String channelId) => null;
Widget? compiledSubscribedChannelStrip() => null;
int compiledChannelUnreadCount(WidgetRef ref) => 0;
"""
    channel_widgets = channel_widgets.rstrip()
    dart_routes = f"""// {header}
{chr(10).join(route_imports)}

const productFeatureRouteManifestHash =
    {json.dumps(contract['manifest_hash'])};
const productFeatureRouteSchemaVersion = {SCHEMA_VERSION};
const productFeatureRouteFeatures = {route_feature_list};

List<RouteBase> compiledProductFeatureRoutes() => {route_list};

{channel_widgets}
"""
    channel_order_import = (
        "import 'package:imboy/page/channel/paid/channel_paywall_view.dart';\n"
        if "channel_order" in features else ""
    )
    channel_order_builder = (
        """Widget? compiledChannelPaywall({
  required ChannelModel channel,
  required VoidCallback onPurchased,
}) => ChannelPaywallView(channel: channel, onPurchased: onPurchased);"""
        if "channel_order" in features else
        """Widget? compiledChannelPaywall({
  required ChannelModel channel,
  required VoidCallback onPurchased,
}) => null;"""
    )
    dart_channel_order = f"""// {header}
import 'package:flutter/widgets.dart';
{channel_order_import}import 'package:imboy/store/model/channel_model.dart';

const channelOrderWidgetManifestHash =
    {json.dumps(contract['manifest_hash'])};
const channelOrderWidgetSchemaVersion = {SCHEMA_VERSION};
const channelOrderWidgetFeatures = {route_feature_list};

{channel_order_builder}
"""
    ts_items = ",\n  ".join(json.dumps(item) for item in features)
    ts = f"// {header}\nexport const productFeatureSchemaVersion = {SCHEMA_VERSION} as const;\nexport const productFeatureManifestHash =\n  {json.dumps(contract['manifest_hash'])} as const;\nexport const compiledProductFeatures = [\n  {ts_items},\n] as const;\n"
    admin_lazy_pages = []
    admin_routes = []

    def add_admin_page(name, module):
        admin_lazy_pages.append(
            f"const {name} = lazy(() => import('{module}').then((m) => ({{ default: m.{name} }})))"
        )

    if "moment" in features:
        for name in ("MomentListPage", "MomentDetailPage", "MomentReportPage"):
            add_admin_page(name, "@/modules/moments")
        admin_routes.append("""    <Route path="/moments" element={(
      <PermissionRoute permission={['moments:read', 'messages:read']} roles={['1', '2']}>
        <FeatureRoute feature="moment"><MomentListPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/moments/reports" element={(
      <PermissionRoute permission={['reports:read', 'moments:report:read', 'messages:read']} roles={['1', '2']}>
        <Navigate to="/reports?target_type=moment" replace />
      </PermissionRoute>
    )} />
    <Route path="/moments/:id" element={(
      <PermissionRoute permission={['moments:read', 'messages:read']} roles={['1', '2']}>
        <FeatureRoute feature="moment"><MomentDetailPage /></FeatureRoute>
      </PermissionRoute>
    )} />""")

    if "channel" in features:
        for name in (
            "ChannelListPage", "ChannelDetailPage", "ChannelMessagePage",
            "ChannelSubscriberPage", "ChannelAdminPage",
        ):
            add_admin_page(name, f"@/pages/channels/{name}")
        admin_routes.append("""    <Route path="/channels" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel"><ChannelListPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/channels/:id" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel"><ChannelDetailPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/channels/:id/messages" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel"><ChannelMessagePage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/channels/:id/subscribers" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel"><ChannelSubscriberPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/channels/:id/admins" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel"><ChannelAdminPage /></FeatureRoute>
      </PermissionRoute>
    )} />""")

    if "channel_invitation" in features:
        add_admin_page("ChannelInvitationPage", "@/pages/channels/ChannelInvitationPage")
        admin_routes.append("""    <Route path="/channels/:id/invitations" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel_invitation"><ChannelInvitationPage /></FeatureRoute>
      </PermissionRoute>
    )} />""")

    if "channel_order" in features:
        add_admin_page("ChannelOrderPage", "@/pages/channels/ChannelOrderPage")
        add_admin_page("PaidChannelOpsPage", "@/pages/channels/PaidChannelOpsPage")
        admin_routes.append("""    <Route path="/channels/paid" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel_order"><PaidChannelOpsPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/channels/:id/orders" element={(
      <PermissionRoute permission="channels:read" roles={['1', '2']}>
        <FeatureRoute feature="channel_order"><ChannelOrderPage /></FeatureRoute>
      </PermissionRoute>
    )} />""")

    for feature, pages, route_text in (
        ("group_vote", ("GroupVoteManagePage",), """    <Route path="/groups/:id/votes" element={(
      <PermissionRoute permission="groups:vote:read" roles={['1', '2']}>
        <FeatureRoute feature="group_vote"><GroupVoteManagePage /></FeatureRoute>
      </PermissionRoute>
    )} />"""),
        ("group_schedule", ("GroupScheduleManagePage",), """    <Route path="/groups/:id/schedules" element={(
      <PermissionRoute permission="groups:schedule:read" roles={['1', '2']}>
        <FeatureRoute feature="group_schedule"><GroupScheduleManagePage /></FeatureRoute>
      </PermissionRoute>
    )} />"""),
        ("group_task", ("GroupTaskListPage", "GroupTaskManagePage"), """    <Route path="/groups/tasks" element={(
      <PermissionRoute permission="groups:task:read" roles={['1', '2']}>
        <FeatureRoute feature="group_task"><GroupTaskListPage /></FeatureRoute>
      </PermissionRoute>
    )} />
    <Route path="/groups/:id/tasks" element={(
      <PermissionRoute permission="groups:task:read" roles={['1', '2']}>
        <FeatureRoute feature="group_task"><GroupTaskManagePage /></FeatureRoute>
      </PermissionRoute>
    )} />"""),
    ):
        if feature in features:
            for page in pages:
                add_admin_page(page, f"@/pages/groups/{page}")
            admin_routes.append(route_text)

    if "e2ee" in features:
        add_admin_page("ComplianceKeyPage", "@/pages/settings/ComplianceKeyPage")
        admin_routes.append("""    <Route path="/settings/compliance-keys" element={(
      <PermissionRoute permission="settings:view" roles={['1']}>
        <FeatureRoute feature="e2ee"><ComplianceKeyPage /></FeatureRoute>
      </PermissionRoute>
    )} />""")

    admin_imports = "import type { ReactNode } from 'react'"
    if admin_lazy_pages:
        router_imports = "Navigate, Route" if "moment" in features else "Route"
        admin_imports = (
            "import { lazy, type ReactNode } from 'react'\n"
            f"import {{ {router_imports} }} from 'react-router-dom'\n"
            "import { FeatureRoute } from '@/components/auth/FeatureRoute'\n"
            "import { PermissionRoute } from '@/components/auth/PermissionRoute'"
        )
    admin_page_declarations = "\n".join(admin_lazy_pages)
    admin_route_tree = (
        "<>\n" + "\n".join(admin_routes) + "\n  </>"
        if admin_routes else "null"
    )
    admin_moment_panel = (
        "<MomentReportPage showPageHeader={false} />"
        if "moment" in features else "null"
    )
    admin_composition = f"""// {header}
{admin_imports}

{admin_page_declarations}

export const adminFeatureCompositionManifestHash =
  {json.dumps(contract['manifest_hash'])}
export const adminFeatureCompositionSchemaVersion = {SCHEMA_VERSION}
export const adminFeatureCompositionFeatures = {json.dumps(features)} as const

export function compiledAdminFeatureRoutes() {{
  return {admin_route_tree}
}}

export function compiledMomentReportPanel(): ReactNode {{
  return {admin_moment_panel}
}}

type CompiledChannelSearchResult = {{
  id: string
  name?: string | null
  subscriber_count?: number
}}

export async function compiledSearchChannels(
  keyword: string,
): Promise<{{ items: CompiledChannelSearchResult[] }}> {{
  {"const { searchChannelsPayload } = await import('@/modules/channels/api/public')" if "channel" in features else "void keyword"}
  return {"searchChannelsPayload({ keyword, limit: 5 })" if "channel" in features else "{ items: [] }"}
}}
"""
    android_properties = (
        f"# {header}\n"
        f"schema_version={SCHEMA_VERSION}\n"
        f"manifest_hash={contract['manifest_hash']}\n"
        f"compiled_features={json.dumps(features, separators=(',', ':'))}\n"
        f"location={'true' if 'location' in features else 'false'}\n"
    )
    android_permissions = ""
    android_application_nodes = ""
    if "location" not in features:
        android_permissions = """    <uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION" tools:node="remove" />
    <uses-permission android:name="android.permission.ACCESS_FINE_LOCATION" tools:node="remove" />
    <uses-permission android:name="android.permission.ACCESS_MEDIA_LOCATION" tools:node="remove" />
"""
        android_application_nodes += """        <service android:name="com.amap.api.location.APSService" tools:node="remove" />
        <service android:name="com.amap.api.maps.MapService" tools:node="remove" />
        <meta-data android:name="com.amap.api.v2.apikey" tools:node="remove" />
"""
    if "e2ee" in features:
        android_application_nodes += """        <activity android:name=".MainActivity">
            <intent-filter>
                <action android:name="android.intent.action.VIEW" />
                <category android:name="android.intent.category.DEFAULT" />
                <data android:scheme="content" android:mimeType="application/octet-stream" android:pathPattern=".*\\.enc" />
                <data android:scheme="file" android:mimeType="application/octet-stream" android:pathPattern=".*\\.enc" />
            </intent-filter>
            <intent-filter>
                <action android:name="android.intent.action.SEND" />
                <category android:name="android.intent.category.DEFAULT" />
                <data android:mimeType="application/octet-stream" />
            </intent-filter>
        </activity>
"""
    android_application = (
        f"    <application>\n{android_application_nodes}    </application>\n"
        if android_application_nodes else ""
    )
    android_overlay = """<!-- Generated by imboy/scripts/generate_product_features.py; do not edit.
schema_version={schema_version}
manifest_hash={manifest_hash}
compiled_features={compiled_features}
-->
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools">
{permissions}{application}</manifest>
""".format(
        schema_version=SCHEMA_VERSION,
        manifest_hash=contract["manifest_hash"],
        compiled_features=json.dumps(features, separators=(",", ":")),
        permissions=android_permissions,
        application=android_application,
    )
    return {
        root / "include/generated/imboy_product_features.hrl": backend,
        root.parent / "imboyapp/lib/app_core/feature_flags/generated_product_features.dart": dart,
        root.parent / "imboyapp/lib/config/router/generated_product_feature_routes.dart": dart_routes,
        root.parent / "imboyapp/lib/app_core/feature_flags/generated_channel_order_widget.dart": dart_channel_order,
        root.parent / "imboyadmin/src/generated/productFeatures.ts": ts,
        root.parent / "imboyadmin/src/generated/generatedFeatureComposition.tsx": admin_composition,
        root.parent / "imboyapp/android/app/product-features.properties": android_properties,
        root.parent / "imboyapp/android/app/src/debug/AndroidManifest.xml": android_overlay,
        root.parent / "imboyapp/android/app/src/profile/AndroidManifest.xml": android_overlay,
        root.parent / "imboyapp/android/app/src/release/AndroidManifest.xml": android_overlay,
    }


def _erl_term(value):
    if isinstance(value, dict):
        return "#{" + ", ".join(f"{_erl_term(k)} => {_erl_term(v)}" for k, v in sorted(value.items())) + "}"
    if isinstance(value, list):
        return "[" + ", ".join(_erl_term(item) for item in value) + "]"
    if isinstance(value, int):
        return str(value)
    return "<<" + json.dumps(value) + ">>"


def _erl_atom_list(values):
    return "[" + ", ".join(values) + "]"


def write_or_check(outputs: dict[Path, str], check: bool) -> None:
    stale = [str(path) for path, content in outputs.items() if not path.exists() or path.read_text() != content]
    if check and stale:
        raise ManifestError("stale generated output: " + ", ".join(stale))
    for path, content in outputs.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)


def require_profile(manifest: dict, required: str | None) -> None:
    if required is not None and manifest.get("profile") != required:
        raise ManifestError(
            f"profile must be {required}, got {manifest.get('profile')}"
        )


def main(argv=None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest", type=Path, default=Path(__file__).resolve().parents[1] / "config/product-feature-manifest.json")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--require-profile")
    args = parser.parse_args(argv)
    try:
        manifest = read_manifest(args.manifest)
        require_profile(manifest, args.require_profile)
        contract = validate(manifest, runtime_catalog(Path(__file__).resolve().parents[1]))
        write_or_check(render(contract), args.check)
    except ManifestError as exc:
        print(f"product feature manifest: {exc}", file=sys.stderr)
        return 1
    print(f"product feature manifest OK {contract['manifest_hash']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
