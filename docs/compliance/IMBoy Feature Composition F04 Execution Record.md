# IMBoy Feature Composition F04 Execution Record

Date: 2026-09-03  
Scope: F-04 Flutter compile-graph slicing, first vertical slice only. No device run, release publication, commit, push, deploy or external action.

## Result

F-04 status: **PARTIAL**.

The `moment`, `location`, `group_vote`, `group_schedule`, `group_task` and `channel` route/page roots are now controlled by the canonical product manifest. Channel discover, invitation, order routes and the order paywall also have independent generated imports. The generator emits the Flutter route registry and Channel order widget factory; the Base router and shells depend on those stable generated contracts instead of importing optional pages. Shared barrels no longer re-export these optional pages or their feature-only services.

For a full-selected manifest, the generated entry imports and registers the selected route roots. For a Base-only manifest, the same generated entry contains an empty route list and nullable Channel widget factories. Bottom Navigation, Web Shell, Conversation and Workspace use those factories, so they do not statically import Channel screens. Existing `RouteFeatureGuard` checks remain unchanged as defense in depth.

The App now rejects a missing, malformed or mismatched manifest hash, schema version or compiled-feature array before caching server state. It no longer treats an incomplete legacy payload as compatible, so an older Backend cannot silently expose a newer App's compiled routes.

## Evidence

| Check | Result |
|---|---|
| Generator unit tests | PASS, 14/14 |
| Canonical focused Flutter analysis | PASS, no issues |
| Generated registry and route feature guard tests | PASS, 13/13 |
| Base-only generated import/symbol audit | PASS; no sliced Moment, Location or optional group module/page/route symbols in the generated entry, Base router or relevant shared barrels |
| Base-only focused Dart analysis | PASS, no issues |
| Base-only Android debug build | PASS; `build/app/outputs/flutter-apk/app-debug.apk`, 289,417,258 bytes |
| Base-only kernel dependency audit | PASS; Moment, Location, group vote/schedule/task, Channel pages and Channel QR page absent from `kernel_snapshot_program.d` |
| Channel-only Android debug build | PASS; `build/app/outputs/flutter-apk/app-debug.apk`, 281,510,319 bytes |
| Channel-only kernel dependency audit | PASS; Channel list/detail pages present while discover, invitation, order list/detail and `paid/channel_paywall_view.dart` are absent from `.dart_tool/flutter_build/4483cba68339622f4311d539704900e0/kernel_snapshot_program.d` |
| Base-only Android arm64 release build | PASS; release APK built and verified by the F-07 matrix |
| Full-selected Android arm64 release build | PASS; release APK built and verified by the F-07 matrix |
| App manifest compatibility tests | PASS; missing, malformed and feature-superset payloads fail closed |
| Canonical output restoration | PASS; `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457` |

## Remaining F-04 work

Channel subfeature routes (`channel_discover`, `channel_invitation`, `channel_order`) and the `channel_order` paywall are now physically absent from the verified Channel-only kernel graph. E2EE enters authentication, chat and settings flows and needs a separate packaging-ceiling decision rather than route-only claims.

Base-only and full-selected release APKs now build and carry the verified manifest contract. No authenticated device navigation/API flow was run: the connected physical device is available, but the repository's test login is a phone-number contact identifier whose use requires separate human confirmation. Optional plugins, permissions and assets remain owned by F-06.
