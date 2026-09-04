# IMBoy Feature Composition F06 Execution Record

Date: 2026-09-03  
Scope: F-06 optional SDK, asset and permission slicing. No commit, push, deploy, publication or external action.

## Result

F-06 status: **PARTIAL**.

The canonical generator emits `imboyapp/android/app/product-features.properties` and generated debug/profile/release Manifest overlays. The `location` feature controls Android coarse/fine/media-location permissions, AMap location/map services, API-key metadata and the app's explicit Gradle `3dmap` dependency. The `e2ee` feature controls the Android `.enc` VIEW/SEND intent filters.

The shared main Manifest remains stable. Selected builds add the optional declarations; Base-only overlays remove declarations injected by the main Manifest or transitive plugins. No iOS file was changed because `ios/*` is a repository-protected area.

## Evidence

| Check | Result |
|---|---|
| Generator unit tests | PASS, 17/17 |
| Canonical selected Android debug APK | PASS |
| Selected merged Manifest | PASS; location permissions/services/API key and E2EE SEND entry present |
| Base-only Android debug APK | PASS |
| Base-only merged Manifest | PASS; all three location permissions, AMap services/API key and E2EE `.enc` VIEW/SEND entries absent |
| Base-only Android arm64 release APK | PASS; 129.8 MB, cross-artifact contract verified |
| Base-only release merged Manifest | PASS; coarse/fine/media/background location, AMap service/API key and E2EE VIEW/SEND declarations absent |
| Base-only release binary inventory | RETAINED SHARED; DEX contains AMap location and Geolocator plugin classes, and APK contains `libjingle_peerconnection_so.so` (12,092,568 bytes) |
| Canonical restoration | PASS; `sha256:b59aae27976015823e315e9a5ee42bb2af3970cec92c97df04b517b4674d1457` |

## Retained Shared Dependencies

Flutter's single `pubspec.yaml`/`pubspec.lock` still contains `amap_flutter_base_plus`, `amap_flutter_location_plus`, `geolocator`, media/camera, WebRTC, push and cryptography plugins. The Base-only Gradle graph still reports project `:amap_flutter_location_plus`; direct release-APK inspection also finds `com.amap.flutter.location`, `com.amap.api.location`, Geolocator and WebRTC binary content. Conditionally rewriting Pub dependencies would require multiple lockfiles or fragile generated metadata, so these packages are retained as Shared under the F-06 stop rule; SDK binary absence is not claimed.

No feature-exclusive asset directory was proven: `assets/images/` is a shared aggregate and privacy/help/migration assets are Base. Backend OTP applications/providers are also shared application infrastructure rather than proven one-feature dependencies. Admin optional feature code/chunks are covered by F-05; no package-level dependency was proven exclusive.

## Remaining Risk

iOS permission and URL/document declarations remain unsliced because the repository prohibits edits under `ios/*`. Base-only Android removes exposed declarations but may retain native SDK code contributed by Flutter plugins. Therefore the strict “last feature disabled implies SDK binary absent” acceptance is not met; F-06 remains PARTIAL rather than PASS.
