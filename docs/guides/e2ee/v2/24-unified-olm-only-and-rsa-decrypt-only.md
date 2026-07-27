# HOTFIX-04 — Unified Olm-only v3 and RSA Decrypt-only Design

> **Status**: Completed / Verified
> **Scope**: `imboy`, `imboyapp`
> **Rule**: New writes strictly never generate RSA `ek` wrappers for recipient devices. RSA wrappers are permanently reserved for legacy decrypt-only historical messages.

---

## 1. Architectural Mandates

As established in ADR 14 and ADR 15, the cryptographic pipeline enforces clear boundary separation between the legacy RSA-OAEP suite and the active, per-device Double Ratchet Olm suite:

1. **Active Suite (`OLM.V1` / `MEGOLM.V1`)**:
   - Every newly created room key (Megolm session) distributed by v2 clients is strictly packaged over per-device Olm channels.
   - Recipient device metadata entries in the room key packet strictly contain only `did` (Device ID) and `kid` (Key ID). They **strictly do not generate or contain any RSA `ek` (encrypted key) wrappers**.

2. **Legacy Suite (`RSA-OAEP-256+AES-256-GCM`)**:
   - Marked strictly **decrypt-only**.
   - No new E2EE message payloads or room-key distribution packets generate RSA wraps for recipient devices.
   - Retained solely for decrypting historical messages to ensure backward compatibility and protect historical data readability.

3. **Compliance Audit Exception (ADR 18)**:
   - The compliance audit receiver (`compliance-audit` entry) continues to utilize `RSA-OAEP-256` wrapping for the audit escrow key, as the compliance server does not maintain active interactive Olm channels. This is handled as an explicit escrow channel rather than a cryptographic rollback.

---

## 2. Implementation Verification

### 2.1 Outbound Room Key Payload Generation
In `imboyapp/lib/service/group_session_service.dart`, `buildRoomKeyPayload` is the single source of truth for outbound room key packaging.

- **No RSA `ek` generation**:
  ```dart
  static Map<String, dynamic> buildRoomKeyPayload({
    String? gid,
    required String sessionId,
    required String exportedKey,
    required Map<String, String> didToPem,
    required Map<String, String> didToKid,
    List<Map<String, dynamic>> extraKeys = const [],
  }) {
    final keys = <Map<String, dynamic>>[];
    for (final did in didToPem.keys) {
      keys.add({'did': did, 'kid': didToKid[did] ?? did}); // Strictly no 'ek' or 'wrap_alg' for normal devices!
    }
    keys.addAll(extraKeys);
    // ...
  }
  ```

### 2.2 Inbound Room Key Processing
In `imboyapp/lib/service/group_session_service.dart`, `_unwrapEntry` enforces the receiver-side state machine:

- **v3 (Olm-only / strict)**:
  - Requires `meta_version >= 3`.
  - Strictly decrypts the Olm wrapper.
  - If the Olm wrapper is missing, malformed, or decrypting fails, it **directly rejects the packet** (fail-closed) and **never falls back to RSA**.
- **Legacy (decrypt-only)**:
  - Allows RSA decryption of `ek` only if `meta_version < 3` and the Olm wrap is absent/invalid.

---

## 3. Test Suite Alignment

All E2EE and group session test suites have been fully aligned with this unified design:

1. **`test/service/group_session_service_test.dart`**:
   - `buildRoomKeyPayload v3：设备条目 Olm-only（无 RSA ek）+ meta_version=3`: Verifies that newly generated room key payloads contain strictly no RSA `ek` wrappers for recipient devices.
   - `T-13-01/05/07 attachOlmWraps：双包 + 无 Olm 回退 + 合规保持 RSA`: Verifies that devices without Olm keys get no `ek` and are strictly skipped on the receiver side under v3 (rather than falling back to RSA).
   - `Strict Olm-Wrap Fail-Closed (HOTFIX-03)`: Verifies that any single wrapping error under strict mode halts sending completely (no partial device success).

2. **`test/service/e2ee/plain_text_log_test.dart`**:
   - Asserts that no plain-text content of sent or edited messages is ever printed to any system logs or telemetry channels.

---

## 4. Conclusion

The codebase is fully compliant with `HOTFIX-04` and the freeze gate. Newly written E2EE data never produces RSA-wrapped keys for recipient devices, ensuring 100% cryptographic separation and robust defense against protocol-rollback attacks.
