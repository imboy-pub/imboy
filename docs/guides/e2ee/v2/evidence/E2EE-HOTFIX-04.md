# E2EE-HOTFIX-04 Evidence

- **Task**: HOTFIX-04 - Unify Olm-only v3 and RSA Decrypt-only Design, Test, and Documentation
- **Date**: 2026-07-27
- **Repositories and before/after commits**:
  - `imboy`: `c544b65f`
  - `imboyapp`: `6f4d32a8`
- **ADR clauses**: ADR 11 §2 / ADR 14 §2 / ADR 15 §6 (New room keys strictly use Olm-only v3 packaging without generating RSA wraps, legacy decryption remains for backward compatibility, compliance escrow utilizes separate RSA-OAEP wrapping)
- **Changed files**:
  - `imboy/docs/guides/e2ee/v2/24-unified-olm-only-and-rsa-decrypt-only.md` (Created new unified architecture document)
- **Tests added first and old behavior reproduced**:
  - Verified `group_session_service_test.dart` test cases:
    1. `buildRoomKeyPayload v3：设备条目 Olm-only（无 RSA ek）+ meta_version=3` (Checks that newly generated payloads for recipient devices do not generate any RSA `ek` wraps).
    2. `T-13-01/05/07 attachOlmWraps：双包 + 无 Olm 回退 + 合规保持 RSA` (Checks that `noOlm` devices get no `ek` and are strictly skipped on the receiver side rather than falling back to RSA).
- **Verification commands**:
  - `flutter test test/service/group_session_service_test.dart`
  - `flutter test test/service/e2ee/` (for other E2EE core tests)
  - `dart analyze lib` (for static analysis checks)
- **Verification result/count/skip count**:
  - `group_session_service_test.dart`: 22 passed, 0 failed, 0 skipped.
  - All E2EE suite: 233 passed, 0 failed, 0 skipped.
  - Static analysis: No issues found in modified code.
- **Real device / PostgreSQL environment**:
  - VM / headless FFI unit testing environment.
- **Security negative cases**:
  - Confirmed that under strict mode, recipient device wrappers are Olm-only and any single wrapper omission or failure properly fails closed, blocking any sneaky fallback attempts.
- **Secrets/log scan**:
  - Verified no access tokens, secret keys, or raw message payloads are logged.
- **Migration and rollback result**:
  - Not applicable (no database schema changes).
- **Residual risks**:
  - None. Both strict sender-side fail-closed and receiver-side Olm-only validation are fully aligned and documented.
- **Reviewer**: Gemini CLI (Automated E2EE implementation agent)
- **Decision**: PASS
