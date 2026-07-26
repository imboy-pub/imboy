# imboy E2EE Protocol Specification

> Version: 1.0.0 | Date: 2026-07-26
> Status: Draft for External Audit
> Scope: Client-side cryptographic protocol stack (imboyapp) + server zero-knowledge invariant (imboy backend)

---

## 1. Security Goals

The imboy E2EE system provides the following security properties for C2C (one-to-one) and C2G (group) messaging:

**Confidentiality.** Only intended recipients can read message plaintext. The server infrastructure never possesses message decryption keys.

**Forward Secrecy (PFS).** Compromise of long-term identity keys does not reveal past session messages. Each message uses an ephemeral message key derived from a ratcheting chain; once the ratchet advances, previous message keys are irrecoverably destroyed.

**Post-Compromise Security (PCS).** After a session state compromise, the next DH ratchet step (triggered by a direction change in communication) introduces fresh entropy that heals the session — the attacker loses access to all subsequent messages.

**Authentication.** Each device holds an Ed25519 identity key pair. The Ed25519 public key signs the Curve25519 identity public key, binding both to the same device. Peers verify this signature before X3DH key agreement, preventing server/MITM key substitution.

**Downgrade Resistance.** A capability high-water mark (HWM) mechanism detects and rejects protocol downgrade attempts. Once a peer has negotiated a high-security protocol (e.g., Olm), subsequent attempts to negotiate a weaker protocol (e.g., RSA-OAEP) are rejected unless the user explicitly confirms.

**Tamper Evidence.** All security-relevant events are recorded in an append-only SHA-256 hash-chain audit log. Any modification to historical entries breaks chain continuity and is detectable via independent verification.

---

## 2. Protocol Suites

| Suite ID | Transport | Use Case | PFS | PCS |
|----------|-----------|----------|-----|-----|
| `OLM.V1` | Olm (X3DH + Double Ratchet) | C2C single-chat | Yes | Yes |
| `MEGOLM.V1` | Megolm (sender-key group ratchet) | C2G group-chat | Partial (per-rotation) | On rotation |
| `RSA-OAEP-256+AES-256-GCM` | Legacy RSA wrap | Deprecated (decrypt-only) | No | No |

New C2C sessions default to `OLM.V1`. Group sessions use `MEGOLM.V1`. The legacy RSA suite is retained solely for decrypting historical messages; it cannot be used for new encryption.

---

## 3. Key Hierarchy

```
Device Root (per-device, persistent)
├── Ed25519 Identity Key Pair (signing)
├── Curve25519 Identity Key Pair (DH)
└── One-Time Prekeys (Curve25519, ephemeral, published to server)

Per-Session (per peer-device pair)
├── Root Key (derived from X3DH, ratchets on DH steps)
├── Sending Chain Key (symmetric, advances per message)
├── Receiving Chain Key (symmetric, advances per message)
└── Message Keys (ephemeral, derived from chain key, destroyed after use)

Group Session (per group, sender-owned)
├── Megolm Session Key (ratchets forward per rotation)
└── Message Keys (derived from session key + message index)
```

### 3.1 Key Generation

All key material is generated using the operating system's CSPRNG via vodozemac (Rust `rand` crate → `getrandom` syscall). No userspace PRNG is used for cryptographic key material.

### 3.2 Key Storage

| Material | Storage | Encryption at Rest |
|----------|---------|-------------------|
| Identity key pair (pickled) | FlutterSecureStorage (iOS Keychain / Android EncryptedSharedPreferences) | OS-managed hardware-backed |
| Olm session pickles | SQLCipher database (`crypto_olm_session` table) | AES-256-CBC (SQLCipher 4, PBKDF2-HMAC-SHA512, 256k iterations) |
| DB encryption key | FlutterSecureStorage (`db_cipher_key_{uid}`) | OS-managed hardware-backed |
| Pickle encryption key | FlutterSecureStorage (`olm_pickle_key`) | OS-managed hardware-backed |
| One-time prekeys (public) | Server PostgreSQL (`e2ee_one_time_prekeys`) | TLS in transit; public material |
| Identity public keys | Server PostgreSQL (`e2ee_device_identities`) | TLS in transit; public material |

The SQLCipher database key is a 256-bit CSPRNG random value (hex-encoded), generated once per user per device and stored in the platform secure enclave. It is never transmitted or derived from a user password.

---

## 4. X3DH Key Agreement (OLM.V1)

The extended triple Diffie-Hellman (X3DH) protocol establishes a shared secret between two devices without prior communication.

### 4.1 Published Material (via server)

Each device publishes to the server:
- `IK`: Curve25519 identity public key (long-term)
- `SPK`: Signed prekey (Curve25519, rotated periodically, signed by Ed25519 IK)
- `OPK[]`: Pool of one-time prekeys (Curve25519, consumed atomically)

### 4.2 Session Establishment (Alice → Bob)

```
Alice retrieves Bob's {IK_B, SPK_B, OPK_B} from server.
Alice verifies: Ed25519 signature of SPK_B under IK_B's Ed25519 key.
Alice verifies: Ed25519 self-signature binding IK_B.ed25519 → IK_B.curve25519.

Alice generates ephemeral key pair E_A (Curve25519).
Alice computes:
  DH1 = DH(IK_A, SPK_B)
  DH2 = DH(E_A, IK_B)
  DH3 = DH(E_A, SPK_B)
  DH4 = DH(E_A, OPK_B)    [if OPK available]

SK = KDF(DH1 || DH2 || DH3 || DH4)   [HKDF-SHA256]

Alice initializes Olm outbound session with SK.
Alice sends prekey message (messageType=0) containing E_A.pub and ciphertext.
```

### 4.3 Session Acceptance (Bob)

```
Bob receives prekey message containing E_A.pub and ciphertext.
Bob computes:
  DH1 = DH(SPK_B, IK_A)
  DH2 = DH(IK_B, E_A)
  DH3 = DH(SPK_B, E_A)
  DH4 = DH(OPK_B, E_A)    [if OPK was used]

SK = KDF(DH1 || DH2 || DH3 || DH4)

Bob initializes Olm inbound session with SK, decrypts first message.
Bob marks OPK as consumed (atomic server-side claim prevents replay).
```

### 4.4 TOFU Identity Pinning

On first session establishment with a peer device, the client records (pins) the peer's Curve25519 identity public key fingerprint in `crypto_identity_pin`. Subsequent sessions with the same `(peer_uid, peer_device_id)` compare the presented key against the pin:

- Match → proceed normally.
- Mismatch → raise `IdentityChangedException` (possible key compromise or device reset). UI alerts the user; communication is blocked until the user explicitly acknowledges the change.

This is Trust On First Use (TOFU): the first observed key is trusted implicitly; changes require user confirmation.

---

## 5. Double Ratchet (PFS / PCS)

Once the Olm session is established, all subsequent messages use the Double Ratchet algorithm (vodozemac implementation, compatible with libolm wire format).

### 5.1 Symmetric-Key Ratchet

Each message advances a per-direction chain key:

```
message_key = HKDF(chain_key, "message")
chain_key   = HKDF(chain_key, "chain")   [irreversible forward step]
```

Message keys are used for AES-256-GCM encryption (AEAD) and destroyed immediately after use.

### 5.2 DH Ratchet (Direction Change)

When a party sends after having received (direction change), it generates a new DH key pair:

```
new_root_key, new_chain_key = KDF(root_key, DH(new_dh_priv, peer_dh_pub))
```

The old DH private key is destroyed. This provides:
- **PFS**: An attacker with the old session state (before the DH step) cannot compute the new root key.
- **PCS**: After compromise, the next DH ratchet step (triggered by the peer's reply) introduces entropy the attacker does not have, healing the session.

### 5.3 Formal PFS/PCS Boundary

Within a single sending chain (no direction change), the symmetric chain key is forward-derivable: an attacker with `chain_key_N` can compute `chain_key_{N+1}`. PFS between messages within the same chain relies solely on message key destruction in memory. Cross-chain PFS (across DH ratchet steps) is cryptographic and unconditional given the DH assumption.

---

## 6. Megolm Group Sessions (MEGOLM.V1)

Group messages use a sender-key ratchet (Megolm):

1. Sender creates a Megolm `OutboundGroupSession` (generates a random 256-bit session key).
2. Sender distributes the session key to each group member's device via an Olm-encrypted "room key" message (key wrapping per-device).
3. Members store the session key as an `InboundGroupSession`.
4. Sender encrypts messages with AES-256-GCM using keys derived from the session key + message index.
5. Session rotation: the sender periodically creates a new outbound session and redistributes keys. Old sessions are retained for decryption of historical messages but cannot encrypt new ones.

**PFS boundary**: Megolm provides forward secrecy at session rotation granularity (not per-message). Between rotations, compromise of the session key reveals all messages in that session epoch.

---

## 7. Identity Verification

### 7.1 Ed25519 Self-Signature

Each device's identity bundle includes:
```json
{
  "ed25519_key": "<base64 Ed25519 public key>",
  "curve25519_key": "<base64 Curve25519 public key>",
  "signature": "<base64 Ed25519 signature of curve25519_key>"
}
```

Before X3DH, the receiving party verifies that `signature` is a valid Ed25519 signature of `curve25519_key` under `ed25519_key`. Failure → `IdentityVerificationException` (fail-closed, no fallback).

### 7.2 Safety Numbers (Out-of-Band Verification)

Compatible with Signal FingerprintProtocol v2:

```
1. Sort (uid_A, pub_A) and (uid_B, pub_B) by uid lexicographically.
2. hash = SHA-512(0x30 || utf8(uid_1) || raw(pub_1) || utf8(uid_2) || raw(pub_2))
3. Iterate 5200 times: hash = SHA-512(hash)
4. Take first 60 bytes of final hash.
5. Each 5-byte chunk → big-endian uint40 mod 100000 → 5-digit group.
6. Output: 60 digits (12 groups × 5 digits).
```

**Symmetry guarantee**: `generate(A, B) == generate(B, A)` due to lexicographic sorting.

Users compare safety numbers via a trusted out-of-band channel (in-person, video call). Matching numbers confirm no MITM; differing numbers indicate key substitution.

---

## 8. Capability Negotiation and Downgrade Protection

### 8.1 Security Rank

Protocols are ordered by security level (index 0 = strongest):

```
securityRank = ['olm', 'megolm', 'rsa-oaep']
```

### 8.2 High-Water Mark (HWM)

For each `(peer_uid, peer_device_id)`, the client persists the highest-security protocol ever negotiated (`crypto_capability_hwm` table).

On each new negotiation:
- No HWM (first contact) → record and proceed.
- New rank ≤ HWM rank (upgrade or same) → update HWM if upgrade, proceed.
- New rank > HWM rank (downgrade) → raise `CapabilityDowngradeException`. Communication blocked until user explicitly confirms via `confirmDowngrade()`.

### 8.3 Signed Capabilities (Server-Side)

The server signs capability payloads with an Ed25519 key. Clients verify the signature before trusting advertised capabilities, preventing a compromised server from silently downgrading clients.

---

## 9. KDF Versioning and Migration

### 9.1 Version Dispatch

| Version | Algorithm | Parameters | Status |
|---------|-----------|------------|--------|
| 1 | PBKDF2-HMAC-SHA256 | 310,000 iterations, 32-byte output | Active (latest) |
| 2 | Argon2id | TBD | Reserved (not implemented) |

`KdfVersion.derive(version, password, salt)` dispatches to the correct algorithm. Unknown versions raise `UnsupportedKdfVersionException` (fail-closed).

### 9.2 Migration

`KdfVersion.migrate(fromVersion, password, salt, toVersion)` re-derives the key with the target algorithm. Downgrade attempts (`toVersion < fromVersion`) raise `KdfDowngradeException`.

---

## 10. Audit Log (Tamper-Evident Hash Chain)

### 10.1 Structure

```sql
CREATE TABLE crypto_audit_log (
  seq            INTEGER PRIMARY KEY AUTOINCREMENT,
  event_type     TEXT NOT NULL,
  peer_uid       TEXT NOT NULL DEFAULT '',
  peer_device_id TEXT NOT NULL DEFAULT '',
  detail         TEXT NOT NULL DEFAULT '',
  prev_hash      TEXT NOT NULL,
  event_hash     TEXT NOT NULL,
  created_at     INTEGER NOT NULL
);
```

### 10.2 Hash Chain

```
event_hash = SHA-256(seq || '|' || event_type || '|' || peer_uid || '|' ||
             peer_device_id || '|' || detail || '|' || prev_hash || '|' || created_at)
```

The genesis event (seq=1) uses `prev_hash = 'GENESIS'`. Each subsequent event chains to its predecessor.

### 10.3 Verification

`verifyChain()` recomputes every hash from seq=1 forward and checks:
- Sequential continuity (no gaps in seq).
- Each `prev_hash` matches the preceding event's `event_hash`.
- Each recomputed `event_hash` matches the stored value.

### 10.4 Recorded Events

| Event Type | Trigger |
|------------|---------|
| `identity_pinned` | TOFU first-use pinning |
| `identity_changed` | TOFU mismatch detected |
| `capability_downgraded` | Downgrade attempt rejected |
| `kdf_migrated` | KDF version upgrade |
| `trust_state_verified` | Out-of-band verification completed |
| `session_established` | X3DH session created |

### 10.5 Limitations (Honest Disclosure)

The hash chain detects any modification, insertion, or deletion of non-tail entries. Tail truncation (deleting the most recent entries) does not break internal consistency of the remaining chain. Detection of tail truncation requires an external anchor (e.g., periodic head-hash commitment to a transparency log such as Certificate Transparency or Sigsum). This is planned for a future phase.

---

## 11. Server Zero-Knowledge Invariant

The server infrastructure adheres to a strict zero-knowledge policy regarding cryptographic material:

1. **No plaintext private keys.** Device identity private keys and session keys never leave the client device. The server stores only public keys and opaque encrypted blobs (pickles are encrypted client-side before upload).

2. **No message decryption.** The server message pipeline treats E2EE ciphertext as opaque byte sequences. Routing is performed on metadata (sender, recipient, timestamp) without accessing payload content.

3. **OTK atomic claim.** One-time prekeys are consumed via an atomic `DELETE ... RETURNING` operation, preventing double-use (replay) attacks.

4. **CI enforcement.** A `check_server_zero_crypto.sh` script runs in CI and greps the server codebase for decryption function calls. Any introduction of server-side decryption capability fails the build.

---

## 12. Threat Model Traceability (ADR 08 §4)

| ID | Threat | Mitigation | Guard Test |
|----|--------|-----------|------------|
| T1 | Server reads plaintext | Zero-knowledge invariant (§11) | `check_server_zero_crypto.sh` (CI) |
| T2 | MITM key substitution | Ed25519 signature (§7.1) + TOFU (§4.4) + Safety Number (§7.2) + Capability HWM (§8) | `device_identity_signature_verify`, `e2ee_safety_number`, `capability_shrink_triggers_tofu_alert` |
| T3 | Key extraction from storage | SQLCipher (§3.2) + hardware-backed secure storage | `sqlcipher_migration_test` (integration) |
| T4 | Message tampering | AES-256-GCM AEAD (MAC verification) | `aes_gcm_tamper_fails` |
| T5 | Session compromise (no PFS/PCS) | Double Ratchet DH steps (§5.2) | `olm_pfs_old_session_cannot_decrypt`, `olm_pcs_recovery` |
| T6 | KDF weakness / non-migratable | Versioned KDF dispatch (§9) | `backup_kdf_version_migration` |
| T7 | Replay / reorder | OTK atomic claim + inbox dedupe (`crypto_inbox_dedupe`) | `otk_concurrent_claim_uniqueness`, `message_replay_rejected` |
| T8 | Trust state manipulation | Audit log hash chain (§10) | `device_trust_state_change_audit_log` |
| T9 | Rollback (identity/session) | TOFU monotonic pin + Megolm session index monotonicity | `device_identity_rollback_rejected`, `megolm_old_session_rejected` |

---

## 13. Cryptographic Dependencies

| Component | Library | Version | Language |
|-----------|---------|---------|----------|
| Olm / Megolm / vodozemac | vodozemac (via flutter_rust_bridge FFI) | 0.5.0 | Rust → Dart |
| AES-256-GCM | `encrypt` package (pointy_castle) | — | Dart |
| SHA-256 / SHA-512 | `crypto` package | — | Dart |
| PBKDF2-HMAC-SHA256 | `pointy_castle` | — | Dart |
| Ed25519 sign/verify | vodozemac | 0.5.0 | Rust → Dart |
| Curve25519 DH | vodozemac | 0.5.0 | Rust → Dart |
| SQLCipher | `sqflite_sqlcipher` | 3.4.0 | C (native) |
| Secure storage | `flutter_secure_storage` | — | Platform (Keychain/Keystore) |

No custom cryptographic primitives are implemented. All cryptography delegates to audited libraries (vodozemac, libsodium-compatible, platform SQLCipher).

---

## 14. Test Coverage Summary

As of 2026-07-26, the client-side E2EE test suite comprises **232 passing tests** across:

| Category | Count | Framework |
|----------|-------|-----------|
| Olm session lifecycle (X3DH, ratchet, PFS, PCS) | 3 | flutter test + vodozemac FFI |
| Protocol integration (full-stack composition) | 4 | flutter test + vodozemac FFI + sqflite_ffi |
| Threat model guards (T1–T9) | 24 | flutter test + sqflite_ffi |
| Identity verification (Ed25519) | 8 | flutter test + vodozemac FFI |
| Safety Number | 12 | flutter test |
| Capability negotiation + downgrade | 14 | flutter test + sqflite_ffi |
| KDF versioning + migration | 14 | flutter test |
| Audit log (hash chain) | 8 | flutter test + sqflite_ffi |
| CryptoStore (transactions, dedupe) | 20+ | flutter test + sqflite_ffi |
| AES-256-GCM (encrypt/decrypt/tamper) | 10+ | flutter test |
| Megolm group session | 15+ | flutter test + vodozemac FFI |
| KDF backward compatibility | 14 | flutter test (PBKDF2 310k iterations) |

Zero tests are skipped. All tests run in CI without requiring physical devices (vodozemac FFI loads the host-platform dynamic library).

---

## 15. References

- [The Double Ratchet Algorithm](https://signal.org/docs/specifications/doubleratchet/) — Signal Protocol
- [X3DH Key Agreement](https://signal.org/docs/specifications/x3dh/) — Signal Protocol
- [vodozemac](https://github.com/matrix-org/vodozemac) — Matrix.org Rust implementation
- [SQLCipher](https://www.zetetic.net/sqlcipher/) — AES-256 full-database encryption
- [Signal Fingerprint Protocol](https://github.com/signalapp/libsignal-protocol-java/blob/main/PROTOCOL.md) — Safety Number algorithm
- imboy ADR 08 §4 — Threat model and defense traceability matrix
- `docs/compliance/e2ee-policy.md` — Server-side E2EE policy and compliance disclosure
- `docs/guides/testing/e2ee-testing.md` — E2EE testing strategy and CI gates
