# ADR 02 — Protocol Registry

> **状态**：Architecture Freeze（接口签名冻结，不可单方面变更）
> **关联**：01-overview（总纲）、03-device-identity、04-capability-negotiation、05-metadata-version、08-threat-model
> **取代**：`imboyapp/lib/service/e2ee_service.dart` 中 `decryptE2EEMessage` 按 `e2ee['e2ee_suite']` 字符串硬编码路由的实现（`if suite == 'OLM.V1' … else if 'MEGOLM.V1' … else RSA`）

---

## 1. 背景：为什么需要 Protocol Registry

v1 路由逻辑写死在业务层（`e2ee_service.dart:312-345`）：

```dart
if (e2ee['e2ee_suite']?.toString() == kOlmSuite) { /* Olm */ }
if (e2ee['e2ee_suite']?.toString() == kMegolmSuite) { /* Megolm */ }
// 否则走 RSA-OAEP-256 + AES-256-GCM legacy
```

这种结构有三个问题：

1. **加协议＝改业务层**：未来引入 MLS 时必须回到 `decryptE2EEMessage` 加 `else if`，业务层与密码学耦合。
2. **能力分散**：每个 suite 的 `encrypt/decrypt/initialize` 散落在 `OlmSessionService` / `GroupSessionService` / `E2EEService`，没有统一调用面。
3. **协商无锚点**：04 的 Capability Negotiation 需要枚举「本端支持哪些套件」，而 v1 没有「套件清单」这个一等概念。

ADR 02 的核心交付：把「套件字符串」升级为「`ProtocolSuite` 数据模型 + `E2eeSessionProtocol` 接口 + Registry 注册表」，业务层只跟 Registry 对话，不认得任何具体协议名。

---

## 2. 决策

### 2.1 `E2eeSessionProtocol` 抽象接口（Dart，签名冻结）

```dart
/// 所有 E2EE 协议插件（Olm / Megolm / RSA-legacy / 未来 MLS）必须实现。
/// 业务层（chat 发送/接收）只通过此接口与协议层交互，不依赖任何具体实现类。
abstract interface class E2eeSessionProtocol {
  /// 套件标识。Registry 按此 key 注册/查找。
  /// 必须返回不可变常量，例如 `ProtocolSuite.parse('OLM.V1')`。
  ProtocolSuite get suite;

  /// 设备本地初始化（首次登录 / 换设备 / 清理后重建）。
  ///
  /// 语义：
  /// - 幂等。已初始化时直接返回，不重置密钥。
  /// - Olm: 创建 Account、发布 identity_key + 一批 OTK 到服务端。
  /// - Megolm: 无设备级初始化（按群/会话域懒创建），返回 no-op。
  /// - RSA-legacy: 加载本地 kid→私钥，发布公钥到 user_keys。
  Future<void> initialize({required String userId, required String deviceId});

  /// 加密明文，产出可被 [decrypt] 还原的密文 + 该协议需要透传的 e2ee 元数据。
  ///
  /// 入参：
  /// - [plaintext]：业务层 JSON 序列化后的字符串（明文）。
  /// - [recipients]：多设备 fan-out 的接收方集合（见 §3.2）。群聊可只含 1 个
  ///   虚拟 recipients（群会话），由协议内部按 room key 分发。
  /// - [context]：协议无关的会话上下文（peer_uid / gid / scope 等）。
  ///
  /// 返回：
  /// - [ciphertext]：协议自定格式（如 Olm 用 Megolm 格式 base64、RSA 用
  ///   `base64(nonce).base64(ct)`、Megolm 用 group message format）。
  /// - [metadata]：写入消息顶层 `e2ee` 字段的 map（含 suite 标识），
  ///   服务端透明透传，接收方据此路由回同一 [suite]。
  Future<E2eeCiphertext> encrypt({
    required String plaintext,
    required List<RecipientDevice> recipients,
    required E2eeContext context,
  });

  /// 解密。入参的 [metadata] 即加密时产出的 map（服务端透传，未解析）。
  ///
  /// 失败时抛 [E2eeDecryptException]，业务层捕获后产出 `_e2ee_failed` 兜底
  /// payload（保留现有 `decryptIncomingPayload` 行为，见 e2ee_service.dart:530）。
  Future<String> decrypt({
    required String ciphertext,
    required Map<String, dynamic> metadata,
    E2eeContext? context,
  });

  /// 清理本端所有会话状态（退出登录 / 用户主动重置）。
  /// 必须清除：Olm pickle、Megolm session、in-memory ratchet 缓存。
  /// 不清除：device identity key（由 03 device-identity 单独管理）。
  Future<void> clearAll();
}

/// 协议无关的接收方设备（保留 v1 RecipientDevice 字段，新增 capabilities）。
class RecipientDevice {
  final String deviceId;
  final String keyId;       // 与 e2ee_service.dart:_userKidCacheByDevice 同语义
  final String publicKey;   // RSA-OAEP-256 PEM（legacy）或 curve25519 base64（Olm）
  final Set<ProtocolSuite> capabilities;  // 由 04 capability 注入，本 ADR 不解释
  const RecipientDevice({required this.deviceId, required this.keyId,
      required this.publicKey, this.capabilities = const {}});
}

class E2eeCiphertext {
  final String ciphertext;
  final Map<String, dynamic> metadata;
  const E2eeCiphertext(this.ciphertext, this.metadata);
}

class E2eeContext {
  final String? peerUid;
  final String? peerDeviceId;
  final String? gid;        // 非空＝群域；空＝单聊
  final String scope;       // 'c2c' / 'c2g'，对应 group_session_service c2cScope
  const E2eeContext({this.peerUid, this.peerDeviceId, this.gid, required this.scope});
}

class E2eeDecryptException implements Exception {
  final String reason;  // 'no_device_key' / 'nonce_mismatch' / 'decrypt_error' …
  const E2eeDecryptException(this.reason);
  @override String toString() => 'E2eeDecryptException($reason)';
}
```

**为什么是 `abstract interface class`（Dart 3）而非 `abstract class`**：接口冻结要求实现类不能继承默认行为（否则协议插件可能隐式依赖基类逻辑）。所有协议插件必须自行实现全部方法，Registry 不提供 fallback。

**为什么 `encrypt` 返回分离的 `ciphertext + metadata` 而非一个整体 blob**：与现有 v2.0 契约对齐（`e2ee` 元数据在消息顶层、`payload` 是密文字符串，见 `e2ee_service.dart:271-281`），同时让服务端继续把 `e2ee` 当不透明 map 透传（§6）。

### 2.2 Protocol Registry 注册机制

```dart
/// 全局单例。启动时由 main() 调用 [register]，业务层只调用 [resolve]/[all]。
class E2eeProtocolRegistry {
  const E2eeProtocolRegistry._();
  static final Map<String, E2eeSessionProtocol> _byProtocol = {};

  /// 注册一个协议实现。key = suite.protocol（小写），如 'olm' / 'megolm' / 'mls'。
  /// 同名重复注册抛 StateError，防止插件冲突。
  static void register(E2eeSessionProtocol impl) {
    final key = impl.suite.protocol;
    if (_byProtocol.containsKey(key)) {
      throw StateError('E2EE protocol already registered: $key');
    }
    _byProtocol[key] = impl;
  }

  /// 按元数据路由：读取 metadata['e2ee_suite']（v1 字符串）或
  /// metadata['protocol']/['version']（v2 三元组），解析后查表。
  static E2eeSessionProtocol resolve(Map<String, dynamic> metadata) {
    final suite = ProtocolSuite.fromMetadata(metadata); // §3, 兼容 v1
    final impl = _byProtocol[suite.protocol];
    if (impl == null) {
      throw StateError('No E2EE protocol registered for: ${suite.protocol}');
    }
    return impl;
  }

  /// 本端已注册的全部套件。供 04 Capability Negotiation 上报能力。
  static List<ProtocolSuite> all() =>
      _byProtocol.values.map((p) => p.suite).toList(growable: false);
}
```

**启动时注册顺序**（`main()` 中）：

```dart
E2eeProtocolRegistry.register(OlmProtocol());
E2eeProtocolRegistry.register(MegolmProtocol());
E2eeProtocolRegistry.register(RsaLegacyProtocol());  // 仅 decrypt，encrypt 已废弃
// MLS 留空（§8）
```

---

## 3. ProtocolSuite 数据模型

### 3.1 三元组定义

```dart
/// 协议套件标识。等价于 (protocol, version, cipher) 三元组。
class ProtocolSuite {
  final String protocol;  // 'olm' / 'megolm' / 'rsa-oaep' / 'mls'（小写，注册表 key）
  final int version;      // 协议版本号；Olm/Megolm 当前为 1。
                          //   注：此 version 是「协议实现版本」，防 T9（rollback）需配合 ADR 03
                          //   的 device identity version + ADR 05 的 message counter 一起构成
                          //   单调度；单独的 ProtocolSuite.version 不足以抗状态回滚。
  final String cipher;    // 底层算法，如 'curve25519+ed25519' / 'aes-256-gcm'

  const ProtocolSuite(this.protocol, this.version, this.cipher);

  /// 序列化为 wire string（写入 e2ee metadata）。格式：'PROTOCOL.VERSION'。
  /// 与 v1 字符串兼容（'OLM.V1' / 'MEGOLM.V1'）。
  String get wire => '${protocol.toUpperCase()}.$version';

  static const olm    = ProtocolSuite('olm',    1, 'curve25519+ed25519+aes-256-gcm');
  static const megolm = ProtocolSuite('megolm', 1, 'curve25519+ed25519+aes-256-gcm');
  static const rsa    = ProtocolSuite('rsa-oaep',    1, 'rsa-oaep-256+aes-256-gcm');
  static const mls    = ProtocolSuite('mls',    0, 'reserved');  // §8 占位
}
```

### 3.2 向后兼容：legacy 字符串解析

历史上落库的 3 种 `e2ee_suite` 字符串必须能被 `ProtocolSuite.fromMetadata` 解析（**这是 ADR 01 §6 验收标准 1 的硬约束**）：

| 历史 `e2ee_suite` 字符串 | 解析为 | `protocol` | `version` |
|---|---|---|---|
| `'OLM.V1'` | `ProtocolSuite.olm` | `'olm'` | `1` |
| `'MEGOLM.V1'` | `ProtocolSuite.megolm` | `'megolm'` | `1` |
| `'RSA-OAEP-256+AES-256-GCM'` | `ProtocolSuite.rsa` | `'rsa-oaep'` | `1` |
| v2 `metadata['protocol']='olm', ['version']=1` | `ProtocolSuite.olm` | `'olm'` | `1` |

```dart
static ProtocolSuite fromMetadata(Map<String, dynamic> m) {
  // 优先读 v2 三元组
  final proto = m['protocol']?.toString().toLowerCase();
  if (proto != null) {
    final version = int.tryParse(m['version']?.toString() ?? '') ?? 1;
    return ProtocolSuite(proto, version, _cipherFor(proto));
  }
  // 回退到 v1 字符串
  final legacy = m['e2ee_suite']?.toString() ?? '';
  switch (legacy) {
    case 'OLM.V1':                       return ProtocolSuite.olm;
    case 'MEGOLM.V1':                    return ProtocolSuite.megolm;
    case 'RSA-OAEP-256+AES-256-GCM':     return ProtocolSuite.rsa;
    default: throw FormatException('unknown e2ee_suite: $legacy');
  }
}
```

**为什么 protocol 字段全小写、wire 字符串大写**：小写做注册表 key 与文件路径风格一致；大写 wire 与 v1 已落库字符串字节级匹配（避免迁移）。两者通过 `.toUpperCase()` 桥接，无歧义。

**为什么 cipher 也写入三元组而非隐含**：未来 Olm 升级算法（如换 ChaCha20-Poly1305）时，`(olm, 2, chacha20)` 可与 `(olm, 1, aes-256-gcm)` 在同一对话共存，Capability Negotiation（04）按三元组精确匹配，不靠协议名猜算法。

---

## 4. Registry 注册机制与扩展流程

### 4.1 查找路径

业务层发送/解密消息时**永远不直接 import `OlmProtocol` / `MegolmProtocol`**，统一走 `E2eeProtocolRegistry.resolve(metadata)`：

```
入站消息 → e2ee metadata → ProtocolSuite.fromMetadata → registry.resolve(suite.protocol) → impl.decrypt(...)
出站消息 → 协商选定 suite → registry._byProtocol[suite.protocol] → impl.encrypt(...)
```

### 4.2 新增 MLS 时的注册流程（验证标准：业务层零改动）

未来落 MLS 时，PR 应只动 2 处，**`grep -rn 'OLM.V1\|MEGOLM.V1' imboyapp/lib/service/e2ee_service.dart imboyapp/lib/service/chat_*.dart` 必须零命中**：

1. 新建 `imboyapp/lib/service/e2ee/mls_protocol.dart`，实现 `E2eeSessionProtocol`；
2. `main()` 中追加一行 `E2eeProtocolRegistry.register(MlsProtocol())`。

`decryptE2EEMessage` / 发送链 / Capability 上报全部零改动，因为它们只跟 Registry 与 `ProtocolSuite` 抽象对话。这是 01-overview §6 验收标准 2 的可执行验证。

### 4.3 删除协议（弃用 RSA）

未来下线 RSA-legacy 时，仅需从 `main()` 移除 `register(RsaLegacyProtocol())` 一行。`resolve` 在遇到旧消息时会抛 `StateError('No E2EE protocol registered for: rsa-oaep')`，业务层捕获后走 `_e2ee_failed` 兜底（§2.1 decrypt 契约）。**不允许**在 `resolve` 内做静默 fallback，否则旧消息会被「假装解密成功」。

---

## 5. 套件协商（Capability Negotiation 协作）

本 ADR 只定义「单个套件如何被选」，协商的完整算法在 04。本节给出 fallback 顺序与 T2 防降级约束。

### 5.1 默认 fallback 顺序（按安全性从高到低）

| 优先级 | 套件 | 适用场景 | 防御的威胁（08） |
|---|---|---|---|
| 1 | `olm` | 单聊，per-message PFS | T5（设备攻陷的前向保密） |
| 2 | `megolm` | 群聊，或单聊对端不支持 Olm | T4（群消息完整性） |
| 3 | `rsa`（仅 decrypt） | 历史消息回看，**不再用于新加密** | T1（旧链路兼容） |

**为什么 Olm 优于 Megolm 做单聊**：Olm 是 per-message Double Ratchet，满足 T5 前向保密；Megolm 是 outbound session 复用（sender key），私钥泄漏后整条出站链可解。单聊场景下 Olm 的安全性严格更强（详见 08 §2 T5 分析）。

### 5.2 T2 防降级攻击约束

08 §2 T2 列明：Compromised Server 可伪造 device identity 强迫降级。Registry 层防御：

1. **不允许运行时降级到 `rsa` 新加密**：`RsaLegacyProtocol.encrypt` 在 `strictE2ee` / `complianceE2ee` 模式下抛 `UnsupportedError`（`EncryptionModeService.current.requiresEncryption` 为真时）。仅 `decrypt` 保留，处理历史消息。
2. **协商结果必须由 Capability 层（04）签名校验**：本 ADR 不实现签名校验，但 `E2eeProtocolRegistry.resolve` 在收到一个 `metadata.protocol='rsa-oaep'` 的新出站请求时，必须拒绝（防止被诱导走老协议）。
3. **fallback 仅单向（高→低）且只发生在对端能力不足时**：对端上报不支持 Olm 时降级到 Megolm；对端不支持 Megolm 时停止加密（不自动降级到 RSA 新加密），由 04 决定是否发 plaintext 兜底。

---

## 6. 服务端契约（零参与声明）

**Erlang 后端在协议层切换中完全不参与**，仅做不透明 map 透传。明文契约清单：

| 服务端职责 | 客户端职责（本 ADR 范围） |
|---|---|
| `msg_c2c_logic` → stage → worker 全链路不解密、不解析 `e2ee` map | 协议选择、密钥生成、加解密、Registry 路由 |
| `imboy_policy:content_bearing_action/1` 对非空 action 一律放行（见 `imboy_policy.erl:185-190`，`content_bearing_action(<<>>)->true; (<<"message_edit">>)->true; (_)->false`）—— 实际上非空非 `message_edit` 的 action 返回 `false`，但 E2EE key 分发消息走的是「非空 action」路径，由更外层 `encrypted_message_body` 判定（`imboy_policy.erl:197-198`，只要 `e2ee` 是非空 map 即视为加密消息） | Olm prekey（`action='e2ee_olm_prekey'`，见 `olm_session_service.dart:18`）/ Megolm room key（`action='e2ee_room_key'`，见 `group_session_service.dart:42`）的分发消息，自动被服务端白名单覆盖，**切换协议后端零改动** |
| `e2ee` 列在 DB 中是 JSONB，schema 不约束内部字段 | metadata 字段演进由 05 管理，v1 字符串与 v2 三元组可共存 |

**关键不变量**（守护测试覆盖，见 §9）：

```
grep -rn "decrypt\|cipher" imboy/src --include="*.erl" | grep -i "e2ee\|payload"  # 应零命中
```

任何让服务端「看懂」某个协议字段的提案都违反本 ADR，必须先发新 ADR 取代本节。

---

## 7. 向后兼容策略

### 7.1 已落库旧消息

存量 DB 中 `msg_c2c.e2ee` 列存有 3 种 legacy 字符串。Registry 通过 `ProtocolSuite.fromMetadata`（§3.2）统一解析，**不需要数据迁移**：

- 旧消息读出 → `metadata['e2ee_suite']='OLM.V1'` → `fromMetadata` 命中 `case 'OLM.V1'` → `resolve('olm')` → `OlmProtocol.decrypt`；
- 同理 `'MEGOLM.V1'` / `'RSA-OAEP-256+AES-256-GCM'`。

### 7.2 新消息的写回格式

新发送的消息**同时写** v1 字符串与 v2 三元组，保证旧客户端（未升级 Registry）仍能解密：

```dart
metadata = {
  'e2ee_suite': suite.wire,   // 'OLM.V1' —— 兼容旧客户端
  'protocol': suite.protocol, // 'olm'    —— v2 三元组
  'version': suite.version,   // 1
};
```

**为什么双写而非只写 v2 三元组**：灰度发布期间新旧客户端混存。只写 v2 会让旧客户端 `decryptE2EEMessage` 走到最后的 RSA 兜底分支并解密失败。双写是过渡期成本最低的兼容方案；待全量升级后可在 05 演进中移除 `e2ee_suite` 字段。

### 7.3 元数据字段透传清单（每个协议需要的入参）

| 协议 | metadata 必备字段（除 suite 标识） | 来源 |
|---|---|---|
| Olm | `peer_uid`, `peer_device_id`, `message_type`（0=pre-key,1=normal） | `e2ee_service.dart:313-320` |
| Megolm | `session_id`, 可选 `gid`（无 gid 视为 `scope='c2c'`） | `e2ee_service.dart:332-344` |
| RSA | `nonce`, `keys[{did,kid,wrap_alg,ek}]` | `e2ee_service.dart:364-401` |

这些字段在 Registry 层不解释，由各 `E2eeSessionProtocol` 实现自己读写。

---

## 8. MLS 占位

本轮**不实现** MLS，但在 Registry 中保留条目，确保未来注册时业务层零改动（§4.2）。

```dart
// mls_protocol.dart —— 本轮不创建此文件，仅在 ADR 中记录契约
class MlsProtocol implements E2eeSessionProtocol {
  @override
  ProtocolSuite get suite => ProtocolSuite.mls; // ('mls', 0, 'reserved')

  @override
  Future<void> initialize(...) =>
      throw UnimplementedError('MLS reserved by ADR 02, not implemented this round');

  @override
  Future<E2eeCiphertext> encrypt(...) =>
      throw UnimplementedError('MLS not implemented');

  @override
  Future<String> decrypt(...) =>
      throw UnimplementedError('MLS not implemented');

  @override
  Future<void> clearAll() async {}; // no-op
}
```

**占位规则**：

- `ProtocolSuite.mls` 的 `version=0` 表示未发布版本；
- `Capability` 上报时 **不包含** `mls`（`all()` 只返回已 `register` 的实现，MLS 未 register）；
- 任何 metadata 中出现 `protocol='mls'` 的入站消息，`resolve` 会抛 `StateError`，业务层兜底为 `_e2ee_failed`。

**为什么留占位而非完全省略**：让 04 Capability Negotiation 的协议枚举、05 metadata 的版本号空间、08 Threat Model 的 MLS 评估位都能引用一个稳定标识，避免 MLS 落地时多份 ADR 同步修改。

---

## 9. 守护测试要求

下列测试用例必须存在，CI 强制运行。缺失任一项视为本 ADR 未落地。

### 9.1 接口与 Registry

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-02-01 | `resolve(metadata)` 对 3 种 legacy 字符串 + v2 三元组都能返回正确 impl | §3.2 兼容矩阵 |
| T-02-02 | `register` 同名 protocol 重复注册抛 `StateError` | §2.2 防冲突 |
| T-02-03 | `resolve` 未知 protocol 抛 `StateError`，不静默 fallback | §4.3 |
| T-02-04 | `all()` 返回的清单 = 已 `register` 的实现集合（未注册的 MLS 不出现） | §8 |

### 9.2 业务层零改动（01 §6 验收标准 2）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-02-05 | mock 一个 `FakeProtocol(suite=ProtocolSuite('fake',1,'x'))`，`register` 后业务层发送/接收能正常路由，**业务层代码不修改** | §4.2 MLS 落地标准 |
| T-02-06 | `grep -rn "OLM.V1\|MEGOLM.V1\|RSA-OAEP-256+AES-256-GCM" imboyapp/lib/` 在非 `*_protocol.dart` / 非 `protocol_suite.dart` 文件中**零命中**（只允许在 suite 定义与 fromMetadata 的 switch 中出现） | 01 §6 标准 1 |

### 9.3 向后兼容

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-02-07 | 一条 v1 落库消息（`e2ee_suite='MEGOLM.V1'`）经 Registry 解密，结果与 v1 `decryptE2EEMessage` 直接调用一致 | §7.1 |
| T-02-08 | 新发送消息的 metadata 同时含 `e2ee_suite`（v1 字符串）和 `protocol`/`version`（v2 三元组），旧客户端能解密 | §7.2 双写 |

### 9.4 服务端零密码学（08 §4 防御点「服务端零密码学」）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-02-09 | Erlang EUnit：`grep -rn "decrypt\|cipher" imboy/src --include="*.erl"` 与 E2EE payload 相关零命中 | §6 不变量 |
| T-02-10 | Erlang EUnit：一条 Olm prekey 分发消息（`action='e2ee_olm_prekey'`）与一条 Megolm room key 分发消息（`action='e2ee_room_key'`）均被 `imboy_policy` 放行（参考既有 `c2g_e2ee_room_key_relayed_opaque_and_skips_gate` 用例，见 `group_session_service.dart:31`） | §6 服务端白名单 |

### 9.5 防降级（08 T2）

| 测试 ID | 用例 | 验证点 |
|---|---|---|
| T-02-11 | `strictE2ee` 模式下，`RsaLegacyProtocol.encrypt` 抛 `UnsupportedError` | §5.2 约束 1 |
| T-02-12 | 对端 capability 不含 `olm` 时，单聊降级到 `megolm`；不含 `megolm` 时不降级到 `rsa` 新加密，而由 04 决定 plaintext 兜底 | §5.2 约束 3 |

---

## 10. 与其他 ADR 的关系

| ADR | 本 ADR 的依赖点 | 对方对本 ADR 的约束 |
|---|---|---|
| **01-overview** | §6 验收标准 1/2 是本 ADR §4.2、§9.2 测试的来源 | 总纲，无反向约束 |
| **03-device-identity** | `RecipientDevice.publicKey` 的具体含义（RSA PEM vs curve25519）由 03 定义；`initialize` 发布的 identity_key 受 03 trust 状态影响 | device identity 列冻结后，`RecipientDevice` 字段不得删 |
| **04-capability-negotiation** | `RecipientDevice.capabilities` 字段、`E2eeProtocolRegistry.all()` 是 04 协商的输入；fallback 顺序（§5.1）由 04 实际执行 | 04 决定何时降级、是否签名校验协商结果；本 ADR 只提供候选清单 |
| **05-metadata-version** | metadata 的 `e2ee_ver` 字段、双写策略（§7.2）由 05 演进 | 05 不得删除本 ADR §3.2 的 legacy 字符串解析（向后兼容硬约束） |
| **08-threat-model** | T2/T5 驱动 §5 协商顺序与防降级；T1/T3 驱动 §6 服务端零密码学；T9（rollback）由 ProtocolSuite.version 配合 03/05 单调度防御 | 08 演进新增威胁时，可能要求本 ADR 追加守护测试 |

**冲突仲裁**：本 ADR 冻结后，接口签名（§2.1）变更必须走 01 §5 的「supersedes」流程。`ProtocolSuite` 三元组结构、Registry `register/resolve/all` 签名、legacy 字符串兼容矩阵（§3.2）三项为**不可单方面变更**冻结项。

---

## 11. 决策摘要（一页速览）

| 决策点 | 选择 | 一句话理由 |
|---|---|---|
| 接口形式 | `abstract interface class`（Dart 3，无默认实现） | 强制每个协议自给自足，防止隐式继承 |
| 套件标识 | `(protocol, version, cipher)` 三元组 | 协议升级算法时可精确区分，不靠名字猜 |
| legacy 兼容 | `fromMetadata` 同时支持 v1 字符串与 v2 三元组 | 零数据迁移，灰度共存 |
| 路由方式 | `Registry.resolve(metadata)` 按 `protocol` key 查表 | 业务层与协议解耦 |
| 新消息写回 | `e2ee_suite`（v1）+ `protocol`/`version`（v2）双写 | 灰度期旧客户端可解密 |
| MLS | 注册占位 `('mls', 0, 'reserved')`，不实现 | 锁定标识，未来零改动落地 |
| 服务端 | 完全不透明透传 `e2ee` map | 服务端零密码学（T1/T3） |
| Fallback 顺序 | olm > megolm > rsa(仅 decrypt) | 安全性单调递减，T5/T2 驱动 |
| RSA 新加密 | strict/compliance 模式下禁用 | 防 T2 降级攻击 |
