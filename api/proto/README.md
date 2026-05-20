# `imboy/api/proto/` — IMBoy WebSocket Protobuf 契约真源

> **关联**：`.claude/plans/quality-loop.md` v1.3 T3.3
> **协议**：imboy.v2 frame（自定义 binary WS frame 包裹 protobuf payload）
> **创建**：2026-05-09 / iteration 68 / T3.3 落地
> **业务设计文档**：`imboy/proto/README.md`（171L 历史，保留作业务参考）

---

## 文件清单

```
imboy/api/proto/
├── README.md                # 本文件（机制层）
└── imboy.proto              # symlink → ../../src/imboy.proto（v2 frame 真源）
```

---

## 真源策略（关键）

**当前文件是 symlink，不是独立副本**：

```
imboy/api/proto/imboy.proto  →  ../../src/imboy.proto  (360 行 / 31 messages)
```

### 为什么 symlink

`src/imboy.proto` 是 **erlang.mk 编译输入**（不能动）：

- `imboy/Makefile:104` 配置 `{i, "src"}` → gpb include 路径
- `imboy/Makefile:108` 配置 `{o_erl, "./src"}` → 生成 `imboy_pb.erl` 输出位置
- `imboy/Makefile:107` 配置 `{o_hrl, "./include"}` → 生成 `imboy_pb.hrl` 输出位置

如果在 `imboy/api/proto/` 复制一份 imboy.proto，会出现「双写漂移」（修一处忘修另一处）。

**symlink 让两边强同步**：
- 修改 `src/imboy.proto`（编译需要的位置）→ `api/proto/imboy.proto` 自动同步
- T3.4 codegen 脚本可以从 `api/proto/imboy.proto` 读取，无歧义

### 旧位置 `imboy/proto/imboy.proto`

历史副本（与 `src/imboy.proto` 内容完全相同，`diff` 验证 identical）。**保留不删**：
- 角色未明（可能是手写真源、IDE 用、或文档展示）
- 删除前需 grep 确认无引用
- 计划 W4 阶段评估清理

---

## 协议结构（v2 frame）

`imboy.proto` 已含 31 messages + 多个 enum：
- 顶层封装：`IMBoyMessage`（envelope，含 frame metadata + bytes payload）
- 方向枚举：`MsgDirection`（C2S / S2C / Bidi）
- 内容枚举：`ContentType`（text / image / audio / video / file 等）
- 业务动作：`S2CAction`（27 种 server→client 推送类型）

详见：
- `imboy/proto/README.md` — 业务设计文档（171L，含 JSON→protobuf 替换思路、使用例）
- `imboy/docs/api/websocket-api-2.md` — WS 协议人类可读规约

---

## TSID 类型约定

按 `imboy/docs/CONVENTIONS.md` §1：
- TSID 在 protobuf 中为 `sint64`（与 PostgreSQL `BIGINT` 对齐）
- 跨端 JSON 传输时 protoc 生成的 ts/dart 代码需手动包装为 `string` 处理
- 见 `imboy.proto` 注释「TSID IDs: sint64 (matching PostgreSQL BIGINT)」

---

## 拆分计划（暂缓）

主计划 v1.0 T3.3 提议把单一 `imboy.proto` 拆为多个 .proto：
- `imboy_v2_frame.proto`（顶层封装）
- `imboy_s2c.proto`（27 种 S2C action payload）
- `imboy_types.proto`（共享 enum + 基础类型）

**当前不拆**：
- 单文件已工作（2026-04-10 完成 v2 frame 后稳定运行）
- 拆分需重新组织 erlang.mk 编译流程（多文件 import 处理）
- T3.6 codegen 验收时再决定（如三端生成代码冲突再考虑）

---

## codegen 入口（T3.4 待写）

- Erlang：`imboy/Makefile` 已有 `compile_proto.erl`（gpb plugin 内联），无需额外脚本
- Dart：`api/codegen/dart.sh` → `protoc --dart_out=../../../imboyapp/lib/api/_gen/proto api/proto/imboy.proto`
- TypeScript：`api/codegen/typescript.sh` → `protoc --plugin=protoc-gen-ts_proto --ts_proto_out=../../../imboy-admin-frontend/src/api/_gen/proto api/proto/imboy.proto`

---

## 验证

```bash
cd imboy
protoc --proto_path=api/proto --descriptor_set_out=/dev/null api/proto/imboy.proto
# 退出码 0 = 语法 OK
```

---

> **修改流程**：编辑 `src/imboy.proto`（symlink 自动同步） → `make compile`（gpb 重新生成 imboy_pb.erl/hrl）→ T3.4 之后再跑 codegen 同步三端
