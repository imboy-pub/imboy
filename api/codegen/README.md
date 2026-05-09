# `imboy/api/codegen/` — 三端代码生成脚本

> **关联**：`.claude/plans/quality-loop.md` v1.3 T3.4
> **创建**：2026-05-09 / iteration 69

---

## 脚本清单

| 脚本 | 目标仓 | 输入 | 输出 | 工具链 |
|------|-------|------|------|--------|
| `erlang.sh` | imboy（自身）| `api/proto/imboy.proto` (symlink) | `imboy/src/imboy_pb.erl` + `imboy/include/imboy_pb.hrl` | `gpb`（erlang.mk 内联，见 Makefile:101 `compile_proto.erl`） |
| `dart.sh` | imboyapp | 同上 | `imboyapp/lib/service/protocol/imboy.pb.dart` 等 | `protoc-gen-dart`（pub global activate protoc_plugin）|
| `typescript.sh` | imboy-admin-frontend | 同上 | `imboy-admin-frontend/src/api/_gen/proto/*.ts` | `protoc-gen-ts_proto`（npm i -g ts-proto）|

**注**：dart.sh 调用 imboyapp 仓内已存在的 `imboyapp/scripts/regen_protobuf.sh`（避免重复造轮子）。

---

## 工具前置就位

| 工具 | 安装命令 | 检测 |
|------|---------|------|
| protoc | `brew install protobuf` | `protoc --version`（已就位 34.1）|
| protoc-gen-dart | `dart pub global activate protoc_plugin` | `which protoc-gen-dart`（已就位 ~/.pub-cache/bin/）|
| protoc-gen-ts_proto | `npm i -g ts-proto` 或 `bun add -g ts-proto` | `which protoc-gen-ts_proto`（**待装**）|

---

## 调用方式

```bash
cd imboy

# 后端（imboy 自身）— 由 erlang.mk 接管
bash api/codegen/erlang.sh

# Flutter 客户端
bash api/codegen/dart.sh

# React 管理后台
bash api/codegen/typescript.sh
```

**或一键三端**：

```bash
cd imboy && \
  bash api/codegen/erlang.sh && \
  bash api/codegen/dart.sh && \
  bash api/codegen/typescript.sh
```

---

## v1.1 路径约定

主计划 v1.1 §324 规定：codegen 输出通过**相对路径**写到邻居仓：

```
imboy/api/codegen/dart.sh        →  ../../imboyapp/...
imboy/api/codegen/typescript.sh  →  ../../imboy-admin-frontend/...
```

每个脚本启动时校验邻居仓存在；缺失时友好报错并退出，不静默失败。

---

## 输出位置策略（实施时调整）

主计划 v1.0 假设输出到 `lib/api/_gen/` / `src/api/_gen/`，但 imboyapp 已有 `imboyapp/lib/service/protocol/`（业务在用）。决策：

- **dart.sh** → 复用 `imboyapp/lib/service/protocol/`（与 imboyapp/scripts/regen_protobuf.sh 一致）
- **typescript.sh** → 用 `imboy-admin-frontend/src/api/_gen/proto/`（admin 无现成位置）
- 输出目录在 `.gitignore` 或受 lint exclude 保护（v1.3 T2.3 已配置）

---

## 修改流程

1. 编辑 `imboy/src/imboy.proto`（symlink 自动同步至 `api/proto/imboy.proto`）
2. `cd imboy && make compile` — Erlang 自动生成 imboy_pb.erl/hrl
3. `bash api/codegen/dart.sh` — 重新生成 Flutter 端
4. `bash api/codegen/typescript.sh` — 重新生成 admin 端
5. 三端测试 + commit
