# Protobuf 代码生成 / Protobuf code generation

本目录处理 WebSocket 的 Protobuf 载荷生成；不生成 REST OpenAPI 客户端。
REST 请求与响应类型以对应受众的 `openapi-*.yaml` 为准。
These scripts generate Protobuf payloads, not REST clients.

| 命令（在后端仓库根运行） | 输入 | 输出／依赖 |
|---|---|---|
| `bash api/codegen/erlang.sh` | `src/imboy.proto` | 委托 `make compile`，生成后端 `.erl` / `.hrl`；需要项目 Erlang 工具链 |
| `bash api/codegen/dart.sh` | `api/proto/imboy.proto` | 委托 `../imboyapp/scripts/regen_protobuf.sh`；需要 `protoc` / `protoc-gen-dart` |
| `bash api/codegen/typescript.sh` | 同上 | `../imboyadmin/src/api/_gen/proto/`；需要 `protoc` / `protoc-gen-ts_proto` |

`api/proto/imboy.proto` 是后端源文件的符号链接，见 [Protobuf 说明](../proto/README.md)。
运行前确认相邻仓库及工具已存在；生成脚本会写入对应仓库，按各仓规则审查差异。
Generation writes into the target repositories. Review those changes separately.

TypeScript 脚本设置 `forceLong=string`，避免实体 TSID 精度损失。
普通消息 `id` 本身就是字符串；JSON 专属字段和 E2EE 的无损编码限制见
[`imboy_codec.erl`](../../src/lib/imboy_codec.erl)。Protobuf 生成通过不代表
REST 契约、跨端通信或真机验收通过。
