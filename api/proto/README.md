# WebSocket Protobuf 载荷 / Protobuf payload

`imboy.proto` 是指向 [`src/imboy.proto`](../../src/imboy.proto) 的符号链接。
编辑该源文件；不要维护第二份可编辑副本。
`imboy.proto` links to the backend compiler input. Edit the source, not a duplicate.

Protobuf 定义 `IMBoyMessage`、枚举及业务载荷；`imboy.v2` 的九字节二进制帧头
由 [`imboy_frame.hrl`](../../include/imboy_frame.hrl) 和
[`imboy_frame.erl`](../../src/lib/imboy_frame.erl) 定义，二者不是同一种封装。
`imboy.v2` 业务帧也支持 JSON 载荷，详见
[协议契约](../../docs/reference/ws-protocol-contract.md) 与
[编解码器](../../src/lib/imboy_codec.erl)。

普通业务消息 `id` 为不透明字符串，实体用户／群 ID 在 Protobuf 中为 `sint64`。
JSON 的实际字段类型以对应接口和编解码实现为准；JavaScript 必须无损处理大整数。
Opaque message IDs and entity TSIDs are different types. Preserve integer precision.

生成入口及工具要求见 [codegen/README.md](../codegen/README.md)。语法检查：

```bash
protoc --proto_path=api/proto --descriptor_set_out=/dev/null api/proto/imboy.proto
```
