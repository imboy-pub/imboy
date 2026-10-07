# C11 真实后端传输接线合同

本合同补充 AC-22/AC-23 的实现前置，不能替代真实旅程验收。源文件和候选绑定见同名 JSON；未执行网络请求，也未创建账号或占用设备。

## 已核对的生产行为

- `websocket_handler.erl:init/2` 从 `authorization` 读取认证，从 `did/cos/vsn` 读取设备与版本；查询参数 token 是回退入口。新 harness 使用显式请求头，不把令牌放在 URL、日志或证据中。
- App `websocket.dart` 请求 `imboy.v2` 等子协议，并在连接 ready 后识别实际 framing。真实 harness 必须记录实际协商结果，不能把 JSON 内容格式 v2 与 WebSocket 二进制 framing v2 混为一谈。
- `message_ds:validate_message/1` 要求非空 id/type，C2C/C2G 内容还需 peer 字段、msg_type 和合法外层 E2EE 信封。C2G 消息 ID 最长 40 字节。协议对象应由生产编码入口生成，不能把 WireFrame.body 直接假设为服务器完整消息。
- Handler 从认证状态取得 CurrentUid，并注入和盖章 sender_did。攻击测试可以注入伪造值，但正常接线不能信任客户端提交的 sender_did。
- `msg_c2c_logic` 把 to 转为整数账号 ID。`SynthAccount.uid` 的 c11-* 字符串只是夹具别名，不能直接作为真实账号 ID。
- App `sendMessage` 返回 true 表示写入本地 channel；服务端 ACK 由后端业务逻辑异步产生。这个 bool 不能映射为 DeliveryReceipt.accepted。

## 必须新增的注册夹具绑定

每轮 fixture seed 后产生仓外运行绑定：run_id、合成账号别名到实际账号 ID、设备别名到注册 DID/platform、群别名到实际群 ID、秘密令牌引用、隔离 HTTP/WS 有效地址、配置与 fixture 指纹。令牌值不写入报告。账号 ID 保留精确整数语义，不经浮点转换。

加载时拒绝未绑定别名、重复 DID、设备归属冲突、错轮 fixture、缺少令牌、默认或共享后端地址。调用方不能复用 App 全局当前用户来替代绑定，否则可能使用现有账号。host-simulated 设备不能产出 Android/macOS 真机 PASS。

## 传输接口语义及实现顺序

1. 先实现独立、不可变的绑定模型与纯离线解析测试，覆盖上述拒绝路径；保留默认 RealBackendTransport 拒绝运行，直到真实实现完成。
2. 连接每个显式设备，等待认证成功与协议 ready；服务器错误帧、关闭、超时不能视为已连接。设备连接生命周期独立，旧连接回调不能覆盖新连接。
3. 用生产序列化入口产生报文，message_id 关联服务端确认。发送超时记录 UNKNOWN/未确认，禁止伪造 accepted 或自动把其当 offline。离线收件人判定需要明确服务器语义，不能凭本地连接表推断。
4. 接收原始帧并解码，验证收件账号/设备和 sender context；保留收到、认证解密、UI 显示三层 oracle。drainInbox 只能读取真实已收帧，不能本地回灌发送报文。
5. 离线恢复、重连、多设备、群加入/踢出/重入与附件均通过真实后端入口。群旅程不能把 C2C recipientUid 接口直接当群协议；需显式群操作和成员能力断言。
6. 记录逐消息发送/ACK/接收/解密/显示关联、候选与运行指纹，保留所有失败尝试，再由独立终审映射 AC-22/AC-23。

## 接口仍需拆开的结果

现有 DeliveryReceipt 三枚举不能表达 ACK 超时和未知结果。实施时新增明确的未确认结果或类型化异常；服务器业务拒绝与基础设施失败分开。send 的返回结果不是收件人交付证明，端到端完成必须由独立接收 oracle 确认。

## 验证门与授权边界

离线模型、编码和生命周期测试可以先完成；mock 仅检验协议适配，不产出真实旅程 PASS。连接隔离后端、写入合成 fixture、运行真机和 MITM/五面取证必须使用已确认资源授权。共享 4323、现有账号和默认设备均不可使用。身份透明度与 MLS/PQ 决策缺失仍按原计划保留，不通过修改本合同降低范围。
