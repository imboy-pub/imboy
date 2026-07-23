# WebSocket 测试（WebSocket Testing）

## 为什么需要
WS 是 IM 命脉,也是评审漂移重灾区:C2S ACK 丢 type、C2G_ERROR 蒸发、`endsWith('_ACK')` 大小写脆弱、默认 ws_url 404、protobuf 幻影枚举、in_reply_to 丢失。这些全在"非快乐路径 × v2 编码"矩阵,必须专项覆盖。

## 覆盖范围
- 帧协议:v2 帧(magic 0x4942/Ver=2 守护/flags 位)、bad magic 拒收、Ver≠2 拒收、帧类型 0x28 回 ERROR
- 编解码:protobuf ↔ JSON 双路、to_pb_map 字段完整性(in_reply_to/error/code 不蒸发)、MsgDirection 枚举外 type 不归零
- 投递:C2C/C2G/S2C 投递、多设备按设备送达、离线拉取、in_reply_to 回显
- ACK:CLIENT_ACK/SERVER_ACK/CONFIRM 语义、大小写不敏感、C2S 出站确认、WEBRTC_SERVER_ACK
- 错误路径:禁言/非成员/限流/@all 拒发的错误帧可达客户端
- 连接:握手子协议协商、token 校验、重连、心跳、please_refresh_token/401 刷新链
- 契约:三端帧字节级对齐

## 推荐框架
- 后端:CT + 真 WS 连接(gun 客户端)+ 真帧编解码;`imboy_frame`/`imboy_codec` 单元 EUnit
- 跨端契约:金标帧向量(fixture)三端共用断言
- 诊断:现有 `bench_websocket.sh`、`websocket_diagnose.sh`

## 目录结构
```
imboy/test/api/websocket_handler_tests.erl + CT suite
imboy/test/lib/imboy_frame_tests.erl / imboy_codec_tests.erl
imboy/test/fixtures/frame_vectors/(三端共享金标)
imboyapp/test/service/(websocket.dart 解码测试)
```

## Mock 策略
帧编解码零 mock(真字节);连接层可 mock syn 注册测投递路由,但**编码路径必须真实**(否则 type/字段蒸发测不出)。

## Fixture 策略
金标帧向量:每种 type 的 v2 帧字节序列作 fixture,三端(后端/Flutter/SDK)共用同一组向量断言编解码一致。错误路径向量(C2G_ERROR/拒发)专门覆盖。

## 数据准备
真连接需真 token(`token_ds:encrypt_token`)、播种在线设备。多设备用例造同用户多 did。

## CI 执行方式
帧单元 Stage 1;WS 集成(真连接)Stage 2;三端帧向量 diff Stage 1 契约门。

## 覆盖率要求
帧编解码 90%;**"非快乐路径 × v2 编码"矩阵 100%**;ACK 全语义覆盖。

## 验收标准
- [ ] 帧向量三端字节级对齐(金标 fixture)
- [ ] C2S ACK/C2G 错误在 v2 可达客户端(ARCH-02 回归)
- [ ] ACK 大小写不敏感、多设备送达
- [ ] bad magic/Ver≠2/0x28 帧拒收有测试
- [ ] ws_url 契约、刷新链有测试
- [ ] protobuf 幻影枚举被契约门拦
