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

## 容量压测（R0 后）

**bench_websocket.sh** 是容量压测工具，不是协议功能测试。R0 后必须同时验证客户端存活与服务端准入计数；仅有 HTTP 101 或客户端 sleep(HOLD) 结束，不能证明连接仍在线。

### 快速运行

    # did-less legacy token：压测工具为每条连接生成唯一 DID header。
    TOKEN="$(escript scripts/imboy_ctl user token <uid>)" \
      CONNS=100 HOLD=30 RAMP_RATE=50 \
      bash scripts/bench_websocket.sh

    # 设备绑定模式：每行一个 JWT；每个 (uid,did) 必须是服务端有效且不同的设备。
    TOKENS_FILE=/secure/bench-tokens.txt CONNS=500 HOLD=30 RAMP_RATE=50 \
      bash scripts/bench_websocket.sh

默认目标为 ws://127.0.0.1:9800/api/v1/ws（当前正式路由），默认从同主机 /metrics 读取 R0 指标。METRICS_URL 可指定内网指标地址。

### 鉴权模式

- TOKEN：只允许单 token 承载多连接的场景使用 did-less legacy JWT。工具解析 JWT payload 中的 uid/did（仅用于选择 DID header，不在客户端验证签名），为 legacy token 的每条连接分配本次运行唯一 DID，并在输出中标记 legacy-unbound。这不代表现代设备绑定登录链路的验证结果。
- 如果 TOKEN 已绑定非空 DID，且 CONNS>1，工具会在建立连接前拒绝运行，避免复用同一设备身份触发互踢并制造假并发。
- TOKENS_FILE：每行一个原始 JWT（允许空行和 # 注释），至少有 CONNS 行。绑定 token 的 (uid,did) 必须唯一；服务端仍会验证签名、有效期、设备记录和 session epoch。不要用仅调用 token_ds:encrypt_token(Uid, Did) 生成、但数据库中没有 active device 记录的 token 冒充有效设备凭证。

### 统计口径与通过条件

1. 按 RAMP_RATE 爬坡建立连接；任何成功连接都保持打开，直到全部连接尝试结束。
2. HOLD 从爬坡结束后开始计时，保证最后建立的连接也经历完整观察窗口。
3. 自动 Ping/Pong keepalive 从爬坡阶段开始运行，避免长时间 ramp 时早期连接因空闲超时被清理；保持窗口两端再做显式 Ping/Pong，并由接收任务检测服务端主动关闭和半开连接；held 只统计完整保持窗口通过的连接。
4. 默认读取 Prometheus 文本格式 /metrics，以 imboy_ws_admission_current 为服务端准入连接口径；对比压测前基线差值与客户端观察到的存活连接数，并记录 limit/rejected_total。
5. 关闭全部客户端连接后，等待并验证服务端准入计数回到压测前基线。连接数未回落、服务端增量不匹配或指标采样失败，均判定失败。
6. 默认 METRICS_REQUIRED=1。若明确只做客户端诊断，可设置 METRICS_URL=off METRICS_REQUIRED=0；输出会标记 CLIENT-ONLY; NOT CAPACITY EVIDENCE，不能用于容量结论。

建议在独占的隔离节点执行，避免其他客户端同时建立/关闭连接影响基线差值。METRICS_TOLERANCE 默认为 0；调大容差会降低证据强度。PING_INTERVAL 默认 15 秒、PING_TIMEOUT 默认 5 秒；METRICS_INTERVAL、CONNECT_PARALLELISM、CONNECT_TIMEOUT、CLEANUP_TIMEOUT 也可通过同名环境变量调整。

脚本自测：

    python3 -m unittest discover -s scripts/test -p 'test_bench_websocket.py' -v

## 覆盖率要求
帧编解码 90%;**"非快乐路径 × v2 编码"矩阵 100%**;ACK 全语义覆盖。

## 验收标准
- [ ] 帧向量三端字节级对齐(金标 fixture)
- [ ] C2S ACK/C2G 错误在 v2 可达客户端(ARCH-02 回归)
- [ ] ACK 大小写不敏感、多设备送达
- [ ] bad magic/Ver≠2/0x28 帧拒收有测试
- [ ] ws_url 契约、刷新链有测试
- [ ] protobuf 幻影枚举被契约门拦
