# P0-A · WebRTC 跨网络 relay 不通 — 根因定位与修复方案

> 版本 2026-07-02 | 方法：后端主会话自查 + 客户端只读 agent trace，全程未改代码。
> 执行分工：本文档由 Fable 出方案，实现由 glm-5.2 盲执行。
> **验证纪律**：本项目有工具输出污染史。下列结论分「确证（静态代码可断定）」与「存疑待验（需真机抓包/生产核实）」两级，glm 执行前须按验收 gate 现场复验。

---

## 核心判断（先读）

> **2026-07-02 二次深挖修正**：一轮更彻底的客户端接收链 trace 推翻了初版排序。真根因是 **WRTC-00 客户端事件总线信令类型断链**（纯客户端、静态可证、最小改动），而非 TURN 配置或 glare。TURN 配置（WRTC-01）降为并存隐患，glare（WRTC-04）基本排除为主因。下列按修正后优先级排列。

症状 `relay permission=0 / Relayed=0KiB` 的**最直接、且静态可确证的解释是：主叫收到 answer 后死在客户端事件总线类型不匹配，永远不设置 remoteDescription，ICE agent 无 checklist，即便 TURN Allocate 成功也永不对任何远端候选发 CreatePermission**（见 WRTC-00）。理由链：

1. 后端信令链路已确证**纯透传**（offer/answer/candidate 走同一 `webrtc_ws_logic:event/4`、不解析不过滤不丢弃），排除"后端丢候选"。
2. 客户端接收链断在 `message_webrtc.dart:128` 发 `AppEventBus.fireData()`（类型 `DataWrapperEvent`）而通话页 `page:245` 订阅 `on<WebRTCSignalingEvent>()`——**类型不匹配、零生产者、answer/candidate 全部进黑洞**。此断链自 `c82e15eb`（2026-01-12 GetX→事件总线重构）起存在，因客户端信令收链无测试，六个月未被发现。
3. glare（answer 双触发）有 `makingAnswer` 守卫挡并发，非并发时第二次 createAnswer 在 stable 态被底层拒绝落入 catch，不产生第二个 answer 帧——**不解释 permission=0**（且该 bug 需信令先能到达，而 WRTC-00 证明到不了）。

**排查顺序 = 先证 WRTC-00（10 分钟真机日志定案，可证伪）→ 修复后必然暴露 WRTC-02（响铃期候选丢失）→ 再验 WRTC-01 配置兜底 → WRTC-03/04 次要**。

---

## WRTC-00 客户端事件总线信令类型断链（真根因）  [未完成/架构] [CRITICAL — 静态确证]

- **根因（确证 · 静态可秒验）**：主叫接收 answer/candidate 的链路断在事件类型不匹配：
  - 生产端：`imboyapp/lib/service/message_webrtc.dart:128` `handleWebRTC` 对 answer/candidate/ringing/busy/bye 调 `AppEventBus.fireData(msgModel)` → 包装成 `DataWrapperEvent<WebRTCSignalingModel>`。
  - 消费端：`imboyapp/lib/page/chat/p2p_call_screen/p2p_call_screen_page.dart:245` `AppEventBus.on<WebRTCSignalingEvent>().listen(...)`。
  - `event_bus_plus` 的 `on<T>()` 严格按类型过滤，`DataWrapperEvent ≠ WebRTCSignalingEvent`。核实命令：`grep -rn "WebRTCSignalingEvent" lib/` → **仅 2 处命中**（类定义 `common_events.dart:530` + 上述订阅），**零 `AppEventBus.fire(WebRTCSignalingEvent(...))` 生产者**。
  - 唯二的 `DataWrapperEvent` 消费者（`chat_event_subscription_manager.dart`、`conversation_page.dart`）只过滤 message 类型，`dataType='WebRTCSignalingModel'` 直接 return。→ **answer 与全部 candidate 到达设备后进黑洞**。
- **为何 offer 不受影响（解释"能响铃、连不通"）**：offer 走 `message_webrtc.dart:86-101` 的 `incomingCallScreen` 弹窗直传，接听后 offer 经 `widget.option` 直传 `page:268`——**不经这条断链**。只有 answer/candidate 依赖事件总线，故被叫能响铃、主叫永远收不到 answer。
- **机理（permission=0/Relayed=0 的完美吻合）**：主叫无 `remoteDescription` → ICE agent 无 checklist → 即便 TURN Allocate 成功也永不发 CreatePermission → 两端 permission=0、Relayed=0KiB。
- **可证伪点（10 分钟真机定案）**：此 bug 预测**同 LAN 通话在当前构建上同样连不通**（主叫收不到 answer，answerTimer 30s 超时挂断）。若真机同 LAN 能通 → 要么跑旧构建（本项目有旧构建误判前科，测前必 `flutter clean` 重编），要么本项不成立。
- **影响范围**：所有音视频通话（跨网+同网），六个月稳定复现。
- **修复方案（最小改动）**：`message_webrtc.dart:128` 把 `AppEventBus.fireData(msgModel)` 改为 `AppEventBus.fire(WebRTCSignalingEvent(data: data))`——`handleWebRTC` 已持有原始 `data` map，与 `page:250` 的 `WebRTCSignalingModel.fromJson(obj.data)` 契约对齐。**须先真机确认 page 订阅侧 `obj.data` 期望的字段结构与 `data` 一致**（agent 判断契约已对齐，glm 落地前核实 fromJson 字段）。
- **边界**：不改后端（后端透传正确）；不动 offer 的 incomingCallScreen 直传路径；不动 glare 仲裁；改动仅此一行 + 补客户端信令收链测试（六个月无测试是断链未被发现的根本原因）。
- **验收 gate**：
  1. `flutter analyze` 零问题。
  2. **真机主叫日志（BLOCKED 真机）**：修复前 `[WS] msg type=WEBRTC_ANSWER`（websocket.dart:701）出现但 `> rtc onMessageP2P`（provider:203）从不出现 = 断链实锤；修复后两条都出现 = 修复生效。
  3. **真机端到端（BLOCKED 真机）**：跨网络通话建连成功、Relayed>0。
  4. 新增客户端信令收链 widget/单元测试：断言 `handleWebRTC` 收到 answer 后 `WebRTCSignalingEvent` 被 fire（防回归）。
- **glm 执行陷阱**：真机验证（禁模拟器）+ 测前 `flutter clean` 重编（防旧构建假象）；改动前核实 `WebRTCSignalingModel.fromJson` 字段与 `data` map 一致；颜色/间距若涉 UI 走 token。
- **分工**：改一行 **[MODEL] glm 可执行**；端到端确认 **[BLOCKED 真机]**。
- **回滚条件**：单行改动，若真机日志显示 `onMessageP2P` 仍不出现（说明 `data` 契约不匹配或另有断点），回滚并深挖 `WebRTCSignalingModel.fromJson`。

---

## WRTC-01 TURN 凭据/配置断链（并存隐患，非稳定 relay=0 主因）  [性能/运维] [HIGH — 待现场核实]

> 修正：本项从初版"头号嫌疑"降为并存隐患。WRTC-00 才是稳定 permission=0 的真根因。但若 WRTC-00 修复后 relay 候选仍为 0，本项即成主因，故保留完整方案。

- **根因（确证 · 代码结构）**：后端有**两条互不联动的 ICE 下发路径**：
  - 路径 A（静态，无 TURN）：`GET /v1/app/ice_servers` → `app_feature_handler:ice_servers/2`（`src/api/app_feature_handler.erl:40-42`）→ `config_ds:ice_servers/0`（`src/ds/config_ds.erl:58-65`）。读 `{imboy, ice_servers}`，而 `config/sys.config:123-133` 的 TURN 段**整段被注释**，只剩两条 Google STUN。
  - 路径 B（动态，有 TURN）：`GET /v1/user/credential` → `user_handler:credential/2`（`src/api/user_handler.erl:178-181`）→ `user_ds:webrtc_credential/1`（`src/ds/user_ds.erl:99-125`）。用 `eturnal_turn_urls`/`eturnal_stun_urls`/`eturnal_secret` + HMAC-SHA1 合成 24h 短期凭据。
  - 客户端**确认走路径 B**（`imboyapp/lib/config/const.dart:111` `turnCredential = '/api/v1/user/credential'`），所以两路径耦合无 bug。
- **根因（确证 · 配置脆弱性）**：`eturnal_turn_urls` / `eturnal_stun_urls` **没有环境变量 override**——`src/lib/imboy_env.erl:82-140` 的 override 清单里只有 `IMBOY_ETURNAL_SECRET → eturnal_secret`（`:111`），**没有 `eturnal_turn_urls` 的 override**。核实命令：`grep -n "eturnal\|TURN" src/lib/imboy_env.erl` → 仅 `:27,:111` 两行，均指 secret。
  - 后果：生产 TURN 地址列表**只能靠改 `sys.config` 落盘或运行时 `application:set_env` 手动注入**（项目记忆 `project_eturnal_webrtc_turn_deploy` 印证"两节点 set_env 热生效+落盘 release sys.config"）。任何一次 relx 重新发布 / sys.config 未正确落盘 / 手动 set_env 未持久化，`eturnal_turn_urls` 就回落到默认 `[]` → `webrtc_credential` 返回**空 turn_urls** → 客户端只有 STUN → 跨 NAT 无 relay → `Relayed=0KiB`。
- **根因（存疑待验 · 需生产核实）**：`user_ds:webrtc_credential/1:104-109` 有防伪造保护——若 `eturnal_turn_urls` 非空但 `eturnal_secret` 为空，返回 `{"error":"eturnal_secret_not_configured", "stun_urls":...}`（客户端识别为空 → 降级纯 STUN，`p2p_call_screen_provider.dart:983-996` `_stunOnlyIceConf`）。**需现场核实生产当前 `eturnal_turn_urls` 与 `eturnal_secret` 是否真非空、且 secret 与 eturnal 服务端逐字节一致。**
- **影响范围**：所有跨网络（非同 LAN）音视频通话；同 LAN 因 host 候选可直连不受影响（解释"部分能通"）。
- **修复方案**（分两步，先诊断后加固）：
  1. **诊断（零改动，先做）**：见验收 gate 的 curl + trickle-ice 步骤，确认是"配置为空"还是"凭据被拒"还是"端口被挡"。
  2. **加固（代码）**：给 `eturnal_turn_urls`/`eturnal_stun_urls` 补 env override，消除"靠手动 set_env"的脆弱性：
     - `src/lib/imboy_env.erl:override_from_env/0` 新增两个 override：读 `IMBOY_ETURNAL_TURN_URLS` / `IMBOY_ETURNAL_STUN_URLS`（逗号分隔 → 转 list），写入对应 app env。参照同文件 `override_binary_key/2` 模式，但 URL 是 list 需新写一个 `override_list_key/2`（split on `,`，trim，转 binary list）。
     - `src/imboy_app.erl:437-449` 的 `ensure_eturnal_secret_if_turn_configured/0` 已有启动校验，保持不动（它是正确的护栏）。
- **边界**：不改 `user_ds:webrtc_credential/1` 的 HMAC 算法与防伪造分支（正确）；不动 `config_ds:ice_servers/0` 静态路径（客户端不用它，但保留作 fallback，勿删）；env 名须与 `deploy/` 的 `.env.example` 同步登记。
- **验收 gate（可机器判定）**：
  1. `IMBOY_ETURNAL_TURN_URLS="turn:106.53.76.53:3478?transport=udp,turn:106.53.76.53:3478?transport=tcp" make run`，启动后 `make ctl ARGS="..."` 或远程 console `application:get_env(imboy, eturnal_turn_urls)` 返回非空 list。
  2. **真机/curl 诊断（BLOCKED 真机）**：持合法 JWT `curl -s https://<域名>/api/v1/user/credential -H "cookie: <jwt>"` → 返回 JSON 含非空 `turn_urls` + `username` + `credential`（不是 `error` 字段）。
  3. **relay 连通性（BLOCKED 真机）**：把返回的 turn_urls/username/credential 填入 https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/ ，点 Gather → 候选列表出现 `typ relay` 行。若无 relay 行 = TURN 不通（转 eturnal 服务端排查：`journalctl -u eturnal`、relay 端口 50000-50500 ufw 放行、腾讯云安全组）。
- **glm 执行陷阱**：改 `imboy_env.erl` 后 `make compile` 验证 list 解析语法；env 名同步 `deploy/.env.example`；erlfmt/DCO 常规；诊断步骤须真机（禁模拟器）。
- **分工**：诊断 **[BLOCKED 真机]**（curl+trickle-ice 需生产环境+真机）；env override 加固 **[MODEL] glm 可独立执行**。
- **回滚条件**：env override 是纯加性（不配则行为不变），无回滚风险；若诊断证明生产配置本就正确非空，则本项降级为"加固预防"，relay=0 根因转 WRTC-02/03。

---

## WRTC-02 信令单次投递无重试 + 多端广播无设备定向  [架构/性能] [HIGH — 确证]

- **根因（确证）**：`webrtc_ws_logic:event/4`（`src/logic/webrtc_ws_logic.erl:37-40`）用 `MsLi = [0]` 调 `message_ds:send_next/4`。核实链路：`message_ds.erl:74` 的 `send_next_loop` 在 `Delay=:=0` 分支走 `erlang:start_timer(0, Pid, Msg)`——**单次立即投递、无重试、无 ACK**（普通聊天消息走 `[2000,5000,7000,11000]` 四次重试+离线转存，webrtc 信令不走这条兜底）。
  - `Pid` 来自 `imboy_syn:list_by_uid(ToUid)`（`message_ds.erl:75`），`DIDLi=[]` → **广播到 ToUid 全部在线设备**，不定向到通话对端设备。
- **影响范围**：(a) 目标 Pid 因连接抖动/syn 注册未完成而瞬时不可达时，offer/answer/**candidate 静默丢失且不补投** → ICE 收集不全 → 可能 relay/srflx 候选缺失（与症状方向一致，但非 relay=0 的充分成因）。(b) 多端在线时信令扇出到所有设备，非目标 session 侧 `webRTCSessions[sid]` 查空跳过——但 `answer` 分支 `s2!.pc` 强解包若 sid 未命中会抛异常（客户端潜在崩溃点，见 WRTC-03 附注）。
- **修复方案**（二选一，需拍板见 D）：
  - **方案 A（低风险，推荐）**：webrtc 信令保留单次投递语义（通话要求双方在线，离线转存无意义），但**给 candidate 加有限重试** `MsLi=[0, 500]`（0ms + 500ms 补投一次），吸收 syn 注册抖动。offer/answer 同理。改 `webrtc_ws_logic.erl:39` 的 `MsLi`。
  - **方案 B（治本，工作量大）**：信令消息带 `device_id` 定向投递（前端 offer 里带主叫 device_id，后端 `send_next/6` 用 `DIDLi=[目标did], IncludeDIDLi=true` 定向）。需前后端协议同步改动。
- **边界**：不改 send_next/send_next_loop 通用逻辑（影响全量消息）；只改 webrtc 信令这一处的 MsLi 参数（方案 A）或新增定向字段（方案 B）；不动好友/黑名单校验。
- **验收 gate**：`make compile && make eunit` 绿；新增 `test/logic/webrtc_ws_logic_tests.erl` 断言 event/4 对在线对端调用 send_next 时 MsLi 含重试元素（方案 A）；真机跨网络通话 candidate 到达率提升（需真机抓 WS 帧计数，BLOCKED）。
- **glm 执行陷阱**：erlfmt/DCO 常规；方案 B 涉及客户端改动须真机验证。
- **分工**：方案 A **[MODEL] glm 可独立执行**（改一处 MsLi + 补 eunit）；方案 B **需 Fable 出前后端协议映射**。
- **回滚条件**：方案 A 单行改动，eunit 失败即回滚；若真机证明抖动丢包非主因（WRTC-01 才是），本项降为预防性加固。

---

## WRTC-03 SDP glare · _createAnswer 双触发竞态  [架构] [MEDIUM — 存疑待验]

- **根因（确证 · 双调用点存在）**：`imboyapp/lib/page/chat/p2p_call_screen/p2p_call_screen_provider.dart` 中 `_createAnswer` 有两个调用点，都挂在处理入向 offer 的同一事件链：
  - 触发点①：`case 'offer':` 分支内，`setRemoteDescription(offer)`（`:257`）+ 消费缓冲候选（`:261-266`）后**显式** `await _createAnswer(...)`（`:268`）。
  - 触发点②：`createSession` 注册的 `pc.onSignalingState`（`:522-528`）在 state 变为 `have-remote-offer` 时调 `_createAnswer(...)`——而 `setRemoteDescription(offer)` 正会触发此状态迁移。
- **根因（存疑待验 · 竞态窗口）**：现有 `makingAnswer` 布尔互斥（`:374-409`），但：(a) 它是 **provider 实例级**单字段（非 per-session，`:115-119`）；(b) 重置 `makingAnswer=false` 在 `finally` 且发生在 `sendWebRTCMsg(...)` **调用后未 await 完成**，保护窗口极短；(c) 触发点①在触发点②之前额外 `await` 了候选消费循环，拉长两次调用时间差，增大"第一次已跑完重置锁、第二次不被拦截"的竞态窗口。**能否真正双触发取决于 flutter_webrtc 平台通道事件投递时序，静态代码无法断定，须真机时间戳日志坐实。**
- **影响范围**：若双触发，会产生两次 `createAnswer/setLocalDescription`，第二次可能覆盖/扰乱本地 SDP，导致 ICE 重启或协商错乱（表现为偶发连不上，**非 relay=0**）。
- **修复方案**（拍板后择一，需先真机确诊）：
  1. **确诊（先做）**：在 `_createAnswer` 入口和 `makingAnswer` 每次 true/false 转换打时间戳日志，真机跨网络通话复现，对比两个调用点是否都实际进入 createAnswer。
  2. **若确诊双触发**：把 `makingAnswer` 改为 **per-session** 字段（挂在 session 对象上），且 `finally` 重置改到 `await sendWebRTCMsg` 之后；或干脆**移除触发点②**（onSignalingState 只做日志，answer 生成统一由触发点①显式驱动，signalingState 不应承担控制流）。**推荐移除触发点②**——控制流单一来源，符合 KISS。
  3. **附注（另一崩溃点，存疑）**：`answer` 分支 `s2!.pc` 强解包（`:278`），sid 未命中会抛异常。多端广播（WRTC-02）会放大此风险。建议改 `s2?.pc` + 空值早返回。
- **边界**：不改 glare 仲裁 `_isPolitePeer`（`:218-268` 已有 perfect-negotiation 角色判定）；改 UI 走 AppColors/AppSpacing token（本项基本不涉 UI）。
- **验收 gate**：`flutter analyze` 零问题；真机日志确认 `_createAnswer` 单次进入（BLOCKED 真机）；真机跨网络通话建连成功率。
- **glm 执行陷阱**：真机验证（禁模拟器）；改动前先确诊，勿盲改互斥逻辑引入新竞态。
- **分工**：确诊 + 修复 **[BLOCKED 真机]**（须真机时间戳日志复现才能确定是否要改、怎么改）。
- **回滚条件**：若真机确诊 glare 不发生（平台通道时序保证②先于①的 continuation），本项关闭仅留附注崩溃点加固。

---

## 前后端信令流转图（确证）

```
Caller(App)                    Backend(webrtc_ws_logic:event, 纯透传)         Callee(App, 可能多端)
_createOffer                                                                  
 sendWebRTCMsg('offer') --WS--> message_router_logic:route(webrtc_*)          
                                好友/黑名单校验                               
                                send_next(to, msg, MsLi=[0])  ← 单次无重试     
                                imboy_syn:list_by_uid(to) → 广播全部在线设备   
                                start_timer(0,Pid,Msg) --WS--> case 'offer':   
                                                               setRemoteDescription
                                                                ├─(副作用)→ onSignalingState(have-remote-offer)
                                                                │              → _createAnswer 触发点②
                                                                ├─ await 消费缓冲candidate
                                                                └─ await _createAnswer 触发点①
                                                                   [makingAnswer 互斥, 重置时机存疑竞态]
case 'answer' <--WS-- send_next(caller,[0]) <-- sendWebRTCMsg('answer')       
 setRemoteDescription(answer)                                                 

candidate: 双方各自 onIceCandidate → sendWebRTCMsg('candidate')
           → 同一透传链(MsLi=[0], 广播全设备) → 对端 addCandidate/缓冲

ICE 配置(一次性, 早于 offer): App --GET /v1/user/credential--> user_ds:webrtc_credential
   [eturnal_secret HMAC-SHA1 合成; eturnal_turn_urls 为空则退化纯STUN → relay=0]
```

---

## 排查执行顺序（给 glm 的操作序，修正版）

1. **先证 WRTC-00**（10 分钟真机定案，可证伪）：`flutter clean` 重编 → 双真机跨网通话 → 主叫日志看 `[WS] type=WEBRTC_ANSWER` 出现但 `> rtc onMessageP2P` 不出现 = 断链实锤。同 LAN 再打一次若同样连不通 = 进一步佐证。**这是稳定 permission=0 的真根因，先修这一行。**
2. **修复 WRTC-00 后必然暴露 WRTC-02b（响铃期候选丢失）**：`WebRTCSignalingEvent` 订阅在通话页 `page:245` initState，而通话页只在接听后创建。主叫 trickle candidate（含最晚生成的 srflx/relay）在被叫响铃期到达时仍无订阅者进黑洞。修法：把 candidate 缓冲提前到 `handleWebRTC` 层，或响铃期就建占位 session。真机看被叫 `> rtc RECV ICE candidate type=srflx/relay queued` 是否在接听前出现。
3. **WRTC-00+02b 修复后仍 relay 候选为 0** → 才转 WRTC-01：生产 console `application:get_env(imboy, eturnal_turn_urls)` + curl `/v1/user/credential` 看 turn_urls 非空 → 空则补 env override；非空则 trickle-ice 测 relay 候选 → 无则 eturnal 服务端/端口/防火墙排查。
4. **旁证工具**：后端日志 grep `client_ack_invalid_params.*WEBRTC`（WRTC 帧的 CLIENT_ACK 被后端白名单拒，每帧一条 WARN）——出现即证明帧确实送达客户端，是 WRTC-00 的有力旁证。**⚠️ 附注地雷**：切勿为消 WARN 把 `WEBRTC` 加进 ACK ValidTypes——所有 webrtc 帧共用同一通话 msgId，一旦 ACK 被接受，`handle_ack_cancel` 会写 40s TTL 去重记录，后续同 msgId 的 candidate 会被 `message_ds.erl:100-117` ACK 去重全部丢弃。正确做法是客户端对 webrtc 帧不发 ACK，或后端对 webrtc 类型静默跳过 ACK。
5. **WRTC-03/04（单次投递无重试 / glare）为次要**：偶发候选缺失可加固，但非稳定 relay=0 成因；glare 已基本排除主因。

## 附：另记的边界崩溃点（非 relay 根因，但值得顺手加固）
- **多设备 answer 崩溃**：`send_next` 发给对端全部在线设备，B 多设备各自 answer，A 收第二个 answer 时 `setRemoteDescription` 在 stable 态抛错 + `provider:278/281` 的 `s2!` 空断言风险。建议 `s2?.pc` + 空值早返回。
- **turn_urls 数组类型透传 [NEEDS-VERIFY]**：后端下发 `turn_urls` 为 JSON 数组，客户端 `provider:960` 直接把 `List<dynamic>` 塞 `'urls'`，flutter_webrtc 原生端对 `List<dynamic>` 解析是否全平台正确未验证。真机 grep `> rtc ICE candidate type: relay` 有 relay 候选即排除此项。
