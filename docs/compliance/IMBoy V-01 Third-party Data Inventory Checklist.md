# IMBoy V-01 Third-party Data Inventory Checklist

> 任务：第三方与数据流清单（Overseas Compliance Implementation Plan Task V-01）
> 日期：2026-09-08 | 状态：**DONE（清单+校验脚本+CI 门落地；DPA 签署为 owner 后续动作）**
> 提交：imboy（本轮，未 push）

## Goal 对照

把实际构建/运行时的第三方提供商绑定到对外披露（bind actual providers to disclosures）。
不新增任何 SDK（计划红线），清单全部来自三端源码与配置的实证勘察。

## 交付物

| 文件 | 内容 |
|---|---|
| docs/compliance/third-party-data-inventory.yml | 22 行清单：12 个真实出站 provider + 3 个 self-hosted 声明行 + 7 个显式 no-data 行，覆盖计划点名的 FCM/APNs/Sentry/AMap/JPush/SMS/SMTP/LiveKit/TURN/payment/LLM/webhooks/CDN/object store 全部类别 |
| scripts/validate_third_party_inventory.py | 四道门校验（schema/覆盖/实证双向/海外发布门），纯标准库 |
| test/scripts/test_validate_third_party_inventory.py | 12 用例 unittest（真仓正向 + 8 类负向 fixture） |
| .github/workflows/quality.yml | 新增 third-party-inventory job（常规校验+unittest，PR/push 均跑） |

## 实证勘察结论（清单的依据）

真实出站（12）：极光 SMS（api.sms.jpush.cn）、极光号码认证（api.verification.jpush.cn，Android
原生 SDK jverification 3.2.8 内嵌运营商预取号）、高德 Web 服务（restapi.amap.com POI/静态图）、
高德定位 SDK（location 6.5.0）、高德 3D 地图运行时（3dmap 10.0.600，仅注册依赖）、Sentry SaaS
（o436562.ingest.sentry.io，DSN gitignored 编译期注入）、支付宝/微信支付/Stripe（经 erlang_pay，
app 端无支付 SDK 仅包可见性唤起）、SMTP（smtp.qq.com）、LLM 双源（火山方舟 ark + 阿里百炼 maas）。

Self-hosted（3）：Garage S3（s3.imboy.pub）、LiveKit SFU（pro.imboy.pub/livekit 反代）、eturnal
TURN/STUN（turn.imboy.pub）——数据不出第三方，dpa_status=not-applicable。

显式 no-data（7）：FCM 与 APNs（sys.pro.config 占位符，远程推送未接入，客户端仅本地通知）、
极光 Push REST（appkey 仅用于 SMS/认证）、阿里云短信（platform=aliyun 有配置无实现，见"发现"）、
第三方 CDN（全自有域名）、出站 Webhook（频道 webhook bot 是入站方向）、统计/广告 SDK（三端均无）。

## 四道门（脚本）

1. **schema**：active 行 14 字段齐备 + region/necessity/dpa_status 枚举 + privacy_url 必须 https 或
   显式 unknown；no_data 行必须给理由。
2. **覆盖**：15 个计划类别每类至少一行（sms/auth/map/location/crash/payment/email/llm/storage/
   rtc/turn/push/cdn/webhooks/analytics）。
3. **实证双向**：内建 19 条 EVIDENCE 特征表与源码互证——(a) 源特征（如 jverification:3.2.8、
   api.stripe.com）必须映射到清单行，否则"未登记数据流"报错；(b) active 行 endpoint host 必须在
   源文本命中，否则"清单漂移/编造"报错；(c) 文件在但特征消失 → 证据失效报错（fail loud）。
   optional 条目（app 仓/erlang_pay 仓/.env 等本仓外文件）在单仓 CI 环境降级为警告。
4. **海外发布门**（--overseas-gate）：任何真实出站行（非 dev-only、非 self-hosted）若
   dpa_status != executed 或 region=unknown → 阻断退出。**当前即红**：12 行 DPA 未签署
   ——这正是验收条款 "unknown DPA/region blocks operated overseas release" 的实现，
   在 owner 完成 DPA/SCC 签署前不得声明海外运营。

## 发现（超出清单本身的）

1. **配置与实现不一致（待 owner 修正）**：sys.pro.config 的 sms.platform 写 aliyun，但阿里云短信
   无实现（imboy_sms 兜底子句返回平台配置错误）——生产验证码短信链路依赖的实际是极光 SMS。
   已在清单 aliyun_sms no_data 行如实记录。
2. **远程推送整体未接入**：FCM/APNs 配置均为占位符；推送通知实为客户端本地通知。对披露影响：
   隐私声明无需列出 FCM/APNs 数据流。
3. **高德 3D 地图 SDK 的运行时自采范围未审计**（仅作为定位插件注册依赖嵌入）——已按保守原则
   列为 active 行而非 no-data，待专项审计后可改。
4. **首轮脚本勘误（自测抓到）**：ark 出站特征实际在 sys.local.config/sys.runtime.config 而非
   sys.pro.config（首轮 grep 用了 config/*.config 全局模式导致归因错误）；同时修正向门语义——
   证据文件存在但特征消失必须报错而非静默放行。

## Acceptance 条款对照

| 验收项 | 结果 |
|---|---|
| every dependency/configured outbound host maps to an inventory row or explicit no-data rationale | ✅ 门 3 实证双向 + 门 2 类别覆盖；no-data 均给理由 |
| unknown DPA/region blocks operated overseas release | ✅ --overseas-gate 阻断（当前 12 行，符合现状）；CI 常规校验不启用该 flag（国内运营不受影响），海外发布为人工触发步骤 |
| no SDK addition | ✅ 零 SDK 变更 |

## Owner 待办

1. **DPA/SCC 签署**：12 个真实出站 provider 的数据处理协议——签署后把清单行 dpa_status 翻为
   executed 并附签署证据；这是海外发布解阻的唯一路径。
2. sms.platform 配置修正（aliyun → jpush 或实现阿里云短信）。
3. 高德 3D 地图 SDK 自采范围专项审计。
4. 海外发布门是否自动挂入 release.yml（本轮未动生产发布流程）。

## 验证记录

- `python3 scripts/validate_third_party_inventory.py` → **PASS**（exit 0）
- `--overseas-gate` → **FAIL**（12 行 DPA 未签署阻断，符合设计）
- `python3 -m unittest test.scripts.test_validate_third_party_inventory` → **12/12 OK**
- T-01 回归 `test_validate_retention_policy` → **10/10 OK**（互不影响）
- quality.yml YAML 语法经本地 python yaml 解析校验
