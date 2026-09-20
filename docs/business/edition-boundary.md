# IMBoy 版次边界 / Edition Boundary

> 配套：[RELEASE.md](../guides/release/RELEASE.md) ｜ 商业策略来源：[./monetization-path-a-private-deployment.md](./monetization-path-a-private-deployment.md)
> 运行时版次标记由环境变量 `IMBOY_EDITION` 控制（community | professional | enterprise），缺省 `community`。

---

## 三档定位 / Three Editions

| 版次 / Edition | `IMBOY_EDITION` | 定位 | 交付形态 |
|---|---|---|---|
| 社区版 / Community | `community`（默认） | 引流、自托管体验、源码可信 | 源码公开（BSL 1.1），单机 docker-compose；每版本发布满四年后转 MPL 2.0 |
| 专业版 / Professional | `professional` | 中小企业、私有社群 | 商业授权 + 闭源商业模块 |
| 企业版 / Enterprise | `enterprise` | 政企/信创/金融 | 专业版 + 信创/合规/SLA |

---

## 功能边界矩阵 / Feature Boundary

图例：✅ 已交付 ｜ ❌ 该版次不含 ｜ 🗺️ **路线图（尚未交付，任何版次均不承诺）**

| 能力 | 社区版 | 专业版 | 企业版 |
|---|:--:|:--:|:--:|
| 单聊 / 群聊 / 朋友圈 | ✅ | ✅ | ✅ |
| E2EE 端到端加密 | ✅ | ✅ | ✅ |
| 单机部署（docker-compose） | ✅ | ✅ | ✅ |
| 钱包 / 站内账务 API | ✅ | ✅ | ✅ |
| 外部支付网关收款 / 订阅计费 | 🗺️ | 🗺️ | 🗺️ |
| 可观测性（Prometheus+Grafana+Loki） | ✅ | ✅ | ✅ |
| **集群部署（水平扩展）** | 🗺️ | 🗺️ | 🗺️ |
| **白标 / 换肤系统** | ❌ | ✅ | ✅ |
| **付费频道运营后台** | ❌ | ✅ | ✅ |
| **加密对象存储增强** | ❌ | ✅ | ✅ |
| **信创国产化适配（达梦/鲲鹏/UOS/国密）** | ❌ | ❌ | ✅ |
| **SSO / 审计合规** | ❌ | ❌ | ✅ |
| **优先 SLA / 快速修复** | ❌ | ✅ | ✅（更强） |

---

## 关键工程约束 / Engineering Constraints

0. **交付形态 = 单节点。** 2026-07-31 拍板：单节点先卖，集群列入路线图。
   `deploy/helm/` 的多副本配置（`replicaCount` 2/3）**未经生产集群验证**，
   不得作为交付承诺，亦不得写进任何售前材料。

1. **专业版/企业版 = 商业授权 + 闭源商业模块，不进本社区仓**。后端与管理后台采用 BSL 1.1（源码公开、限制竞争性商业使用）：Additional Use Grant 已覆盖"组织内部生产使用 / 单客户私有部署 / 集成进自有产品"，而**多租户托管（SaaS）与竞争性付费产品或服务需要商业授权**——许可证本身即是杠杆，不必再叠加版次残缺开关。参考野火 IM：社区版单机，专业版闭源集群。许可细节见 [docs/legal/licensing.md](https://github.com/imboy-pub/imboy/blob/main/docs/legal/licensing.md)。

2. **`IMBOY_EDITION` 当前仅作"标记 + 启动日志"**（见 `src/lib/imboy_env.erl` 的 `edition/0` 与 `override_edition/0`）。**社区版代码不得被植入按版次的残缺收费开关**——避免开源用户看到"被阉割"的半成品逻辑。真正的版次功能开关随闭源模块一起提供。

3. 启动时后端日志打印 `IMBoy edition: <community|professional|enterprise>`，供运维/支持快速识别部署版次。

---

## 与变现路径的关系

本边界对应变现路径 A（To B 私有化授权）的定价分层：社区版免费引流 → 专业版 ¥3.9 万/套（终身，绑定域名）→ 企业版 ¥12 万/年起。详见 [monetization-path-a-private-deployment.md](./monetization-path-a-private-deployment.md) 第 2-3 节。
