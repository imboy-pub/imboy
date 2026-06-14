# IMBoy 版次边界 / Edition Boundary

> 配套：[RELEASE.md](./RELEASE.md) ｜ 商业策略来源：[../analysis/monetization-path-a-private-deployment.md](../analysis/monetization-path-a-private-deployment.md)
> 运行时版次标记由环境变量 `IMBOY_EDITION` 控制（community | professional | enterprise），缺省 `community`。

---

## 三档定位 / Three Editions

| 版次 / Edition | `IMBOY_EDITION` | 定位 | 交付形态 |
|---|---|---|---|
| 社区版 / Community | `community`（默认） | 引流、自托管体验、开源信任 | 开源（MulanPSL-2.0），单机 docker-compose |
| 专业版 / Professional | `professional` | 中小企业、私有社群 | 闭源商业模块 + 商业授权 |
| 企业版 / Enterprise | `enterprise` | 政企/信创/金融 | 专业版 + 信创/合规/SLA |

---

## 功能边界矩阵 / Feature Boundary

| 能力 | 社区版 | 专业版 | 企业版 |
|---|:--:|:--:|:--:|
| 单聊 / 群聊 / 朋友圈 | ✅ | ✅ | ✅ |
| E2EE 端到端加密 | ✅ | ✅ | ✅ |
| 单机部署（docker-compose） | ✅ | ✅ | ✅ |
| 钱包 / 支付基础 API | ✅ | ✅ | ✅ |
| 可观测性（Prometheus+Grafana+Loki） | ✅ | ✅ | ✅ |
| **集群部署（水平扩展）** | ❌ | ✅ | ✅ |
| **白标 / 换肤系统** | ❌ | ✅ | ✅ |
| **付费频道运营后台** | ❌ | ✅ | ✅ |
| **加密对象存储增强** | ❌ | ✅ | ✅ |
| **信创国产化适配（达梦/鲲鹏/UOS/国密）** | ❌ | ❌ | ✅ |
| **SSO / 审计合规** | ❌ | ❌ | ✅ |
| **优先 SLA / 快速修复** | ❌ | ✅ | ✅（更强） |

---

## 关键工程约束 / Engineering Constraints

1. **专业版/企业版功能为闭源商业模块，不进本开源仓**。许可证为木兰宽松 v2（MulanPSL-2.0），无 AGPL 传染性，**无法靠 license 杠杆**逼迫付费；差异化只能靠闭源模块 + 服务。参考野火 IM：社区版开源单机，专业版闭源集群。

2. **`IMBOY_EDITION` 当前仅作"标记 + 启动日志"**（见 `src/lib/imboy_env.erl` 的 `edition/0` 与 `override_edition/0`）。**社区版代码不得被植入按版次的残缺收费开关**——避免开源用户看到"被阉割"的半成品逻辑。真正的版次功能开关随闭源模块一起提供。

3. 启动时后端日志打印 `IMBoy edition: <community|professional|enterprise>`，供运维/支持快速识别部署版次。

---

## 与变现路径的关系

本边界对应变现路径 A（To B 私有化授权）的定价分层：社区版免费引流 → 专业版 ¥3.9 万/套（终身，绑定域名）→ 企业版 ¥12 万/年起。详见 [monetization-path-a-private-deployment.md](../analysis/monetization-path-a-private-deployment.md) 第 2-3 节。
