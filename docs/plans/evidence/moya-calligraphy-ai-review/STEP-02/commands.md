# STEP-02 commands — 合规核验

> 工作目录：/Users/leeyi/project/imboy.pub（工作区根，非 git 仓）；imboy 仓内操作均只读。
> 执行者：Agent A（PRODUCT-COMPLIANCE-UX）。日期：2026-09-09。

## 基线确认（只读）

| 命令 | 工作目录 | 退出码 | 摘要 |
|---|---|---|---|
| `git rev-parse HEAD` | /Users/leeyi/project/imboy.pub/imboy | 0 | `5b7e2055f71087930eb4e896d1ce0f895140d614`（与任务书 BASE_SHA 一致，未移动） |
| `git rev-parse HEAD` | /Users/leeyi/project/imboy.pub/moya | 0 | `9644a2e804ecabc232fd313d87f7825339b4aceb`（只读，未修改） |

## 在线核验（WebSearch / WebFetch，全部只读）

| # | 类型 | 查询/URL | 结果 |
|---|---|---|---|
| 1 | WebSearch | 微信小程序 个人/企业主体 服务类目 区别（site:developers.weixin.qq.com） | 命中官方 introduction/product/material 页 |
| 2 | WebSearch | 小程序备案 ICP 备案 要求（site:developers.weixin.qq.com） | 命中官方 record 指引与 FAQ |
| 3 | WebSearch | 儿童个人信息网络保护规定 监护人同意（site:gov.cn） | 命中 cac.gov.cn 规定全文（第 2/9/10 条） |
| 4 | WebSearch | 未成年人网络保护条例 2024（site:gov.cn） | 命中 cac.gov.cn 条例全文（国务院令 766 号，2024-01-01 施行） |
| 5 | WebSearch | 教育类目 非学科培训 资质（site:developers.weixin.qq.com） | 命中官方 material 页：非学科类培训机构 4 选 1 资质 |
| 6 | WebSearch | 微信小程序认证 费用 年审 | 命中官方 renzheng 页（费用不退、到期前 3 个月年审）；金额 300 元为第三方交叉 |
| 7 | WebSearch | 小程序简介 字数限制 | 第三方一致 4–120 字、每月 5 次修改；官方原文未命中 → 列待人工核验 |
| 8 | WebSearch | PIPL 第 31 条（site:cac.gov.cn） | 命中 PIPL 全文（第 28/29/31 条） |
| 9 | WebFetch | https://developers.weixin.qq.com/miniprogram/product/material/ | 成功：教育类目资质原文、个人主体仅"教育信息展示"且不支持视频 |
| 10 | WebFetch | https://developers.weixin.qq.com/miniprogram/product/record/record_guidelines.html | 成功：备案 5 环节流程、个人/单位材料差异、页面当日有效 |
| 11 | WebSearch | 微信支付 小程序开通条件 | 命中 pay.weixin.qq.com 官方指引：非个人主体+认证+执照+对公账户 |
| 12 | WebSearch | 小程序用户隐私保护指引 摄像头 相册 | 命中官方 user-privacy 页：未声明则 API 拦截、提审驳回 |
| 13 | WebSearch | wx.chooseMedia maxDuration | 命中官方 API 文档：拍摄 3–60 秒上限，相册选择不受限 |

## 写入

| 路径 | 说明 |
|---|---|
| imboy/docs/plans/2026-09-09-moya-compliance-verification.md | Step 2 交付文档（核验矩阵/三阶段门禁/草拟材料） |
| imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-02/commands.md | 本文件 |
| imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-02/notes.md | 结论笔记 |

## 边界遵守声明

- 未执行任何 git 写操作；未读取任何 .env；未使用真实 AppID/secret/儿童数据/机构联系方式；未联系任何第三方；未提交微信审核或备案。
