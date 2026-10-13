# 客服坐席正文长度 / Customer-service Seat Body Length

## 已批准合同 / Approved Contract

客服坐席出站消息的正文使用 UTF-8，最大 **4000 个 Unicode 码点**。这不是 4000 字节、JavaScript UTF-16 code unit 数或肉眼可见字形数。

Customer-service seat message bodies use UTF-8 and allow at most **4000 Unicode code points**, not bytes, UTF-16 code units, or grapheme clusters.

- 汉字、ASCII 字母、数字、空格、换行各按其 Unicode 码点计数；组合字符与组合 emoji 按实际码点数量计数，不做 Unicode 归一化。
- 正好 4000 个码点可发送；超过 4000 或含无效 Unicode/UTF-8 的正文拒绝，不截断为合规正文。
- 坐席工作台保留已有发送前首尾空白 trim 行为，长度校验针对实际要发送的正文；超限时不上传附件、不发消息请求，保留原始草稿和 File。
- 后端按解码后的正文执行相同码点边界，直接 API 调用不能绕过；非法正文返回现有 `invalid_argument` / HTTP 422 错误矩阵，不在错误中回显正文。
- 附件大小独立执行原有合同。空正文加合法附件仍沿现有附件消息合同；正文及附件皆空的拒绝逻辑不变。
- 未知发送结果只查询原操作真源，长度校验不阻止查询，也不重新发送原操作。
- 本合同仅新增客服坐席消息限制，不修改访客正文或 internal API `content` 的既有合同。

Chinese characters, ASCII, whitespace, and combining sequences are counted by code points without normalization. The workbench retains its existing trim behavior and validates the outgoing body. Over-limit input preserves the draft and file and starts neither upload nor message submission. Server validation precedes the canonical write and uses the existing HTTP 422 matrix without echoing the body. Attachment and unknown-result recovery contracts remain unchanged.

## 示例 / Examples

| 正文 / Body | 码点 / Code points | UTF-8 字节 / Bytes | 结果 / Result |
|---|---:|---:|---|
| `a` × 4000 | 4000 | 4000 | 允许 / Allowed |
| `中` × 4000 | 4000 | 12000 | 允许 / Allowed |
| `😀` × 4000 | 4000 | 16000 | 允许 / Allowed |
| `中` × 4001 | 4001 | 12003 | 拒绝 / Rejected |

## 实现与证据范围 / Implementation and Evidence

沿 `useSeatSendState` 与现有客服会话 → 企业消息唯一真源实现；不新增消息服务或存储。边界单测、mock/application 回归与构建结果不等于真实浏览器、PG/Garage、完整 Required、用户亲验或生产验收。

Implementation uses the existing seat send hook and customer-service canonical-message path. Unit/mock checks and builds do not establish real-browser, PG/Garage, complete Required, UAT, or production acceptance.
