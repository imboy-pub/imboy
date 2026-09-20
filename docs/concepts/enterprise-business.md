# 企业业务域（Enterprise Business）

> Purpose：定义组织对外客户经营子系统：托管联系人、企业会话/消息/资产、合规留持与离岗交接。
> 注意：**企业业务（EB，功能域）≠ 商务版（Business Edition，分发版次）**，英文都含 business，写作必须用全称区分。

## Concept：组织拥有的客户经营闭环

```
组织（Organization）
  ├── 业务身份（Business Identity：sales / customer_service 职能，稳定经办主体）
  ├── 企业联系人（Enterprise Contact：组织拥有的客户关系，非个人好友）
  │      └── 企业会话（Enterprise Conversation，含同意书 notice_version/consent_at）
  │             └── 企业消息（Enterprise Message：托管密文 + 送达账本）
  ├── 企业资产（Enterprise Asset：presign/confirm 直传 + 密文元数据）
  ├── 留持（Retention Policy / Hold：合规保留）
  └── 离岗交接（Offboarding Case：离职移交状态机）
```

与个人消息域的根本区别：**数据 owner 是组织而非个人**——企业消息不入个人 `msg_c2c`/ACK 清理链，有独立的密文、审计、留持与交接生命周期。

## Current

**已实现：**

- **业务身份**（迁移 114）：`organization_business_identity(+_assignment)`——不可登录/不发 JWT/不拥有好友；`function_key` 创建后不可变；同一身份至多一个 active 经办人；active 经办人的 user 行删除被 DB 拒绝（fail-closed）；handover 不改 ID。
- **联系人**（115）：`enterprise_contact(_identity/_assignment)`——渠道标识（imboy/wechat/phone/email/other）只存 HMAC+掩码，可幂等去重；可选关联个人号（user 删除 SET NULL）；同一联系人至多一个 active primary 经办。
- **会话与消息**（116 + 121-124 修正）：`enterprise_conversation`（workspace_id 服务端解析；未同意同意书不得持久化内容）、`enterprise_message`（`body_cipher`+密钥版本+AAD 哈希；保留期快照）、`enterprise_message_delivery`（按 (org,message,recipient,device) 幂等）；出站消息必须有 actor（触发器）；`retain_until` 只能后移；hide=追加墓碑改可见性。
- **资产**（118）：`enterprise_asset` + presign/confirm + 内容经企业鉴权代理读取（`/assets/:id/content`）。
- **留持**（117/121）：`enterprise_retention_policy(_hold)`——hold 阻断 bounded purge。
- **审计**（119）：`enterprise_audit_event` append-only——本仓该模式的始祖，Grant/客服事件复用。
- **离岗交接**（120）：`enterprise_offboarding_case(_item)`——幂等条目 + 失败原因 + 审计关联；与组织成员移除守卫并存（直接 removed 前必须完成交接）。
- **API 面**：租户面 `/api/v1/enterprise/*`（contacts/conversations/messages/assets/offboarding）；平台面（只读 + 交接执行）`/api/adm/enterprise-business/*`。
- **三端分布**：App 端 `lib/modules/enterprise/`（含 sqflite 本地 store）；Admin 端 `/enterprise-business` 三页。

## Contract

1. 企业消息与企业资产的密钥体系独立于个人 E2EE（托管密文 + `key_version`），组织可依留持策略合规持有。
2. 渠道标识永不存明文（HMAC+掩码），去重靠 HMAC 幂等。
3. Admin 平台面对本域默认只读，唯一写动作是离岗交接执行。

## Constraints

- 未取得同意（consent_at）的会话不得持久化消息内容。
- 定期清除（bounded purge）被 hold 阻断；提前删除被触发器拒绝。

## References

- 迁移：114-124（家族）、115-120（各实体）
- 客户端：imboyapp `lib/modules/enterprise/`；imboyadmin `src/modules/enterprise_business/`
- 概念关联：[客服域](./customer-service.md)（共用业务身份；客服消息落本域）
