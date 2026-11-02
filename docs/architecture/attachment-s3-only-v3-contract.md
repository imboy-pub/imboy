# IMBoy S3-only V3 合同冻结（G01）

冻结基线：backend 4f4e396 / flutter 9c4ac20 / admin b55ae8e（本地 worktree HEAD）。
本文为 V3 执行合同（计划族 imboy-garage-direct-v1），与 direct-v1 协议文档并存；
冲突时以本文 + 用户 20 项答复为准。

## 1. 旧正文通道移除合同（RETIRE 面，执行于 V3-20）

| 通道 | 判定 | 执行卡 |
|---|---|---|
| Disk driver（attachment_storage_disk）+ 专用 C NIF（attachment_disk_lock_nif） | 删除注册/路由/构建调用；客户端缓存、Worker 临时目录、PG/Garage 数据盘不误删 | V3-20 |
| attachment_storage_gateway_handler 旧 gateway 正文路由 | 删除 | V3-20 |
| attach_handler multipart/form-data 直传路由与 upload_url 派生 | 删除（direct register/declare/grant 替代） | V3-20 |
| BEAM put/get/copy 正文原语 78 处（含 publish_avatar put_object、EB content 读） | 迁 Copy/Worker/边缘或改有界控制；HEAD/Copy/Delete 有界响应保留 | V3-13/17/19/20 |
| 存储失败回退 disk 的 fallback 分支 | 删除 | V3-20 |

登记册：状态根 v3/inventory/beam-body-channel-inventory.md（G00）。

## 2. 五要素冻结

**时间窗**：presign 有效期 600s（V2.3 冻结值，elib_s3_sign presign_signed_url_at）；
attempt deadline 与 hold/quota 复核窗沿 direct-v1 §状态机；迁移号已用至 238，
V3 新迁移自 239 起预约（单 owner：A 线）。

**错误码**：direct 协议错误码迁移号 200 起（V2.3 冻结）；S3 层错误透传有界化
（200+错误 XML 不算 Copy 成功——attachment_storage_garage 合同）。

**required_headers**：所有 s3_put 消费显式 descriptor（kind/method/origin/headers）；
签名头精准发送；业务 JWT/Seat Bearer/Admin Cookie/OA HMAC 密钥绝不进入对象存储请求；
presign 绑定 content-type + x-amz-checksum-sha256（同输入同 URL，deterministic）。

**大小限制**：普通附件 104857600 字节（100MiB）、企业 26214400 字节（25MiB），
E2EE 密文开销计入；超限硬拒绝点与残留语义沿 T08。不擅自扩限。

**下载权限**：普通 scope 短 URL 授权窗或逐请求授权（U 决策前不启用）；
Internal/企业/客服逐请求授权 + 隐藏 origin 由边缘承载（V3-08/17）；
Range/HEAD/416/错误状态/缓存身份隔离必测。

## 3. 组织不是租户

organization 是业务对象，部署实例才是隔离边界；禁止 Redis；禁止改 vendored erlang.mk
（backend AGENTS）。旧代码"tenant"注释不构成授权规范（F11）。

## 4. U05 矩阵：消息类型 × 会话类型（枚举源自本候选代码）

消息类型（56，flutter lib/service/message_type_constants.dart 实取）：
text, image, voice, audio, video, file, location, expression, quote, channel_card,
talk_record_card, channel_message, transfer, custom, unsupported, message_read,
message_read_ack, message_revoke, message_revoke_ack, message_edit, message_edit_ack,
message_input, e2ee_room_key, message_reaction, pull_offline_msg, c2c_revoke,
c2c_del_everyone, c2g_del_everyone, c2g_del_for_me, group_member_join, group_dissolve,
group_member_leave, group_member_alias, apply_friend, apply_friend_confirm, user_cancel,
in_denylist, not_a_friend, online, offline, hide, logged_another_device,
device_force_offline, please_refresh_token, app_upgrade, moment_new, moment_like,
moment_comment, moment_deleted, channel_text, channel_image, channel_video,
channel_audio, channel_file, channel_link, channel_location。

会话域（5）：C2C 单聊（c2c）、普通群聊（c2g）、企业群（org c2g 域）、
企业频道（channel*）、指定员工（OA 代发目标，c2c 形态）。

矩阵语义（D06）：**业务消息类型**（text/image/voice/audio/video/file/location/
expression/quote/channel_card/talk_record_card/channel_message/transfer/custom +
channel_{text,image,video,audio,file,link,location} 共 22 类）× 5 会话域为必测格；
控制/通知类（read/revoke/edit/reaction/在线/朋友圈/升级等 34 类）按其会话语义测。
点名卡片（roll call 5 类，用户点名）**当前枚举不存在 → 登记缺口**，V3-23 逐格裁定，
不得删除需求；合法组合未实现记 FAIL/BLOCKED，不缩成"企业群文本"。

发送主体：真人用户（H-H E2EE 语义不变）与企业应用代发（保留应用身份/调用凭证/
被代表用户标识 + 审计；UI 标注企业应用代发；不伪造 H-H E2EE、不自动加好友）。
