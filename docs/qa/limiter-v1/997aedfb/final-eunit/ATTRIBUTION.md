候选: 997aedfb（含全部 limiter 提交 + main b3865652 吸收）
结果: Passed 12066 / Failed 132 / Skipped 0
归因: 132 个失败全部位于非 limiter 模块（cs_widget 13=unknown_channel 域断言、enterprise PG 迁移、cache closeout、hird、personal_order PG、adm sidebar、push_token PG、msg_send_performance、org_invite_landing 等），与 L0 时代基线归档 fcc9013a/final-eunit 的既有失败家族一致，无一由本任务引入；limiter 面模块（login_attempt_*/login_security_*/passport_*/throttle_*/auth_oidc_*/elib_req_*）零失败。
