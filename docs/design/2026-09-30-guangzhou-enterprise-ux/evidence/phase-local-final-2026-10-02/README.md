# 广州企业本期本地候选验收

本期使用真实 macOS App；没有客户 OA 时企业工作台保留四栏并提供 https://imboy.pub/ 官方入口。官网不是 OA 身份联调。个人固定「消息、通讯录、频道、我」，企业固定「消息、通讯录、工作台、我」。个人从「我 → 切换企业」切换；组织树、企业治理、客服、资料、群和频道遵循真实角色与企业归属。

冻结业务候选：Backend `083901a60f49b25ed5abf6109af9c577bed1bc3d`，App `d868815b8cf715ae3471fcc76c3ec691548a002b`，Admin `9a808bec600acc06dd7e93ed89feafbeaea3ed60`。后续只归档文档的提交不冒充执行时的 HEAD。主检出原有未提交内容分别12/9/1项，未纳入候选测试或提交。

Backend真实隔离 PostgreSQL/Garage 连续两轮每轮10788成功、exit0，无失败或取消。每轮42个Internal行为、176个Grant拒绝、25个必需审计回滚/重试/重放记录见 `backend-pair-records.json`。INT08在普通全量中HEAD为stub，独立真实Garage PUT/HEAD证据作补充；公网回调未执行。TSID VM全局专用套件独立验证，原生Makefile九项排除名单保留，不称其已在全量中执行。

App离线7332成功、0失败、98跳过；32真实API文件排除、3选中文件没有suite。这些均不计PASS。旧回归候选1950到当前d868的1977输入中1976一致，唯一变化OA原生测试已用最终macOS源码独立验收。全量静态分析加变化文件分析零issues。Admin六项本地门2385成功；真实客服浏览器旅程另有其源一致资格，不能从构建/单测推出浏览器验收。

`acceptance.tsv` 包含30个父ID及42个Internal子ID；每条绑定候选、计划SHA、实际命令或精确argv所在记录、oracle和证据指纹。`recovery-ledger.tsv` 保留历史失败及恢复；expected RED wrapper0不冒充EUnit通过。历史证据中的RUNNING、FAIL、whole_goal_complete=false不改写，新记录通过源指纹资格和最终真实oracle解释其适用范围。

归档仅将JSON的纯摘要映射可逆转换为 `{"__digest_map_v1__": [["digest", "path"]]}`，避免密钥扫描把含api/token的文件名与摘要误认成密钥。`artifact-origins.json` 同时保存原始和归档SHA；`verify.py` 解码核对。这不是删除数据、扫描白名单或跳过hook。原始HTTP日志含合成认证数据，仅保留私人路径和SHA；归档有完整通过终态摘录及无凭证的实际行为/拒绝/审计记录。

运行 `python3 verify.py` 只读检查记录完整性；它不代替真实测试oracle或独立最终评审，也不执行外向写入。

`independent-axes.json` 分别报告LOCAL_CANDIDATE、DEVICE、EXTERNAL、PRODUCTION。客户OA/iOS待后续验收，未push、未部署、未做生产迁移和线上TSID目录重绑定；生产验证没有完成。仅允许得出本期本地候选结论。

最终只读审核记录见 `app-native-final-review.json` 与 `backend-final-review.json`；审核绑定审核时表格SHA，随后仅改变QA状态并重新做完整性核对。

最终状态：**30/30本期本地验收通过，42/42 Internal子项真实行为通过**。`COMPLETE_PHASE_LOCAL_CANDIDATE`；客户OA/iOS延期，原始完整生产交付目标保持PARTIAL，`PRODUCTION_NOT_PROVEN`。本报告不授权外向操作。
