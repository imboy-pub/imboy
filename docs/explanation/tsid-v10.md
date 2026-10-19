# TSID V10：静态业务游标、固定Offset与无持久化

> 更新：2026-10-19。真源：src/lib/elib_tsid.erl、elib_tsid_inventory.erl、imboy_sup.erl。参数与协议见[字段规范](../reference/tsid-field-convention.md)、[环境变量](../reference/env-vars.md)。

## 为什么按实体分配游标

同一实体主键空间须唯一，不同实体无需争抢同一序列。152个canonical Namespace各有一个独立游标，atomics按stride=8隔开（1216个物理槽位）；没有default游标。三条别名group → group_info、audit → enterprise_audit_event、enterprise_audit → enterprise_audit_event不增加游标。

热路径读取Runtime、静态查槽、读时钟、有界CAS；无数据库/磁盘/远程调用或每域GenServer。启动init串行发布Runtime，同配置重复初始化保留游标，不同配置拒绝重置。使用elib_tsid:generate(user)等明确标签，未知标签报错；没有generate/0、generate_n/1或register/registered过渡接口，测试也不得依赖它们。客服AI配置、FAQ、job分别使用ai_cs_config、ai_cs_faq、cs_ai_job；对应不同实体，不能借用其他实体的游标。

初始化互斥依赖OTP的global_name_server；服务缺失时以`{elib_tsid_init_lock_unavailable, global_name_server}`拒绝初始化，不发布Runtime。测试入口保留该服务，不能使用`-kernel start_distribution false`运行正常初始化测试。This initialization lock requires OTP's global_name_server; initialization fails closed when it is unavailable.

游标及reservation计数仅支持当前runtime布局；不提供旧LegacyStats回退。升级须通过受控进程重启，不以热加载复用旧槽表；仍受下文跨重启唯一性边界约束。

Namespace不编码进ID。跨域同值合法，因此表、引用、缓存及权限判断必须保留Domain，不能只比较数字。

## 时钟与容量取舍

布局sign=0 + 42-bit相对毫秒 + 10-bit CombinedNode + 11-bit序列，纪元2025-01-01 UTC，范围1..2^63−1，PostgreSQL BIGINT。每Namespace每节点每毫秒2048个序列位置是理论容量，不是生产吞吐承诺。

启动采集Offset=system_time−monotonic_time，运行时用monotonic_time+固定Offset，不持续校准。墙钟回拨不会直接拉回生成时钟，但二者可能偏离。readiness低频检查，超限设置clock_unsafe闸，发号入口读闸拒绝生成；不是每次发号都重测墙钟偏差。/livez判断存活，/readyz聚合PG和TSID，/healthz为readiness兼容入口。

序列耗尽不得主动借未来毫秒，容量等待/CAS遵守预算并明确报错。大批量按块预留，批尾也受时钟约束；不能用旧版借支突发成绩评价新容量。纪元前、时间段耗尽、非法Node布局、异配置初始化均拒绝而非截断。

## 无持久化的边界

persistent_term仅在BEAM内存，不是磁盘持久化。没有Durable Fence、Store、跨实例锁、catalog扫描或bootstrap manifest；不恢复旧游标，也不擅自清除未知旧数据。[origin位ADR0004](../adr/0004-tsid-origin-namespace.md)尚未实现。

同BEAM同域唯一；跨节点同域唯一以全体实例NodeId不重为前提。重启仅在新时间戳严格大于旧发号水位时验证零重叠。同毫秒重启、跨重启回拨、同NodeId独立BEAM均可能重复，“没有回拨”也不足以保证安全。已知清单检查不能发现未登记实例。

蓝绿部署无TSID_STATE_DIR或bootstrap参数。两色必须显式配置不同NodeId；DEPLOY_TSID_DC_ID（默认1）和DEPLOY_TSID_DC_BITS（默认3）须匹配部署布局，dc_bits全集群一致。脚本按10-dc_bits校验节点范围，完整节点身份原子写入该release的sys.config，并附非敏感配置标记；这是节点配置，不是Cursor/Fence持久化。首启和后续重启保持同一身份。

停旧writer前从其实际运行runtime读取CombinedNode与dc_bits，转换并固化原DC/Node/DCbits；失败恢复仅使用该已验证身份，不根据本次.env猜测。已经停机的旧release须有与配置字段一致的身份标记；首代旧release缺标记时拒绝自动恢复，须人工核对原运行身份。实际身份冲突或活动实例与候选dc_bits不一致时拒绝启动并行writer。首次legacy割接的停写确认DEPLOY_LEGACY_STOP_ACK=I-CONFIRM-OLD-WRITER-STOPPED在任何expand/停机分支前检查。

TSID可透露时间/节点，不能作为不可猜测令牌或代替鉴权。跨域排序不是全局业务顺序，消息顺序使用conv_seq等业务游标。

## 协议与证据边界

常规业务API保持JSON integer，已有显式string端点依自身合同。Admin在解析阶段保护大整数并使用EntityId字符串；原生Flutter用64位int；Web须保护超过2^53−1的值。WS/PFv3等独立协议不应套用统一REST类型。

2026-10-11本地后端候选a828bb9745063439c47495ea03b2d8c9884fa71c通过全量EUnit11085、隔离TSID72、Agent真实PG28、客服真实PG25；一小时121样本偏差0～1ms。不是三端UAT或生产准入。

同宿主三轮对照早期V10，公平单ID吞吐变化−3.28%～+1.82%；batch约−93.39%但批尾约束不同。对照不是V9，不支持零回归、稳定提升或生产峰值结论；一小时观测也不证明任意长期/NTP行为。

V10 requires explicit entity namespaces, with no default cursor or registration compatibility API. Customer-service AI configuration, FAQ and job use independent cursors. Blue/green deployments always inject distinct node IDs under a shared DC layout; disk/bootstrap settings are removed, while the legacy stop-writer acknowledgement remains required. V10 uses static business cursors and monotonic time with a fixed offset, without persistent fences. Equal IDs across domains and unsupported restart collisions are possible. Unique NodeIds, domain-qualified references and explicit error handling remain required.
