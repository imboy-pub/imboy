# V5 迁移合同 / Migration contract

本文件SQL是未来迁移设计输入，**本轮未执行、未写入priv/migrations**。W1-02必须在专用PG中完成完整Up/Down/upgrade验证，才能冻结可执行迁移；不能把本文片段当已经验收的迁移。编号在实施时重新取当前最大值+1，本轮观测候选为00000166，不预占其他会话编号。

## M1 必要性与表复用

F02/F03/F06证明现有表无完整位置/状态；F08/F10证明已有队列但缺通用主体与可靠领取。最小新增表数=0；新增字段/触发器不是第二套生命周期。保留业务status、EAR/EB治理、现有引用结构；不改历史Up/Down。

建议拆三个有序迁移：M1-expand（位置/状态）、M2-queue（扩展现有队列）、M3-guards（写入守卫/索引）。M1默认不赋予清理资格；M2现有job一律blocked等待位置证明；M3的定义与DML负例先在独立scratch验证，**实际迁移文件只在W4-04全部写入口适配后进入priv/migrations**；W1-02只提交M1/M2与M3 SQL合同（docs内），不提交可被自动加载的M3文件。迁移与worker切换不可穿插运行旧drain。

### M1-expand SQL合同

以下各ALTER在原迁移器单事务执行，lock_timeout=5s，statement_timeout=15min；超时立即回滚，最多2次重试，禁止自动增大超时或禁触发器。

```sql
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='15min';
ALTER TABLE attachment
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN lifecycle_state text NOT NULL DEFAULT 'live',
 ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;
ALTER TABLE attach_pending
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN scope_ref text,
 ADD COLUMN session_state text NOT NULL DEFAULT 'registered',
 ADD COLUMN upload_expires_at timestamptz,
 ADD COLUMN upload_drained_at timestamptz,
 ADD COLUMN confirmed_attachment_id bigint,
 ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;
ALTER TABLE enterprise_asset
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN lifecycle_state text NOT NULL DEFAULT 'live',
 ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;
ALTER TABLE attachment ADD CONSTRAINT ck_v5_attachment_lifecycle
 CHECK(lifecycle_state IN ('live','delete_intent','deleted'));
ALTER TABLE enterprise_asset ADD CONSTRAINT ck_v5_eb_lifecycle
 CHECK(lifecycle_state IN ('live','delete_intent','deleted'));
ALTER TABLE attach_pending ADD CONSTRAINT ck_v5_pending_state
 CHECK(session_state IN ('registered','confirmed','rejected','expired','delete_intent','deleted'));
ALTER TABLE attach_pending ADD CONSTRAINT fk_v5_pending_confirmed
 FOREIGN KEY(confirmed_attachment_id) REFERENCES attachment(id) ON DELETE RESTRICT;
```

对三表分别生成同形的location CHECK（不是仅对attachment）：

```sql
ALTER TABLE attachment ADD CONSTRAINT ck_v5_attachment_location CHECK (
 (storage_driver IS NULL AND storage_namespace IS NULL AND storage_container IS NULL)
 OR (storage_driver IS NOT NULL AND storage_driver IN ('garage','disk') AND storage_namespace IS NOT NULL
     AND storage_namespace<>'' AND storage_container IS NOT NULL AND storage_container<>''));
ALTER TABLE attachment ADD CONSTRAINT ck_v5_attachment_gc CHECK (
 NOT gc_verified OR (storage_driver IS NOT NULL AND storage_namespace IS NOT NULL
                    AND storage_container IS NOT NULL));
```

attach_pending旧bucket暂留非空列以兼容旧INSERT；新行storage_container为真源，Garage必须与bucket相等，Disk旧bucket用空串。不能两者矛盾时猜测。新客户端字段不是DB位置授权来源。confirmed_attachment_id只用于普通/app附件，EB pending复用自己的主行。

### M2-queue SQL合同

旧PK是`enterprise_asset_delete_queue_pkey`（W1以catalog复核）。转为job_id PK，同时保留旧三键unique，现有ON CONFLICT仍可解析。nullable org/ws允许个人/应用主体；原workspace FK保留并补租户形状CHECK，不能用伪造workspace占位。

```sql
ALTER TABLE enterprise_asset_delete_queue
 ADD COLUMN job_id bigint GENERATED ALWAYS AS IDENTITY,
 ADD COLUMN subject_kind text NOT NULL DEFAULT 'enterprise_asset',
 ADD COLUMN source_key text,
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN job_state text NOT NULL DEFAULT 'blocked',
 ADD COLUMN attempts integer NOT NULL DEFAULT 0,
 ADD COLUMN next_attempt_at timestamptz NOT NULL DEFAULT now(),
 ADD COLUMN lease_token uuid,
 ADD COLUMN lease_until timestamptz,
 ADD COLUMN last_error_code text,
 ADD COLUMN completed_at timestamptz;
UPDATE enterprise_asset_delete_queue
 SET source_key=organization_id::text||'/'||workspace_id::text||'/'||asset_id::text,
     last_error_code='legacy_location_unverified';
ALTER TABLE enterprise_asset_delete_queue ALTER COLUMN source_key SET NOT NULL;
ALTER TABLE enterprise_asset_delete_queue DROP CONSTRAINT enterprise_asset_delete_queue_pkey;
ALTER TABLE enterprise_asset_delete_queue ADD PRIMARY KEY(job_id);
ALTER TABLE enterprise_asset_delete_queue
 ADD CONSTRAINT uq_v5_legacy_asset_job UNIQUE(organization_id,workspace_id,asset_id);
ALTER TABLE enterprise_asset_delete_queue ALTER COLUMN organization_id DROP NOT NULL;
ALTER TABLE enterprise_asset_delete_queue ALTER COLUMN workspace_id DROP NOT NULL;
ALTER TABLE enterprise_asset_delete_queue ALTER COLUMN asset_id DROP NOT NULL;
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT ck_v5_job_subject CHECK (
 subject_kind IN ('enterprise_asset','attachment','pending') AND
 (subject_kind<>'enterprise_asset' OR
  (organization_id IS NOT NULL AND workspace_id IS NOT NULL AND asset_id IS NOT NULL)));
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT uq_v5_job_source
 UNIQUE(subject_kind,source_key);
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT ck_v5_job_state CHECK (
 job_state IN ('ready','leased','retry_wait','blocked','done') AND attempts>=0 AND attempts<=8);
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT ck_v5_job_lease CHECK (
 (job_state='leased' AND lease_token IS NOT NULL AND lease_until IS NOT NULL)
 OR (job_state<>'leased' AND lease_token IS NULL AND lease_until IS NULL));
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT ck_v5_job_location CHECK (
 job_state='blocked' OR
 (storage_driver IS NOT NULL AND storage_driver IN ('garage','disk') AND storage_namespace IS NOT NULL
 AND storage_namespace<>'' AND storage_container IS NOT NULL AND storage_container<>''));
CREATE UNIQUE INDEX uq_v5_delete_identity ON enterprise_asset_delete_queue
(storage_driver,storage_namespace,storage_container,object_key)
WHERE storage_driver IS NOT NULL;
CREATE INDEX ix_v5_job_ready ON enterprise_asset_delete_queue(next_attempt_at,job_id)
WHERE job_state IN ('ready','retry_wait','leased');
CREATE INDEX ix_v5_pending_scan ON attach_pending(created_at,object_key)
WHERE session_state IN ('registered','expired','rejected') AND gc_verified;
CREATE INDEX ix_v5_attachment_scan ON attachment(created_at,id)
WHERE lifecycle_state='live' AND gc_verified;
```

source_key固定：EB=`org/ws/asset_id`；attachment=`attachment_id`；pending=`object_key`。同一物理identity经不同删除原因发现时复用原job，校验subject归属一致；不得ON CONFLICT吞掉不一致。pending确认后的记录禁止再产生pending清理job。location CHECK必须使用NOT NULL显式判定避免SQL NULL使CHECK通过（W1负例测试覆盖）。

旧enqueue缺source_key而失败是**切换不兼容点**：M2部署前停旧worker和旧Purge入口；不能声称SQL可解析就等于整个旧worker写兼容。读上传兼容依赖M1；M2仅能在安全版本统一切换窗口启用。最低风险选择保留此明确停机门，不加隐藏推断触发器自动拼旧数据。

### M3-guards合同

触发器函数使用新前缀`fn_v5_attachment_*`，禁止CREATE OR REPLACE覆写企业既有函数。W1-02提交完整SQL，并以直接DML负例实测，而非只检索触发器名称。

1. 三表位置全空→完整只能受控证明回填或新INSERT；填入后driver/namespace/container/key不可变。runtime role不得拥有表/禁触发器权限。metadata.creator、租户键、scope/cipher/anchor_msg_id不可变。
2. scope_ref允许moment从NULL首绑，anchor_conv_seq从NULL绑定确定序号，group_file_id从NULL首绑；已绑定不同值拒绝。首绑也必须live，无delete job。合法迁移回填路径需独立权限，审计记录证明清单。
3. attachment/EB life只允许live→delete_intent→deleted；session状态按主计划；metadata物理DELETE禁止绕过提交意图。既有EB Purge可保留删metadata方式，但同事务必须已写完整job，既有Hold/Retention触发器仍生效；其asset ID终身墓碑由done job保护。
4. EAR在对象delete_intent/deleted时拒绝新增Hold/延长留存；EB新增Hold锁Workspace后裁决；所有direct DML入口不能绕开此顺序。普通物理DELETE（包括管理员、账号注销）改为受控资格事务，不依赖存储侧失败来保护。
5. job的subject、source、key和创建时间不可改；I填入后不可改。旧job只允许专用proof迁移角色在blocked、attempts=0、lease_token/lease_until/completed_at全NULL时，将三个location字段从全NULL一次性填为完整值；必须同事务写证明审计并过identity唯一约束；runtime role无此权限。last_error_code=legacy_location_unverified且上述零尝试条件成立时，proof事务必须重验全部删除资格，才允许首次blocked→ready并清此错误码；无法证明历史资格或位置则继续blocked，不复活任何已经尝试的任务。state修改检查租约CAS；done不可复活；不能物理删除防复用墓碑。禁止老finish DELETE queue行，更新为done。
6. 直接SQL持业务角色测试“新增引用到墓碑”被拒：能用FK/触发器覆盖的现有关系补守卫；密文/JSON无法完整约束的范围gc_verified保持false。不得删除约束来使测试通过。

### Worker领取SQL（参数化；设计）

```sql
WITH chosen AS (
 SELECT job_id FROM enterprise_asset_delete_queue
 WHERE attempts<8 AND (
   (job_state IN ('ready','retry_wait') AND next_attempt_at<=clock_timestamp())
   OR (job_state='leased' AND lease_until<clock_timestamp()))
 ORDER BY next_attempt_at,job_id FOR UPDATE SKIP LOCKED LIMIT $1
)
UPDATE enterprise_asset_delete_queue q
 SET job_state='leased',lease_token=$2::uuid,
     lease_until=clock_timestamp()+interval '120 seconds', attempts=attempts+1
 FROM chosen c WHERE q.job_id=c.job_id
 RETURNING q.*;
-- COMMIT，之后才DELETE；一批token可相同但完成须绑定job_id。
UPDATE enterprise_asset_delete_queue
 SET job_state='done',completed_at=clock_timestamp(),lease_token=NULL,lease_until=NULL
 WHERE job_id=$1 AND job_state='leased' AND lease_token=$2::uuid;
```

attempts=8且租约过期必须由reaper转blocked（不再DELETE），不能永远留leased；失效token更新0行按stale处理，不是成功。reaper同样有限批次。所有时钟租约用数据库时钟。

## M4 回填、历史与Down

- 不UPDATE所有旧附件为garage。先只读聚合统计：按scope/location缺失/引用歧义/残留pending/旧queue/flag分组；不导出URL、正文、用户标识到仓库。
- proof manifest：对象唯一ID、来源配置版本摘要、原bucket/实例证明、业务关系证明、预期值摘要、审批与时间；仅合成fixture可入仓。真实历史证明保存在授权的仓外位置。
- 历史bucket=空、旧FastDFS/legacy_key、hash共用、scope不匹配、关联缺失、一键多位置全部保留gc_verified=false。位置已证明不等于引用已证明。
- 每批≤500，按ID keyset，单事务CAS旧值为空、证明版本未变才更新；提交cursor和行数，断点重跑影响0或同值，不覆盖后来的新数据。先dry-run；回填不伴随删除。
- 新FK对历史脏数据先NOT VALID +隔离清单，修复并VALIDATE后Gate通过；不能永久NOT VALID却报告完成。M1新列初始NULL故pending确认FK不需猜测历史。
- 空库Down可逆移除本迁移新增内容；非空/回填后Down使用DO块RAISE EXCEPTION `'V5_DOWN_BLOCKED_USE_FORWARD_FIX'`。判据包括任何非NULL位置、confirmed session、新kind job、非blocked旧job或lifecycle!=live；实现必须列全。
- 已有00000162/163的Down不能成为绕过V5墓碑的路径；迁移执行器禁止跨越V5阻断版本。备份恢复演练使用专用scratch DB +测试bucket/root，不触现有数据。
- 升级与回退测试：旧schema合成历史→当前165→M1/M2/M3→旧读取payload/new上传/worker；模拟每个DDL失败，核对事务原子；空库Down/Up；有任务Down被阻断；数据库恢复后精确身份重放不误删。

## Schema验证门

本轮验证的只有基线164个Up。上述新DDL、触发器、旧数据升级、磁盘行为均 **UNVERIFIED / W1与W6必须执行**。不得把设计SQL能读懂替代编译/执行结果。

## 分阶段加载与中间提交隔离

W0-03 gate提供以下接口并自测失败退出：`--schema-phase base`（原始165）、`--schema-phase expand-queue`（只加载M1/M2）、`--schema-phase guards-contract`（向另一个独立库应用docs内M3 SQL）、`--schema-phase integrated`（W4-04后才允许正式全部Up）。命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --schema-phase <phase>`；没有相应SQL文件必须失败，禁止跳过。
每个phase都重新检查run-owned数据库名/端口、schema hash及phase输入文件hash。W1阶段只启测试PG连接池，禁止启动普通应用/ecron/旧drain；runner启动集成服务前检查M3激活所需W2/W3/W4-01..03 Gate和旧worker未注册断言。W4-04将M3提交到priv/migrations后，重新建integrated scratch库并跑所有写入口。中间commit不具部署/自动迁移资格，不运行make run。若测试框架会自动启动应用，W0必须提供隔离配置/启动门且以进程清单证明旧worker未启动；不能为了跑单测默许旧GC。
location NULL负例必须枚举driver/namespace/container三个字段所有部分NULL组合；ready/leased/done job同样执行。未经G1实测，本文件SQL仅为设计输入。
