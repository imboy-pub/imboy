# V5.5 迁移合同 / Migration contract

本文件SQL是未来迁移设计输入，**本轮未执行、未写入priv/migrations**。W1-02必须在专用PG中完成完整Up/Down/upgrade验证，才能冻结可执行迁移；不能把本文片段当已经验收的迁移。编号在实施时重新取当前最大值+1，本轮最大版本为00000172，下一可用版本须重核，不预占00000173，不预占其他会话编号。

## M1 必要性与表复用

F02/F03/F06证明现有表无完整位置/状态；F08/F10证明已有队列但缺通用主体与可靠领取。最小新增表数=0；迁移168的普通队列和Workspace永久保留账本已存在，必须复用；新增字段/触发器不是第二套生命周期。保留业务status、EAR/EB治理、现有引用结构；不改历史Up/Down。

建议拆三个有序迁移：M1-expand（位置/状态）、M2-queue（扩展现有队列、安装全部协议及守卫函数定义）、M3-guards（挂接最终写入守卫/收紧权限）。M1默认不赋予清理资格；M2现有job一律blocked等待位置证明；M3的定义与DML负例先在独立scratch验证，**实际迁移文件只在W4-04全部写入口适配后进入priv/migrations**；W1-02只提交M1/M2与M3 SQL合同（docs内），不提交可被自动加载的M3文件。迁移与worker切换不可穿插运行旧drain。

### M1-expand SQL合同

以下各ALTER在原迁移器单事务执行，lock_timeout=5s，statement_timeout=15min；超时立即回滚，最多2次重试，禁止自动增大超时或禁触发器。

```sql
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='15min';
ALTER TABLE attachment
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN storage_locator_sha256 text,
 ADD COLUMN lifecycle_state text NOT NULL DEFAULT 'live',
 ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;
ALTER TABLE attach_pending
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN storage_locator_sha256 text,
 ADD COLUMN client_upload_id uuid,
 ADD COLUMN upload_registration jsonb,
 ADD COLUMN upload_content jsonb,
 ADD COLUMN session_deadline timestamptz,
 ADD COLUMN scope_ref text,
 ADD COLUMN session_state text NOT NULL DEFAULT 'registered',
 ADD COLUMN upload_expires_at timestamptz,
 ADD COLUMN upload_drained_at timestamptz,
 ADD COLUMN confirmed_attachment_id bigint,
 ADD COLUMN gc_verified boolean NOT NULL DEFAULT false;
ALTER TABLE enterprise_asset
 ADD COLUMN client_upload_id uuid,
 ADD COLUMN upload_registration jsonb,
 ADD COLUMN upload_content jsonb,
 ADD COLUMN session_deadline timestamptz,
 ADD COLUMN upload_expires_at timestamptz,
 ADD COLUMN upload_drained_at timestamptz,
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN storage_locator_sha256 text,
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
ALTER TABLE attach_pending ADD CONSTRAINT ck_v5_session_link
 CHECK ((session_state='confirmed') = (confirmed_attachment_id IS NOT NULL));
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

### M2-queue SQL合同（替代旧通用企业队列设计）

保留两队列主键/FK和字段，禁止DROP PK、搬迁普通队列或新增第三套队列。以下是完整新增字段模板；M1 location CHECK对两个队列同样实施。W1须补齐实际触发器函数、Up/Down与升级测试，未完成不得宣称SQL合同已可部署。

```sql
SET LOCAL lock_timeout='5s';
SET LOCAL statement_timeout='15min';
ALTER TABLE attachment_object_deletion
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN storage_locator_sha256 text,
 ADD COLUMN job_state text NOT NULL DEFAULT 'blocked',
 ADD COLUMN lease_token uuid,
 ADD COLUMN lease_until timestamptz,
 ADD COLUMN last_error_code text DEFAULT 'legacy_location_unverified';
ALTER TABLE enterprise_asset_delete_queue
 ADD COLUMN client_upload_id uuid,
 ADD COLUMN upload_registration jsonb,
 ADD COLUMN storage_driver text,
 ADD COLUMN storage_namespace text,
 ADD COLUMN storage_container text,
 ADD COLUMN storage_locator_sha256 text,
 ADD COLUMN job_state text NOT NULL DEFAULT 'blocked',
 ADD COLUMN attempts integer NOT NULL DEFAULT 0,
 ADD COLUMN next_retry_at timestamptz NOT NULL DEFAULT now(),
 ADD COLUMN lease_token uuid,
 ADD COLUMN lease_until timestamptz,
 ADD COLUMN last_error_code text DEFAULT 'legacy_location_unverified',
 ADD COLUMN completed_at timestamptz;
```

普通既有attempts非零不得清零；旧completed_at非NULL映射done，即使位置未知也只保留tombstone，永不再次DELETE。其余旧job均blocked；旧attempts超过8也保留，约束不能让升级失败，runtime限制attempts>=8不再claim。两个queue新增同形CHECK：state合法、attempts>=0；leased必须token/until非NULL，非leased二者必须NULL；ready/leased/retry_wait要求完整有效I；done允许未知旧I但completed_at必须非NULL，非done为NULL。必须显式IS NOT NULL，不依赖CHECK UNKNOWN。普通bucket保留，完整Garage I与bucket必须相等；企业对象位置不能从运行时默认配置猜出。

企业恢复墓碑M2另需：

```sql
ALTER TABLE enterprise_asset_delete_queue ADD CONSTRAINT ck_v5_eb_recovery_snapshot CHECK (
 (client_upload_id IS NULL AND upload_registration IS NULL)
 OR (client_upload_id IS NOT NULL AND upload_registration IS NOT NULL
     AND jsonb_typeof(upload_registration) = 'object'));
CREATE UNIQUE INDEX uq_v5_eb_deleted_client_upload
 ON enterprise_asset_delete_queue(client_upload_id) WHERE client_upload_id IS NOT NULL;
```

M2的register及intent/Purge函数实现跨enterprise_asset与queue的UUID查重和原行快照复制，具体锁序/授权/COMMIT_UNKNOWN见JSON enterprise_recovery_tombstone。M3禁止业务角色绕函数直接INSERT企业登记行或更改UUID/快照，metadata DELETE守卫验证原UUID/快照已等值落入同asset的queue；无记录或不等值整事务失败。源client_upload_id为NULL时queue两恢复字段都NULL，不复制孤立声明；queue旧NULL行仅保留，不猜测历史UUID；带UUID新记录不得丢快照。角色直接DML、跨表竞争、复制后回滚、done重复登记均须测；有任何恢复墓碑时Down阻断。索引负载/5秒writer阻塞边界仍适用，不因新增索引放宽。

两个queue各建(next_retry_at,稳定主键)的ready/retry_wait/leased部分索引；普通既有completed_at索引保留直至新查询执行计划证明可替换。以同一不可变domain分离key空间，写入事务先取跨domain共同完整I锁，再取既有path锁，随后查另一queue与两类metadata以拒绝已证明跨domain同I；不能用各表UNIQUE声称跨表唯一。历史混用禁止自动GC。企业保留ON CONFLICT(org,ws,asset_id)兼容；enqueue必须显式给新state/location。新旧worker不混跑。done永久保留；完成改UPDATE而非DELETE。

M2期间旧任务未证明位置保持blocked，上传读兼容沿用M1。W4-04适配全部producer后激活guard。proof仅零尝试且从未leased的历史blocked job可首次激活；普通旧attempts>0未完成任务不能套用该例外。新runtime禁止写legacy_location_unverified。

### M3-guards合同

触发器函数定义由M2安装，M3仅挂接；函数使用新前缀`fn_v5_attachment_*`，禁止CREATE OR REPLACE覆写企业既有函数。W1-02提交完整SQL，并以直接DML负例实测，而非只检索触发器名称。

1. 三表位置全空→完整只能受控证明回填或新INSERT；填入后driver/namespace/container/key不可变。runtime role不得拥有表/禁触发器权限。metadata.creator、组织/Workspace键、scope/cipher/anchor_msg_id不可变；EB uploaded_by_user_id依audit_actor_contract保留合法FK匿名化，不作为业务归属或GC授权依据。
2. scope_ref允许moment从NULL首绑，anchor_conv_seq从NULL绑定确定序号，group_file_id从NULL首绑；已绑定不同值拒绝。首绑也必须live，无delete job。合法迁移回填路径需独立权限，审计记录证明清单。
3. attachment/EB life只允许live→delete_intent→deleted；session状态按主计划；metadata物理DELETE禁止绕过提交意图。既有EB Purge可保留删metadata方式，但同事务必须已写完整job，既有Hold/Retention触发器仍生效；其asset ID及client_upload_id/原主体声明终身墓碑由既有job保护，必须复制后才允许删metadata；恢复查queue，不创建新资产。
4. EAR在对象delete_intent/deleted时拒绝新增Hold/延长留存；EB新增Hold锁Workspace后裁决；所有direct DML入口不能绕开此顺序。普通物理DELETE（包括管理员、账号注销）改为受控资格事务，不依赖存储侧失败来保护。
5. job的队列domain、既有主键、object key和创建时间不可改；I填入后不可改。旧job只允许专用proof迁移角色在blocked、attempts=0、lease_token/lease_until/completed_at全NULL时，将三个location字段从全NULL一次性填为完整值；必须同事务写证明审计并过跨queue完整identity检查；runtime role无此权限。last_error_code=legacy_location_unverified且上述零尝试条件成立时，proof事务必须重验全部删除资格，才允许首次blocked→ready并清此错误码；无法证明历史资格或位置则继续blocked，不复活任何已经尝试的任务。state修改检查租约CAS；done不可复活；不能物理删除防复用墓碑。两个queue禁止老finish DELETE行，更新为done。
6. 直接SQL持业务角色测试“新增引用到墓碑”被拒：能用FK/触发器覆盖的现有关系补守卫；密文/JSON无法完整约束的范围gc_verified保持false。不得删除约束来使测试通过。

### Worker领取SQL（同形参数化；设计）

普通队列具体模板：每次claim仅一行、$1为该任务新token；批次最多100次claim。企业版本同样单行claim，以三列复合主键JOIN，不引入job_id：

```sql
WITH candidate AS (
 SELECT path FROM attachment_object_deletion
 WHERE attempts<8 AND (
  (job_state IN ('ready','retry_wait') AND next_retry_at<=clock_timestamp())
  OR (job_state='leased' AND lease_until<clock_timestamp()))
 ORDER BY next_retry_at,path FOR UPDATE SKIP LOCKED LIMIT 1
)
UPDATE attachment_object_deletion q
 SET job_state='leased',lease_token=$1,lease_until=clock_timestamp()+interval '120 seconds',
     attempts=q.attempts+1,last_error_code=NULL
 FROM candidate c WHERE q.path=c.path RETURNING q.*;
```

claim提交后DELETE；完成/退避UPDATE WHERE主键+lease_token+state=leased，0行表示过期worker，不覆盖新租约。第8次已leased任务崩溃且过期，由reaper CAS为blocked，不能因attempts<8过滤后永久停留leased。重新claim之前完成旧租约核验；任何已在途旧DELETE都只作用永久不复用I。普通/企业各有非饥饿排序与独立限额。不同任务逐个生成token，不依赖整批同token表达worker身份。

## M4 回填、历史与Down

- 不UPDATE所有旧附件为garage。先只读聚合统计：按scope/location缺失/引用歧义/残留pending/旧queue/flag分组；不导出URL、正文、用户标识到仓库。
- proof manifest：对象唯一ID、来源配置版本摘要、原bucket/实例证明、业务关系证明、预期值摘要、审批与时间；仅合成fixture可入仓。真实历史证明保存在授权的仓外位置。
- 历史bucket=空、旧FastDFS/legacy_key、hash共用、scope不匹配、关联缺失、一键多位置全部保留gc_verified=false。位置已证明不等于引用已证明。
- 每批≤500，按ID keyset，单事务CAS旧值为空、证明版本未变才更新；提交cursor和行数，断点重跑影响0或同值，不覆盖后来的新数据。先dry-run；回填不伴随删除。
- 新FK对历史脏数据先NOT VALID +隔离清单，修复并VALIDATE后Gate通过；不能永久NOT VALID却报告完成。M1新列初始NULL故pending确认FK不需猜测历史。
- 空库Down可逆移除本迁移新增内容；非空/回填后Down使用DO块RAISE EXCEPTION `'V5_DOWN_BLOCKED_USE_FORWARD_FIX'`。判据包括任何非NULL位置、confirmed session、新kind job、非blocked旧job或lifecycle!=live；实现必须列全。
- 已有00000162/163的Down不能成为绕过V5墓碑的路径；迁移执行器禁止跨越V5阻断版本。备份恢复演练使用专用scratch DB +测试bucket/root，不触现有数据。
- 升级与回退测试：旧schema合成历史→当前W0冻结的172/171原Up集合（执行前按hash重核）→M1/M2/M3→旧读取payload/new上传/worker；模拟每个DDL失败，核对事务原子；空库Down/Up；有任务Down被阻断；数据库恢复后精确身份重放不误删。

## Schema验证门

历史证据仅支持原基线170个Up；本次修订未重跑数据库。上述新DDL、触发器、旧数据升级、磁盘行为均 **UNVERIFIED / W1与W6必须执行**。不得把设计SQL能读懂替代编译/执行结果。

## 分阶段加载与中间提交隔离

W0-03 gate提供以下接口并自测失败退出：`--schema-phase base`（W0冻结的172/171原Up集合（执行前重核hash））、`--schema-phase expand-queue`（只加载M1/M2）、`--schema-phase guards-contract`（向另一个独立库应用docs内M3 SQL）、`--schema-phase integrated`（W4-04后才允许正式全部Up）。命令：`bash scripts/test/attachment_lifecycle_v5_gate.sh --schema-phase <phase>`；没有相应SQL文件必须失败，禁止跳过。
W1-01只调用base并冻结静态状态/锁/位置/治理合同，不加载尚由W1-02编写的guard SQL。W1-02按JSON schema_phase_sequence先验证expand-queue，再在另一独立scratch库验证guards-contract；各自记录原Up、M1/M2、docs M3输入hash及catalog前后态。migration/down-block在两阶段分别验收，immutable/session-link-integrity在guards-contract验收；禁止复合phase参数或把第一阶段成功当作第二阶段通过。DDL提交结果未知只读调和，矛盾CONTRACT_CONFLICT，guard安全断言失败NO_GO。
每个phase都重新检查run-owned数据库名/端口、schema hash及phase输入文件hash。W1阶段只启测试PG连接池，禁止启动普通应用/ecron/旧drain；runner启动集成服务前检查M3激活所需W2/W3/W4-01..03 Gate和旧worker未注册断言。W4-04将M3提交到priv/migrations后，重新建integrated scratch库并跑所有写入口。中间commit不具部署/自动迁移资格，不运行make run。若测试框架会自动启动应用，W0必须提供隔离配置/启动门且以进程清单证明旧worker未启动；不能为了跑单测默许旧GC。
location NULL负例必须枚举driver/namespace/container三个字段所有部分NULL组合；ready/leased/retry_wait job同样执行；历史done未知位置允许但只保留终态，不能重新领取。未经G1实测，本文件SQL仅为设计输入。

当前原迁移171重放不等于M1/M2/M3验证。M3保持W4-04才正式加载；本轮不创建迁移文件。Down同时检查两个queue所有未完成/已完成tombstone与新身份：无法回退旧安全语义则硬阻断，禁止删除证据或清队列强行Down；只允许forward-fix。

## Session关联与namespace解析：强制数据合同

confirmed当且仅当confirmed_attachment_id非NULL。首次confirmed必须同事务比对目标path/key、完整I、storage_locator_sha256、creator、初始scope及合法首绑规则；confirm首次确定cipher声明，之后只接受与已存metadata等价声明。confirmed id不可改，原session不可改回registered或删除关联。非confirmed的id必须NULL；历史旧session不猜关联、不假标confirmed。moment scope_ref首绑仍按原合同允许，session保存原上传scope声明，不能因合法首绑而强迫改session身份。W1补BEFORE状态/关联触发器，直接DML负例覆盖confirmed+NULL、registered+ID、错key但合法FK、同key错owner/scope/location/cipher、确认后重绑和重新registered；每例事务零变更。合法重复confirm返回原id且计数/审计不增加。

storage_locator_sha256是每个namespace不可变定位记录的SHA256（不含凭据）；新增此列的必要性是防止namespace字符串不变但endpoint/root被重映射。三主体与两queue同样存储，首次写入后不可变；历史NULL默认不能GC。active job与gc_verified要求digest非NULL且匹配`^[0-9a-f]{64}$`；历史done允许NULL，仅保留墓碑。M1旧读兼容不要求补值；回填仅proof事务一次，必须同时冻结完整I和digest。不可把bucket、namespace名或全局当前配置hash当定位证明。

定位记录为(driver,namespace,storage_root_id,canonical_internal_endpoint_or_root,container_rules,locator_digest)，配置复用config/sys.config中的版本化registry，不新建DB表。root_id表示物理存储根，不随endpoint别名改变；一个root只能有一个canonical namespace，别名只能在配置中指向该canonical记录。canonical endpoint保留协议/host/端口/路径，凭据轮换不改定位摘要；Disk取固定root及访问受控的真实路径，不允许符号链接重定向。新registry只允许新增根或显式别名，不能删除/重映射已有根。namespace曾使用即永不复用。

W1冻结locator规范与摘要字节编码，W4用run-owned随机哨兵对象验证两入口是否同物理root；未声明别名读到对方哨兵则注册失败，不同host/namespace字符串不能证明分离。权限不足、定位来源不可信或不能证明配置继承则namespace未验证，拒绝签发和DELETE，禁止回退default。此验证仅合成资源，生产注册/配置切换另需用户授权。runtime每次解析核对持久digest；default切换只影响新上传，旧对象按冻结registry条目解析。

跨domain锁统一以规范JSON数组[driver,storage_root_id,container,object_key]的UTF-8字节为输入，PG `hashtextextended(jsonb_build_array($1::text,$2::text,$3::text,$4::text)::text,1685)`；所有字符串保留原字节/大小写，不对key做URL解码、Unicode归一或路径简化；普通legacy path锁仍用seed168。同物理root别名先归canonical再取锁。哈希碰撞只能串行化，锁内还须比对完整身份。不同实现必须用同一规范并通过Erlang/PG相同输入测试，不能各自拼字符串。

## V5.4 SQL补全责任与激活边界

M1对attach_pending、enterprise_asset分别新增可空client_upload_id UUID和部分唯一索引（WHERE client_upload_id IS NOT NULL）；旧行保持NULL，不猜测恢复ID。两表session_deadline固定不可延长，upload_expires_at为历次签发预留最大值且不超过deadline；旧行允许NULL但禁止新能力/GC，直至证据证明。新V5登记必须同时有deadline与expiry；upload_drained_at只能在最后能力到期之后由受控drain证明写入。renew只能registered/EB pending_confirm，按02同锁原子更新expiry，不能缩短旧值、改ID/声明或重新激活终态。合法旧客户端新登记同样由服务端填写时间。状态查询不写expiry。W1-02完成CHECK、索引、Up/Down和直接DML断言，W2使用之。

M2安装两队列受限intent/claim/ack/reaper/proof及register/confirm/pin函数和M3触发器函数定义，配置NOLOGIN owner、固定search_path、REVOKE PUBLIC EXECUTE与精确EXECUTE授权；M3只挂接最终守卫并收紧旧表/列权限，不另建队列或业务表。意图函数必须读取完整资格；未知引用保持gc_verified=false，不把调用者提供的布尔值当证明。enqueue写life/session intent、job ready和既有审计同事务；不接受外部任意location/state/attempt/audit。INSERT同样不得伪造gc_verified=true或intent/终态；合法注册只可写live/registered初值，宽泛INSERT grant不能绕过列权限/BEFORE检查。受保护列直接更改由权限及触发器拒绝，延迟一致性检查在提交前保证精确身份job与intent/审计配套。函数不自行COMMIT，调用方事务未知不得DELETE。

权限合同必须有可执行SQL：NOLOGIN函数owner、固定search_path、schema-qualified对象、REVOKE PUBLIC EXECUTE、明确GRANT EXECUTE；运行角色不拥有表/超级用户/owner成员资格。两个queue撤销runtime表级INSERT/UPDATE/DELETE；metadata/session先撤销宽泛UPDATE再授予不受保护列的显式列表，不能指望列级REVOKE覆盖表级GRANT。Worker无enqueue/任意更改attempt能力；proof独立角色不可由runtime SET ROLE。W1冻结完整权限catalog，W4-04检查实际角色继承、函数签名和全部writer兼容；权限不符合即停止，不扩大grant“修复”。

W1-02/intent-authority对两个队列逐项直接DML：伪造ready/leased/retry_wait/done、历史未证明激活、伪造gc_verified（UPDATE及INSERT分开）、直接session意图、live引用/Hold/未来Retention下形成意图、缺审计、缺metadata转移、直接改life、runtime切owner、Worker任意enqueue。均须拒绝且零变化。另测合法意图与审计原子成功。W6在integrated真实角色与两个driver重跑，拒绝路径字节保持；不能用mock权限或超级用户运行后声称业务角色受限。

uploaded_by_user_id非NULL→NULL仅在OLD用户于同事务已不存在且其余受保护字段不变时许可，保留迁移118 FK；父用户仍存在时置NULL、换成另一用户均拒绝。W1/guards-contract真实用户DELETE验证触发器可见性，W3/W6证明不级联企业资产/治理或授权清理。不得仅用pg_trigger_depth辨认可信写入。

大表边界：lock_timeout=5s是取得锁上限，15min是迁移总statement上限，不能误作可接受15分钟阻塞业务写入。W1-02用合成规模fixture（至少100万行，或经授权只读统计所得最大表行数，两者取大）及持续读写负载记录表/索引大小、最长写阻塞和DDL时间；允许取得锁后连续写阻塞上限5秒，监测者超限取消迁移并核对回滚。缺资源则BLOCKED_EXTERNAL，不能用空表成绩放行。普通CREATE INDEX须在此停写窗口内实测；超界必须先修订为有独立恢复/ledger合同的CONCURRENTLY阶段，禁止把CONCURRENTLY塞进现有单事务迁移或自动延长窗口。NOT VALID检查/FK与批量回填分开，批≤500、锁后CAS、提交cursor、恢复不重置证明版本；最终VALIDATE必过。以上是未来isolated验证/发布边界，不授权生产测试。

滚动期M1/M2仅扩展且gc默认关闭，旧producer写入结果默认不可清理；禁止混跑旧删除worker。W2/W3/W4-01..03按任务适配writer，W4-04先生成明确writer矩阵并完成所有兼容性（注册/confirm/引用/解绑/Hold/Retention/匿名化/proof/enqueue/claim/ack）。guards-contract库可提前验证，正式M3仅在适配全部完成后进入priv；中间commit没有自动部署资格。M3提交结果未知依strict ledger+catalog+输入hash只读调和，不靠重跑DDL试探。


## V5.4 登记/内容快照及函数安装完整性

M1的两个JSONB列仅加在attach_pending与enterprise_asset，不复制到attachment、不另造session表。upload_registration全字段形状/规范化为JSON upload_registration_contract；upload_content形状为{version:1,sha256:小写64hex,size_bytes:非负十进制字符串,mime:校验值}。W1-02必须输出可执行CHECK（类型、固定键集合、必需非NULL字段、合法surface/subject、EB/widget必需域），显式拒绝JSON null与SQL NULL混淆或CHECK UNKNOWN逃逸；旧整列NULL保留兼容。新受限register必须写完整registration且content初始NULL；受限pin只允许NULL→真实计算receipt一次，相同receipt幂等、不同值拒绝；confirmed前必须已有receipt。expand阶段这些条件由函数保障，M3再封闭直接DML路径；不把应用自述当数据库保护。

registration首次写入后不可变；content固定后不可清空/替换；包括INSERT伪造已确认/receipt、UPDATE清空或异值均拒绝。proof仅旧未知、足够来源及写能力drain证明、同锁审计后一次填入，不得重置已pin值；未知保持保护。声明中原scope_ref不随moment合法首绑改变。普通E2EE原object_hash允许NULL，真实pin保存密文hash；绝不以明文hash作为登记必填。上传主体不是可匿名化审计actor，账号删除拒绝后续恢复而不级联改写快照。

Down阻断判据追加：任何非NULL client_upload_id、upload_registration、upload_content、session_deadline、upload_expires_at/upload_drained_at，及已有意图/墓碑；不能只检查location/life漏掉持久登记。空库Down逆序移除本版权限/触发器/函数/列且核对owner及依赖，无数据时才可执行。任何不兼容持久状态只允许forward-fix。

W1-02分别记录expand-queue与guards-contract的函数清单(signature/owner/definition SHA256/ACL)。前者必须能调用注册/pin/confirm/intent/claim/ack/reaper/proof，后者不得补装缺函数，函数定义摘要必须相同；合法调用与错误授权分开测试。两库必须使用相同M1/M2字节；M3只挂接trigger/最终一致性约束并撤销旧宽DML权限。W2/W3/W4-01..03每次校验所需函数可用，W4-04再以完整writer矩阵加载正式M3。修改函数定义需要新forward迁移及受影响重验，不能CREATE OR REPLACE悄悄改变已验输入。

普通session的confirmed是终态，永远保留confirmed_attachment_id；已确认资产GC只转metadata life并写job，不能把其session改成delete_intent/deleted而违反confirmed iff linked约束。从未确认的session才走rejected/expired→delete_intent→deleted。恢复回原asset ID并明确metadata物理状态，不能把原confirmed结果当作仍可读写。W1 session-link-integrity及W6双driver验证该全程。

V5.5不增加表：EB metadata仍可物理清理，message RESTRICT删除顺序保持；queue不加指向message/user/contact的FK。迁移172阅读子行CASCADE兼容由W4-02实际Purge及W6验证。W1-02必须把恢复查重/复制所需函数定义放M2，不能到M3才补函数。
