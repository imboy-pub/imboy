# 豆包 ASR 2.0 增量合同（待独立审查与冻结）

2026-10-23。用户批准豆包 ASR 与 GLM-5.3-Flash。本文作为修订10候选异步增量合同，状态DRAFT_NOT_ACCEPTED；补充并取代修订9的MiniMax ASR新派发部分，不冒充已实施或已验收；旧 MiniMax 任务、结果未知事实与凭据命名空间保留。T01—T25、A01—A09及 QUALITY.md 阈值不变。产品仍为最长2小时录音、拼装音频小于128MiB、最多200个匿名说话人；供应商上限不自动扩大产品范围。

## 供应商与输入

- 官方标准版 ASR 2.0：resource_id=`volc.seedasr.auc`；请求 `model_name=bigmodel`。 现有应用兼容字段model也取`volc.seedasr.auc`，表示产品所选资源，不等同HTTP的model_name；App与后端必须一致。配置、同意、请求摘要和恢复身份必须同时绑定 provider、resource_id、model_name、分人参数；资源选择器不是具体内部权重版本。
- submit：`POST https://openspeech.bytedance.com/api/v3/auc/bigmodel/submit`；query：相同 origin 的 `/api/v3/auc/bigmodel/query`。新版鉴权为 `X-Api-Key`，不外发 IMBoy JWT、签名头或用户联系方式。
- 标准版读取 `audio.url`；URL必须来自专用外发授权，不能接受客户端任意 URL、不能使用永久公开链接，也不能假定现有普通附件或回听 URL 可供供应商读取。
- 使用成功发布的 assembly 音频附件：现有 App 解密本地录音后上传明文 AAC，服务端渲染并登记派生 AAC；recordKey 不上传。接口签发前必须核实际 assembly、附件 hash/大小及 source manifest。源码追踪不替代真实音频解码验收。
- 首版固定完整request profile：`model_name=bigmodel`、`enable_speaker_info=true`、`show_utterances=true`、`ssd_version="300"`、`enable_itn=true`、`enable_punc=true`、`enable_ddc=false`。请求体与请求摘要必须复用同一份profile对象，不能遗漏默认参数或手工维护两份字段列表。300是模型版本，绝不是人数。不开启声纹注册/实名识别，不把跨任务相同标签自动当作同一人。

## 持久化及不重复提交

客户端 submission_request_id（X-Api-Request-Id）必须在任何外部 submit 前生成并持久化，绑定 record/job/run/attempt/seq、assembly附件与hash、输入版本、actor、provider/resource及外发同意。客户端加密 checkpoint 同样先落盘；原子 intent 成功且返回 may_submit=true 才允许一次 submit。幂等重放永远不能重新授予 may_submit。

新增独立 provider-task 事实，不把原有 terminal unknown 行改回未派发。每个被批准的首次提交意图至多创建一个 submission_request_id；唯一约束覆盖 job、输入、阶段及意图，防止换幂等键、换设备或新建 job 绕过重复计费屏障。本次整合新增迁移为225，保留main的224_retire_moya及已应用的223；旧环境的provider-224仅是历史测试候选，不能作为225的升级凭据。

| 事实 | 可执行动作 | 不允许的动作 |
|---|---|---|
| prepared、尚未获 submit 许可 | 重新验证权限后创建一次 durable intent | 未保存意图就调用供应商 |
| intent 已持久化、submit 回执未知 | 有已保存的 provider_task_id 时只可 query；没有则停在结果未知，单独双重风险确认后才可新提交 | 自动再次 submit、把客户端请求ID冒充供应商task_id |
| provider accepted / processing | 有界 query、显示处理中 | 因手机 lease 过期生成新提交 |
| query 结果可用 | 校验并提交归一结果、按当前权限原子发布 | 用供应商受理回执替代结果或质量验收 |
| submit 明确拒绝 | 保留原请求事实并显示诊断；重新提交需明确授权和风险确认 | 根据一次查询超时、未找到或未知结果认定终态 |
| 已取消、删除、撤权或输入/策略变化 | 停止新外发与发布；保留必要计费审计元数据 | 将取消解释为已撤销供应商处理或退款 |

查询请求被401/402/404/429拒绝，或查询超时/限流/Key失效，只能说明本次查询失败，不能据此将原ASR任务标为终态failed或重新授予付费提交权。保留provider_task_id和unknown/accepted事实，恢复Key、余额或网络后继续查询。当前首版尚未核定可用于重新提交的query终态失败语义，因此query_receipt不接受failed；未支持的错误类型保持unknown，已知ID只查询。

设备执行租约只控制当前设备动作，不代表供应商任务寿命。恢复必须重新取得当前 actor/device、record/企业/工作区、mode、外发同意、policy/config和输入版本授权；query-only恢复明确禁止 submit。旧设备 token 不得发布。 同账号同设备可复用精确绑定本task、尚未过期的当前run执行查询，不旋转租约、不延长音频读取窗口、不新增提交意图；复用source submit-run也不重新授予submit权。其他设备不得抢占有效run，过期后重新领取query-only租约。若以新 run 恢复查询，显式关联原 provider-task，保留旧 unknown事实，不要求为纯查询承认重复提交费用；只有新的 submit 才要求重复计费风险确认。

官方新版示例中，提交头的请求UUID与返回的 task_id 不同，query要求返回的 task_id。因此必须区分 submission_request_id 与 provider_task_id：后者只接受受限响应中的1–128字符标识符（字母、数字、下划线、点、冒号或连字符），在收到后立即保存加密检查点并提交服务端确认。不得假定二者相等，也不得承诺提交回执完全丢失且无task_id时可以查询恢复。该情况保留unknown，不自动重提；若官方以后明确支持按提交ID查询，须另做真实协议验证后启用。

GET恢复视图仅向当前有权主体返回 submission_request_id、可空的provider_task_id、处理阶段、输入pin及允许动作，不能回传API Key、音频URL或ticket。查询请求 ID 不是访问记录的权限，不能作为任意ID查询代理。

## 表结构及 API 实施边界（待冻结）

异步元数据使用新增迁移扩展，旧223不重写；实施编号以当时仓库最大值和其他工作树为准。建议如下，最终字段与约束需跟实际repo/handler/OpenAPI一起审查：

| 对象 | 最小持久字段与不变量 |
|---|---|
| provider_task | 内部id；record_id/job_id；origin_run_id/seq；submission_request_id UUID唯一且不可变；可空provider_task_id仅首次受理回执可填且之后不可换；provider/resource_id/model_name/profile_hash；源assembly/attachment/hash/bytes/duration；输入manifest/content/transcript版本；外发consent/policy/config；phase及安全错误码；创建/更新时间。没有平台APIKey或完整源URL。 |
| provider_task_event | 追加式意图/受理/查询/失败/完成事件，绑定task、当前actor/device/run及规范幂等摘要；不删除或覆盖旧unknown/计费事实，不保存原始响应、音频URL或完整ticket。 |
| recovery association | 新run显式关联旧provider_task，purpose=query_only；原提交来源与本次查询执行者分别留痕。query-only不能创建新的submit意图或获得may_submit。结果提交继续核当前合法run及输入/权限。 |

跨job去重必须覆盖同record、阶段、输入及处理配置；换幂等键/设备不能获得第二次免费提交授权。新的提交尝试必须显式关联前次任务和风险确认。归一结果及安全provider metadata可在原结果体系发布，不创建第二套转写正文存储。

建议在现有 client execution 路径下增加专用动作（名称在OpenAPI冻结时确定，不冒称已有路由）：

- submit-intent：现有JWT+device+run token+ETag+幂等键；原子冻结submission_request_id、输入/profile及外发同意，唯一首次返回may_submit=true。返回的音频capability仅App立即转交供应商，不写普通checkpoint。
- submit-receipt：绑定原task及当前有效执行权限，首次保存合法provider_task_id及受理阶段；不能仅凭受理就发布结果。重放相同事实安全，换ID冲突。
- query-authority：当前主体重新授权后领取query-only能力；要求已有provider_task_id，响应不包含APIKey/源URL，不授予submit权。已丢失task_id的任务保持unknown。
- provider-result / complete：归一校验、精确输入pin及当前policy/consent/ACL重验，唯一原子发布；取消/撤权后不能靠旧查询授权发布。

数据库check/unique/FK/trigger、handler参数白名单、logic授权、repo参数化SQL与OpenAPI必须一致。provider_task长寿命不赋予无限源读取或无限处理许可；任务关闭、留存/删除、跨设备恢复与过期清理需要独立真实测试。

## 实施 wire v1（待候选集成冻结）

基准 `/api/v1/talk-records`，定义 `RUN=/:id/jobs/:job_id/client-executions/:run_id/provider-tasks`。以下是App/BE协作约定，正在实现，不能从本文推断线上接口已存在。

| 方法和路径 | 请求 | 响应及边界 |
|---|---|---|
| POST RUN | `{}`；run token、run ETag、幂等键 | `{task,run,may_submit:false}`；服务器冻结完整assembly为seq0，不使用旧MiniMax底层分段计划。 |
| POST RUN/:task/submit-intents | `{profile_hash}`；task ETag、run token、幂等键 | 首次才有`may_submit:true,source_url`；相同意图重放false且无URL，不能再外发。 |
| POST RUN/:task/submission-receipts | `{client_request_id,provider_task_id,state,status_code?}` | state为accepted/unknown/rejected；accepted必须有合法供应商task_id，换ID冲突。 |
| GET /:id/provider-tasks/:task | 当前JWT | `{task}`，从无source_url/ticket/APIKey；不是向供应商查询的代理。 |
| POST /:id/provider-tasks/:task/query-executions | `{processing_consent}`；task ETag、幂等键、当前JWT/device | `{task,run,run_token,may_submit:false}`；仅已保存task_id时允许，新run只能query/result/complete。 |
| POST RUN/:task/query-intents | `{}`；task ETag、run token、幂等键 | `{task,run,may_query,query_task_id}`；不含源URL，不触发submit意图。 |
| POST RUN/:task/query-receipts | `{provider_task_id,state,result?,result_hash?}` | 首版state为processing/unknown/result_ready；ready才允许严格归一结果。查询错误不能回报failed；历史failed状态不授重提权。 |
| POST RUN/:task/complete | `{}`；task ETag、run token、幂等键 | 原子发布后`{task,run}`，重放不重复发布；旧输入、撤权、删除或策略失效拒绝。 |
| POST /:id/provider-tasks/:task/reconcile-receipt | `{client_request_id,provider_task_id,state:"accepted"}`；当前JWT/device、task ETag、幂等键 | 仅原actor、有当前write权限、同输入policy/consent且已有submit意图时首次补报task_id；不依赖旧run lease，不授予submit/source能力。 |
| POST /:id/provider-tasks/:task/resubmissions | `{processing_consent,ack_possible_duplicate_charge}`；task ETag、幂等键、当前JWT | ack=true仅unknown且无provider_task_id，或明确submit rejected；双重确认风险后创建新prepared Task与submit-purpose run，返回`{task,run,run_token,may_submit:false}`；旧unknown保留，新Task仍需B动作。ack=false仅`allowed_actions`含`resume_preparation`：prepared且无intent、供应商ID、source窗口、request hash及result，原part未派发，且为当前最新Task；单独“继续准备”与本阶段同意后恢复，无重复计费确认。 |
| POST /:id/provider-tasks/:task/cancel | `{}`；task ETag、幂等键、当前JWT | `{task}`；撤销本地source与操作权，保留计费未知事实，不表示供应商已停止或退款。 |

claim成功后、prepare之前退出也必须能恢复，不能因为尚无task_id而永久停住。补充job级发现与未派发恢复：

| 端点 | 输入 | 返回与约束 |
|---|---|---|
| GET /:id/jobs/:job_id/client-executions/latest | 当前JWT/device | `{run,task,allowed_actions,recovery_status}`；run/task可空，不含token/source/付费授权。已有task时转现有任务流程；状态为no_execution/ready/task_available/device_busy/reconciliation_required/published/cancelled。字面latest路由必须先于`:run_id`。 |
| POST /:id/jobs/:job_id/client-executions/recover-unprepared | `{processing_consent}`；latest run IfMatch、独立fresh幂等键、当前JWT/device | `{run,run_token,task:null,may_submit:false}`。record/job锁下确认整个job没有providerTask、dispatch intent/request hash/result/confirmed或unknown派发事实，且当前ACL/policy/input及assembly资产有效；同设备有效run可复用但不续租，其他有效设备返回冲突；仅明确过期且从未派发才创建新run。 |

旧claim或prepare回执丢失时，先读取latest发现服务端事实，再按允许动作保存独立恢复幂等键；不得无限重放失效claim key，也不得以发现或恢复响应为音频提交许可。prepare已成功但回执丢失时，latest返回已有task，后续仍受task的提交屏障控制。

所有task动作响应的HTTP ETag是task ETag，run.etag在payload中独立提供给心跳；不能混用。task视图含id/job_id/source_run_id/attempt/seq、client_request_id、可空provider_task_id、provider/model/resource_id/model_name、完整request_profile/profile_hash、flat输入pins(input_manifest_hash/input_content_revision/input_transcript_version，最后一项int或null)、assembly(id/sha256/byte_size/duration_ms/mime_type=audio/aac)、source_authorization(id/issued_at/expires_at)、state/revision/etag/allowed_actions。wire的client_request_id对应本文概念submission_request_id，二者不是第二个不同ID。

当本地已加密保存provider_task_id、但C回报丢失且旧run过期时，使用reconcile-receipt补报后再领取query-only执行；不能重发音频。G已result_ready但H前退出时，E允许新query-only run从已持久结果恢复confirmed part并直接H，不强制再次query。

准备阶段尚未获任何付费授权却中断或租约过期，也必须能继续：通过专用受控恢复创建带未派发前驱的新run/task，ack=false只在上述精确未派发事实成立时合法，不借普通claim绕过历史屏障。

旧generic客户端plan/dispatch/result/complete/settle不能承接豆包；同job存在任何历史run后，generic claim也不得绕开专用查询/风险重提流程。unknown无task_id时，同设备双确认后可在一个事务内将本task的有效旧run交还为保留费用事实的unknown，再创建新submit-run/task；事务失败不留下半截授权。

App沿用真实加密checkpoint，先保存task/操作幂等键/输入pins；收到provider task_id后先保存再回报服务端。不存source_url或ticket/APIKey；source_url只在原子提交授权的当次内存中交付供应商。ASR切至新豆包路径，GLM旧流保留。显式重新提交必须由单独风险确认触发，不能将普通重试或恢复暗中变成resubmissions。停止本机操作不得抹掉未决任务或未知计费事实，不能通过取消后普通重试绕过风险屏障。个人、企业及各工作区的演示与正式任务状态必须分别绑定当前record/input；打开确认后上下文变化应使确认失效。实现与行为验收未完成前，不能显示虚假可用按钮。

## 音频外发 capability

复用既有派生附件、存储读取和业务引用生命周期，不新增音频桶，不放开通用 derived view_url 拒绝规则。新增独立用途的短时 bearer capability：

读取路径固定为当前部署规范 HTTPS origin 下的 `/api/v1/talk-records/client-audio-source`，query仅允许一个非空、有界的ticket参数；禁止userinfo、fragment、重复query键、其他path/origin或重定向。返回源描述固定audio/aac、ADTS（现有assembly 16kHz单声道AAC），submit使用format=aac，不能把同一字节谎报wav。实际路由在持久provider-task授权合同完成前不开放。


1. 签发时核当前用户/企业/工作区权限、记录未删除、assembly已发布、绑定附件/hash/大小与当前run输入、豆包外发同意及企业策略。
2. ticket使用独立purpose派生签名，绑定部署、record/job/run/attempt/seq、actor、provider/resource、policy/config、assembly附件/hash和到期。不能跨用途使用播放或下载票据。完整ticket不进入日志或普通JSON checkpoint。
3. 无JWT的读取端绑定durable provider-task和原外发授权，不能依赖旧设备300秒执行lease继续有效；逐次重验上述事实与当前撤权/删除/策略状态，返回精确音频字节；只支持受限GET/HEAD/Range，不接受任意对象键、路径或转址。错误不回显token/URL/凭据。响应no-store。
4. 期限有上界；供应商排队抓取及重试窗口须实测，不能凭5小时音频上限把能力延长为5小时。标准query不能更新提交时的audio.url：旧URL不允许暗中换绑或续期，重新签发不同URL不能修复旧任务。到期且未抓取成功时保留明确失败/unknown事实；新的submit仍需显式授权及重复计费风险确认。若以后引入稳定handle和有界续期，须单独定义并审查，当前不宣称支持。实际公网HTTPS可达和无重定向读取必须验收。
5. 到期/撤销仅阻止后续读取，不能承诺收回供应商已读取的字节。附件删除、retain/hold及物理清理仍遵循现有生命周期；外发读取不创建永久业务引用或绕过hold。

## 返回值、Key与质量

HTTP200不等于识别成功；必须核 `X-Api-Status-Code=20000000`及严格响应结构，再解析result.utterances整数毫秒时轴、additions.speaker和文本边界。未知/缺失说话人不得编造；供应商未明确的特殊哨兵规则需保留未知，不能猜测真实姓名。归一结果保留匿名块内label，由服务端唯一添加时间偏移/全局作用域。资源标识可记录为provider metadata，不能声称锁定了供应商内部权重。

兼容部署：服务端 `talk_record_client_processing.doubao_enabled` 默认false，保留MiniMax旧合同；明确启用豆包还需 `audio_source_enabled=true` 和合法HTTPS `base_url`。App使用独立默认关闭的 `TALK_RECORD_DOUBAO_ENABLED` 开关，双端都就绪才能转写，摘要能力独立。

API Key仅设备原生安全存储，新的豆包命名空间不能复用或迁移MiniMax Key；换设备重新填写。用户粘贴在聊天的凭据不写入文件或用于测试。真实调用仅获准公开/合成样本，本轮目标预算5元；重新生成的Key仅在设备填写。GLM继续通用API，年度Max不自动等于API额度。

必须新增验收：submit前崩溃、submit已送达但回执丢失且无task_id时阻断、已保存provider_task_id后的query恢复、query超时、lease过期/换设备、新幂等键绕过、权限/策略/输入变化、源URL过期/撤销、真实解码/外网取音频、完整时轴分人及原质量门。mock、纯解析测试、静态签票成功和供应商HTTP200均不能替代这些验收。

官方依据（2026-10-23核对）：
- https://docs.volcengine.com/docs/DoubaoVoice/task-submission-http-1?lang=zh
- https://docs.volcengine.com/docs/DoubaoVoice/result-query-http-1?lang=zh
- https://docs.volcengine.com/docs/DoubaoVoice/speaker-separation?lang=zh
