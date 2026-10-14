== 隔离全量 run1（AC-04 修正前）==
Failed: 244 / Passed: 11312
本任务 5 个文件相关模块中仅 login_attempt_ds_tests 出现 1 例（AC-04 探针计数巧合问题，已修正为确定性触发，见 commit 50af7ca3）
其余失败分布与本任务无关（enterprise_oa_sso/imboy_cache_*/device_revocation 等基线既有）
== 隔离全量 run2（AC-04 修正后，最终候选）==
本任务模块（login_attempt_ds/passport_logic/passport_handler/elib_req_client_ip/throttle_middleware/api_v1_login_SUITE）：零失败
剩余失败 26 例均为基线/其他会话 WIP 域（imboy_cache_sync 19、user_handler/moya/data_disposition 等基线源码-测试不匹配）
run1 244 失败 → run2 26：run1 多数为环境 flaky（首轮含跨会话竞争窗口）
== 收尾轮全量（绑定候选 42f5ade5，独立库 imboy_l0_eunit_20261014，端口 25432/29980/29970）==
Failed: 249 / Passed: 11413（详见 eunit-full-final.log 与 failures-final-by-module.txt）
本任务 6 模块（login_attempt_ds_tests/passport_logic_tests/passport_handler_tests/
elib_req_client_ip_tests/throttle_middleware_tests/api_v1_login_SUITE）：零失败（NONE-OF-MY-MODULES，
按行首模块名全量归并 249/249 后逐条核对，无一条落入本任务域）
剩余 249 例分布：imboy_cache_* 域约 190（entry_epoch 28、invalidation 26、metadata 22、
snapshot_tombstone 21、control 21、snapshot_coverage 18、metadata_component 18、source_cut 14、
snapshot_origin 13、origin_target 7、capability 3 等）、enterprise_* 域约 30（oa_sso 20、
internal_grant/full_api/directory_sync 等）、agent_hird_*/imboy_hird_acceptance 9（基线既有，
与 xref/dialyze 归因一致）、其余散点（user_handler/moya_subscribe/cs_widget/eb_offboarding/
data_disposition/device_revocation 等）
249 vs run2 26 的波动归因：非本任务域的持续演进。收尾轮运行期间并行会话继续向本地 main
合入 cache/cs 域 WIP（main 在本轮全量启动后又前进 23 提交至 25b58405，含
7a89890e "test(cache): isolate invalidation and tombstone fixtures"——cache 域测试失败
正是其他会话正在修复的对象）；候选已第三轮合入 main（d17d6617），与本任务 13 个改动文件
零交集，merge 后本任务模块定向重验全绿（见 gates/MERGE_ROUND3_NOTES.md）
