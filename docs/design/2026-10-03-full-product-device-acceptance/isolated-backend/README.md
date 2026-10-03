# 隔离PG18准备模板

此模板只准备全产品/E2EE测试的独立数据库资源，尚未启动或验收，BACKEND_READY=false。禁止连接共享imboy_pg18或4323，禁止读取既有账号/生产数据。

启动前必须取得本轮隔离后端/合成账号资源授权并登记租约。必须指定唯一Compose project与ACCEPTANCE_RUN_ID，检查ACCEPTANCE_PG_PORT未占用且不等于4323或15432，现场确认镜像及扩展与当前产品要求一致。ACCEPTANCE_PG_PASSWORD为仅用于本次合成夹具的新密码；无需现有凭证，不写入仓库或报告。Compose config的完整展开内容可能含密码，不保存或打印真实展开值。

`ACCEPTANCE_PG_IMAGE_ID` 必须是独立核验后的本地 `sha256:<64位小写hex>` image ID，
不得使用可变 tag。现场用只读 image inspect 核对该 ID、OS 和架构，记录冻结输入；
本次已观察到 arm64/linux 镜像存在，但 RepoDigests 为空，因此不能声称 registry digest
已核验。Compose 参数必填只防止缺省，不认证输入；启动前仍须验证值确实是现场批准的
镜像 ID。模板保持 pull_policy=never；不为缺失镜像自动访问外部 registry。扩展和实际
PostgreSQL 启动行为只能由获准运行校准，镜像存在不表示 BACKEND_READY。

模板只使用本地已有镜像，内部专用网络、loopback端口和临时PG数据，无共享volume、host数据库挂载、external network或重启策略。tmpfs停止后数据丢失：停止前须先保存脱敏断言、失败历史及hash，不能把此模板当作备份恢复持久性测试的存储。

授权后仍须配置独立App HTTP/Admin端口、生成隔离sys.config，重指向此数据库，并关闭第三方推送/支付/短信/对象写入。模板不包含后端配置或fixture初始化；不能只启动PG便标BACKEND_READY。

完整EUnit入口当前默认读取config/sys.local并使用4323转发，绝不能原样用于此模板。须核对EUNIT_CONFIG/EUNIT_RELAY_TARGET及所有专项PG配置，确认没有共享资源回退。运行前验证迁移、namespace、合成账号与清理owner；AG31专项和TSID独占套件单独运行。最终门要求冻结候选的完整两轮成功及被排除套件证据。

资源回收只针对登记的唯一project和run标签；不能使用全局prune、删除共享容器或无owner检查清理。当前阶段只执行离线Compose配置校验，不执行up/down或数据库连接。
