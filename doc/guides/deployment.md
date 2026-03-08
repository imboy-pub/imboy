# Imboy 部署与功能开关联调清单

> Last Updated: 2026-03-08  
> Status: 长期交付与部署文档  
> Related docs: `doc/guides/customer-acceptance-checklist.md`, `doc/guides/module-feature-flag-config-draft.md`, `doc/operations/dependencies.md`

## 功能开关部署与联调执行清单（2026-03-08）

说明：仓库不再保留绑定具体域名、机器 IP、证书路径的 `nginx` / 文件存储样例配置；此类模板应在交付环境或独立运维仓按环境维护。

适用场景：

- 面向客户交付基础版 / 专业版 / 行业扩展版；
- 需要通过 `features` 控制二期模块实际生效范围；
- App 与管理后台都要围绕同一份后端功能矩阵做入口控制与关闭态兜底。

### 一、部署前准备

发布前至少确认以下事项：

1. 已明确本次交付版本的售卖范围：
   - 基础版：建议关闭 `location`、`moment`、`channel_discover`、`channel_invitation`、`channel_order`、`group_vote`、`group_schedule`、`group_task`；
   - 专业版：按合同显式打开 `channel` 及其必要子能力；
   - 行业扩展版：按项目范围逐项开启二期模块。
2. 运维已准备实际部署使用的 `sys.config`，不要依赖“缺省开启”作为正式交付配置；
3. App、后台、实施方都已确认功能关闭时的预期表现：
   - App 隐藏入口；
   - 后台隐藏菜单或页面入口；
   - 后端统一返回 `5190 / 功能未启用`；
   - 不出现点进后白屏、404、死链接。

### 二、配置落地

后端配置以 `config/sys.config.example` 中的 `features` 配置块为模板。

建议执行顺序：

1. 在目标环境 `sys.config` 中显式写出全部已登记功能；
2. 对未售卖模块写 `enabled => false`，不要省略；
3. 对 `channel_discover`、`channel_invitation`、`channel_order`，先确认 `channel` 是否已开启；
4. 配置变更后重启服务，并确认新配置已被进程读取。

推荐最小核对项：

- `core` 是否为 `true`；
- `channel=false` 时，三个 `channel_*` 子能力是否也对外表现为 `false`；
- 没有误把二期模块留在“缺省开启”状态。

### 三、接口联调

当前以两个只读接口作为三端统一事实源：

- 公共端：`GET /v1/app/features`
- 后台端：`GET /adm/admin/config/features`

联调步骤：

1. 部署后先请求 `GET /v1/app/features`，确认返回 `payload` 与目标售卖版本一致；
2. 使用具备 `settings:view` 权限的后台账号请求 `GET /adm/admin/config/features`，确认返回值与 App 侧一致；
3. 使用不具备 `settings:view` 权限的后台账号验证应返回无权限结果；
4. 对一个已关闭模块做反向验证：
   - 入口隐藏；
   - 手工直调业务接口返回 `5190`；
   - 业务页面没有出现部分元素仍可操作的情况。

建议联调命令示例：

```bash
curl -sS https://<domain>/v1/app/features
curl -sS -H 'authorization: Bearer <token>' https://<domain>/adm/admin/config/features
```

如需一次性做公共端 / 后台端 / 低权限拒绝 / 期望值核对，可直接执行：

```bash
bash ./script/run_feature_flag_smoke.sh \
  --base-url https://<domain> \
  --admin-header 'authorization: Bearer <admin_token>' \
  --forbidden-header 'authorization: Bearer <limited_token>' \
  --expect core=true \
  --expect channel=true \
  --expect moment=false \
  --expect group_task=false
```

该脚本会完成四类检查：

1. `GET /v1/app/features` 是否返回成功；
2. `GET /adm/admin/config/features` 是否返回成功且与公共端 `payload` 一致；
3. 低权限后台账号是否被正确拒绝；
4. 关键模块开关是否符合本次交付预期。

如果你更习惯 `make`，也可以直接执行：

```bash
make feature-smoke \
  FEATURE_SMOKE_BASE_URL=https://<domain> \
  FEATURE_SMOKE_ADMIN_HEADER='authorization: Bearer <admin_token>' \
  FEATURE_SMOKE_FORBIDDEN_HEADER='authorization: Bearer <limited_token>' \
  FEATURE_SMOKE_EXPECTS='core=true channel=true moment=false group_task=false'
```

### 四、三端验收口径

App 侧至少验收：

1. 首页 / 个人页 / 群详情 / 频道入口是否按开关显隐；
2. 已缓存旧入口时，进入关闭模块是否提示“功能未启用”；
3. 启动阶段拉到的功能矩阵是否可覆盖本地旧缓存。

后台侧至少验收：

1. 菜单、列表页、详情页、操作按钮是否与功能矩阵一致；
2. 无 `settings:view` 权限账号不能读取后台功能矩阵；
3. `moment`、`group_vote`、`group_schedule`、`group_task`、频道邀请/订单相关页面关闭后不再暴露。

后端侧至少验收：

1. `moment`、`location`、`group_vote`、`group_schedule`、`group_task` 的公共接口拦截生效；
2. `channel_discover`、`channel_invitation`、`channel_order` 是按 action 级别拦截，而不是整条频道能力全关；
3. 后台管理接口与公共接口的拦截口径一致。

### 五、发布与回滚

建议发布顺序：

1. 先发后端配置与接口；
2. 再发 App / 后台入口显隐；
3. 最后做客户验收与截图留档。

若出现配置错误，优先按以下顺序回滚：

1. 回滚 `sys.config` 中的 `features` 配置块；
2. 重启服务并重新核对 `GET /v1/app/features`；
3. 必要时临时恢复入口显隐配置，但不要跳过后端校验。

### 六、常见风险

1. `features` 未显式配置，导致新登记功能被“缺省开启”；
2. 只做了前端隐藏，没有做后端接口拦截；
3. 只验了 App，没有验后台权限差异；
4. 子能力开关打开了，但父能力 `channel=false`，导致实施方误判为配置失效；
5. 客户验收时只看 UI，没有做接口级验证。
