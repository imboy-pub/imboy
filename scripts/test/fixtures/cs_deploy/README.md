# cs_deploy 端到端离线 harness fixtures（CSD-TEST-01 / run 20260920t150315）

被 `scripts/test/customer_service_deploy_test.sh` 消费的隔离 fixture。全部内容均为
明显假数据：假域名 `cs.test.local`、假主机 `deploy-fake-20260920t150315.test.invalid`、
假上游端口；**零真实 secret、零真实证书私钥、零网络访问**（ssh/rsync/scp/nginx/curl/
certbot/bun 全部为 PATH 注入的 fake binary）。

## 布局

| 路径 | 用途 |
|---|---|
| `build-src/package.json` | fake 本地构建源码仓：`cs` 的 `cs_resolve_build_paths` 要求 `CS_BUILD_DIR` 上级含 `build:widget` 脚本。真实构建由 fake bun 完成（产物 `widget-dist/` 仅运行期生成，harness 保证清理，不入库）。 |
| `fake-remote/` | fake 远端树模板（首次安装基线）：批准根 marker `.imboy-cs-root`、foreign release 目录（验证永不删除）、API vhost（蓝 upstream 9800 恰一行）。 |
| `fake-remote-upgrade-overlay/` | 升级场景叠加层：旧 release `20260101000000-old`、旧 CS vhost。`current` symlink 由 harness 运行期创建（必须指向原始形式绝对路径 `/www/wwwroot/cs.test.local/releases/...`，供路径翻译往返一致）。 |

## 证书

测试证书（自签 `CN=cs.test.local`，两天有效期）由 harness 在运行期用本地 openssl
生成到临时目录后拷入各场景的 fake 远端树——**证书私钥绝不入库**。

## 约定

- fake 远端路径前缀（`/www/wwwroot/`、`/etc/nginx/`、`/etc/letsencrypt/`）在 fake ssh
  内被 sed 翻译到 `$FAKE_ROOT` 下的同名子树执行，stdout 再反向翻译——父进程
  （imboy-deploy.sh）全程只看到原始形式路径，文件系统副作用真实落在 fake 树内。
- `releases/20991231000000-foreign-unknown/FOREIGN_SENTINEL.txt` 代表非本工具创建的
  unknown/foreign release；每个场景结束后断言其仍存在（合同 S7-I4/I5）。
- 诱饵 secret：`CS_TEST_CANARY_9f2b`（作为 `DEPLOY_COOKIE` 写入 fixture env），
  verbose 输出中必须只以 `CS***` 脱敏形式出现（合同 S7-I7）。
