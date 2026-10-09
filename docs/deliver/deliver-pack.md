# 一期交付包说明（AC27）

> 状态：current | 生成器：`scripts/deliver/make_delivery_pack.sh` | 合同：主计划 AC27

## 一句话

后端源包 = **固定 HEAD 的已提交文件按正向类别白名单选择**，再排除 AC27 禁项（真实配置/凭据/日志/crash/缓存/临时 evidence）。正向规则位于 `scripts/deliver/backend_allowlist.regex`，对应[产品交付清单](../product/delivery-package-manifest.md)后端部分；新增目录默认不进入包。
输出 manifest + tar；生成物只落仓外目录（默认 `/tmp`），仓库内不产生任何打包残留。

## 生成命令

```bash
# manifest + 自检 + tar（输出到 /tmp/imboy-delivery-<sha8>-<时间戳>/）
scripts/deliver/make_delivery_pack.sh

# 只要 manifest + 自检（不产 tar）
scripts/deliver/make_delivery_pack.sh --no-tar

# 默认已要求一期；显式写出同一要求也有效
scripts/deliver/make_delivery_pack.sh --require-profile phase1_delivery

# 仅开发源包：明确指定实际已提交 profile，不获得一期交付资格
scripts/deliver/make_delivery_pack.sh --require-profile full-selected --no-tar

# 指定输出目录（必须在仓库外，脚本会拒绝仓内路径）
scripts/deliver/make_delivery_pack.sh --out-dir /path/outside/repo

# 附加环境特定凭据指纹扫描（指纹文件一行一个字面量，仓外维护绝不提交）
scripts/deliver/make_delivery_pack.sh --secret-fingerprints ~/.imboy/secret-fp.txt
```

## 排除规则（禁项 ↔ 规则对照）

| 类别 | 规则 | 说明 |
|---|---|---|
| 真实配置 | `config/sys.{local,dev,prod,eunit}*`（任意层级） | 环境配置永不入包 |
| 真实 env | `.env`、`.env.<variant>`；`*.example` 豁免 | example 是配置入口模板 |
| 日志/crash | `*.log`、`erl_crash.dump`/`crash.dump` | 归档可追溯性由 git 历史与结论文档承担 |
| 执行材料 | 任意层级 `evidence/`、`test-results/`、`playwright-report/` | 不随交付包分发 |
| 非交付文件 | `docs/qa/`、`docs/design/`、`docs/plans/`、`docs/archive/design/`、`docs-site/`、工具元数据及 Agent Hub 运维篇 | 按[交付清单](../product/delivery-package-manifest.md)后端排除合同执行 |
| 生产环境快照 | `deploy/nginx/prod-vhosts/` | 普通 nginx 模板仍保留 |
| 缓存 | `node_modules/`、`_build/`、`.otp/` | — |
| 凭据命名 | basename 含 `private[_-]?key` 一律拦；`password` 词元且扩展非源码/契约/文档 | `elib_password.erl` 等业务语义文件豁免 |
| 内容级（硬失败） | 完整私钥 PEM 块，解码后至少 32 字节；可选指纹扫描 | 支持分行 Base64；不是私钥头字符串扫描 |

排除与自检共用同一规则函数（`credential_named_paths`），两者逻辑不漂移；
自检对最终 manifest 断言，任一命中即 exit 1（双保险）。

## 校验方法

1. `manifest.meta.md`：记录 git SHA、tracked/excluded/manifest 计数、规则摘要——核对计数闭合（tracked = excluded + manifest）。
   同时记录实际 `profile`、`manifest_file_sha256` 和正向规则哈希。缺少 manifest 或默认一期要求不匹配均失败；开发源包必须显式指定其它实际 profile。
2. `tar.sha256`：tar 的 SHA256；接收方 `shasum -a 256 -c tar.sha256` 验完整性。
3. tar 条数 == manifest 条数（脚本内建断言）。
4. `.pem` 文件触发 WARN 清单：需人工确认全部为公钥/开发 fixture（当前唯一命中 `priv/vendor_pubkey_dev.pem` 已确认为 `BEGIN PUBLIC KEY`）。
5. 有环境凭据指纹清单时，用 `--secret-fingerprints` 复跑一轮，命中即 FAIL。

## 已知边界

- 白名单由固定提交的文件树产生，检查和打包均读取同一提交的归档快照。暂存、未暂存和未跟踪内容均不进入包，工作区不会被脚本覆盖。
- 符号链接必须指向同一快照中被纳入交付的普通文件；越界、悬空链接和未声明的子模块会被拒绝。输出目录已有快照时拒绝复用，需选择新的仓外目录。
- 若未来 tracked 集混入禁项，第 2 步排除 + 第 3 步自检会拦截，但**新增禁项类别**需同步更新
  排除段与自检段两处（规则函数注释有对应关系表）。
- 本脚本只打包 imboy 主仓；App/Admin 仓交付物由各仓自己的发布通道产出（见各自 README），
  三仓指纹对应关系由 Q12 终验冻结（AC30）。
- 后端源包成功不证明三仓装配完成；Admin 第三方许可证清单及三仓必备文件检查仍须在统一交付流程中验证。
- 脚本不会自动切换产品 profile：归档中的 manifest 与固定 HEAD 相同。开发完整版源码包不能据此标记为一期交付产物；一期需另外冻结 profile、生成物和构建指纹并验收。
