# 发布方：制作原生二进制交付物

客户拿到的是组装后的完整候选目录，不需要源码、Git、编译器或独立安装 Erlang。下面操作只由发布方在独立构建机执行。工具包初始材料版 `PAYLOAD_READY=no`；收到目录实际状态以 `metadata/release.env` 为准。`no` 表示尚未含有二进制，不能改一个字段就冒充完整发行物。

本组工具采用 Bash、Git、GNU 工具、curl、jq 和项目本身的 Erlang 构建链，不调用 Python 或容器。固定源码的 `make rel` 消费已有产品特性生成结果，不重新执行 Python 生成器。如果后续版本引入必需的 Python 构建步骤，工具会失败，不会跳过步骤。

## 平台与固定输入

支持封装平台为 Ubuntu 24.04 x86_64 或 Debian 13 x86_64。每份候选只声明实际构建机的组合，目标机必须相同；这些组合仍需分别做真实安装验收。固定源码 SHA 为 `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`，后端版本 `1.0.0-alpha.83`，编译档 `phase1_delivery`，迁移头 `243`。

2026-11-08 集成到仓库后，这些冻结值仍不变。本次核对的 main 快照 `8957f0ed6cf7b3251259dbdf777fc1ae43837574` 已是 alpha.84／迁移 `275`，新 `sys.config` 摘要为 `14e448376e6512ca5f25bcdd4825eb54bd323d2d753d2033825c63b272851232`。本 PR 未适配它；保持配置摘要拒绝，不能仅更改 `--sha`、元数据或摘要常量宣称支持。完整边界见 [验证范围](../docs/10_VALIDATION.md)。

源仓即使有未提交改动，工具也仅通过 `git archive` 导出指定 commit，不修改原仓工作树、索引、分支或远端。构建输出必须使用新目录，失败后保留，不自动覆盖重装。所有输出路径要求为不含空白和Shell特殊字符的绝对路径。

## 1. 构建依赖

若已有可信的同平台 OTP29、完整 PG18 prefix 及 Garage2.4.1，可复用。PG 在编译时的 `--prefix` **必须是 `/opt/imboy/runtime/postgresql`**，与安装器的目标运行路径一致；组装器会硬核验。仅把发行版分散的 `/usr/bin`、`/usr/share` 文件复制到一起不符合此合同。

缺少依赖产物时，先查看构建依赖清单，再运行源码构建配方：

```bash
bash tools/build-native-dependencies.sh --help

bash tools/build-native-dependencies.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --work /srv/build/native-deps-20261105 \
  --otp-prefix /srv/build/otp-29.1.2 \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --jobs 4
```

调用者须有创建输出目录的权限。只在没有既有 IMBoy 安装的专用构建机使用上述 `/opt/imboy` 路径。脚本不会提权或安装 apt 包；`--help` 列出所需包，由发布方通过可信发行版仓库准备。请求保留本机代理和 TLS 验证设置。

配方依据当前源码的 `scripts/ci/native-eunit-pins.json`，固定 OTP29.1.2、PG18.6、PostGIS3.6.4、TimescaleDB2.23.1、pgvector0.8.7、pg_jieba commit/子模块、词典及 Garage2.4.1 哈希。下载校验失败即停止。只取后端必要依赖，不构建 Agent、Textguard 或 RTC 服务。原生 CI 调用的 Python 获取器没有进入本包。

PG必需扩展为：`pgcrypto`、`pg_jieba`、`timescaledb`、`vector`、`postgis`、`pg_trgm`、`btree_gin`、`btree_gist`、`citext`、`unaccent`、`intarray`、`pg_stat_statements`，并需默认分词词典和源码中的 `docker/pg_jieba_userdict.txt.big`。配方只构建文件，不启动数据库；扩展SQL和业务迁移由客户安装流程执行、验收。

## 2. 构建后端

```bash
export PATH=/srv/build/otp-29.1.2/bin:$PATH

bash tools/build-backend.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --out /srv/build/backend-20261105 \
  --jobs 4
```

输出为 `/srv/build/backend-20261105/backend`。工具在 `work/imboy` 内构建，目录名与 relx 的应用发现行为保持一致。它核对 `VERSION` 与 `relx.config`，不依赖有歧义的 `RELX_REL_VSN` 参数；要求 `dev_mode=false`、`include_erts=true`。

产物包括随包ERTS、业务及依赖BEAM、完整迁移、CLI与构建收据。工具拒绝运行数据、日志、私钥和外部符号链接；要求 sys.config/vm.args 仍为无秘密模板；检查构建机动态库并运行 `erlexec -version`。这些步骤不代表应用启动或业务验收通过。

固定主仓SHA不能冻结 `include/deps.mk` 中仍引用 `master` 的第三方依赖。工具记录实际依赖commit并用最终文件哈希冻结本次候选；重新构建可能产生不同字节，需视为新候选重新验收，不声称可逐字节复现。

## 3. 组装客户候选

```bash
bash tools/prepare-payload.sh \
  --repo /srv/source/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --release-dir /srv/build/backend-20261105/backend \
  --pg-prefix /opt/imboy/runtime/postgresql \
  --garage-bin /srv/build/native-deps-20261105/garage \
  --release-id alpha.83-cb95fa72-native \
  --out /srv/deliveries/imboy-native-candidate-20261105
```

组装器核对：后端收据与完整哈希、SHA/版本/平台、配置/CLI/产品manifest/迁移集合与源码字节、PG版本/编译前缀/扩展文件/词典、Garage固定哈希。原始 `sys.config` 哈希还必须等于当前工具包 `lib/render.sh` 声明的 `IMBOY_NATIVE_CONFIG_SHA256`；不一致须先适配并验证渲染器，不能仅改元数据放行。通过后生成新的候选目录、`metadata/release.env`、`metadata/host-runtime-packages.tsv`、`CHECKSUMS.sha256`，并设置 `PAYLOAD_READY=yes`。新候选中的无秘密文件统一为目录755、普通文件644、可执行文件755，使服务账户可以读取。

这个状态只表示产物已组装。`metadata/validation-status.tsv` 将干净机安装、业务UAT、备份恢复、升级回退列为 `NOT_RUN`，不得直接改成PASS。现场 `site.env` 不进入哈希；真实密钥在目标机初始化时生成，不提前打包。

## 4. 主机运行依赖与客户入口

ERTS、PG扩展和Garage仍可能依赖发行版动态库。封包时逐一检查实际ELF的 `ldd` 输出，排除已在payload内供应的库，其余用 `dpkg-query` 映射系统包，并用 `apt-cache depends --installed` 收集已安装的 Depends/PreDepends 闭包，记录准确包名、版本、架构。未知、无法映射或缺失的动态库会阻止封包；不会猜测库名对应的软件包。

发布机还须已安装以下运行工具：`bash curl jq openssl ca-certificates tar coreutils util-linux passwd iproute2 file nginx imagemagick fonts-dejavu-core`。这些工具及其依赖同样写入版本锁。锁文件为三列TSV，表头为 `package`、`version`、`architecture`。它随候选整体校验；不能手工删包或改版本绕过失败。

```bash
# 目标机只读检查：无网络、无APT索引刷新。
bash tools/prepare-host.sh --check

# 从现场已经配置、批准的APT源准备精确依赖。
sudo bash tools/prepare-host.sh --apply

# 客户首次安装也可合并使用主安装入口。
sudo bash install.sh --apply --prepare-host
```

`--check` 是默认模式；版本缺失/较旧时返回3，其他阻断返回非零。`--apply` 仅在有待安装包时执行 `apt-get update`，核对待安装精确版本可用，再模拟并检查安装计划，最后安装锁内包。已全部符合时不刷新索引。工具不修改源、不整机升级、不删除包；目标已有更高版本会在刷新索引前拒绝，绝不自动降级。依赖求解如要求锁外包也拒绝，须由发布方重做闭包和候选。它不启动IMBoy、PostgreSQL或Garage；正常OS软件包维护动作可能启动或配置Nginx。已有配置使用dpkg的保留策略。

即使现场预装了依赖，主安装器、升级和恢复仍会执行精确锁检查。新主机尚未安装到 `/opt/imboy/runtime/postgresql` 时，最终动态库检查临时使用候选内 `payload/postgresql/lib`，以核对随包PG库。

这不是全离线依赖包，需要可达的已批准APT源提供锁定版本。源中版本已撤下或现场离线时，由发布方另外供应同平台、同版本的批准镜像/离线依赖集合并重新验收；工具不会自动改源、换版本或下载未锁定库。不要把构建机glibc覆盖到目标系统目录。最终运行路径、扩展加载、首装和业务闭环必须在干净机验证。

## 验证范围

权威范围说明见 [docs/10_VALIDATION.md](../docs/10_VALIDATION.md)。在本交付目录运行三套合成测试：

```bash
bash tests/run.sh
bash tools/test-payload-tools.sh
bash tools/test-host-tools.sh
```

2026-11-05 初始材料记录合计 62 项通过（29 + 21 + 12）。本次及后续执行结果以对应 PR/CI 为准；专用工作流 `.github/workflows/native-shell-delivery.yml` 在 Ubuntu 24.04 上自动执行三套测试，不构建 OTP/PG，不部署服务。运行输出保留在 CI 或仓库外的执行记录中，不提交运行日志。

这些测试使用随机临时目录、合成配置、假的 Git 仓库及 PG/Garage/APT 执行桩。其中封包测试会使用本机只读 `dpkg-query` / `apt-cache` 查询，并为缺少的 Nginx 包提供测试桩。它们核对合同与拒绝行为，不证明真实 APT 安装、原生编译、SQL 迁移或客户部署成功。真实候选仍须完成构建、空机首装、业务、恢复和升级验收。

## English status summary

The Chinese guide is authoritative. The publisher tools remain frozen at `cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af`, `1.0.0-alpha.83`, migration head `243`. The main snapshot `8957f0ed6cf7b3251259dbdf777fc1ae43837574` (alpha.84, migration `275`) has a different configuration digest and is not adapted by this PR. Keep the renderer digest gate; changing metadata cannot establish compatibility.

This is Bash-only native packaging tooling. `PAYLOAD_READY=yes` is written only after assembling actual binaries and does not certify deployment or business readiness. The historical 62 checks are synthetic; fresh results are reported by the PR and the dedicated native-shell CI workflow. No OTP/PostgreSQL build, service deployment, real migration, business UAT, restore or upgrade acceptance is implied. Follow the full validation scope in [10_VALIDATION.md](../docs/10_VALIDATION.md).

## 源码证据

- [relx.config](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/relx.config)：ERTS、版本、非开发发布。
- [Makefile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Makefile)：配置复制、构建依赖、已有特性生成结果。
- [Dockerfile](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/Dockerfile)：relx目录命名、erlang_pay归档pin、CLI与动态库依据；本工具没有调用容器。
- [原生输入pins](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-pins.json)、[原生bootstrap](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-bootstrap.sh)、[原生迁移](https://github.com/imboy-pub/imboy/blob/cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af/scripts/ci/native-eunit-run.sh)：配方及12项扩展依据，不能当成本包的实测结果。
