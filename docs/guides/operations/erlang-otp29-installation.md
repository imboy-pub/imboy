# 如何在 Ubuntu/Debian 上安装 Erlang/OTP 29

> Last Updated: 2026-09-08
> Status: 长期运维文档
> Scope: Ubuntu 22.04/24.04 LTS、Debian 12/13 服务器与开发机上安装 Erlang/OTP 29，供 imboy 后端编译与运行
> Related docs: `docs/guides/operations/dependencies.md`, `docs/guides/operations/deployment/day1-quickstart.md`, `.github/workflows/backend-ci.yml`

## 1. 适用场景与版本说明

- imboy 后端要求 **Erlang/OTP 28+**；CI 现役 OTP 28，本文档面向准备升级/新装机使用 **OTP 29** 的环境。
- 生产构建使用 `RELX_INCLUDE_ERTS=true`（ERTS 打包进 release），宿主机 OTP 只需满足**构建期**编译；但生产宿主机保持与构建机同大版本最稳妥。
- 三种安装路径按推荐顺序：**源码编译**（生产推荐，版本精确可控）→ **系统 apt 包**（最快，但仓库版本通常滞后，先核对版本号）→ **kerl**（开发机多版本并存）。

## 2. 源码编译安装（生产推荐）

### 2.1 前置依赖

```bash
sudo apt update
sudo apt install -y build-essential git autoconf libncurses-dev \
  libssl-dev
```

说明：

| 包 | 用途 |
|----|------|
| `build-essential` | gcc/g++/make，编译主体 |
| `libncurses-dev` | `erl` 终端 shell 依赖 |
| `libssl-dev` | crypto/ssl 应用（IM 长连接 TLS、JWT 签名强依赖） |
| `autoconf` | 重新生成 configure 时需要 |

imboy 服务端不使用 GUI/Java/ODBC，编译时显式排除这些组件，可少装 `libwxgtk3.*-dev`、`unixodbc-dev`、default-jdk 等大依赖。

### 2.2 下载与编译

```bash
# 以 29.x 实际最新补丁版本为准，可到 https://www.erlang.org/downloads 核对
OTP_VSN=29.0.6

cd /usr/local/src
sudo curl -fLO "https://github.com/erlang/otp/releases/download/OTP-${OTP_VSN}/otp_src_${OTP_VSN}.tar.gz"
sudo tar xzf "otp_src_${OTP_VSN}.tar.gz"
cd "otp_src_${OTP_VSN}"

sudo ./configure --prefix=/usr/local/erlang-${OTP_VSN} \
  --without-wx --without-javac --without-odbc --without-jinterface --without-megaco
sudo make -j"$(nproc)"
sudo make install
```

要点：

- `--prefix` 带版本号目录：多版本并存、升级回退都只是换 symlink。
- 编译耗时与核数相关，2C4M 云主机约 10–20 分钟，属正常。
- 若需要重新 configure（换参数），先 `make distclean` 或直接解压干净源码。

### 2.3 接入 PATH

```bash
# 直用版本化路径，不建 /usr/local/erlang 统一入口：
# 该名字若已被旧的真目录占用（历史手工安装），ln -sfn 不会替换目录，
# 只会把链接塞进旧目录，PATH 命中的仍是老版本（2026-09-08 生产机实踩）。
echo 'export PATH=/usr/local/erlang-'"${OTP_VSN}"'/bin:$PATH' | sudo tee /etc/profile.d/erlang.sh
source /etc/profile.d/erlang.sh
```

### 2.4 验证（勿跳过）

```bash
erl -noshell -eval 'io:format("OTP ~s / ERTS ~s~n", [erlang:system_info(otp_release), erlang:system_info(version)]), halt().'
# 期望输出：OTP 29 / ERTS 29.x.x

# crypto/ssl 自检（生产 IM 必过：TLS 握手、token 签名都走这里）
erl -noshell -eval 'application:ensure_all_ok(crypto), application:ensure_all_ok(ssl), io:format("crypto/ssl OK: ~s~n", [crypto:info_lib()]), halt().'
```

若 `crypto` 起不来，九成是 `libssl-dev` 没装或 configure 时没找到 OpenSSL——重装依赖后重新编译。

## 3. apt 安装（快速可用）

```bash
sudo apt update && sudo apt install -y erlang-base erlang-dev erlang-crypto erlang-ssl erlang-asn1 erlang-public-key erlang-eunit erlang-parsetools erlang-syntax-tools erlang-dialyzer erlang-observer
erl -noshell -eval 'io:format("~s~n",[erlang:system_info(otp_release)]), halt().'
```

- 上面的应用子包是 imboy 编译（`make compile`/`make eunit`/`make dialyze`）的最小集合。
- **先核对第 2.4 节版本输出**：apt 仓库的 Erlang 通常落后官方 1–2 个大版本，如果打印出的不是 29，退回源码编译方案。
- Debian `trixie`/Ubuntu 新 LTS 的仓库版本以实际源为准，本文不预设具体小版本。

## 4. kerl 安装（开发机多版本并存）

```bash
curl -fLO https://raw.githubusercontent.com/kerl/kerl/master/kerl && chmod +x kerl && sudo mv kerl /usr/local/bin/
kerl update releases
kerl build 29.0 29.0
kerl install 29.0 ~/.kerl/installs/29.0
source ~/.kerl/installs/29.0/activate
```

只装到用户目录，不污染系统；`kerl deactivate` 退出。适合本机同时维护 OTP 28（对齐 CI）与 OTP 29 的场景。

## 5. 与 imboy 构建的衔接

```bash
cd /path/to/imboy
make compile     # 验证编译链
make eunit       # 验证测试链
IMBOYENV=local make run   # 本地起节点
```

常见问题：

| 现象 | 原因与处理 |
|------|-----------|
| `crypto`/`ssl` load 失败 | OpenSSL 开发头缺失或版本过老，见 2.4 排查 |
| `make rel` 产物在无 OTP 的机器起不来 | 构建时未开 `RELX_INCLUDE_ERTS=true`，或目标机 libc 与构建机不一致 |
| 编译期 `undef` 报错指向 stdlib 模块 | apt 装的 Erlang 缺应用子包，对照第 3 节清单补齐 |

## 6. 回滚方案

- 源码安装：`sudo ln -sfn /usr/local/erlang-<旧版本> /usr/local/erlang` 即切回；删除目录即卸载，无系统级残留。
- apt 安装：`sudo apt remove erlang-base`（会连带依赖子包）。
- 生产节点回滚不涉及宿主机 OTP：release 内已打包 ERTS，回滚 release 目录即可（见 `docs/guides/operations/deployment/deploy-script.md`）。
