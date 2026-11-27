# IMBoy 后端 Docker 镜像 / Backend Docker Image
# 多阶段：Erlang/OTP 29 编译 + relx 发布 + Debian slim 运行
# Multi-stage: OTP 29 build + relx release + debian-slim runtime
#
# 构建 / Build（在 imboy 仓根执行）:
#   docker build -t imboy/imboy-backend:$(cat VERSION) .
# 运行 / Run（生产经 docker-compose.prod.yml 注入 IMBOY_* 环境变量）:
#   docker run -p 9800:9800 -e IMBOYENV=pro -e IMBOY_PG_HOST=... imboy/imboy-backend:<ver>

# ─────────────────────────────────────────────────────────────
# Stage 1: Builder（固定 OTP 29.1.3 多架构官方镜像，与 runtime 同为 trixie）
# Pin the official multi-platform image; dependencies use include/deps.mk.
# 不设 IMBOYENV：Makefile 从模板生成 config/sys.runtime.config。
# ─────────────────────────────────────────────────────────────
FROM erlang:29.1.3@sha256:a78e8a76fcd0618410c9b7590616c386d404278aa61d50b255f463912f2b80a5 AS builder

# WORKDIR 必须叫 /imboy（== app 名）：relx 4.10 发现主 app 走 code:lib_dir/1，
# 其实现按 code path 目录（-pa ebin/）的父目录名匹配 app 名。本机仓目录名
# 恰为 imboy 故 make rel 一直可跑；容器内目录名若为 /build 则 app_not_found,
# imboy, undefined（OTP 28/29 同此行为，实测 5 轮 docker build 定位）。
WORKDIR /imboy

# 全量复制源码（.dockerignore 已排除 _build/deps/_rel/.git/ebin 等，强制干净构建）
COPY . .

# erlang_pay 已在 include/deps.mk 固定为 GitHub v0.3.2；复用 erlang.mk
# 的 rebar 自动适配，与本地/CI 构建一致，不再预置另一版本的 Gitee 归档。
# Use the same pinned dependency and rebar compatibility path as local/CI builds.

# 补齐默认 sys 配置源：Makefile 默认分支（不设 IMBOYENV）要求 config/sys.config
# 存在（cp 为 sys.runtime.config 供 relx 组装）。sys.config 本体在 alpha.42
# （f5420d70）被移除出仓，只剩 example 模板——example 即"默认值 + IMBOY_* env
# 覆盖"设计（见其头部注释），容器构建用它作为默认源语义正确。
RUN cp config/sys.config.example config/sys.config

# relx.config 的版本与 VERSION 保持一致；dev_mode=false/include_erts=true
# 生成自包含 release。make rel 拉依赖、编译并输出 _rel/imboy。
# Build the self-contained release, including ERTS, from the shared Makefile.
RUN make rel

# 暂存 ERTS 运行所需系统库到固定目录。不能手工维护有限的库清单：OTP 29
# 的 beam/erlexec 还依赖 libstdc++、libgcc_s 等，缺任一个都会令启动脚本以
# 127 退出。由 ldd 从实际打包的 ERTS 和 release NIF 可执行文件提取依赖；先
# 解析 /lib 等 usr-merge 兼容符号链接再保留真实绝对路径，避免 COPY 覆盖运行
# 镜像的 /lib 链接，并天然适配 amd64/arm64。
RUN set -eu; \
    mkdir -p /runtime-libs/etc/ssl; \
    { find /imboy/_rel/imboy/erts-* /imboy/_rel/imboy/lib \
        -type f -perm /111 -exec ldd {} \; 2>/dev/null || true; } \
      | awk '/=> \// { print $3 } /^\// { print $1 }' \
      | sort -u \
      | while IFS= read -r lib; do \
          resolved_lib="$(readlink -f "$lib")"; \
          cp -L --parents "$resolved_lib" /runtime-libs; \
        done; \
    cp -r /etc/ssl/certs /runtime-libs/etc/ssl/

# ─────────────────────────────────────────────────────────────
# Stage 2: Runtime（debian-slim，glibc 匹配 builder 的 ERTS）
# 勿用 alpine：ERTS 针对 glibc 编译，musl 会崩
#
# release 启动脚本在输出第一条日志前就用 awk 解析 vm.args；debian:slim 不保证
# 提供它，缺失会静默以 127 退出。acl 用于安装权限准备；OTP 29 通过 dlopen
# 加载 libsctp1，ldd 无法发现它，缺失时 stdout 警告会破坏 relx eval 健康检查。
# Install libsctp1 explicitly: this dlopen dependency is absent from ldd output.
# 私有化部署用户只拉取已构建镜像，不依赖其现场网络。其余 ERTS 库与 CA 从 builder 拷贝：
# ① builder/runtime 同为 trixie/glibc 基线；② glibc 自带 C.UTF-8，无需 locales。
# ─────────────────────────────────────────────────────────────
FROM debian:trixie-slim AS runtime

RUN apt-get update \
    && apt-get install -y --no-install-recommends mawk acl libsctp1 \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 imboy \
    && useradd --uid 10001 --gid 10001 --create-home --home-dir /home/imboy \
       --shell /usr/sbin/nologin imboy

# ERTS 运行所需系统库 + CA 证书（builder 已按本机架构暂存到 /runtime-libs）
# crypto NIF 需 libssl/libcrypto；erlang shell 需 ncurses/tinfo；出站 TLS 需 CA 证书
COPY --from=builder /runtime-libs/ /

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=Asia/Shanghai \
    IMBOYENV=pro \
    HOME=/home/imboy \
    RELX_OUT_FILE_PATH=/opt/imboy/var \
    ERL_CRASH_DUMP=/opt/imboy/log/erl_crash.dump

# 复制自包含 release
COPY --from=builder /imboy/_rel/imboy /opt/imboy

# imboy_ctl 管理 CLI（escript）。install.sh 的 --admin-phone/--admin-password
# 在 backend 容器内调用它创建超管（部署机只有 Docker 没有 Erlang，escript 由
# 上方自包含 ERTS 提供）：
#   /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl adm create ...
# scripts/ 未被 .dockerignore 排除，构建上下文中始终存在。
COPY scripts/imboy_ctl /opt/imboy/bin/imboy_ctl
COPY scripts/start_download_edge.sh /opt/imboy/bin/start_download_edge.sh
COPY scripts/start_enterprise_direct_proof.sh /opt/imboy/bin/start_enterprise_direct_proof.sh

# 开发密钥放入独立运行目录，挂载 named volume 后重建容器仍保留登录密钥。
# Persist local RSA keys across container recreation through the runtime volume.
# 生产显式 IMBOY_LOGIN_RSA_*_FILE 路径不变；BEAM/ERTS 保持 root 所有。
# Bind mounts are prepared by deploy/prepare-backend-runtime.sh before startup.
RUN set -eu; \
    mkdir -p /opt/imboy/log /opt/imboy/var /opt/imboy/priv_runtime/dev_keys; \
    chown 10001:10001 /opt/imboy/log /opt/imboy/var /opt/imboy/priv_runtime \
      /opt/imboy/priv_runtime/dev_keys; \
    chmod 0700 /opt/imboy/priv_runtime/dev_keys; \
    for app_priv in /opt/imboy/lib/imboy-*/priv; do \
      test -d "$app_priv"; \
      rm -rf "$app_priv/dev_keys"; \
      ln -s /opt/imboy/priv_runtime/dev_keys "$app_priv/dev_keys"; \
      chown -R 10001:10001 "$app_priv"; \
    done
USER 10001:10001

# 使用真实运行身份验证打包的 ERTS；不启动应用或连接 PostgreSQL。
# Validate bundled ERTS as the actual runtime user, without starting the app/DB.
# 缺失加载器/系统库会在构建期失败，不拖到 Golden 健康检查才暴露。
RUN /opt/imboy/erts-*/bin/erlexec -version

WORKDIR /opt/imboy
EXPOSE 9800

# 启动初期 EPMD/迁移需要时间，start-period 给足 60s
#
# C-50：`ping` 只证明 **Erlang 节点活着**，不证明它能服务 —— PG 连不上时
# 节点照样 ping 得通，于是 nginx 会被放行去把流量打到一个连不上库的后端。
# 改为复用 /healthz 的同一个探测（healthz_handler:probe_db/0）。
# 用 `bin/imboy eval` 而不是 curl：运行镜像是 debian-slim，**没有装 curl/wget**。
# 两段与关系：先 ping（节点没起来时 eval 会超时/报错，先 ping 出错更清晰），
# 再探依赖。
#
# 审计 A3/C-4（2026-10-10）：第三段补 elib_tsid:readiness/0 —— TSID 时钟
# 闸没有内置周期探针，偏差暴露窗口 = 本健康检查轮询间隔（30s）。sup 在启动
# 早期同步 init TSID，ping 通过后 readiness 必为 ready | clock_unsafe。
# 注意 eval 语义：nodetool 经 rpc:call 在**运行节点上**求值；probe_db 段的
# halt(1) 是 C-50 既有自愈语义（远程 halt 杀掉节点交 restart 策略拉起）。
# 修复（2026-10-11，容器级实证）：relx_nodetool 的 eval 子命令对表达式异常
# **只打印 {error,...} 且恒退 0**（badrpc 非零退出是 rpc 子命令的行为，eval
# 没有）——erlang:error 分支门禁形同虚设。改为输出比较：布尔表达式打印
# true/false，任何异常/非 true 输出（节点挂掉、rpc 失败、readiness≠ready）
# 都判失败；本段绝不 halt（那会杀掉后端节点本身）。
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD /opt/imboy/bin/imboy ping >/dev/null 2>&1 \
     && /opt/imboy/bin/imboy eval 'case healthz_handler:probe_db() of true -> ok; _ -> halt(1) end.' \
     && [ "$(/opt/imboy/bin/imboy eval 'elib_tsid:readiness() =:= ready.' 2>&1)" = "true" ] \
     || exit 1

# foreground 让进程前台运行交给 Docker 托管（勿用 start，后台 daemon 会让容器立即退出）
CMD ["/opt/imboy/bin/imboy", "foreground"]
