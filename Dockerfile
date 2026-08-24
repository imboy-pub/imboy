# IMBoy 后端 Docker 镜像 / Backend Docker Image
# 多阶段：Erlang/OTP 28 编译 + relx 发布 + Debian slim 运行
# Multi-stage: OTP 28 build + relx release + debian-slim runtime
#
# 构建 / Build（在 imboy 仓根执行）:
#   docker build -t imboy/imboy-backend:$(cat VERSION) .
# 运行 / Run（生产经 docker-compose.prod.yml 注入 IMBOY_* 环境变量）:
#   docker run -p 9800:9800 -e IMBOYENV=pro -e IMBOY_PG_HOST=... imboy/imboy-backend:<ver>

# ─────────────────────────────────────────────────────────────
# Stage 1: Builder（完整 erlang:29，含 wx 供 relx 组装 observer）
# 不设 IMBOYENV：Makefile 用 relx.config + 完整 config/sys.config，
# 并在 make 时自动生成 config/sys.runtime.config（Makefile:22）
#
# 基础镜像必须 OTP 29（与生产打包环境 scripts/deploy.sh 一致，本机 29.0.2 实测
# make rel 通过）：relx 4.10 的 app 兜底发现走 code:lib_dir/1，OTP 28 对本项目
# 扁平 ebin/ 布局返回 bad_name（app_not_found, imboy, undefined），OTP 29 可
# 从 -pa 的 code path 正确解析。AGENTS.md 基线为 OTP 28+，29 满足。
# ─────────────────────────────────────────────────────────────
FROM erlang:29 AS builder

# WORKDIR 必须叫 /imboy（== app 名）：relx 4.10 发现主 app 走 code:lib_dir/1，
# 其实现按 code path 目录（-pa ebin/）的父目录名匹配 app 名。本机仓目录名
# 恰为 imboy 故 make rel 一直可跑；容器内目录名若为 /build 则 app_not_found,
# imboy, undefined（OTP 28/29 同此行为，实测 5 轮 docker build 定位）。
WORKDIR /imboy

# 全量复制源码（.dockerignore 已排除 _build/deps/_rel/.git/ebin 等，强制干净构建）
COPY . .

# erlang_pay 目前只在 Gitee 公开发布。发布构建不能依赖该站点的 Git
# smart-HTTP（GitHub runner 会被重定向至登录页），因此拉取固定提交的公开
# 归档并校验 SHA-256。预置到 deps/ 后 erlang.mk 不会再次克隆该依赖。
ARG ERLANG_PAY_COMMIT=e06909e67fb996ec0b954b388b19e95177cb33c6
ARG ERLANG_PAY_ARCHIVE_SHA256=ae5a379bddabd0c85f2bd641fb0645a217d938c8142d8c3907923ba9cd8b79f4
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends ca-certificates curl; \
    rm -rf /var/lib/apt/lists/*; \
    mkdir -p deps; \
    curl --fail --location --retry 3 --retry-delay 2 \
      "https://gitee.com/imboy-pub/erlang_pay/repository/archive/${ERLANG_PAY_COMMIT}.tar.gz" \
      -o /tmp/erlang_pay.tar.gz; \
    echo "${ERLANG_PAY_ARCHIVE_SHA256}  /tmp/erlang_pay.tar.gz" | sha256sum -c -; \
    tar -xzf /tmp/erlang_pay.tar.gz -C /tmp; \
    mv "/tmp/erlang_pay-${ERLANG_PAY_COMMIT}" deps/erlang_pay; \
    { printf '%s\n' '.PHONY: all' 'all:'; \
      printf '\t%s\n' '../../.erlang.mk/rebar3/rebar3 compile' 'mkdir -p ebin' \
        'cp _build/default/lib/erlang_pay/ebin/* ebin/'; \
    } > deps/erlang_pay/Makefile; \
    test -f deps/erlang_pay/rebar.config

# 补齐默认 sys 配置源：Makefile 默认分支（不设 IMBOYENV）要求 config/sys.config
# 存在（cp 为 sys.runtime.config 供 relx 组装）。sys.config 本体在 alpha.42
# （f5420d70）被移除出仓，只剩 example 模板——example 即"默认值 + IMBOY_* env
# 覆盖"设计（见其头部注释），容器构建用它作为默认源语义正确。
RUN cp config/sys.config.example config/sys.config

# 出自包含 release（与 scripts/deploy.sh 生产打包路径一致的 RELX_* 变量）：
#   RELX_REL_VSN=$(cat VERSION)：版本单一真源=VERSION 文件，覆盖 relx.config 的
#     硬编码版本——两者曾漂移（alpha.45 vs rc.1）导致 relx 按 release 声明的
#     vsn 找 ebin/imboy.app 找不到（app_not_found, imboy, undefined）
#   relx.config 中的 dev_mode=false / include_erts=true：不产生指向源码树的
#     符号链接，并 bundle ERTS，runtime 无需装 Erlang。
# make rel 会自动拉依赖 + 编译 + relx 组装；输出 _rel/imboy
RUN make rel RELX_REL_VSN="$(cat VERSION)"

# 暂存 ERTS 运行所需系统库到固定目录。不能手工维护有限的库清单：OTP 29
# 的 beam/erlexec 还依赖 libstdc++、libgcc_s 等，缺任一个都会令启动脚本以
# 127 退出。由 ldd 从实际打包的 ERTS 可执行文件提取依赖；先解析 /lib 等
# usr-merge 兼容符号链接再保留真实绝对路径，避免 COPY 覆盖运行镜像的 /lib 链接，
# 并天然适配 amd64/arm64。
RUN set -eu; \
    mkdir -p /runtime-libs/etc/ssl; \
    for executable in /imboy/_rel/imboy/erts-*/bin/*; do \
        ldd "$executable" 2>/dev/null || true; \
    done \
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
# 提供它，缺失会静默以 127 退出。因此在镜像构建期装最小运行工具 mawk。私有化
# 用户只拉取已构建镜像，不依赖其现场网络。其余 ERTS 库与 CA 从 builder 拷贝：
# ① builder/runtime 同为 trixie/glibc 基线；② glibc 自带 C.UTF-8，无需 locales。
# ─────────────────────────────────────────────────────────────
FROM debian:trixie-slim AS runtime

RUN apt-get update \
    && apt-get install -y --no-install-recommends mawk \
    && rm -rf /var/lib/apt/lists/*

# ERTS 运行所需系统库 + CA 证书（builder 已按本机架构暂存到 /runtime-libs）
# crypto NIF 需 libssl/libcrypto；erlang shell 需 ncurses/tinfo；出站 TLS 需 CA 证书
COPY --from=builder /runtime-libs/ /

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=Asia/Shanghai \
    IMBOYENV=pro

# 复制自包含 release
COPY --from=builder /imboy/_rel/imboy /opt/imboy

# 在最终运行层执行 ERTS 二进制，防止构建绿但运行时因缺动态加载器/共享库以
# 127 静默退出。该自检不启动应用、不依赖 PostgreSQL；它失败时构建日志会保留
# 底层加载器报错，避免把问题拖到 Golden Install 的 120 秒健康超时才发现。
RUN /opt/imboy/erts-*/bin/erlexec -version

# imboy_ctl 管理 CLI（escript）。install.sh 的 --admin-phone/--admin-password
# 在 backend 容器内调用它创建超管（部署机只有 Docker 没有 Erlang，escript 由
# 上方自包含 ERTS 提供）：
#   /opt/imboy/erts-*/bin/escript /opt/imboy/bin/imboy_ctl adm create ...
# scripts/ 未被 .dockerignore 排除，构建上下文中始终存在。
COPY scripts/imboy_ctl /opt/imboy/bin/imboy_ctl

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
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD /opt/imboy/bin/imboy ping >/dev/null 2>&1 \
     && /opt/imboy/bin/imboy eval 'case healthz_handler:probe_db() of true -> ok; _ -> halt(1) end.' \
     || exit 1

# foreground 让进程前台运行交给 Docker 托管（勿用 start，后台 daemon 会让容器立即退出）
CMD ["/opt/imboy/bin/imboy", "foreground"]
