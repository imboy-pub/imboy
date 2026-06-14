# IMBoy 后端 Docker 镜像 / Backend Docker Image
# 多阶段：Erlang/OTP 28 编译 + relx 发布 + Debian slim 运行
# Multi-stage: OTP 28 build + relx release + debian-slim runtime
#
# 构建 / Build（在 imboy 仓根执行）:
#   docker build -t imboy/imboy-backend:$(cat VERSION) .
# 运行 / Run（生产经 docker-compose.prod.yml 注入 IMBOY_* 环境变量）:
#   docker run -p 9800:9800 -e IMBOYENV=pro -e IMBOY_PG_HOST=... imboy/imboy-backend:<ver>

# ─────────────────────────────────────────────────────────────
# Stage 1: Builder（完整 erlang:28，含 wx 供 relx 组装 observer）
# 不设 IMBOYENV：Makefile 用 relx.config + 完整 config/sys.config，
# 并在 make 时自动生成 config/sys.runtime.config（Makefile:22）
# ─────────────────────────────────────────────────────────────
FROM erlang:28 AS builder

WORKDIR /build

# 全量复制源码（.dockerignore 已排除 _build/deps/_rel/.git 等，强制干净构建）
COPY . .

# 关闭 dev_mode 出自包含 release：
#   - dev_mode=true 会用符号链接指向源码树，拷到 runtime 阶段即断链
#   - 关闭后 relx 默认 include_erts=true，bundle ERTS，runtime 无需装 Erlang
# make rel 会自动拉依赖 + 编译 + relx 组装；输出 _rel/imboy
RUN sed -i 's/{dev_mode, true}/{dev_mode, false}/' relx.config \
    && make rel

# 暂存 ERTS 运行所需系统库到固定目录（arch 自适应：amd64=x86_64-linux-gnu / arm64=aarch64-linux-gnu）
# runtime 阶段单次 COPY 即可，避免写死架构路径
RUN set -e; \
    TRIPLET=$(gcc -dumpmachine 2>/dev/null || echo x86_64-linux-gnu); \
    mkdir -p /runtime-libs/usr/lib/$TRIPLET /runtime-libs/etc/ssl; \
    for lib in libssl.so.3 libcrypto.so.3 libtinfo.so.6 libncursesw.so.6; do \
        cp -L /usr/lib/$TRIPLET/$lib /runtime-libs/usr/lib/$TRIPLET/ 2>/dev/null \
        || cp -L /lib/$TRIPLET/$lib /runtime-libs/usr/lib/$TRIPLET/ 2>/dev/null || true; \
    done; \
    cp -r /etc/ssl/certs /runtime-libs/etc/ssl/

# ─────────────────────────────────────────────────────────────
# Stage 2: Runtime（debian-slim，glibc 匹配 builder 的 ERTS）
# 勿用 alpine：ERTS 针对 glibc 编译，musl 会崩
#
# 不依赖 apt：从 builder 拷贝 ERTS 运行所需系统库 + CA 证书。
# 原因：① 私有化客户网络常受限/无外网，构建期 apt 不可靠；
#       ② erlang:28 与 debian:bookworm-slim 同为 bookworm/glibc 基线，ABI 兼容；
#       ③ glibc 自带 C.UTF-8，无需 locales/locale-gen。
# ─────────────────────────────────────────────────────────────
FROM debian:bookworm-slim AS runtime

# ERTS 运行所需系统库 + CA 证书（builder 已按本机架构暂存到 /runtime-libs）
# crypto NIF 需 libssl/libcrypto；erlang shell 需 ncurses/tinfo；出站 TLS 需 CA 证书
COPY --from=builder /runtime-libs/ /

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=Asia/Shanghai \
    IMBOYENV=pro

# 复制自包含 release
COPY --from=builder /build/_rel/imboy /opt/imboy

WORKDIR /opt/imboy
EXPOSE 9800

# 启动初期 EPMD/迁移需要时间，start-period 给足 60s
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD /opt/imboy/bin/imboy ping || exit 1

# foreground 让进程前台运行交给 Docker 托管（勿用 start，后台 daemon 会让容器立即退出）
CMD ["/opt/imboy/bin/imboy", "foreground"]
