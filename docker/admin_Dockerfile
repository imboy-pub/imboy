# IMBoy 管理后台 Docker 镜像 / Admin Frontend Docker Image
# 多阶段构建：Bun 构建 + Nginx 服务
# Multi-stage build: Bun build + Nginx serve
#
# 构建 / Build（发布链由 imboy 仓 release.yml build-admin-candidate 推送
# ghcr.io/imboy-pub/imboy-admin:<version>；本地手动构建示例）:
#   docker build -t ghcr.io/imboy-pub/imboy-admin:<version> .
#
# 运行 / Run（IMBOY_API_HOST 注入后端地址，未设置时回落同源 /api/adm）:
#   docker run -p 80:80 -e IMBOY_API_HOST=https://api.example.com \
#     ghcr.io/imboy-pub/imboy-admin:<version>

# ─────────────────────────────────────────────────────────────
# Stage 1: Builder
# ─────────────────────────────────────────────────────────────
# 与 CI / Widget 使用相同 Bun / Match the CI and Widget Bun toolchain.
FROM oven/bun:1.3.14-slim AS builder

WORKDIR /build

# 安装依赖（利用层缓存）/ Install dependencies (layer cache)
COPY package.json bun.lock* bunfig.toml* ./
# 安装依赖前复制本地包与根 postinstall / Local package and root postinstall inputs
COPY vendor/ ./vendor/
COPY scripts/ts6-sidecar.sh ./scripts/ts6-sidecar.sh
RUN bun --version && bun install --frozen-lockfile

# 复制源码并构建 / Copy source and build
COPY . .
# Bun 预载 env 早于 Vite；仅构建时选择 production，安装仍保留 devDependencies。
# Bun loads env before Vite; select production only for the build, not install.
RUN NODE_ENV=production bun run build \
    && bun scripts/verify-admin-build.mts

# ─────────────────────────────────────────────────────────────
# Stage 2: Runtime (Nginx)
# ─────────────────────────────────────────────────────────────
FROM nginx:alpine AS runtime

# 非 root master/worker：PID 与临时文件落 /tmp；配置仍由 root 拥有。
# Non-root master/workers: writable PID/temp paths; keep config owned by root.
# 保留官方 Docker 24+ Compose 的内部 80 端口（默认允许非 root 绑定）。
# Preserve port 80 for the supported Docker 24+ Compose runtime defaults.
RUN rm /etc/nginx/conf.d/default.conf \
    && sed -i '/^user[[:space:]]/d; s|/var/run/nginx.pid|/tmp/nginx.pid|; s|/run/nginx.pid|/tmp/nginx.pid|' /etc/nginx/nginx.conf \
    && sed -i '/^http {/a\    client_body_temp_path /tmp/client_temp;\n    proxy_temp_path /tmp/proxy_temp;\n    fastcgi_temp_path /tmp/fastcgi_temp;\n    uwsgi_temp_path /tmp/uwsgi_temp;\n    scgi_temp_path /tmp/scgi_temp;' /etc/nginx/nginx.conf

# 写入 SPA 配置（history 路由回退）/ SPA config with history fallback
RUN printf 'server {\n\
    listen 80;\n\
    root /usr/share/nginx/html;\n\
    index index.html;\n\
    # 安全响应头 / Security headers\n\
    add_header X-Content-Type-Options nosniff always;\n\
    add_header X-Frame-Options DENY always;\n\
    add_header X-XSS-Protection "1; mode=block" always;\n\
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;\n\
    add_header Referrer-Policy strict-origin-when-cross-origin always;\n\
    add_header Permissions-Policy "camera=(), microphone=(), geolocation=(), payment=()" always;\n\
    # gzip\n\
    gzip on;\n\
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml image/svg+xml;\n\
    # 静态资源强缓存 / Static assets: long cache\n\
    location ~* \\.(js|css|woff2?|ttf|eot|svg|png|jpg|webp|ico)$ {\n\
        expires 1y;\n\
        add_header Cache-Control "public, immutable";\n\
        # location 内出现任意 add_header 即不再继承 server 级安全头，须逐字重复声明\n\
        add_header X-Content-Type-Options nosniff always;\n\
        add_header X-Frame-Options DENY always;\n\
        add_header X-XSS-Protection "1; mode=block" always;\n\
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;\n\
        add_header Referrer-Policy strict-origin-when-cross-origin always;\n\
        add_header Permissions-Policy "camera=(), microphone=(), geolocation=(), payment=()" always;\n\
    }\n\
    # SPA 路由回退 / SPA history fallback\n\
    location / {\n\
        try_files $uri $uri/ /index.html;\n\
    }\n\
    # 健康检查 / Health check\n\
    location /health {\n\
        return 200 "ok";\n\
        add_header Content-Type text/plain;\n\
        # 同上：location 内 add_header 切断继承，须逐字重复 server 级安全头\n\
        add_header X-Content-Type-Options nosniff always;\n\
        add_header X-Frame-Options DENY always;\n\
        add_header X-XSS-Protection "1; mode=block" always;\n\
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;\n\
        add_header Referrer-Policy strict-origin-when-cross-origin always;\n\
        add_header Permissions-Policy "camera=(), microphone=(), geolocation=(), payment=()" always;\n\
    }\n\
}\n' > /etc/nginx/conf.d/imboy-admin.conf

# 复制构建产物 / Copy build artifacts
COPY --from=builder /build/dist /usr/share/nginx/html
# sed -i 需要目录写权限来替换文件；仅静态产物交给运行用户。
# sed -i needs writable directories to replace files; grant only the static tree.
RUN chown -R 101:101 /usr/share/nginx/html

# 运行时配置注入：构建时用占位符 __IMBOY_API_HOST__ 代替硬编码域名，
# 容器启动时 sed 替换为 IMBOY_API_HOST 环境变量（compose 传入
# https://${API_DOMAIN}）。未设置时回落到同源相对路径 /api/adm。
# Vite 构建时 import.meta.env 已静态替换，运行时 env 无效——必须替换产物文件。
# 替换 JS + HTML（含 CSP connect-src 的域名白名单）。
COPY --chmod=0755 docker/admin/entrypoint.sh /docker-entrypoint-imboy.sh

USER 101

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=5s --retries=3 \
    CMD wget -qO- http://localhost/health || exit 1

ENTRYPOINT ["/docker-entrypoint-imboy.sh"]
