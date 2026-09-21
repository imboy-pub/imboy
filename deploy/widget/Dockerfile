# IMBoy CS Widget 独立静态镜像（DEP-01 A01/A04）
#
# 构建上下文 = 本目录（deploy/widget/）：
#   ./nginx.conf  —— 站点配置（缓存策略 + SSE 直通，见 A04/A05）
#   ./dist/       —— admin 仓 `bun run build:widget` 的 dist-widget/ 产物拷贝
#                    （.gitignore 已忽略，dist 本体永不入仓）
#
# 构建示例（产物就位后）：
#   docker build -t imboy-widget:v1 deploy/widget/
#
# 运行约定：
#   * 非 root：容器内以 uid 101（nginx）运行，全部可写路径收口到 /tmp；
#   * 入口直接 exec nginx（不走官方 docker-entrypoint.sh——本镜像 nginx.conf
#     为构建期固化，无 envsubst 模板，绕开 entrypoint 对 /etc/nginx 的写需求）；
#   * 后端上游为构建期 ARG（镜像不可变原则：升级与回滚都以 tag 为单位，A06）。
ARG NGINX_IMAGE=nginx:1.27-alpine
FROM ${NGINX_IMAGE}

# SSE 直通上游（构建期固化）。演练用宿主后端时传：
#   --build-arg WIDGET_BACKEND_UPSTREAM=http://host.docker.internal:9802
# 生产与社区栈反代后端容器：默认值由 compose 层或派生镜像决定（imboy_backend:9800）
ARG WIDGET_BACKEND_UPSTREAM=http://imboy_backend:9800

COPY nginx.conf /etc/nginx/nginx.conf
RUN set -eux; \
    sed -i "s|__WIDGET_BACKEND_UPSTREAM__|${WIDGET_BACKEND_UPSTREAM}|g" /etc/nginx/nginx.conf; \
    mkdir -p /tmp/client_body /tmp/proxy /tmp/fastcgi /tmp/uwsgi /tmp/scgi; \
    chown -R 101:101 /tmp /var/cache/nginx

# 站点内容只读即可（nginx 无需写 web root）
COPY --chown=101:101 dist/ /usr/share/nginx/html/

USER 101
EXPOSE 8080
STOPSIGNAL SIGQUIT
HEALTHCHECK --interval=5s --timeout=3s --start-period=3s --retries=12 \
  CMD wget -q -O /dev/null http://127.0.0.1:8080/healthz || exit 1
ENTRYPOINT ["nginx", "-g", "daemon off;"]
