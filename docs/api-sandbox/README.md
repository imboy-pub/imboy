# IMBoy API 文档沙盒 / API Sandbox

本目录提供两种本地查看 IMBoy OpenAPI 文档的方式。

## 方式一：静态 HTML（无需 Docker）

### 使用 Python 内置服务器

```bash
# 在 imboy/ 目录下运行（spec 真源在 api/openapi.yaml，静态页以 ../../api/openapi.yaml 引用，
# 故不能以 docs/api-sandbox 为服务根目录）；--bind 127.0.0.1 必须保留——
# 服务根是整个仓库，绑默认 0.0.0.0 会把 .env* 等本地机密暴露到局域网
cd /path/to/imboy.pub/imboy
python3 -m http.server 8888 --bind 127.0.0.1
```

然后访问：
- **Redoc 只读文档**：http://localhost:8888/docs/api-sandbox/index.html
- **Swagger UI 交互文档**：http://localhost:8888/docs/api-sandbox/swagger-ui.html

### 使用 npx serve

```bash
cd /path/to/imboy.pub/imboy
# -l 显式绑 127.0.0.1：serve 默认监听 0.0.0.0，且同样不做任何过滤（实测 14.2.6
# 对 .gitignore 条目与 dotfile 均照常提供，.env* 可被直接下载）——-l 是唯一屏障，必须保留
npx serve . -l tcp://127.0.0.1:8888
```

> **注意**：需从 `imboy/` 根目录启动服务，浏览器才能正确加载 `../../api/openapi.yaml`（该文件由 `api/gen_aggregate.py` 从 `openapi-api.yaml` / `openapi-adm.yaml` / `openapi-internal.yaml` 三个编辑真源聚合生成）。

---

## 方式二：Docker Compose

```bash
# 启动（推荐，无需 Python/Node）
make docs-serve

# 停止
make docs-stop
```

访问 **http://localhost:8080** 查看 Swagger UI 交互文档。

---

## 后端地址

本地开发后端运行在 `http://127.0.0.1:4000`，在 Swagger UI 的 Servers 下拉框中选择对应环境后可直接发起请求。

---

## 文件说明

| 文件 | 说明 |
|------|------|
| `index.html` | Redoc 静态只读文档（CDN） |
| `swagger-ui.html` | Swagger UI 交互文档，支持 Try it out（CDN） |
| `docker-compose.yml` | Docker 一键启动文档服务 |

---

# IMBoy API Sandbox

Two ways to browse the IMBoy OpenAPI spec locally.

## Option 1: Static HTML (no Docker)

```bash
# Run from imboy/ root so ../../api/openapi.yaml resolves correctly
# (do NOT use --directory docs/api-sandbox; the spec lives outside docs/)
# Keep --bind 127.0.0.1: the server root is the whole repo, so the default
# 0.0.0.0 would expose .env* and other local secrets to the LAN
cd /path/to/imboy.pub/imboy
python3 -m http.server 8888 --bind 127.0.0.1
```

- **Redoc (read-only)**: http://localhost:8888/docs/api-sandbox/index.html
- **Swagger UI (interactive)**: http://localhost:8888/docs/api-sandbox/swagger-ui.html

## Option 2: Docker Compose

```bash
make docs-serve   # start
make docs-stop    # stop
```

Open **http://localhost:8080**.
