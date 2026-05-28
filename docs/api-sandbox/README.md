# IMBoy API 文档沙盒 / API Sandbox

本目录提供两种本地查看 IMBoy OpenAPI 文档的方式。

## 方式一：静态 HTML（无需 Docker）

### 使用 Python 内置服务器

```bash
# 在 imboy/ 目录下运行（确保相对路径 ../openapi.yaml 可访问）
cd /path/to/imboy.pub/imboy
python3 -m http.server 8888 --directory docs/api-sandbox
```

然后访问：
- **Redoc 只读文档**：http://localhost:8888/index.html
- **Swagger UI 交互文档**：http://localhost:8888/swagger-ui.html

### 使用 npx serve

```bash
cd /path/to/imboy.pub/imboy
npx serve docs/api-sandbox -p 8888
```

> **注意**：需从 `imboy/` 根目录启动服务，浏览器才能正确加载 `../openapi.yaml`。

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
# Run from imboy/ root so ../openapi.yaml resolves correctly
cd /path/to/imboy.pub/imboy
python3 -m http.server 8888 --directory docs/api-sandbox
```

- **Redoc (read-only)**: http://localhost:8888/index.html
- **Swagger UI (interactive)**: http://localhost:8888/swagger-ui.html

## Option 2: Docker Compose

```bash
make docs-serve   # start
make docs-stop    # stop
```

Open **http://localhost:8080**.
