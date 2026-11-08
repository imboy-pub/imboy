# 发布产物位置

此材料包不含已构建的 IMBoy、PostgreSQL 或 Garage 二进制。

发布方运行 `tools/build-backend.sh`、`tools/prepare-payload.sh` 后，新的客户候选目录将在本目录包含 `backend/`、`postgresql/`、`garage/garage`。目标机不克隆源码、不编译 Erlang、不使用 Python 或 Docker。

缺少真实产物时安装器会以 `PAYLOAD` 错误退出，不创建数据库或伪装安装成功。详见文档中的“可交付内容与待补齐项”。
