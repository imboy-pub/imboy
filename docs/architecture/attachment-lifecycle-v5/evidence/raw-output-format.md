原始输出按字节保存在同名 .raw.gz 文件（gzip 解压即原始字节）；文本展示版只移除行尾空白和多余末尾空行，未改变测试结果、退出码或 SQL 内容。SHA256 清单同时覆盖原始压缩件和展示版。

- catalog.txt
- reconstructed-schema.sql
- schema-rebuild.log
- baseline-probes.log

Schema展示版额外为两个Olm公钥列名添加行内gitleaks误报注释（只有DDL列名，没有密钥值）；仓库现有.gitleaks.toml也明确说明该类误报。原始pg_dump字节仍完整保存在.raw.gz，不改变全局扫描规则。
