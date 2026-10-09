# 文档维护 / Documentation maintenance

> **状态**：CURRENT · **读者**：三仓文档维护者

文档分类、状态、归属和归档规则以[文档门户](../README.md)为准。本目录提供写作工具和模板，不重复维护仓库盘点、迁移路线图或执行成绩。

AI 任务计划、审查报告、测试报告、截图、日志和一次性测试材料保存在仓外，不进入 Git 或交付包。自动化测试源码及必要合成数据随代码维护。仓内归档仅保留仍有决策价值的产品文档，失效且重复的内容可以删除。

写作时先确定读者和目的，再选择相应模板：

- [写作指南](writing-guide.md)
- [文档检查工具与配置说明](docs-as-code.md)
- [README 模板](templates/readme-template.md)
- [教程模板](templates/tutorial-template.md)
- [操作指南模板](templates/howto-template.md)
- [参考文档模板](templates/reference-template.md)
- [架构决策模板](templates/adr-template.md)

检查工具的存在不代表某次 CI 或交付已通过。运行结果必须绑定实际版本与环境，证据保存在仓外；文档中的配置示例也不能替代实际启用的门禁。
