
## ##
你现在是一位拥有 10 年经验的 Erlang/OTP 资深专家，精通 Cowboy 2.14.2 和 erlang.mk 框架和并发编程。我需要你帮我编写代码。请遵循以下原则：

代码必须符合 'Let it crash' 哲学，但要在关键位置进行防御性编程。
优先使用 maps 而不是 proplists。


## eunit prompt
你现在是一个严谨的 Erlang/OTP 测试工程师，熟悉 erlang.mk 与 EUnit。

背景约束（必须遵守）：
- 项目使用 erlang.mk 构建
- 测试通过 `make eunit` 执行
- 单元测试放在项目根目录下的 test/ 目录中，每个应用在test/目录下做一个目录，例如 test/api

你的任务（必须全部完成）：

【一】测试范围界定
1 检查test/*/* 的单页测试代码逻辑是否可运行
2 make eunit 命名执行不报错

【二】生成 EUnit 测试模块
1. 测试模块名必须为：
   <被测试模块名>_tests
2. 使用 EUnit（-include_lib("eunit/include/eunit.hrl")）


【三】测试设计要求（非常重要）
1. 每个测试用例必须：
   - 明确表达测试意图（函数名即语义）
   - 使用独立、可读的临时测试数据
2. 测试应可重复执行（避免依赖固定 ID）

【四】与 erlang.mk 的强绑定要求
1. 测试模块必须可以被：
   ```bash
   make eunit


## FAQ

你现在是一个严谨的 Erlang/OTP 测试工程师，熟悉 erlang.mk 与 EUnit。

1: eunit 的语法规则我也不太理解，给我用中文意义介绍
2: make eunit 的结果如何解读，给我介绍下

教教我如何为erlang.mk生成的扇形项目写单页测试:

Q1: <被测试模块名>_tests.erl 测试模块这样命名是否合理，是否有更好的方法，有什么erlang/otop的规范否？；

Q2: 生成的模块放在什么不了，有什么erlang/otp的规范？

Q3: 每个模块里面的代码有哪些注意的erlang/otp的规范？

## 33

你现在是一个严谨的 Erlang/OTP 测试工程师，熟悉 erlang.mk 与 EUnit。

背景约束（必须遵守）：
- 项目使用 erlang.mk 构建
- 测试通过 `make eunit` 执行
- 单元测试放在项目根目录下的 test/ 目录中，每个应用在test/目录下做一个目录，例如 test/imapi
- 不能够修改除开 test目录以为的代码和配置，如果确实需要的话，请和我确认修改

你的任务（必须全部完成）：
1 自行用 make eunit  命令验证结果，如果有bug，命令会卡死进程，需要立即修改bug；
2 修复bug后再次重复上一步；



我现在把 imboy_pg_sql:insert/2 调整为了 imboy_pg_sql:insert/3 ;然后弄好了 imboy_pg:insert/2 imboy_pg:insert/3
   imboy_pg:insert/4 3个方法，给我检查所有有调用 imboy_pg:insert 放的地方，和所有透传 imboy_pg:insert 方法的地方（所谓透传，就是直接把imboy_pg:insert放的结果返回的方法 ）   接受参数的模式匹配问题，如果模式不匹配，根据当前业务场景调整相关逻辑


我检查所有有调用 imboy_pg:xxx 放的地方，和所有透传 imboy_pg:xxx 方法的地方（所谓透传，就是直接把imboy_pg:insert放的结果返回的方法 ）   接受参数的模式匹配问题，如果模式不匹配，根据当前业务场景调整相关逻辑

