# IMBoy 调试工具使用指南

本文档介绍 IMBoy 项目中的调试工具和生产环境调试最佳实践。

## 目录

- [现有调试工具](#现有调试工具)
- [新增调试工具](#新增调试工具)
- [生产环境调试](#生产环境调试)
- [日志管理](#日志管理)
- [性能监控](#性能监控)
- [故障排除](#故障排除)
- [最佳实践](#最佳实践)

## 现有调试工具

### 1. Lager 日志系统

项目使用 Lager 作为主要的日志系统，配置文件位于 `config/sys.config`。

**配置特性：**
- 支持多种日志级别（debug, info, warning, error）
- 文件轮转（10MB，保留999个文件）
- 控制台彩色输出
- 日志根目录：`./log`

**使用示例：**
```erlang
% 在代码中使用
-include_lib("imlib/include/log.hrl").

?DEBUG_LOG(["调试信息", Variable]),
imboy_log:info("信息日志: ~p", [Data]),
imboy_log:error("错误日志: ~p", [Error]).
```

### 2. Recon 生产诊断工具

**主要功能：**
- 系统信息概览
- 进程监控和分析
- 内存泄漏检测
- 函数调用跟踪

**常用命令：**
```erlang
% 系统信息
recon:info().

% 内存使用最多的10个进程
recon:proc_count(memory, 10).

% 消息队列最长的10个进程
recon:proc_count(message_queue_len, 10).

% 检测二进制内存泄漏
recon:bin_leak(10).
```

### 3. Observer CLI 命令行监控

**功能：**
- 实时系统监控
- 进程树查看
- 内存和CPU使用情况
- ETS表监控

**启动方式：**
```bash
# 通过 remote_console 启动
bin/imboy remote_console

# 在 Erlang shell 中
observer_cli:start().
```

### 4. Redbug 调试跟踪

**功能：**
- 函数调用跟踪
- 返回值监控
- 调用栈分析

**使用示例：**
```erlang
% 跟踪模块的所有函数调用
redbug:start("user_logic:*", [{time, 10000}]).

% 跟踪特定函数
redbug:start("msg_c2c_logic:send_msg/3 -> return", [{time, 5000}]).
```

### 5. 崩溃转储分析

项目包含崩溃转储分析脚本：`script/erl_crashdump_analyzer.sh`

**功能：**
- 提取崩溃时间
- 内存使用分析
- 进程队列长度统计
- 错误日{"invoked_by":"Trae Builder","toolcall":{"thought":"现在让我创建一个调试工具使用文档，总结所有的改进建议和使用方法。","name":"write_to_file","params":{"rewrite":false,"file_path":"/Users/leeyi/project/imboy.pub/imboy/doc/debug_tools_guide.md","content":