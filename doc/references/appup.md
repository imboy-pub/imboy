# Erlang相关Appups/热更介绍及坑点

https://www.erlang.org/doc/design_principles/appup_cookbook

## load_module

find apps -name '*.appup' -exec rm {} \;

简单代码替换，针对某功能模块部分修改，仅需载入新版本即可

> {load_module, Module}

## update
同步代码替换，针对进程(某个gen_server/gen_fsm/gen_statem/gen_event等)的内部状态的格式发生改变，通过调用回调函数code_change转换内部状态，特殊进程调用回调函数system_code_change/4，处理方式为挂起进程，替换内部状态，切换新版，删除旧版，恢复进程
```
{update, Mod}

{update, Mod, supervisor}

{update, Mod, Change}

{update, Mod, DepMods}

{update, Mod, Change, DepMods}

{update, Mod, Change, PrePurge, PostPurge, DepMods}

{update, Mod, Timeout, Change, PrePurge, PostPurge, DepMods}

{update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, DepMods}
  Mod = atom()
  ModType = static | dynamic
  Timeout = int()>0 | default | infinity
  Change = soft | {advanced,Extra}
    Extra = term()   (default = > soft)
  PrePurge = PostPurge = soft_purge | brutal_purge　(default = > brutal_purge)
  DepMods = [Mod]
```
附：若改变督程启动规格，则需带supervisor参数

## add_module/delete_module
引入/删除模块

> {add_module, Module}
> {delete_module, Module}

## 应用(application)指令:
* {add_application, app} - 增加app应用-根据relup自动生成
* {remove_application, app} - 删除app应用-根据relup自动生成
* {restart_application, app} - 重启app应用

# 参考
* https://zhuanlan.zhihu.com/p/29647820

```
%% imboy.appup
{
    "新母项目版本号", %% 新的母项目版本号
    [ %% 升级指令
        %% 升级 imapi 子应用
        {apply, imapi, {appup, "新imapi版本号"}}
        %% 其他子应用的升级指令...
    ],
    [ %% 降级指令
        %% 降级 imapi 子应用
        {apply, imapi, {appup, "旧imapi版本号"}}
        %% 其他子应用的降级指令...
    ],
    %% 从其他任何版本升级到当前版本的指令
    {downfrom, {除当前版本外的任何版本, []}}
}
```

```
{downfrom, {"1.0.3", [
    %% 从 1.0.3 降级到 1.0.2 的降级指令
    {apply, {module, downgrade_function, [Args]}}
    %% ... 其他降级指令 ...
]}}
```

从版本 2.0 降级到版本 1.0，并且在版本 2.0 中你添加了一个新模块或者新功能，那么你可能需要在降级时删除这个模块或撤销这些更改。downfrom 指令就是用来描述这些步骤的。
```
{downfrom, {"2.0", [
    {delete_module, new_module},
    {load_module, old_module}
]}}
```
