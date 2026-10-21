-module(talk_record_processor_config).
-moduledoc """
# 面聊本机处理器协议 v1

此文描述工程接线，不代表已部署模型或通过 ASR、匿名分人、摘要质量验收。
当前没有内置模型；不下载权重、不调用云服务、不自动执行发现的程序。

## 显式部署配置

`{imboy, talk_record_processor}` 为 map：`enabled => true`、`approved => true`、
`version => <<"经批准的配置版本">>`，并含 `transcribe` / `summarize` 子 map。
每项需 `executable`（批准的绝对文件路径，具有执行位且不可被组/其他人写入）、
`model_version`（非空、不超过 64 字节）与可选固定 `args`；转写另需
`diarization => true`。程序、模型权重、用途及质量合同应先按产品计划批准。
此配置不自动赋予下载模型、联网、训练、保留副本或外传数据的许可。

能力面依据有效配置返回 `asr_enabled`、`summary_enabled` 与
`processor_config_version`。客户端必须在用户明确同意后发送真实的
`policy_version` / `processor_config_version`。同意事件实际入库后，job 的
`processor_input` 固化事件 ID、用途、配置版本及配置摘要。旧 NULL 不是已同意，
修改配置也不能沿用旧 job 执行；需新有效同意和处理请求。

## 程序协议

服务端 `spawn_executable`，不经 shell。stdin 写一行 JSON，程序输出一个 JSON 后退出；
stderr 合并进入受控输出；除约定的结果 JSON 外不输出诊断日志，尤其不得在日志中写原始音频或用户文本。
每次执行 timeout_ms 默认 120 秒，管理员批准配置可到 2 小时；输出上限 4 MiB。
heartbeat_ms 为 50—10000ms（默认 5000），未过期当前 token/attempt 才能续 job 与源引用租约。
超时/取消须确认 Port 退出；执行证明 prepared/running/unknown 不会按时间自动变 stopped，清理状态因此保持 pending。
程序不得派生脱离生命周期的后台进程、调用网络、执行记录中的指令或保留音频/文本副本。
是否满足这些约束需由实际批准的程序和部署隔离验证，路径配置本身不是证明。

公共输入：`protocol_version=1`、`kind`、`record_id`、`model_version`、
`processor_config_version`、固定的数据处理说明。

- `transcribe`：另带 `audio_path`、`language`。路径仅在私有目录、附件授权 spool
  回调期间存在。输出 `{model_version,status,speakers,segments}`；status 为
  `ready` / `no_speech`。speakers 为 `{label,confidence?}`，label 最长 48 字节；
  segments 为 `{start_ms,end_ms,speaker_label,text,confidence?}`，有效时轴必须落在音频范围。
  仅匿名簇，不接受用户 ID 或自动实名绑定；未知语音可用 `unknown` 簇和空文本保留范围。
- `summarize`：另带当前转写版本的 `segments`（id、时轴、文本）。输出
  `{model_version,overview,decisions,actions}`。每条 decision/action 必须带非空
  `segment_ids`、`text`，引用必须属于当前同记录版本。`owner_user_id`、`due_date`
  只接受未指定值；明确负责人/日期后可由人工校对，模型输出不创建任务或通知。

结构、时轴与引用校验不能证明识别准确率或摘要事实性；真实质量仍需冻结样本、
CER/WER、DER/JER、重叠/噪声/长音频、事实性及出处阈值的实际模型验收。

## 发布与生命周期

IO 在事务外；准备及发布事务均核 live record、current input、真实 job lease、
同意及配置 pin。发布 transcript 新版本、版本隔离的匿名 speaker、segments 与 job
成功原子提交；原文内容变化推进 `content_revision`，不推进设置/权限的 record revision。
需摘要时，只在成功转写后按新内容版本入队；无语音为 `no_speech`、摘要 `not_requested`。
摘要仅为 draft；原文校对/重跑使旧摘要 stale。已删除、失租、旧内容或旧转写版本不得发布。
音频处理引用具有 attempt 租约；临时本机文件在同步调用结束后删除，存储对象继续由既有
附件系统管理。意外进程/主机崩溃后的临时目录处置与实际处理器副本清除仍须部署验收。

仓内 `talk_record_processor_relay_tests` 使用明确标注的自建 synthetic 可执行协议替身，
只验证工程协议、权限/版本 fence、结构与事务；不能用于模型质量或完整业务验收通过。

""".
-compile({no_auto_import, [get/1]}).
-export([get/1, available/1, version/0, fingerprint/0, capabilities/1]).
-include_lib("kernel/include/file.hrl").

get(Kind) ->
    Bundle = config_ds:env(imboy, talk_record_processor, #{}),
    try
        #{enabled := true, approved := true, version := Version} = Bundle,
        true = text(Version),
        Provider = maps:get(kind(Kind), Bundle),
        #{executable := Exe, model_version := Model} = Provider,
        true = text(Model),
        true = is_list(Exe),
        absolute = filename:pathtype(Exe),
        {ok, #file_info{type = regular, mode = Mode, uid = Owner}} = file:read_file_info(Exe),
        true = trusted_path(filename:absname(Exe), Owner, 20),
        true = Mode band 8#111 =/= 0,
        true = Mode band 8#022 =:= 0,
        true = valid_args(maps:get(args, Provider, [])),
        true = Kind =/= <<"transcribe">> orelse maps:get(diarization, Provider, false) =:= true,
        Timeout = maps:get(timeout_ms, Provider, 120000),
        Beat = maps:get(heartbeat_ms, Provider, 5000),
        true = is_integer(Timeout) andalso Timeout >= 100 andalso Timeout =< 7200000,
        true = is_integer(Beat) andalso Beat >= 50 andalso Beat =< 10000,
        {ok, Provider#{
            config_version => Version,
            timeout_ms => Timeout,
            heartbeat_ms => Beat,
            max_output_bytes => 4194304
        }}
    catch
        _:_ -> {error, waiting_processor}
    end.

kind(<<"transcribe">>) -> transcribe;
kind(<<"summarize">>) -> summarize.
available(K) ->
    case get(K) of
        {ok, _} -> true;
        _ -> false
    end.
version() ->
    case get(<<"transcribe">>) of
        {ok, C} -> maps:get(config_version, C);
        _ -> null
    end.
fingerprint() ->
    binary:encode_hex(
        crypto:hash(
            sha256,
            term_to_binary(config_ds:env(imboy, talk_record_processor, #{}))
        ),
        lowercase
    ).

capabilities(Cap) ->
    Asr = available(<<"transcribe">>),
    Summary = Asr andalso available(<<"summarize">>),
    Cap#{
        <<"asr_enabled">> => Asr,
        <<"summary_enabled">> => Summary,
        <<"processing_available">> => Asr,
        <<"processor_config_version">> => version()
    }.
text(B) -> is_binary(B) andalso byte_size(B) > 0 andalso byte_size(B) =< 64.
valid_args(A) ->
    is_list(A) andalso length(A) =< 32 andalso
        lists:all(fun(X) -> is_list(X) andalso length(X) =< 4096 end, A).

%% An approved file must not be replaceable through an untrusted writable
%% directory/symlink ancestor. Sticky system temp roots retain safe test use.
trusted_path(Path, Owner, Left) when Left > 0 ->
    trusted_parts(filename:split(Path), "", Owner, Left);
trusted_path(_, _, _) ->
    false.
trusted_parts([], _, _, _) ->
    true;
trusted_parts([Part | Rest], Prefix, Owner, Left) ->
    Current =
        case Prefix of
            "" -> Part;
            _ -> filename:join(Prefix, Part)
        end,
    case file:read_link_info(Current) of
        {ok, #file_info{type = symlink, uid = Uid}} when Uid =:= 0; Uid =:= Owner ->
            case file:read_link(Current) of
                {ok, Target} ->
                    trusted_path(
                        filename:join([filename:absname(Target, filename:dirname(Current)) | Rest]),
                        Owner,
                        Left - 1
                    );
                _ ->
                    false
            end;
        {ok, #file_info{type = Type, uid = Uid, mode = Mode}} when Uid =:= 0; Uid =:= Owner ->
            Safe =
                Mode band 8#022 =:= 0 orelse
                    (Type =:= directory andalso Uid =:= 0 andalso Mode band 8#1000 =/= 0),
            Safe andalso trusted_parts(Rest, Current, Owner, Left);
        _ ->
            false
    end.
