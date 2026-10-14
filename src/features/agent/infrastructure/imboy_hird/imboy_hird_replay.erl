%%% @doc Hirð deterministic **re-execution consistency** bridge
%%%（AG31-10B；G16/G17/A18）。
%%%
%%% 【命名修正（IMBOY_HIRD_AUTH_REPLAY_POC_V1 P2）】本模块**不是**离线
%%% Replay：它是「同一程序重复执行两次 + 归一化 audit hash 一致性」——
%%% 两次执行都会真实解析 handler（尽管是纯/mock），不使用 hird_replay
%%% cursor。真正的离线 Replay（录制的 audit 日志驱动程序执行、handler
%%% 与业务零调用）由 hird runtime 的 `hird_replay` cursor 提供，其端到端
%%% 验证见 test/features/agent/integration/imboy_hird_p2_replay_tests.erl。
%%% G16/G17/A18 的「deterministic replay / replay offline」命名沿用历史
%%% 验收矩阵字面，实际语义以本说明为准。
%%%
%%% 录制=一次执行的（audit JSONL 行集 + 归一化 hash）；重放=同输入二次
%%% 执行，比对 hash 一致性。
%%%
%%% 归一化：audit 行内的 timestamp 字段按骨架裁剪（保留
%%% caller/tool/result），保证确定程序两次运行 hash 稳定。
-module(imboy_hird_replay).

-export([record/3, normalize_audit/1, audit_hash/1]).

%% @doc 录制一次执行：
%% ```
%% record(ProgramModule, AuditFile, RunFun)
%%   -> #{audit_lines => [bin], hash => bin, result => term()}
%% '''
record(ProgramModule, AuditFile, RunFun) ->
    Lines = run_with_audit(ProgramModule, AuditFile, RunFun),
    #{
        audit_lines => Lines,
        hash => audit_hash(Lines),
        result => ok
    }.

run_with_audit(ProgramModule, AuditFile, RunFun) ->
    _ = file:delete(AuditFile),
    ok = filelib:ensure_dir(AuditFile),
    {ok, _} = hird_audit:start_link([{sink, {file, AuditFile}}]),
    try
        ok = hird_audit:register_tools(ProgramModule:hird_tools@()),
        Result = RunFun(),
        ok = hird_audit:sync(),
        Lines = read_lines(AuditFile),
        put(?MODULE, {result, Result}),
        Lines
    after
        try
            gen_server:stop(hird_audit)
        catch
            _:_ -> ok
        end
    end.

%% @doc 归一化：逐行 JSONL 抽取 caller/tool/result 骨架并排序（audit
%% 单例下跨实例行序由调度决定，排序后比 hash）。
normalize_audit(Lines) ->
    lists:sort([
        begin
            Caller = grab(Line, <<"\"caller\":\"">>, <<"\"">>),
            Tool = grab(Line, <<"\"tool\":\"">>, <<"\"">>),
            Result = grab(Line, <<"\"result\":">>, <<",">>),
            <<Caller/binary, "|", Tool/binary, "|", Result/binary>>
        end
     || Line <- Lines
    ]).

audit_hash(Lines) ->
    Normalized = normalize_audit(Lines),
    Data = io_lib:format("~p", [Normalized]),
    binary:encode_hex(erlang:md5(unicode:characters_to_binary(Data))).

grab(Bin, Prefix, Stop) ->
    case binary:split(Bin, Prefix) of
        [_, Rest] ->
            case binary:split(Rest, Stop) of
                [Head | _] -> Head;
                _ -> <<>>
            end;
        _ ->
            <<>>
    end.

read_lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].
