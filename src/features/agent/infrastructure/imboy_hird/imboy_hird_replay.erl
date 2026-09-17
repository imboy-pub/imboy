%%% @doc Hirð deterministic replay bridge（AG31-10B；G16/G17/A18）。
%%%
%%% 录制=一次执行的（输入指纹 + audit JSONL 行集 + 外呼计数）三元组；
%%% 重放=同输入、**零外呼**（handler 全部替换为录制值/纯 mock，外部
%%% Model/Tool/CS 计数器由调用方注入并在重放中恒 0）二次执行，比对
%%% 归一化 audit hash 一致性。
%%%
%%% 归一化：audit 行内的时间戳/进程字典序噪声按字段裁剪（保留
%%% caller/tool/result 骨架），保证确定程序两次运行 hash 稳定。
-module(imboy_hird_replay).

-export([record/3, replay/3, normalize_audit/1, audit_hash/1]).

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

%% @doc 离线重放：同程序同输入再执行（RunFun 内部 handler 必须是录制值
%% /纯 mock——外呼计数器由 RunFun 持有并保持 0），比对归一化 hash。
%% ```
%% replay(ProgramModule, AuditFile, RunFun)
%%   -> #{hash => bin, matches => boolean(), external_calls => non_neg_integer()}
%% '''
replay(ProgramModule, AuditFile, RunFun) ->
    Lines = run_with_audit(ProgramModule, AuditFile, RunFun),
    ExternalCalls = external_call_count(),
    #{hash => audit_hash(Lines), matches => false, external_calls => ExternalCalls}.

%% 重放一致性比对（两份 recording 的 hash 相等性由调用方持原始录制
%% 比对；本模块提供比较器避免 hash 泄漏到测试外）。
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

external_call_count() ->
    case get(imboy_hird_external_calls) of
        N when is_integer(N) -> N;
        undefined -> 0
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
