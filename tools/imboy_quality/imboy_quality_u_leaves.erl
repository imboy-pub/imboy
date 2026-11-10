%% IMBOY_ERLANG_QUALITY：U 叶级期望表的准入绑定加载器（D1/D2/D3 加固）。
%% gate 只接受「就是被准入的那一份」叶表：文件字节 sha256 必须等于 admission
%% 的 dynamic_U。全部失败都给具名 reason（不再靠 badmatch/裸二进制约解码），
%% 使运维能区分 缺失 / 不可读 / 未绑定 / 指纹不符 / 解码失败。
-module(imboy_quality_u_leaves).

-export([load/2]).

%% 64 位小写 hex：与 quality.escript 里 ActualU 的比较口径一致。
-define(FINGERPRINT_PATTERN, "^[0-9a-f]{64}$").

%% load(CtrlDir, AdmissionMap) -> {ok, LeafMap} | {error, Reason}
%%
%% 顺序固定：先读文件，再校验 dynamic_U 形状，最后比 sha256。
%% 读优先保证「文件不存在」不会被误报成「未绑定」。
load(CtrlDir, Admission) ->
    Path = filename:join([CtrlDir, "evidence", "u-leaves-r5.term"]),
    case file:read_file(Path) of
        {ok, Bin} ->
            case expected_fingerprint(Admission) of
                {ok, Expected} -> verify(Bin, Expected, Path);
                error -> {error, {u_leaves_fingerprint_unbound, CtrlDir}}
            end;
        {error, enoent} ->
            {error, {u_leaves_missing, Path}};
        {error, Reason} ->
            {error, {u_leaves_unreadable, Path, Reason}}
    end.

%% admission 里的 dynamic_U 必须是 64 位小写 hex；缺失/空/半截/大写/非 binary
%% 一律视为「未绑定」，不能拿它当期望指纹。
expected_fingerprint(Admission) when is_map(Admission) ->
    case maps:get(<<"dynamic_U">>, Admission, undefined) of
        Fingerprint when is_binary(Fingerprint) ->
            case re:run(Fingerprint, ?FINGERPRINT_PATTERN, [{capture, none}]) of
                match -> {ok, Fingerprint};
                _ -> error
            end;
        _ ->
            error
    end;
expected_fingerprint(_) ->
    error.

%% sha 匹配才允许返回叶表。不匹配时再解码一次，用来区分
%% 「换了另一份合法 ETF」（fingerprint_mismatch）与「字节已损坏」（decode_failed）。
verify(Bin, Expected, Path) ->
    Actual = hex(crypto:hash(sha256, Bin)),
    case Actual =:= Expected of
        true ->
            decode(Bin);
        false ->
            case decode(Bin) of
                {ok, _} ->
                    {error,
                        {u_leaves_fingerprint_mismatch, #{
                            expected => Expected,
                            actual => Actual,
                            path => Path
                        }}};
                {error, _} = Error ->
                    Error
            end
    end.

hex(Bin) ->
    string:lowercase(binary:encode_hex(Bin)).

%% 本任务自产 ETF（evidence/u-leaves-r5.term，可信源；框架 §11 允许本任务
%% 可信数据直接解码——safe 标志会拒绝自产原子集合）。
decode(Bin) ->
    try binary_to_term(Bin) of
        Term when is_map(Term) ->
            case shape(Term) of
                ok -> {ok, Term};
                {error, Key} -> {error, {u_leaves_decode_failed, {shape_invalid, Key}}}
            end;
        Other ->
            {error, {u_leaves_decode_failed, {not_a_map, Other}}}
    catch
        error:Reason ->
            {error, {u_leaves_decode_failed, Reason}}
    end.

%% 叶表形状：每个 K => [K]，K 为 non-empty binary（roots 与 leaves 同源）。
shape(Map) ->
    shape_entries(maps:to_list(Map)).

shape_entries([]) ->
    ok;
shape_entries([{Key, [Key]} | Rest]) when is_binary(Key), byte_size(Key) > 0 ->
    shape_entries(Rest);
shape_entries([{Key, _} | _]) ->
    {error, Key}.
