-module(epgsql_codec_rfc3339_bin).
-moduledoc "epgsql RFC3339 时间戳编解码器（behaviour epgsql_codec）。".
-dialyzer({nowarn_function, [encode/3]}).
% -behaviour(epgsql_codec).
-export([init/2, names/0, encode/3, decode/3]).

% 1970-01-01 00:00:00 的格里高利秒数
-define(UNIX_EPOCH_GREGORIAN, 62167219200).
% calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}).
-define(POSTGRESQL_GS_EPOCH, 63113904000).

%% 初始化编解码器（无需特殊操作）
-spec init(any(), any()) -> undefined.
init(_Opts, _Conn) ->
    undefined.

-spec names() -> [epgsql:type_name()].
names() -> [timestamptz].

%% @doc 编码 timestamptz 类型的数据
%% @param Bin RFC3339 格式的时间戳二进制字符串
%% @param timestamptz 类型名称
%% @param _CodecState 编解码器状态（未使用）
%% @returns PostgreSQL 内部时间戳格式的二进制数据
%% R19（IMBOY-TIMEZONE-REPAIR-V1）：非法 RFC3339 输入不再回退为 PG epoch 0
%% （= 2000-01-01 伪造日期），而是抛出明确错误让整条语句/事务失败——
%% 坏值必须以错误显形，不得静默变成一个看似正常的日期。
-spec encode(binary(), timestamptz, any()) -> binary().
encode(Bin, timestamptz, _) when is_binary(Bin) ->
    case elib_dt:rfc3339_to(Bin, microsecond) of
        {error, _} ->
            error({invalid_timestamptz_input, Bin});
        LocalMicro when is_integer(LocalMicro) ->
            PgMicro = LocalMicro - 946684800000000,
            <<PgMicro:64/big-signed-integer>>
    end;
%% calendar datetime 元组（agent_grant/agent_run 域的冻结合同形状）→ UTC
%% RFC3339。IT-10 集成实测（run 20260918T124045Z-6af21b0d）：此前本 codec 只收
%% binary，元组入参直接 function_clause，grant/run 全部写路径在真实部署中
%% 无法落库。已知秒级精度损失（元组无亚秒位）；binary 入参行为不变。
encode({{_, _, _}, {_, _, _}} = Dt, timestamptz, _) ->
    encode(dt_tuple_to_rfc3339(Dt), timestamptz, undefined).

dt_tuple_to_rfc3339(Dt) ->
    Gs = calendar:datetime_to_gregorian_seconds(Dt),
    UnixMicro = (Gs - 62167219200) * 1000000,
    elib_dt:to_rfc3339(UnixMicro, microsecond, "Z").

%% @doc 解码 PostgreSQL timestamptz 类型为 RFC3339 格式
%% R19（IMBOY-TIMEZONE-REPAIR-V1）两项修复：
%%   1. 有符号 64 位解码：原 binary:decode_unsigned(big) 把 PG epoch 之前的
%%      负微秒（<2000-01-01，如 1999 年）误读为超大正数，输出离谱的未来日期；
%%      PG timestamptz 内部即 int64 微秒（2000-01-01 起，可为负）。
%%   2. 输出显式 "Z"（UTC）：不再经 elib_dt:to_rfc3339/2 的默认 offset ""
%%      （= BEAM 服务器本地时区偏移）泄漏服务器时区；且绕开其 1970~2100
%%      范围夹紧（超范围值曾被静默替换为伪造的 1970-01-01）。数据库边界
%%      忠实呈现 PG 存的值；仅当值超出 calendar 可表达范围（公元 0~9999，
%%      PG 上限 294276 年）时才落入兜底分支（有 warning 日志，非静默）。
%% @param Bin PostgreSQL 内部时间戳格式的二进制数据（8 字节 int64 微秒）
%% @param 'timestamptz' 类型名称
%% @param _CodecState 编解码器状态（未使用）
%% @returns RFC3339 格式的时间戳二进制字符串（UTC "Z"，微秒精度保留）
-spec decode(binary(), 'timestamptz', any()) -> binary().
decode(Bin, 'timestamptz', _CodecState) ->
    <<PgMicro:64/big-signed-integer>> = Bin,
    UnixMicro = PgMicro + 946684800000000,
    try
        iolist_to_binary(
            calendar:system_time_to_rfc3339(UnixMicro, [
                {unit, microsecond}, {time_designator, $T}, {offset, "Z"}
            ])
        )
    catch
        _:_ ->
            % 检测到无法表达的损坏/超界时间戳值，记录错误并返回安全默认值
            error_logger:warning_msg(
                "Undecodable timestamptz value detected: ~p (raw: ~p)~n",
                [UnixMicro, PgMicro]
            ),
            % 返回 epoch 时间作为安全默认值，避免读路径崩溃
            <<"1970-01-01T00:00:00Z">>
    end.
