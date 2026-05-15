-module(epgsql_codec_rfc3339_bin).
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
-spec encode(binary(), timestamptz, any()) -> binary().
encode(Bin, timestamptz, _) when is_binary(Bin) ->
    case elib_dt:rfc3339_to(Bin, microsecond) of
        {error, _} ->
            error_logger:warning_msg("Invalid timestamptz value detected: ~p~n", [Bin]),
            <<0:64/big-signed-integer>>;
        LocalMicro when is_integer(LocalMicro) ->
            PgMicro = LocalMicro - 946684800000000,
            <<PgMicro:64/big-signed-integer>>
    end.

%% @doc 解码 PostgreSQL timestamptz 类型为 RFC3339 格式
%% @param Bin PostgreSQL 内部时间戳格式的二进制数据
%% @param 'timestamptz' 类型名称
%% @param _CodecState 编解码器状态（未使用）
%% @returns RFC3339 格式的时间戳二进制字符串
-spec decode(binary(), 'timestamptz', any()) -> binary().
decode(Bin, 'timestamptz', _CodecState) ->
    MicroSecs = binary:decode_unsigned(Bin, big),
    % MS = MicroSecs + (?POSTGRESQL_GS_EPOCH * 1000000) - (?UNIX_EPOCH_GREGORIAN * 1000000),
    MS = MicroSecs + 946684800000000,
    try
        elib_dt:to_rfc3339(MS, microsecond)
    catch
        _:_ ->
            % 检测到损坏的时间戳值，记录错误并返回安全默认值
            error_logger:warning_msg(
                "Invalid timestamptz value detected: ~p (raw: ~p)~n",
                [MS, MicroSecs]
            ),
            % 返回 epoch 时间作为安全默认值，避免崩溃
            <<"1970-01-01T00:00:00Z">>
    end.
