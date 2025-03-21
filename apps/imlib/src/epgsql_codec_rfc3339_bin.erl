-module(epgsql_codec_rfc3339_bin).
% -behaviour(epgsql_codec).
-export([init/2, names/0, encode/3, decode/3]).

-define(UNIX_EPOCH_GREGORIAN, 62167219200). % 1970-01-01 00:00:00 的格里高利秒数
-define(POSTGRESQL_GS_EPOCH, 63113904000). % calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}).

%% 初始化编解码器（无需特殊操作）
init(_Opts, _Conn) ->
    undefined.

%% 声明处理的类型
names() ->
    ['timestamptz'].

encode(Bin, timestamptz, _) when is_binary(Bin) ->
    LocalMicro = imboy_dt:rfc3339_to(Bin, microsecond),
    % PgMicro = LocalMicro - (?POSTGRESQL_GS_EPOCH * 1000000) + (?UNIX_EPOCH_GREGORIAN * 1000000),
    PgMicro = LocalMicro - 946684800000000,
    <<PgMicro:64/big-signed-integer>>;
%% 编码逻辑（使用默认实现）
encode(Data, TypeName, CodecState) ->
    io:format("data ~p, type ~p, State: ~p, ~n", [Data, TypeName, CodecState]),
    epgsql_codec_datetime:encode(Data, TypeName, CodecState).

%% 二进制解码逻辑
decode(Bin, 'timestamptz', _CodecState) ->
    MicroSecs = binary:decode_unsigned(Bin, big),
    % MS = MicroSecs + (?POSTGRESQL_GS_EPOCH * 1000000) - (?UNIX_EPOCH_GREGORIAN * 1000000),
    MS = MicroSecs + 946684800000000,
    list_to_binary(imboy_dt:to_rfc3339(MS, microsecond)).
