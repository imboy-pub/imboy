-module(epgsql_codec_rfc3339_bin).
-behaviour(epgsql_codec).
-export([init/2, names/0, encode/3, decode/3]).

-define(POSTGRESQL_GS_EPOCH, 63113904000). % calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}).

%% 初始化编解码器（无需特殊操作）
init(_Opts, _Conn) ->
    undefined.

%% 声明处理的类型
names() ->
    ['timestamptz'].

%% 编码逻辑（使用默认实现）
encode(Data, TypeName, CodecState) ->
    epgsql_codec_datetime:encode(Data, TypeName, CodecState).

%% 二进制解码逻辑
decode(Bin, 'timestamptz', _CodecState) ->
    %% PostgreSQL timestamptz 二进制格式为自 2000-01-01 的微秒数
    MicroSecs = binary:decode_unsigned(Bin, big),
    % Secs = MicroSecs / 1000000 - 62167219200, % 转换为 UNIX 时间戳
    MS = MicroSecs + (?POSTGRESQL_GS_EPOCH * 1000000) - (62167219200 * 1000000),
    list_to_binary(imboy_dt:to_rfc3339(MS, microsecond)).
