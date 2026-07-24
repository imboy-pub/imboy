-module(elib_uuid).
-eqwalizer(enable).

%%% @doc UUIDv7 生成器（时序 UUID，RFC 9562）
%%% 格式：48-bit unix_ts_ms | ver=7 | 12-bit rand_a | var=10 | 62-bit rand_b

-export([gen_v7/0, gen_v7_bin/0]).

%% @doc 生成 UUIDv7 字符串（时序 UUID，36 字节）
-spec gen_v7() -> binary().
gen_v7() ->
    <<U0:32, U1:16, U2:16, U3:16, U4:48>> = gen_v7_bin(),
    iolist_to_binary(
        io_lib:format(
            "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
            [U0, U1, U2, U3, U4]
        )
    ).

%% @doc 生成 UUIDv7 原始 16 字节二进制（用于 PostgreSQL 原生 uuid 类型）
-spec gen_v7_bin() -> <<_:128>>.
gen_v7_bin() ->
    TsMs = erlang:system_time(millisecond),
    <<RandA:12, RandB:62, _:6>> = crypto:strong_rand_bytes(10),
    <<TsMs:48, 7:4, RandA:12, 2:2, RandB:62>>.
