#!/usr/bin/env escript
%%! -noshell
%%%===================================================================
%%% recrypt_user_collect.escript — A-06 主密钥轮换 / user_collect.info 清洗
%%%
%%% 把 user_collect.info 的历史形态统一升级成 A-05 的应用层密文：
%%%
%%%   形态 1  encode(encrypt('<base64(明文)>', '<主密钥>', ...), 'base64')
%%%           A-05 之前的脏数据。从未真正加密，且**内含全站主密钥**。
%%%   形态 2  {...} / [...]   明文 JSON（迁移 00000053 清洗后的中间态）
%%%   形态 3  aesg1_<base64>  已是目标形态，跳过
%%%
%%%   → 一律用 --key 指定的**新**密钥重新加密成 aesg1_<base64>
%%%
%%% 幂等：重复执行只会跳过形态 3。可与迁移 00000053 任意先后顺序执行。
%%%
%%% 用法:
%%%   escript scripts/recrypt_user_collect.escript --dry-run [选项]
%%%   escript scripts/recrypt_user_collect.escript --apply   [选项]
%%%
%%% 选项（未给出时读环境变量，再退回本地开发默认值）:
%%%   --host H      PGHOST      默认 127.0.0.1
%%%   --port P      PGPORT      默认 4323
%%%   --user U      PGUSER      默认 imboy_user
%%%   --password W  PGPASSWORD  无默认值，必须给（口令不进仓库）
%%%   --db D        PGDATABASE  默认 imboy_v1
%%%   --key K       IMBOY_POSTGRE_AES_KEY  （32 字节，必填，用于**新**密钥）
%%%   --batch N     每批处理行数，默认 500
%%%
%%% --dry-run 只读：统计各形态行数并抽样打印前 5 行明文，绝不写库。
%%%
%%% ⚠️ 生产执行前先读 docs/runbooks/postgre-aes-key-rotation.md
%%% @end
%%%===================================================================

-define(PREFIX, <<"aesg1_">>).

main(Args) ->
    _ = application:ensure_all_started(crypto),
    case parse(Args, #{mode => undefined, batch => 500}) of
        {error, Msg} ->
            io:format(standard_error, "错误: ~ts~n~n", [Msg]),
            usage(),
            halt(2);
        Opts ->
            ok = ensure_epgsql(),
            run(Opts)
    end.

usage() ->
    io:format(standard_error,
        "用法: escript scripts/recrypt_user_collect.escript --dry-run|--apply [--key <32字节>] ...~n"
        "     详见脚本头部注释。~n", []).

%%%===================================================================
%%% 参数
%%%===================================================================

parse([], #{mode := undefined}) ->
    {error, "必须指定 --dry-run 或 --apply"};
parse([], Opts) ->
    Key =
        case maps:get(key, Opts, undefined) of
            undefined ->
                case os:getenv("IMBOY_POSTGRE_AES_KEY") of
                    false -> undefined;
                    V -> list_to_binary(V)
                end;
            K0 ->
                K0
        end,
    case Key of
        K when is_binary(K), byte_size(K) =:= 32 ->
            Opts#{key => K};
        K when is_binary(K) ->
            {error, "--key 长度必须为 32 字节，当前为 " ++ integer_to_list(byte_size(K))};
        _ ->
            {error, "缺少 --key（或环境变量 IMBOY_POSTGRE_AES_KEY）"}
    end;
parse(["--dry-run" | T], Opts) -> parse(T, Opts#{mode => dry_run});
parse(["--apply" | T], Opts) -> parse(T, Opts#{mode => apply});
parse(["--host", V | T], Opts) -> parse(T, Opts#{host => V});
parse(["--port", V | T], Opts) -> parse(T, Opts#{port => list_to_integer(V)});
parse(["--user", V | T], Opts) -> parse(T, Opts#{user => V});
parse(["--password", V | T], Opts) -> parse(T, Opts#{password => V});
parse(["--db", V | T], Opts) -> parse(T, Opts#{db => V});
parse(["--key", V | T], Opts) -> parse(T, Opts#{key => list_to_binary(V)});
parse(["--batch", V | T], Opts) -> parse(T, Opts#{batch => list_to_integer(V)});
parse([Other | _], _Opts) -> {error, "未知参数: " ++ Other}.

opt(Opts, Name, EnvVar, Default) ->
    case maps:get(Name, Opts, undefined) of
        undefined ->
            case os:getenv(EnvVar) of
                false -> Default;
                V -> V
            end;
        V ->
            V
    end.

%%%===================================================================
%%% 主流程
%%%===================================================================

run(Opts0) ->
    Mode = maps:get(mode, Opts0),
    Key = key_from(Opts0),
    Port =
        case maps:get(port, Opts0, undefined) of
            undefined ->
                case os:getenv("PGPORT") of
                    false -> 4323;
                    P -> list_to_integer(P)
                end;
            P ->
                P
        end,
    Conn = connect(#{
        host => opt(Opts0, host, "PGHOST", "127.0.0.1"),
        port => Port,
        username => opt(Opts0, user, "PGUSER", "imboy_user"),
        password => require(opt(Opts0, password, "PGPASSWORD", undefined), "--password / PGPASSWORD"),
        database => opt(Opts0, db, "PGDATABASE", "imboy_v1")
    }),
    io:format("模式: ~p~n", [Mode]),
    report_counts(Conn),
    Rows = fetch_pending(Conn, maps:get(batch, Opts0)),
    io:format("待处理行数: ~p~n", [length(Rows)]),
    case Mode of
        dry_run ->
            sample(Rows),
            io:format("~n[dry-run] 未写入任何数据。~n", []);
        apply ->
            N = apply_rows(Conn, Rows, Key),
            io:format("已重加密 ~p 行。~n", [N]),
            report_counts(Conn)
    end,
    ok = epgsql:close(Conn).

require(undefined, What) ->
    io:format(standard_error, "错误: 缺少 ~ts~n", [What]),
    halt(2);
require(V, _What) ->
    V.

key_from(Opts) ->
    case maps:get(key, Opts, undefined) of
        K when is_binary(K) -> K;
        _ -> erlang:error(missing_key)
    end.

connect(Cfg) ->
    case epgsql:connect(Cfg) of
        {ok, Conn} ->
            Conn;
        {error, Reason} ->
            io:format(standard_error, "数据库连接失败: ~p~n", [Reason]),
            halt(3)
    end.

report_counts(Conn) ->
    Sql =
        "SELECT count(*) AS total,"
        " count(*) FILTER (WHERE info LIKE 'encode(encrypt(''%') AS legacy_literal,"
        " count(*) FILTER (WHERE info LIKE 'aesg1\\_%') AS already_ciphered,"
        " count(*) FILTER (WHERE info <> '' AND info NOT LIKE 'encode(encrypt(''%'"
        "                    AND info NOT LIKE 'aesg1\\_%') AS plaintext"
        " FROM public.user_collect",
    {ok, _, [{Total, Legacy, Ciphered, Plain}]} = epgsql:squery(Conn, Sql),
    io:format(
        "user_collect 统计: 总计=~ts  含主密钥脏数据=~ts  已密文=~ts  明文=~ts~n",
        [Total, Legacy, Ciphered, Plain]
    ).

%% 取出所有还不是 aesg1_ 形态的非空行
fetch_pending(Conn, Batch) ->
    Sql =
        "SELECT id, info FROM public.user_collect"
        " WHERE info <> '' AND info NOT LIKE 'aesg1\\_%'"
        " ORDER BY id LIMIT $1",
    {ok, _, Rows} = epgsql:equery(Conn, Sql, [Batch]),
    Rows.

sample(Rows) ->
    io:format("~n抽样（最多 5 行，只显示明文前 120 字节）:~n", []),
    lists:foreach(
        fun({Id, Info}) ->
            Plain = plaintext_of(Info),
            Show = binary:part(Plain, 0, min(120, byte_size(Plain))),
            io:format("  id=~p  明文=~ts~n", [Id, Show])
        end,
        lists:sublist(Rows, 5)
    ).

apply_rows(Conn, Rows, Key) ->
    lists:foldl(
        fun({Id, Info}, Acc) ->
            case plaintext_of(Info) of
                <<>> ->
                    io:format(standard_error, "跳过 id=~p：无法提取明文~n", [Id]),
                    Acc;
                Plain ->
                    Cipher = <<?PREFIX/binary, (gcm_encrypt(Plain, Key))/binary>>,
                    Sql = "UPDATE public.user_collect SET info = $1 WHERE id = $2",
                    {ok, _} = epgsql:equery(Conn, Sql, [Cipher, Id]),
                    Acc + 1
            end
        end,
        0,
        Rows
    ).

%%%===================================================================
%%% 形态识别与加密（与 elib_hasher / elib_cipher 保持一致）
%%%===================================================================

%% 形态 1：从含主密钥的 SQL 表达式字面值里抠出 base64(明文)
plaintext_of(<<"encode(encrypt('", Rest/binary>>) ->
    case binary:split(Rest, <<"'">>) of
        [B64, _] ->
            try
                base64:decode(B64)
            catch
                _:_ -> <<>>
            end;
        _ ->
            <<>>
    end;
%% 形态 2：明文
plaintext_of(Info) when is_binary(Info) ->
    Info.

%% 与 elib_cipher:aes_gcm_encrypt/2 完全一致：
%% base64(salt(16) || iv(12) || ciphertext || tag(16))，salt 兼作 AAD
gcm_encrypt(Plain, Key) ->
    IV = crypto:strong_rand_bytes(12),
    Salt = crypto:strong_rand_bytes(16),
    {Cipher, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, IV, Plain, Salt, 16, true
    ),
    base64:encode(<<Salt/binary, IV/binary, Cipher/binary, Tag/binary>>).

%%%===================================================================
%%% 依赖加载
%%%===================================================================

%% escript 不走 rebar/erlang.mk 的 code path，这里显式把 deps 挂上
ensure_epgsql() ->
    Root = filename:dirname(filename:dirname(escript:script_name())),
    Paths =
        filelib:wildcard(filename:join([Root, "deps", "*", "ebin"])) ++
            [filename:join([Root, "ebin"])],
    _ = [code:add_pathz(P) || P <- Paths, filelib:is_dir(P)],
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            ok;
        {error, Reason} ->
            io:format(
                standard_error,
                "未找到 epgsql（~p）。请先在仓库根执行 make deps 或 make app。~n",
                [Reason]
            ),
            halt(4)
    end.
