-module(assembly_io_worker_tests).
-moduledoc "V3-18 P2：汇编 IO Worker 三原语端到端（真 Garage：upload→copy→fetch）".
%%%
%%% 断言：
%%%   * upload_file：本地文件 → Worker 自签 PUT（checksum）→ 返回 sha/size
%%%     与内容一致；
%%%   * copy_staging：服务端 CopyObject（staging→目标键）零 BEAM 字节；
%%%   * fetch_files：GET 下载 + size/sha 全量校验（内容逐字节一致）；
%%%   * 反例：fetch 错 sha fail-closed；upload 空文件拒绝。
%%% 前置：A0_ENDPOINT/A0_AK/A0_SK（v3/fixture/garage-v3run1.env）。
%%%

-include_lib("eunit/include/eunit.hrl").

assembly_io_test_() ->
    {timeout, 120, fun assembly_io/0}.

assembly_io() ->
    false = os:getenv("A0_ENDPOINT") =:= false,
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    Ep = list_to_binary(os:getenv("A0_ENDPOINT")),
    Ak = list_to_binary(os:getenv("A0_AK")),
    Sk = list_to_binary(os:getenv("A0_SK")),
    Cfg = #{
        endpoint => Ep,
        bucket => <<"imboy">>,
        access_key => Ak,
        secret_key => Sk,
        region => <<"garage">>
    },
    PrevRegistry = application:get_env(imboy, attachment_storage_registry),
    %% with_snapshot/copy_dispatch 以 garage env 与 registry endpoint 比对
    %% （canonical 一致性），二者必须同源——否则 location_changed fail-closed。
    PrevGarage = application:get_env(imboy, garage),
    ok = application:set_env(imboy, garage, Cfg),
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{
            <<"a0-root">> => #{
                driver => <<"garage">>,
                namespace => <<"a0-ns">>,
                endpoint => Ep,
                container => <<"imboy">>,
                access_key => Ak,
                secret_key => Sk,
                region => <<"garage">>
            }
        }
    }),
    Run = binary_to_list(<<"g18-io-", (integer_to_binary(os:system_time(millisecond)))/binary>>),
    Tmp = filename:join("/tmp", Run ++ ".bin"),
    Body = <<"G18-ASSEMBLY-IO-PAYLOAD-", 0:(40 * 8)>>,
    ok = file:write_file(Tmp, Body),
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    try
        %% 1) upload_file（Worker 自签 PUT + checksum）
        Staging = <<"assembly-staging/g18/", (list_to_binary(Run))/binary>>,
        {ok, #{sha256 := UpSha, size := UpSize}} =
            attachment_worker_port:upload_file(
                #{
                    src_path => Tmp,
                    endpoint => Ep,
                    ak => Ak,
                    sk => Sk,
                    bucket => <<"imboy">>,
                    region => <<"garage">>,
                    object_key => Staging,
                    mime => <<"application/octet-stream">>
                }
            ),
        ?assertEqual(Sha, UpSha),
        ?assertEqual(byte_size(Body), UpSize),
        io:format("worker_upload_checksum: ~pB ok~n", [UpSize]),
        %% 2) copy_staging（服务端 Copy；目标键；registry a0-ns）
        Dst = <<"talk-record/assembly/g18-", (list_to_binary(Run))/binary>>,
        {ok, #{etag := _}} =
            attachment_derived_artifact:copy_staging(
                #{namespace => <<"a0-ns">>, container => <<"imboy">>, key => Dst},
                Staging
            ),
        io:format("server_side_copy: ok~n"),
        %% 3) fetch_files（GET + size/sha 校验）
        GetUrl = elib_s3_sign:presign_signed_url_at(
            <<"GET">>, Ep, <<"imboy">>, Dst, [], calendar:universal_time(), 300, Cfg
        ),
        Dl = filename:join("/tmp", Run ++ "-dl.bin"),
        {ok, _} = attachment_worker_port:fetch_files([
            #{url => GetUrl, path => Dl, sha256_hex => Sha, size => byte_size(Body)}
        ]),
        {ok, Body} = file:read_file(Dl),
        io:format("worker_fetch_bytes_exact: ok~n"),
        %% 4) 反例：fetch 错 sha → fail-closed
        ?assertMatch(
            {error, {part_integrity_mismatch, 1}},
            attachment_worker_port:fetch_files([
                #{url => GetUrl, path => Dl, sha256_hex => <<"dead">>, size => byte_size(Body)}
            ])
        ),
        io:format("fetch_bad_sha_rejected: ok~n"),
        %% 5) 反例：upload 空文件拒绝
        Empty = filename:join("/tmp", Run ++ "-empty.bin"),
        ok = file:write_file(Empty, <<>>),
        ?assertMatch(
            {error, _},
            attachment_worker_port:upload_file(
                #{
                    src_path => Empty,
                    endpoint => Ep,
                    ak => Ak,
                    sk => Sk,
                    bucket => <<"imboy">>,
                    region => <<"garage">>,
                    object_key => <<"assembly-staging/g18/empty-", (list_to_binary(Run))/binary>>,
                    mime => <<"application/octet-stream">>
                }
            )
        ),
        io:format("empty_upload_rejected: ok~n"),
        _ = file:delete(Dl),
        _ = file:delete(Empty),
        ok
    after
        _ = file:delete(Tmp),
        case PrevRegistry of
            undefined -> application:unset_env(imboy, attachment_storage_registry);
            {ok, R} -> application:set_env(imboy, attachment_storage_registry, R)
        end,
        case PrevGarage of
            undefined -> application:unset_env(imboy, garage);
            {ok, G} -> application:set_env(imboy, garage, G)
        end
    end.
