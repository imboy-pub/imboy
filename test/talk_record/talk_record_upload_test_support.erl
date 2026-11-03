-module(talk_record_upload_test_support).
-export([configure/0, bytes/1, upload/2, upload_failclosed/1]).

%% Explicit isolated root required; never fall back to a previous run's root.
%% S3-only direct 合同（基线 garage-direct-v3）：分段注册经 direct-v1 链，
%% ck_dv1_attempt_identity 只接受 garage 身份——设置
%% TALK_RECORD_PRIVATE_GARAGE_CONFIG 时 talk_record 根改挂 garage；
%% 未设置时保留 disk 根（仅适用于不经 direct 注册链的套件）。
configure() ->
    Root = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    true = is_list(Root) andalso filename:pathtype(Root) =:= absolute,
    ok = filelib:ensure_dir(filename:join(Root, "x")),
    ok = application:set_env(imboy, jwt_key, <<"synthetic-talk-upload-test-key-0123456789">>),
    configure_root().

configure_root() ->
    Namespace = <<"talk-upload-test">>,
    case os:getenv("TALK_RECORD_PRIVATE_GARAGE_CONFIG") of
        false ->
            application:set_env(imboy, attachment_storage_registry, #{
                version => 1,
                upload_namespaces => #{<<"talk_record">> => Namespace},
                roots => #{
                    talk_upload_test => #{
                        driver => <<"disk">>,
                        namespace => Namespace,
                        container => <<"capture">>,
                        endpoint =>
                            <<"file://",
                                (list_to_binary(os:getenv("TALK_UPLOAD_DISK_ROOT")))/binary>>
                    }
                }
            });
        Path ->
            {ok, Bytes} = file:read_file(Path),
            C = jsone:decode(Bytes),
            Endpoint = maps:get(<<"endpoint">>, C),
            true = byte_size(Endpoint) > 0,
            Garage = #{
                endpoint => Endpoint,
                bucket => maps:get(<<"bucket">>, C),
                region => maps:get(<<"region">>, C),
                access_key => maps:get(<<"access_key">>, C),
                secret_key => maps:get(<<"secret_key">>, C)
            },
            application:set_env(imboy, garage, Garage),
            application:set_env(imboy, attachment_storage_registry, #{
                version => 1,
                upload_namespaces => #{<<"talk_record">> => Namespace},
                roots => #{
                    talk_upload_test => #{
                        driver => <<"garage">>,
                        namespace => Namespace,
                        container => maps:get(bucket, Garage),
                        endpoint => Endpoint
                    }
                }
            })
    end.

bytes(Seq) -> binary:copy(<<(Seq rem 256)>>, 4096).

%% S3-only direct 合同：按 register 返回的 grant 描述符直 PUT（逐字头部，
%% 含 x-amz-checksum-sha256），再由 confirm 面收口 CONFIRMED。
upload(_Uid, Part) ->
    application:ensure_all_started(inets),
    Ref = pick(Part, upload_ref, <<"upload_ref">>),
    Grant = pick(Ref, grant, <<"grant">>),
    Url = pick(Grant, url, <<"url">>),
    Hs0 = pick(Grant, required_headers, <<"required_headers">>),
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs0],
    Bytes = bytes(maps:get(<<"seq">>, Part)),
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(
            put,
            {binary_to_list(Url), Hs, "audio/aac", Bytes},
            [],
            [{body_format, binary}]
        ),
    ok.

%% 声明不防劣负例（fail-closed 合同）：presign 头携带声明 sha 的 checksum，
%% 与正文不一致 → S3 在 PUT 即 400 拒收（早于 confirm 侧验证生效）。
upload_failclosed(Part) ->
    application:ensure_all_started(inets),
    Ref = pick(Part, upload_ref, <<"upload_ref">>),
    Grant = pick(Ref, grant, <<"grant">>),
    Url = pick(Grant, url, <<"url">>),
    Hs0 = pick(Grant, required_headers, <<"required_headers">>),
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hs0],
    Bytes = bytes(maps:get(<<"seq">>, Part)),
    {ok, {{_, 400, _}, _, _}} =
        httpc:request(
            put,
            {binary_to_list(Url), Hs, "audio/aac", Bytes},
            [],
            [{body_format, binary}]
        ),
    ok.

pick(Map, A, B) ->
    case maps:is_key(A, Map) of
        true -> maps:get(A, Map);
        false -> maps:get(B, Map)
    end.
