-module(eb_asset_content_streaming_tests).
-moduledoc "step-15：企业 content 受控流式读（stream_content_chunked；HEAD/Range/缓存语义冻结）".
%%%
% stream_content_chunked 合同（协议 §7）：
%   * garage frozen 行（有物理键或直传位置）→ read_fun 受控流（分块拼回=
%     原字节）+ size（HEAD 语义）+ mime；
%   * 416/越界 → eof；存储 5xx → 错误（不泄漏内部签名/位置）；
%   * 历史行（legacy_unknown）→ {fallback_full, _}（回退既有全量，兼容不变）；
%   * 删除态 → not_found（无权限隐藏）；
%   * 缓存语义：调用方 content_respond 既有 private,no-store 头（cs_widget
%     handler 现状），本套件钉住 no-store 存在于 handler 源码。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(ORG, 990930).
-define(WS, 990937).

streaming_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun chunked_read_roundtrip/0,
        fun head_gives_size_and_mime/0,
        fun out_of_range_maps_to_eof/0,
        fun storage_error_not_leaked/0,
        fun deleted_asset_hidden/0,
        fun no_store_header_present/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy,
        garage,
        #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Ns = <<"dv1cs-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            Ns => #{
                driver => <<"garage">>,
                namespace => Ns,
                endpoint => EP,
                container => <<"b">>
            }
        }
    }),
    #{booted => Booted, before_env => Before, ns => Ns, ep => EP}.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"SET LOCAL session_replication_role = replica">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM enterprise_asset WHERE organization_id=$1">>,
                [?ORG]
            ),
            ok
        end),
        eunit_runner:eunit_cleanup(Booted)
    catch
        _:_ -> eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

insert_eb_asset(Id, ObjectKey, Mime) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO enterprise_asset (id, organization_id, workspace_id, object_key,"
            " object_hash, mime, size_bytes, status, key_version, version, uploaded_by_user_id,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,$2,$3,$4,$5,$6,12,'active',1,1,990935,"
            " 'garage',$9,'b',$7,$8)"
        >>,
        [Id, ?ORG, ?WS, ObjectKey, hex64(Id), Mime, locator(), ObjectKey, current_ns()]
    ),
    ok.

current_ns() ->
    {ok, Reg} = application:get_env(imboy, attachment_storage_registry),
    Roots = maps:get(roots, Reg, #{}),
    [Ns] = [
        K
     || {K, #{namespace := _}} <- maps:to_list(Roots),
        binary:match(K, <<"dv1cs-">>) =/= nomatch
    ],
    Ns.

locator() ->
    attachment_storage:locator_digest(current_ns(), #{
        driver => <<"garage">>,
        root_id => current_ns(),
        endpoint => <<"http://127.0.0.1:39999">>,
        container => <<"b">>
    }).

with_read_mocks(Bin, HeadFun, Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(attachment_storage_garage, head_bounded, 3, HeadFun),
    ok = meck:expect(
        attachment_storage_garage,
        presign_snapshot,
        2,
        fun(_I, _T) -> {ok, <<"http://mock/get">>} end
    ),
    ok = meck:new(httpc, [no_link]),
    ok = meck:expect(httpc, request, 4, fun(get, {_U, Headers}, _Opts, _ClientOpts) ->
        Range = proplists:get_value("range", Headers),
        <<"bytes=", RangeRest/binary>> = list_to_binary(Range),
        [FromS, ToS] = binary:split(RangeRest, <<"-">>, [global]),
        From = binary_to_integer(FromS),
        To = binary_to_integer(ToS),
        case From >= byte_size(Bin) of
            true ->
                {ok, {{"HTTP/1.1", 416, "Range Not Satisfiable"}, [], <<>>}};
            false ->
                End = min(To, byte_size(Bin) - 1),
                {ok, {
                    {"HTTP/1.1", 206, "Partial Content"}, [], binary:part(Bin, From, End - From + 1)
                }}
        end
    end),
    try
        Inner()
    after
        meck:unload(httpc),
        meck:unload(attachment_storage_garage)
    end.

%% ---------------------------------------------------------------- cases

chunked_read_roundtrip() ->
    Bin = sample_bytes(),
    Key = <<"enterprise/990930/990937/a1">>,
    ok = insert_eb_asset(990930001, Key, <<"image/png">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin), content_type => <<"image/png">>}} end,
        fun() ->
            {ok, #{read_fun := ReadFun, size := Size}} =
                eb_asset_store:stream_content_chunked(?ORG, 990937, 990930001),
            ?assertEqual(byte_size(Bin), Size),
            %% 分块拼回 = 原字节
            Collected = collect(ReadFun, 0, 5, <<>>),
            ?assertEqual(Bin, Collected)
        end
    ),
    ok.

head_gives_size_and_mime() ->
    Bin = sample_bytes(),
    Key = <<"enterprise/990930/990937/a2">>,
    ok = insert_eb_asset(990930002, Key, <<"video/mp4">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin), content_type => <<"video/mp4">>}} end,
        fun() ->
            {ok, Info} = eb_asset_store:stream_content_chunked(?ORG, 990937, 990930002),
            %% HEAD 语义：size/mime 在不含正文的情况下可取（传输审计字段）
            ?assertEqual(byte_size(Bin), maps:get(size, Info)),
            ?assertEqual(<<"video/mp4">>, maps:get(mime, Info))
        end
    ),
    ok.

out_of_range_maps_to_eof() ->
    Bin = sample_bytes(),
    Key = <<"enterprise/990930/990937/a3">>,
    ok = insert_eb_asset(990930003, Key, <<"image/png">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin)}} end,
        fun() ->
            {ok, #{read_fun := ReadFun}} =
                eb_asset_store:stream_content_chunked(?ORG, 990937, 990930003),
            ?assertEqual(eof, ReadFun(byte_size(Bin) + 100, 16)),
            ok
        end
    ).

storage_error_not_leaked() ->
    Key = <<"enterprise/990930/990937/a4">>,
    ok = insert_eb_asset(990930004, Key, <<"image/png">>),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        head_bounded,
        3,
        fun(_N, _C, _K) -> {error, {storage_status, 500}} end
    ),
    try
        R = eb_asset_store:stream_content_chunked(?ORG, 990937, 990930004),
        %% 存储错误映射为 object_store 包装（不泄漏内部签名/URL，T33）
        ?assertMatch({error, {object_store, _}}, R)
    after
        meck:unload(attachment_storage_garage)
    end,
    ok.

deleted_asset_hidden() ->
    %% 删除态：not_found（无权限隐藏，T23 语义）
    ok = meck:new(eb_pg_asset_meta, [passthrough, no_link]),
    ok = meck:expect(eb_pg_asset_meta, fetch_asset, 3, {ok, #{status => deleted}}),
    try
        ?assertEqual(
            {error, not_found},
            eb_asset_store:stream_content_chunked(?ORG, ?WS, 990930099)
        )
    after
        meck:unload(eb_pg_asset_meta)
    end,
    ok.

no_store_header_present() ->
    %% 缓存隔离：widget content 响应头 private,no-store（现状钉住）
    Src = os:cmd(
        "grep -c 'private, no-store' src/features/customer_service/interfaces/cs_widget_handler.erl"
    ),
    ?assertEqual(1, list_to_integer(string:trim(Src))),
    ok.

%% ---------------------------------------------------------------- helpers

sample_bytes() ->
    %% 3 块（5 字节块）+ 尾块
    <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13>>.

collect(ReadFun, Off, Len, Acc) ->
    case ReadFun(Off, Len) of
        {ok, Bin} -> collect(ReadFun, Off + byte_size(Bin), Len, <<Acc/binary, Bin/binary>>);
        eof -> Acc;
        {error, R} -> erlang:error({read_failed, R})
    end.
