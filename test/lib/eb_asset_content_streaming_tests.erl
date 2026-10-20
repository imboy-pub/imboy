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


streaming_test_() ->
    isolated_pg_fixture:test(fun streaming_fixture/0).

streaming_fixture() ->
    {setup, fun setup_database/0, fun cleanup_database/1, fun(Database) ->
    {foreach, fun() -> setup(Database) end, fun cleanup/1, [
        fun(S) -> fun() -> chunked_read_roundtrip(S) end end,
        fun(S) -> fun() -> head_gives_size_and_mime(S) end end,
        fun(S) -> fun() -> out_of_range_maps_to_eof(S) end end,
        fun(S) -> fun() -> storage_error_not_leaked(S) end end,
        fun(S) -> fun() -> deleted_asset_hidden(S) end end,
        fun(S) -> fun() -> no_store_header_present(S) end end
    ]}
    end}.

setup_database() ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    Original = #{apps => [A || {A, _, _} <- application:which_applications()],
        pooler_env => application:get_env(pooler, pools)},
    try
        Marker = inttest_marker_db:provision(#{env_prefix => <<"EB_STREAMING_INTTEST">>}),
        S = Marker#{original_runtime => Original},
        try
            application:set_env(pooler, pools, []),
            {ok, _} = application:ensure_all_started(pooler),
            {ok, _} = application:ensure_all_started(crypto),
            Opts = (maps:with([host, port, username, password], maps:get(server, S)))#{
                database => binary_to_list(maps:get(db, S)),
                codecs => [{epgsql_codec_rfc3339_bin, []}]},
            {ok, _} = pooler:new_pool(#{name => pgsql, init_count => 2, max_count => 4,
                start_mfa => {epgsql, connect, [Opts]}}),
            S
        catch Class0:Reason0:Stack0 ->
            CleanupResult = try cleanup_database(S)
                catch CleanupClass:CleanupReason -> {error, {CleanupClass, CleanupReason}} end,
            case CleanupResult of
                ok -> erlang:raise(Class0, Reason0, Stack0);
                _ -> error({fixture_setup_and_cleanup_failed,
                    {Class0, Reason0}, CleanupResult})
            end
        end
    catch Class:Reason:Stack ->
        restore_runtime(Original), erlang:raise(Class, Reason, Stack)
    end.

cleanup_database(S) ->
    %% Always attempt DB disposal, even when partial setup never made a pool.
    try
        PoolResult = try pooler:rm_pool(pgsql)
            catch PoolClass:PoolReason -> {error, {PoolClass, PoolReason}} end,
        DbResult = try inttest_marker_db:release(S)
            catch DbClass:DbReason -> {error, {DbClass, DbReason}} end,
        case {PoolResult, DbResult} of
            {ok, ok} -> ok;
            Other -> error({owned_fixture_cleanup_failed, Other})
        end
    after restore_runtime(maps:get(original_runtime, S))
    end.

restore_runtime(#{apps := OriginalApps, pooler_env := OriginalEnv}) ->
    NewApps = [A || {A, _, _} <- application:which_applications(),
        not lists:member(A, OriginalApps)],
    [application:stop(A) || A <- NewApps],
    case OriginalEnv of
        undefined -> application:unset_env(pooler, pools);
        {ok, Value} -> application:set_env(pooler, pools, Value)
    end,
    ok.

setup(_Database) ->
    Before = [{K, application:get_env(imboy, K)}
        || K <- [garage, attachment_storage_registry]],
    Base = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    EP = <<"http://127.0.0.1:39999">>,
    Ns = <<"dv1cs-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    S = #{before_env => Before, ns => Ns, ep => EP,
        uid => Base, org => Base + 1, ws => Base + 2, asset => Base + 3},
    try
        ok = application:set_env(imboy, garage,
            #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}),
        Registry = attachment_storage:registry(),
        Roots = maps:get(roots, Registry, #{}),
        ok = application:set_env(imboy, attachment_storage_registry, Registry#{
            roots => Roots#{Ns => #{driver => <<"garage">>, namespace => Ns,
                endpoint => EP, container => <<"b">>}}}),
        ok = elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES($1,$2,'','127.0.0.1','owned-streaming-fixture')">>,
                [Base, <<"streaming-", (integer_to_binary(Base))/binary>>]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO organization(id,name,owner_id) VALUES($1,'owned-streaming-fixture',$2)">>,
                [maps:get(org, S), Base]),
            {ok, _} = elib_pg:query(Conn,
                <<"INSERT INTO workspace(id,organization_id,name,owner_id) VALUES($1,$2,'owned-streaming-fixture',$3)">>,
                [maps:get(ws, S), maps:get(org, S), Base]),
            ok
        end),
        S
    catch Class:Reason:Stack ->
        cleanup(S), erlang:raise(Class, Reason, Stack)
    end.

cleanup(#{before_env := Before}) ->
    %% Rows live only until the enclosing marker DB is dropped. Never bypass
    %% bounded-purge authorization or suppress referential-integrity checks.
    [case Value of
        undefined -> application:unset_env(imboy, K);
        {ok, V} -> application:set_env(imboy, K, V)
     end || {K, Value} <- Before],
    ok.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

insert_eb_asset(S, ObjectKey, Mime) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO enterprise_asset (id, organization_id, workspace_id, object_key,"
            " object_hash, mime, size_bytes, status, key_version, version, uploaded_by_user_id,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,$2,$3,$4,$5,$6,12,'active',1,1,$10,"
            " 'garage',$9,'b',$7,$8)"
        >>,
        [maps:get(asset, S), maps:get(org, S), maps:get(ws, S), ObjectKey,
            hex64(maps:get(asset, S)), Mime, locator(S), ObjectKey, maps:get(ns, S), maps:get(uid, S)]
    ),
    ok.

asset_key(S, Suffix) ->
    iolist_to_binary(["enterprise/", integer_to_binary(maps:get(org, S)), "/",
        integer_to_binary(maps:get(ws, S)), "/", Suffix]).

locator(S) ->
    Ns = maps:get(ns, S),
    attachment_storage:locator_digest(Ns, #{
        driver => <<"garage">>, root_id => Ns, endpoint => maps:get(ep, S),
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

chunked_read_roundtrip(S) ->
    Bin = sample_bytes(),
    Key = asset_key(S, <<"a1">>),
    ok = insert_eb_asset(S, Key, <<"image/png">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin), content_type => <<"image/png">>}} end,
        fun() ->
            {ok, #{read_fun := ReadFun, size := Size}} =
                eb_asset_store:stream_content_chunked(maps:get(org, S), maps:get(ws, S), maps:get(asset, S)),
            ?assertEqual(byte_size(Bin), Size),
            %% 分块拼回 = 原字节
            Collected = collect(ReadFun, 0, 5, <<>>),
            ?assertEqual(Bin, Collected)
        end
    ),
    ok.

head_gives_size_and_mime(S) ->
    Bin = sample_bytes(),
    Key = asset_key(S, <<"a2">>),
    ok = insert_eb_asset(S, Key, <<"video/mp4">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin), content_type => <<"video/mp4">>}} end,
        fun() ->
            {ok, Info} = eb_asset_store:stream_content_chunked(maps:get(org, S), maps:get(ws, S), maps:get(asset, S)),
            %% HEAD 语义：size/mime 在不含正文的情况下可取（传输审计字段）
            ?assertEqual(byte_size(Bin), maps:get(size, Info)),
            ?assertEqual(<<"video/mp4">>, maps:get(mime, Info))
        end
    ),
    ok.

out_of_range_maps_to_eof(S) ->
    Bin = sample_bytes(),
    Key = asset_key(S, <<"a3">>),
    ok = insert_eb_asset(S, Key, <<"image/png">>),
    with_read_mocks(
        Bin,
        fun(_N, _C, _K) -> {ok, #{size => byte_size(Bin)}} end,
        fun() ->
            {ok, #{read_fun := ReadFun}} =
                eb_asset_store:stream_content_chunked(maps:get(org, S), maps:get(ws, S), maps:get(asset, S)),
            ?assertEqual(eof, ReadFun(byte_size(Bin) + 100, 16)),
            ok
        end
    ).

storage_error_not_leaked(S) ->
    Key = asset_key(S, <<"a4">>),
    ok = insert_eb_asset(S, Key, <<"image/png">>),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        head_bounded,
        3,
        fun(_N, _C, _K) -> {error, {storage_status, 500}} end
    ),
    try
        R = eb_asset_store:stream_content_chunked(maps:get(org, S), maps:get(ws, S), maps:get(asset, S)),
        %% 存储错误映射为 object_store 包装（不泄漏内部签名/URL，T33）
        ?assertMatch({error, {object_store, _}}, R)
    after
        meck:unload(attachment_storage_garage)
    end,
    ok.

deleted_asset_hidden(S) ->
    %% 删除态：not_found（无权限隐藏，T23 语义）
    ok = meck:new(eb_pg_asset_meta, [passthrough, no_link]),
    ok = meck:expect(eb_pg_asset_meta, fetch_asset, 3, {ok, #{status => deleted}}),
    try
        ?assertEqual(
            {error, not_found},
            eb_asset_store:stream_content_chunked(maps:get(org, S), maps:get(ws, S), maps:get(asset, S))
        )
    after
        meck:unload(eb_pg_asset_meta)
    end,
    ok.

no_store_header_present(_S) ->
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
