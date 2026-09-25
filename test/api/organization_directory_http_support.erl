%%% @doc INT-BE-04：Human Organization Directory 真实 HTTP/JWT/PG 闭环的
%%% test-only harness（不进任何 release）。
%%%
%%% intbe02_http_support 同款三件套（无新发明）：
%%%   1. **disposable PG**：`inttest_marker_db:provision`（env 前缀
%%%      INTBE04_INTTEST；一次性 marker 库 + 全链迁移，与共享库隔离）；
%%%   2. **pooler `pgsql` 池**指向 marker 库：被测 handler 内 `elib_pg`
%%%      （池化路径）与 organization_directory_fixture 的种子 exec 全部
%%%      落到同一 marker 库——真库真事务；
%%%   3. **真 Cowboy listener**：`imboy_router:get_routes()` 全量 dispatch
%%%      + 与 imboy_app 同序的中间件链（auth_middleware 含 Human JWT 门），
%%%      HTTP 层零 mock、认证链零 mock——Human token 由
%%%      `token_ds:encrypt_token/1` 对合成 user 真签发（HS256 测试密钥
%%%      签发/校验同进程自洽；legacy 空 did 形态，verify 侧无设备比对）。
%%%
%%% 合成租户由 `organization_directory_fixture:new_scope/0` 建立（随机
%%% TSID；场景矩阵：suspended/removed/outsider/archived org/跨 Org 隔离
%%% 全齐）；marker 库随 release 整库 DROP，不触碰共享库。
-module(organization_directory_http_support).

-export([
    setup_all/0,
    teardown_all/1,
    http/3,
    http/4,
    bearer/1,
    base/1,
    sql_exec/2,
    one/2
]).

-define(LISTENER, orgdir_http_listener).

-define(MIDDLEWARES, [
    cowboy_router,
    cors_middleware,
    security_headers_middleware,
    auth_middleware,
    feature_gate_middleware,
    throttle_middleware,
    cowboy_handler
]).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

-spec setup_all() -> map().
setup_all() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [group_info, group_member, enterprise_message, enterprise_audit_event, attachment]
    ),
    %% throttle rates 在 ensure_all_started 之前注入（成功路径会穿过
    %% throttle_middleware 的 api_per_ip；放大上限避免套件被限流误伤）。
    application:set_env(throttle, rates, [
        {api_per_user, 100000, per_minute},
        {api_per_ip, 100000, per_minute}
    ]),
    {ok, _} = application:ensure_all_started(throttle),
    catch throttle:setup(api_per_ip, 100000, per_minute),
    catch throttle:setup(api_per_user, 100000, per_minute),
    %% Human JWT 测试密钥（token_ds 签发 / auth_ds 校验同 env 自洽）。
    application:set_env(imboy, jwt_key, <<"orgdir_http_test_jwt_key_0123456789">>),
    %% CURSOR-V2 签名密钥（enterprise_cursor_v2:signing_key/0 读；缺失 → 503）。
    application:set_env(
        imboy, enterprise_internal_cursor_signing_key, <<"orgdir_cursor_key_0123456789abcdef">>
    ),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"INTBE04_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    ensure_pool(State),
    Scope =
        try
            organization_directory_fixture:new_scope()
        catch
            Class:Reason:Stack ->
                release_pool(),
                inttest_marker_db:release(State),
                erlang:raise(Class, {orgdir_seed_failed, Reason}, Stack)
        end,
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
    ),
    Port = ranch:get_port(?LISTENER),
    State#{scope => Scope, port => Port}.

-spec teardown_all(map()) -> ok.
teardown_all(State) ->
    try
        ok = cowboy:stop_listener(?LISTENER)
    catch
        _:_ -> ok
    end,
    release_pool(),
    _ = inttest_marker_db:release(State),
    application:unset_env(imboy, jwt_key),
    application:unset_env(imboy, enterprise_internal_cursor_signing_key),
    ok.

ensure_pool(State) ->
    Server = maps:get(server, State),
    Db = maps:get(db, State),
    #{host := Host, port := Port, username := User, password := Pass} = Server,
    ConnOpts = #{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        ssl => false,
        timeout => 10000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    },
    {ok, _} = application:ensure_all_started(pooler),
    catch pooler:rm_pool(pgsql),
    _ = pooler:new_pool(#{
        name => pgsql,
        max_count => 8,
        init_count => 2,
        start_mfa => {epgsql, connect, [ConnOpts]}
    }),
    ok.

release_pool() ->
    catch pooler:rm_pool(pgsql),
    ok.

%%%===================================================================
%%% HTTP（intbe02 同款裸 TCP：定长 body，返回 #{status, body, json}）
%%%===================================================================

base(State) ->
    <<"http://127.0.0.1:", (integer_to_binary(maps:get(port, State)))/binary>>.

%% Human JWT：token_ds 真签发（legacy 空 did；verify 侧无设备比对、
%% auth_session epoch 走 DB —— 合成 user 无 session 行 → epoch 回落放行）。
bearer(Uid) when is_integer(Uid) ->
    #{<<"authorization">> => <<"Bearer ", (token_ds:encrypt_token(Uid))/binary>>}.

-spec http(map(), binary(), map()) -> map().
http(State, UrlPath, Opts) ->
    http(State, <<"GET">>, UrlPath, Opts).

-spec http(map(), binary(), binary(), map()) -> map().
http(State, Method, UrlPath, Opts) ->
    Body = maps:get(body, Opts, <<>>),
    Headers = maps:merge(
        #{
            <<"host">> => <<"localhost">>,
            <<"connection">> => <<"close">>,
            <<"content-type">> => <<"application/json">>,
            <<"content-length">> => integer_to_binary(byte_size(Body))
        },
        maps:get(headers, Opts, #{})
    ),
    HeaderBin = iolist_to_binary([[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- maps:to_list(Headers)]),
    Req = iolist_to_binary([
        Method, <<" ">>, UrlPath, <<" HTTP/1.1\r\n">>, HeaderBin, <<"\r\n">>, Body
    ]),
    {ok, Socket} = gen_tcp:connect(
        {127, 0, 0, 1}, maps:get(port, State), [binary, {active, false}], 15000
    ),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket, []),
    ok = gen_tcp:close(Socket),
    Parsed = parse(Raw),
    Body0 = maps:get(body, Parsed),
    Json =
        case Body0 of
            <<>> ->
                undefined;
            _ ->
                try
                    jsx:decode(Body0, [return_maps])
                catch
                    _:_ -> undefined
                end
        end,
    Parsed#{json => Json}.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 15000) of
        {ok, Data} -> recv_all(Socket, [Data | Acc]);
        {error, closed} -> iolist_to_binary(lists:reverse(Acc));
        {error, _Timeout} -> iolist_to_binary(lists:reverse(Acc))
    end.

parse(Raw) ->
    case binary:split(Raw, <<"\r\n\r\n">>) of
        [Head, Body] ->
            [StatusLine | HeaderLines] = binary:split(Head, <<"\r\n">>, [global]),
            [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
            Headers = headers(HeaderLines),
            #{
                status => binary_to_integer(StatusBin),
                headers => Headers,
                body => decode_chunked(Body, Headers),
                raw => Raw
            };
        [_Only] ->
            #{status => 0, headers => #{}, body => <<>>, raw => Raw}
    end.

headers(Lines) ->
    maps:from_list([
        {string:lowercase(K), V}
     || Line <- Lines,
        [K, V] <- [binary:split(Line, <<": ">>)],
        K =/= <<>>
    ]).

decode_chunked(Body, #{<<"transfer-encoding">> := <<"chunked">>}) ->
    chunked(Body, []);
decode_chunked(Body, _Headers) ->
    Body.

chunked(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
chunked(Bin, Acc) ->
    case binary:split(Bin, <<"\r\n">>) of
        [SizeBin, Rest] ->
            case catch binary_to_integer(SizeBin, 16) of
                0 ->
                    iolist_to_binary(lists:reverse(Acc));
                Size when is_integer(Size), Size > 0 ->
                    <<Chunk:Size/binary, _CRLF:2/binary, Tail/binary>> = Rest,
                    chunked(Tail, [Chunk | Acc]);
                _ ->
                    iolist_to_binary(lists:reverse(Acc))
            end;
        _Incomplete ->
            iolist_to_binary(lists:reverse(Acc))
    end.

%%%===================================================================
%%% SQL（fixture 同款 elib_pg 池化路径；断言用只读）
%%%===================================================================

sql_exec(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

one(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} ->
            case maps:values(Row) of
                [Value | _] -> Value;
                [] -> undefined
            end;
        {ok, []} ->
            undefined;
        {error, Reason} ->
            erlang:error({orgdir_http_sql_failed, Sql, Reason})
    end.
