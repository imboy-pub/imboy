-module(attachment_storage_gateway_handler).

%%%
% V5.5 受控存储网关（02 §14 写字节边界，W2-01 Garage 路径）
%
% 新 V5 会话的 put 能力指向本网关而非可任意覆写的 Garage 直传 URL：
%   * 网关在既有限额内把实际内容写入独占 0600 临时文件，计算
%     SHA256/size，校验登记时冻结的原声明；
%   * 取父/I/session 锁，将 upload_content 从 NULL 一次固定为
%     {version,sha256,size_bytes,mime} 并 COMMIT，再发送同一已校验
%     文件的内容（pin COMMIT 之后才 PUT 对象存储）；
%   * 同 key 不同字节/size/mime 返回 409 且 provider PUT=0；
%   * 失败只清本请求临时文件，不补偿删除对象；
%   * 旧客户端无 JWT/Cookie 的直传 PUT 不受影响（兼容保留）。
%
% W4-03 将本网关迁移到统一 driver 边界并扩展 Disk 同 I OS 锁。
%%%

-export([init/2]).
-export([put/3]).

-include("log.hrl").
-include("error_code.hrl").

-define(CHUNK, 65536).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            put -> put(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% @doc PUT /api/v1/attachment/gateway/{session_id}
%% 网关能力使用专用 upload audience 的短期 token（imboy_jwt），
%% 不是读或 confirm 授权；body 为原始字节流。
-spec put(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
put(<<"PUT">>, Req0, _State) ->
    case gateway_session(Req0) of
        {ok, #{key := Key, mime := DeclMime}} ->
            receive_body(Req0, Key, DeclMime);
        {error, bad_token} ->
            cowboy_req:reply(403, #{}, <<"invalid gateway capability">>, Req0);
        {error, expired} ->
            cowboy_req:reply(410, #{}, <<"capability expired">>, Req0)
    end;
put(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% 网关能力 token：JWT audience=upload，claim 携带 session key 与声明 mime。
gateway_session(Req0) ->
    Token = cowboy_req:header(<<"x-upload-capability">>, Req0),
    case Token of
        undefined ->
            case cowboy_req:parse_qs(Req0) of
                Qs -> token_check(proplists:get_value(<<"capability">>, Qs))
            end;
        _ ->
            token_check(Token)
    end.

token_check(undefined) ->
    {error, bad_token};
token_check(Token) ->
    case imboy_jwt:verify_upload_capability(Token) of
        {ok, #{<<"key">> := Key, <<"mime">> := Mime, <<"exp">> := Exp}} ->
            case erlang:system_time(second) < Exp of
                true -> {ok, #{key => Key, mime => Mime}};
                false -> {error, expired}
            end;
        _ ->
            {error, bad_token}
    end.

%% 流式收体 → 0600 临时文件 → hash/size → 校验声明 → pin（受限函数，
%% NULL 一次固定）→ COMMIT 后转发同一文件字节到 Garage。
receive_body(Req0, Key, DeclMime) ->
    TmpDir = tmp_dir(),
    ok = filelib:ensure_dir(TmpDir ++ "/"),
    Tmp =
        TmpDir ++ "/" ++ binary_to_list(Key) ++ "." ++
            integer_to_list(erlang:unique_integer([positive])),
    case read_body_to_file(Req0, Tmp, 0) of
        {ok, Size} ->
            try
                serve_pinned(Req0, Key, DeclMime, Tmp, Size)
            after
                _ = file:delete(Tmp)
            end;
        {error, too_large} ->
            _ = file:delete(Tmp),
            cowboy_req:reply(413, #{}, <<"payload exceeds limit">>, Req0);
        {error, _} ->
            _ = file:delete(Tmp),
            cowboy_req:reply(500, #{}, <<"body receive failed">>, Req0)
    end.

read_body_to_file(Req0, Tmp, Size) ->
    Max = max_bytes(),
    {ok, Fh} = file:open(Tmp, [write, exclusive, raw, binary]),
    try
        read_loop(Req0, Fh, Size, Max)
    after
        file:close(Fh)
    end.

read_loop(Req0, Fh, Size, Max) ->
    case cowboy_req:read_body(Req0, #{length => ?CHUNK, period => 5000}) of
        {ok, Data, Req1} ->
            N = Size + byte_size(Data),
            case N > Max of
                true ->
                    {error, too_large};
                false ->
                    ok = file:write(Fh, Data),
                    {ok, Req1, N}
            end;
        {more, Data, Req1} ->
            N = Size + byte_size(Data),
            case N > Max of
                true ->
                    {error, too_large};
                false ->
                    ok = file:write(Fh, Data),
                    read_loop(Req1, Fh, N, Max)
            end;
        {error, _} = E ->
            E
    end.

serve_pinned(Req0, Key, DeclMime, Tmp, Size) ->
    case file_sha256(Tmp) of
        {ok, Sha256} ->
            Receipt = #{
                <<"version">> => 1,
                <<"sha256">> => Sha256,
                <<"size_bytes">> => integer_to_binary(Size),
                <<"mime">> => DeclMime
            },
            case attach_pending_repo:register_content_pin(Key, Receipt) of
                ok ->
                    %% pin 已 COMMIT；转发同一已校验文件（不重新读可替换路径）
                    Bucket = elib_oss:get_bucket(<<"private">>),
                    case elib_oss:put_object_from_file(Bucket, Key, Tmp, DeclMime) of
                        ok ->
                            cowboy_req:reply(
                                200,
                                #{
                                    <<"content-type">> => <<"application/json">>
                                },
                                elib_json:encode(#{
                                    <<"pinned">> => true,
                                    <<"sha256">> => Sha256,
                                    <<"size">> => Size
                                }),
                                Req0
                            );
                        {error, put_failed} ->
                            %% 对象存储 PUT 失败：pin 保留，同 receipt 重试合法
                            cowboy_req:reply(
                                502, #{}, <<"object store put failed, retry same content">>, Req0
                            )
                    end;
                {error, content_mismatch} ->
                    %% 同 key 不同字节 —— 409，provider PUT=0
                    cowboy_req:reply(
                        409, #{}, <<"content already pinned to different receipt">>, Req0
                    );
                {error, not_found} ->
                    cowboy_req:reply(404, #{}, <<"unknown session">>, Req0)
            end;
        {error, _} ->
            cowboy_req:reply(500, #{}, <<"hash failed">>, Req0)
    end.

file_sha256(Path) ->
    try
        {ok, Data} = file:read_file(Path),
        Digest = crypto:hash(sha256, Data),
        {ok, iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Digest)])}
    catch
        _:_ -> {error, hash_failed}
    end.

tmp_dir() ->
    case os:getenv("IMBOY_V5_GATEWAY_TMP") of
        false -> "/tmp/imboy-v5-gateway";
        D -> D
    end.

max_bytes() ->
    elib_oss:max_file_size().
