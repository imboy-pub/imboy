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
-export([serve_pinned/6]).

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
        {ok, #{key := Key, mime := DeclMime, exp := Exp}} ->
            receive_body(Req0, Key, DeclMime, Exp);
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
                true -> {ok, #{key => Key, mime => Mime, exp => Exp}};
                false -> {error, expired}
            end;
        _ ->
            {error, bad_token}
    end.

%% 流式收体 → 0600 临时文件 → hash/size → 校验声明 → pin（受限函数，
%% NULL 一次固定）→ COMMIT 后转发同一文件字节到 Garage。
%%
%% 转存签名用冻结 Context（02 §15）：expires_at 取 capability token 的
%% exp（签发时自登记冻结值），issued_at 为本请求转存时刻——由本层显式
%% 构造传入，elib_oss/elib_s3_sign 不读取当前时钟重新计时。
receive_body(Req0, Key, DeclMime, CapabilityExp) ->
    TmpDir = tmp_dir(),
    ok = filelib:ensure_dir(TmpDir ++ "/"),
    %% Tmp 是平坦目录下的独立文件：ObjectKey 的 '/' 不构成子目录结构
    SafeKey = lists:map(
        fun
            ($/) -> $_;
            (C) -> C
        end,
        binary_to_list(Key)
    ),
    Tmp = TmpDir ++ "/" ++ SafeKey ++ "." ++ integer_to_list(erlang:unique_integer([positive])),
    try
        receive_to_file(Req0, Key, DeclMime, CapabilityExp, Tmp)
    catch
        _:_ -> cowboy_req:reply(500, #{}, <<"body receive failed">>, Req0)
    after
        _ = file:delete(Tmp)
    end.

receive_to_file(Req0, Key, DeclMime, CapabilityExp, Tmp) ->
    case read_body_to_file(Req0, Tmp, 0) of
        {ok, Size} ->
            serve_pinned(Req0, Key, DeclMime, CapabilityExp, Tmp, Size);
        {error, too_large} ->
            cowboy_req:reply(413, #{}, <<"payload exceeds limit">>, Req0);
        {error, _} ->
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
        %% {ok, _, _}：body 收完；{more, _, _}：还有分片，继续累计
        {Fin, Data, Req1} when Fin =:= ok; Fin =:= more ->
            N = Size + byte_size(Data),
            case N > Max of
                true ->
                    {error, too_large};
                false ->
                    ok = file:write(Fh, Data),
                    case Fin of
                        ok -> {ok, N};
                        more -> read_loop(Req1, Fh, N, Max)
                    end
            end;
        {error, _} = E ->
            E
    end.

serve_pinned(Req0, Key, DeclMime, CapabilityExp, Tmp, Size) ->
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
                    %% pin 已 COMMIT；转发同一已校验文件（不重新读可替换路径）。
                    %% 签名 Context 冻结：expires_at=capability exp（unix 秒），
                    %% issued_at=转存时刻；provider 端不重新计时。
                    Ctx = #{
                        issued_at => unix_to_datetime(erlang:system_time(second)),
                        expires_at => unix_to_datetime(CapabilityExp)
                    },
                    case provider_put_ctx(Key, Tmp, DeclMime, Ctx) of
                        ok ->
                            cowboy_req:reply(
                                200,
                                #{
                                    <<"content-type">> => <<"application/json">>
                                },
                                jsone:encode(#{
                                    <<"pinned">> => true,
                                    <<"sha256">> => Sha256,
                                    <<"size">> => Size
                                }),
                                Req0
                            );
                        {error,historical_location_unverified}->
                            cowboy_req:reply(409,#{<<"content-type">>=><<"application/json">>},
                                jsone:encode(#{error=><<"historical_location_unverified">>,retry_action=><<"reupload_new_session">>}),Req0);
                        {error, invalid_signature_context} ->
                            %% capability 过短/异常：不 PUT，pin 保留供同 receipt 重试
                            cowboy_req:reply(
                                410, #{}, <<"capability window invalid">>, Req0
                            );
                        {error, _put_failed} ->
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
                    cowboy_req:reply(404, #{}, <<"unknown session">>, Req0);
                {error, _} ->
                    cowboy_req:reply(500, #{}, <<"content pin failed">>, Req0)
            end;
        {error, _} ->
            cowboy_req:reply(500, #{}, <<"hash failed">>, Req0)
    end.

%% provider PUT 容错包装：elib_oss 抛出异常按 PUT 失败处理（pin 保留），
%% invalid_signature_context 透传给上层区分 410。
provider_put_ctx(Key, Tmp, DeclMime, Ctx) ->
    try
        case attachment_storage:session_location(Key) of
            {ok,legacy_unknown} -> {error,historical_location_unverified};
            {ok,#{driver := garage}=I} ->
                attachment_storage_garage:put_snapshot(I,{file,Tmp},DeclMime,Ctx);
            {ok,#{driver := disk}=I} ->
                Expiry=calendar:datetime_to_gregorian_seconds(maps:get(expires_at,Ctx))-62167219200,
                attachment_storage_disk:put_snapshot(I,{file,Tmp},Expiry);
            {error,_}=E -> E;
            _ -> {error,unsupported_driver}
        end
    catch _:_ -> {error,put_failed} end.

unix_to_datetime(UnixSec) ->
    calendar:gregorian_seconds_to_datetime(UnixSec + 62167219200).

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
