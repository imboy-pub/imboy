-module(attach_handler).

%%%
% 附件 presigned URL 生成接口
% 供 Flutter 客户端获取 Garage S3 直传 URL
%
% GET /v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg&expires=3600
% 响应: { put_url, object_key, expires_at }
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").
-include("common.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            presign -> presign(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

-spec presign(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
presign(<<"GET">>, Req0, _State) ->
    Qs = cowboy_req:parse_qs(Req0),
    FileName = proplists:get_value(<<"filename">>, Qs, <<"file">>),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, <<"application/octet-stream">>),
    ExpiresRaw = proplists:get_value(<<"expires">>, Qs, <<"3600">>),
    Expires = min(86400, max(60, safe_int(ExpiresRaw, 3600))),
    case elib_oss:validate_file_type(MimeType) of
        false ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
        true ->
            FileId = elib_oss:generate_file_id(),
            SafeName = filename:basename(FileName),
            ObjectKey = <<FileId/binary, "/", SafeName/binary>>,
            PutUrl = elib_oss:presign_put_for_key(ObjectKey, MimeType, Expires),
            ExpiresAt = erlang:system_time(second) + Expires,
            elib_response:success(
                Req0,
                #{
                    <<"put_url">> => PutUrl,
                    <<"object_key">> => ObjectKey,
                    <<"expires_at">> => ExpiresAt
                },
                "success."
            )
    end;
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 安全解析整数，非法输入回退默认值（避免 binary_to_integer badarg 崩溃）
-spec safe_int(binary() | integer(), integer()) -> integer().
safe_int(V, _Default) when is_integer(V) ->
    V;
safe_int(V, Default) when is_binary(V) ->
    try
        binary_to_integer(V)
    catch
        _:_ -> Default
    end;
safe_int(_, Default) ->
    Default.
