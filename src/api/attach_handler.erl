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
            confirm -> confirm(Method, Req0, State);
            view_url -> view_url(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% @doc GET /v1/attachment/presign?filename=x.jpg&mime_type=image/jpeg
%% 生成绑定当前 uid 的上传 presigned PUT URL
-spec presign(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
presign(<<"GET">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    FileName = proplists:get_value(<<"filename">>, Qs, <<"file">>),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, <<"application/octet-stream">>),
    case attach_logic:presign(Uid, FileName, MimeType) of
        {ok, Data} ->
            elib_response:success(Req0, Data, "success.");
        {error, invalid_file_type} ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST)
    end;
presign(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc POST /v1/attachment/confirm
%% body: { object_key, md5, mime_type, size }
%% 客户端 PUT 直传成功后回调，落库附件元数据
-spec confirm(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
confirm(<<"POST">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    ObjectKey = maps:get(<<"object_key">>, PostVals, <<>>),
    case attach_logic:confirm(Uid, ObjectKey, PostVals) of
        {ok, Data} ->
            elib_response:success(Req0, Data, "success.");
        {error, forbidden_key} ->
            elib_response:error(Req0, <<"非法对象归属"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid_key} ->
            elib_response:error(Req0, <<"非法对象键"/utf8>>, ?ERR_BAD_REQUEST);
        {error, object_not_found} ->
            elib_response:error(Req0, <<"对象不存在或未完成上传"/utf8>>, ?ERR_BAD_REQUEST);
        {error, file_too_large} ->
            elib_response:error(Req0, <<"文件超过大小限制"/utf8>>, ?ERR_BAD_REQUEST);
        {error, invalid_file_type} ->
            elib_response:error(Req0, <<"不支持的文件类型"/utf8>>, ?ERR_BAD_REQUEST);
        {error, _Reason} ->
            elib_response:error(Req0, <<"附件落库失败"/utf8>>, ?ERR_BAD_REQUEST)
    end;
confirm(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc GET /v1/attachment/view_url?object_key=xxx
%% 签发短时下载 presigned GET URL（替代 bucket 公开读）
-spec view_url(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
view_url(<<"GET">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"object_key">>, Qs, <<>>) of
        <<>> ->
            elib_response:error(Req0, <<"缺少 object_key"/utf8>>, ?ERR_BAD_REQUEST);
        ObjectKey ->
            case attach_logic:view_url(Uid, ObjectKey) of
                {ok, Url} ->
                    elib_response:success(Req0, #{<<"url">> => Url}, "success.");
                {error, forbidden} ->
                    %% fail-closed：非归属或归属校验失败，拒绝签发下载 URL
                    elib_response:error(Req0, <<"无权访问该附件"/utf8>>, ?ERR_BAD_REQUEST)
            end
    end;
view_url(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
