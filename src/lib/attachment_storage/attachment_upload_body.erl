%% 有界 HTTP 收体；成功后由调用方在业务处理结束时关闭不透明 spool。
%% Bounded HTTP receive; the caller closes the returned spool after processing.
-module(attachment_upload_body).
-export([read/2]).

read(Req, Max) when is_integer(Max), Max > 0 ->
    case attachment_upload_spool:new() of
        {ok, Writer} ->
            try receive_chunks(Req, Writer, 0, Max) of
                {ok, _, _} = Result -> Result;
                Error -> attachment_upload_spool:close(Writer), Error
            catch
                Class:Reason:Stack ->
                    attachment_upload_spool:close(Writer),
                    erlang:raise(Class, Reason, Stack)
            end;
        {error, _} = Error -> Error
    end.

receive_chunks(Req, Writer, Size, Max) ->
    case cowboy_req:read_body(Req, #{length => 65536, period => 10000}) of
        {Fin, Data, NextReq} when Fin =:= ok; Fin =:= more ->
            NextSize = Size + byte_size(Data),
            case NextSize =< Max of
                false -> {error, {invalid_param, payload}};
                true ->
                    case attachment_upload_spool:append(Writer, Data) of
                        {ok, NextWriter} when Fin =:= more ->
                            receive_chunks(NextReq, NextWriter, NextSize, Max);
                        {ok, NextWriter} ->
                            case attachment_upload_spool:seal(NextWriter) of
                                {ok, Source} -> {ok, Source, NextReq};
                                Error -> Error
                            end;
                        Error -> Error
                    end
            end;
        {error, _} = Error -> Error
    end.
