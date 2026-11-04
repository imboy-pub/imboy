-module(imboy_quality_garage_cleanup).
-include_lib("kernel/include/file.hrl").
-export([cleanup/2, verify/1]).

cleanup(
    #{dir := Dir, dir_identity := Identity, dir_token := Token} = G,
    #{exit := ExitCode, cleanup := retained, receipt := Receipt}
) when is_reference(Receipt) ->
    case get({?MODULE, Receipt}) of
        {Dir, Identity, Token, ExitCode} ->
            try
                ok = verify(G),
                case file:del_dir_r(Dir) of
                    ok ->
                        erase({?MODULE, Receipt}),
                        ok;
                    {error, _} = Error ->
                        Error
                end
            catch
                _:_ -> {error, ownership_not_verified}
            end;
        _ ->
            {error, stop_not_verified}
    end;
cleanup(_, _) ->
    {error, stop_not_verified}.

verify(#{dir := Dir, dir_identity := Identity, dir_token := Token}) ->
    Identity = imboy_quality_resources:directory_identity(Dir),
    Marker = filename:join(Dir, ".imboy-quality-owner"),
    {ok, #file_info{type = regular, links = 1}} = file:read_link_info(Marker),
    {ok, Token} = file:read_file(Marker),
    ok.
