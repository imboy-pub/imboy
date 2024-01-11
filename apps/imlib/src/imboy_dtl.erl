-module(imboy_dtl).

% -export([template/2]).
-export([imadm_param/1]).
-export([template/3]).


% imboy_dtl:template(login_dtl, [], imadm).
-spec template(atom(), list(), atom()) -> binary().
template(Name, Vars, AppName) ->
    Path = imboy_cnv:implode("", [code:priv_dir(AppName), "/template/", Name, ".html"]),
    erlydtl:compile(binary_to_list(Path), Name),
    Name:render(Vars).


imadm_param(State) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    Nickname = case maps:get(adm_user_id, State, 0) of
        AdmUserId when AdmUserId > 0 ->
            Key = {adm_user_sample, AdmUserId},
            {true, {_, Nickname0}} = adm_user_logic:find(AdmUserId, <<"id,nickname">>, Key),
            Nickname0;
        _ ->
            <<>>
    end,
    [
         {system_name, "IMBoy Admin System"}
         , {adm_nickname, Nickname}
    ].


