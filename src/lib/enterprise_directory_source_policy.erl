-module(enterprise_directory_source_policy).
-moduledoc "Directory sources may explicitly enable OA identity capabilities; bootstrap never grants those capabilities.".
-export([allowed/1, directory_scopes/1]).

allowed(Scopes) when is_list(Scopes) ->
    lists:member(<<"identities:write">>, Scopes) andalso
        Scopes --
            [
                <<"identities:read">>,
                <<"identities:write">>,
                <<"members:write">>,
                <<"application:read">>,
                <<"oa:login:issue">>,
                <<"sso:exchange">>
            ] =:= [];
allowed(_) ->
    false.

directory_scopes(Scopes) ->
    [
        Scope
     || Scope <- Scopes,
        lists:member(Scope, [<<"identities:read">>, <<"identities:write">>])
    ].
