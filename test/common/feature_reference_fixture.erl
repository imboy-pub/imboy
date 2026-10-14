-module(feature_reference_fixture).
-export([refs/1]).

%% 模块归属以源码路径判定；未知纵切前缀仍拒绝，避免拼写错误漏检。
refs(Source) ->
    [Root | _] = [D || D <- ["src", "../src", "../../src"], filelib:is_dir(D)],
    Index = maps:from_list([
        {
            list_to_binary(filename:basename(Path, ".erl")),
            lists:prefix(filename:join(Root, "features") ++ "/", Path)
        }
     || Path <- filelib:wildcard(Root ++ "/**/*.erl")
    ]),
    case re:run(Source, "\\b([a-z][a-z0-9_]*)\\b", [global, {capture, [1], binary}]) of
        {match, Matches} ->
            lists:usort([Name || [Name] <- Matches, is_feature(Name, Index)]);
        nomatch ->
            []
    end.

is_feature(Name, Index) ->
    case maps:find(Name, Index) of
        {ok, Feature} ->
            Feature;
        error ->
            case re:run(Name, "^(enterprise_|customer_service_|cs_|eb_)[a-z0-9_]+$") of
                {match, _} -> true;
                nomatch -> false
            end
    end.
