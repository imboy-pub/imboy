-module(imboy_quality_state_permissions_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

state_receipt_is_private_test() ->
    Dir = filename:join(tmp_root(), "imboy-state-private-" ++ os:getpid()),
    Path = filename:join(Dir, "STATE.json"),
    ok = file:make_dir(Dir),
    try
        {ok, _} = imboy_quality_state:write(Path, #{owner => #{token => <<"synthetic">>}}),
        {ok, Info} = file:read_link_info(Path),
        ?assertEqual(8#600, Info#file_info.mode band 8#777)
    after
        file:delete(Path),
        file:del_dir(Dir)
    end.

%% Resolve the macOS /tmp symlink while retaining the real Linux /tmp path.
tmp_root() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.
