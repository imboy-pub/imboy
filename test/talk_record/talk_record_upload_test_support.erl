-module(talk_record_upload_test_support).
-export([configure/0, bytes/1, upload/2]).

%% Explicit isolated root required; never fall back to a previous run's root.
configure() ->
    Root = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    true = is_list(Root) andalso filename:pathtype(Root) =:= absolute,
    ok = filelib:ensure_dir(filename:join(Root, "x")),
    ok = application:set_env(imboy, jwt_key, <<"synthetic-talk-upload-test-key-0123456789">>),
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        upload_namespaces => #{<<"talk_record">> => <<"talk-upload-test">>},
        roots => #{
            talk_upload_test => #{
                driver => <<"disk">>,
                namespace => <<"talk-upload-test">>,
                container => <<"capture">>,
                endpoint => <<"file://", (list_to_binary(Root))/binary>>
            }
        }
    }).

bytes(Seq) -> binary:copy(<<(Seq rem 256)>>, 4096).
upload(Uid, Part) ->
    Ref = maps:get(<<"upload_ref">>, Part),
    Key = maps:get(<<"object_key">>, Ref),
    Root = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    Name = filename:join(Root, "spool-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Bytes = bytes(maps:get(<<"seq">>, Part)),
    ok = file:write_file(Name, Bytes),
    try
        {ok, _} = attachment_upload_logic:upload_multipart(
            Uid, Key, <<"audio/aac">>, Name, byte_size(Bytes)
        ),
        ok
    after
        file:delete(Name)
    end.
