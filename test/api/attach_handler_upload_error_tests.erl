-module(attach_handler_upload_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% Exercise the public handler, real multipart parser/private file lifecycle and
%% real response envelope. Only transport, authentication and logic are mocked.
%% 覆盖公开 handler、真实 multipart/私有文件清理和响应构造，不启动数据库。
-define(KEY, <<"u7/file/20260911/file_1/x.jpg">>).

upload_error_status_test_() ->
    [{atom_to_list(Reason), fun() -> assert_upload_error(Reason, 409, 409) end}
     || Reason <- [content_mismatch, tombstone, session_not_renewable, location_changed,
                   location_unverified, scope_conflict, historical_location_unverified]] ++
    [{"original expiry", fun() -> assert_upload_error(invalid_signature_context, 410, 410) end},
     {"DB error retains envelope", fun() -> assert_upload_error({db_error, pool_down}, 200, 500) end},
     {"storage error retains envelope", fun() ->
         assert_upload_error({storage_error, timeout}, 200, 500) end},
     {"legacy MIME error retains envelope", fun() ->
         assert_upload_error(invalid_file_type, 200, 400) end},
     {"legacy size error retains envelope", fun() ->
         assert_upload_error(file_too_large, 200, 413) end}].

assert_upload_error(Reason, Status, Code) ->
    Modules = [cowboy_req, auth_ds, attachment_upload_logic],
    meck:new(Modules, [passthrough, no_link]),
    erase(multipart_handler_path),
    try
        Req = #{},
        meck:expect(auth_ds, current_uid, fun(_) -> 7 end),
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(cowboy_req, parse_qs, fun(_) ->
            [{<<"object_key">>, ?KEY}, {<<"mime_type">>, <<"image/jpeg">>}]
        end),
        meck:expect(cowboy_req, header, fun(<<"content-type">>, _) ->
            <<"multipart/form-data; boundary=multipart-boundary-test">>
        end),
        meck:expect(cowboy_req, read_body, fun(R, _) ->
            {ok, multipart_body(), R}
        end),
        meck:expect(cowboy_req, reply, fun(HttpStatus, Headers, Body, R) ->
            R#{test_status => HttpStatus, test_headers => Headers, test_body => Body}
        end),
        meck:expect(attachment_upload_logic, upload_multipart,
            fun(7, ?KEY, <<"image/jpeg">>, Path, 8) ->
                ?assertEqual({ok, <<"verified">>}, file:read_file(Path)),
                {ok, #file_info{mode = FileMode}} = file:read_file_info(Path),
                {ok, #file_info{mode = DirMode}} = file:read_file_info(filename:dirname(Path)),
                ?assertEqual(8#600, FileMode band 8#777),
                ?assertEqual(8#700, DirMode band 8#777),
                put(multipart_handler_path, Path),
                {error, Reason}
            end),
        {ok, Result, #{}} = attach_handler:init(Req, #{action => upload}),
        ?assertEqual(Status, maps:get(test_status, Result)),
        Envelope = jsone:decode(maps:get(test_body, Result)),
        ?assertEqual(Code, maps:get(<<"code">>, Envelope)),
        case Reason of
            historical_location_unverified ->
                ?assertEqual(#{<<"retry_action">> => <<"reupload_new_session">>},
                    maps:get(<<"payload">>, Envelope));
            _ -> ok
        end,
        Path = get(multipart_handler_path),
        ?assert(is_list(Path)),
        ?assertEqual({error, enoent}, file:read_file_info(Path)),
        ?assertEqual({error, enoent}, file:read_file_info(filename:dirname(Path))),
        ?assertEqual(1, meck:num_calls(attachment_upload_logic, upload_multipart, 5)),
        lists:foreach(fun(M) -> ?assert(meck:validate(M)) end, Modules)
    after
        erase(multipart_handler_path),
        meck:unload(Modules)
    end.

multipart_body() ->
    <<"--multipart-boundary-test\r\n"
      "Content-Disposition: form-data; name=\"file\"; filename=\"x.jpg\"\r\n"
      "Content-Type: image/jpeg\r\n\r\nverified\r\n"
      "--multipart-boundary-test--\r\n">>.
