-module (test_repo).


-export ([create_user_test/3]).
-export ([create_friend_test/1, create_friend_test/2]).

-export ([test_use_catch/0, test_user_try_catch/0]).

% test_repo:create_user_test("13", 1, 10).
% test_repo:create_user_test("15", 1, 500000).
create_user_test(_Prefix, _Num, Limit) when Limit < 1 ->
    error;
create_user_test(_Prefix, Num, Limit) when Num > Limit->
    ok;
create_user_test(Prefix, Num, Limit) ->
    % password is admin888
    Mob = lists:concat([Prefix, io_lib:format("~9.10.0B",[Num])]),
    MobBin = list_to_binary(Mob),
    Username = MobBin,
    Sql = <<"INSERT INTO `user` (`level_id`, `password`, `account`, `mobile`, `email`, `experience`, `gender`, `avatar`, `sign`, `login_count`, `last_login_ip`, `last_login_at`, `ref_user_id`, `status`, `created_at`, `reg_ip`, `reg_client`) VALUES (0, 'gmqrFZenECbzbUramWn1X108J0XZMSwLIynjJDI2jqal9oW10Bd6kOqrIZkR1KxuKXsN0UmXHKQwtL0u6jDRRA==', '", Username/binary ,"', '", MobBin/binary ,"', NULL, 0, 'hide', '', '', 0, '', NULL, 0, 1, NULL, NULL, NULL)">>,
    % ?LOG(Sql),
    imboy_db:query(Sql, no_params),
    create_user_test(Prefix, Num + 1, Limit).

% delete from user_friend where id > 7
% test_repo:create_friend_test(2, 3).
% test_repo:create_friend_test(13335).
create_friend_test(FromId) when FromId > 500000 ->
    ok;
create_friend_test(FromId)->
    [create_friend_test(FromId, ToId) || ToId <- lists:seq(FromId - 100, FromId)],
    create_friend_test(FromId + 1).

create_friend_test(FromId, ToId) when FromId == ToId ->
    ok;
% test_repo:create_friend_test(513238, 1).
% test_repo:create_friend_test(1, 513238).
% test_repo:create_friend_test(513238, 513238).
create_friend_test(FromId, ToId) when FromId > 513237; ToId > 513237 ->
    ok;
create_friend_test(FromId, ToId) ->
    Sql = <<"INSERT INTO `user_friend` (`from_user_id`, `to_user_id`, `status`, `created_at`) VALUES (?, ?, 1, ?)">>,
    imboy_db:query(Sql, [FromId, ToId, imboy_func:milliseconds()]).


generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> error(a);
generate_exception(4) -> exit(a);
generate_exception(5) -> {'EXIT', a}.


% test_repo:test_use_catch()
test_use_catch() ->
    [{I, catch generate_exception(I)} || I <- lists:seq(1, 5)].

% test_repo:test_user_try_catch()
test_user_try_catch() ->
  [begin
    try generate_exception(I) of
        NormalRes ->
            {I, normal, NormalRes}
    catch
        ErrorType : Error ->
            {I, exception, ErrorType, Error}
    end
  end || I <- lists:seq(1, 5)].
