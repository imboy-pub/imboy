-module(talk_record_recovery_repo).
-moduledoc "创建告知事实的参数化持久化；事务由DS持有。".
-export([insert_notice_tx/2]).

insert_notice_tx(C, Params) ->
    Sql = <<
        "INSERT INTO talk_record_consent_event"
        " (id,record_id,actor_id,kind,policy_version,processing_mode,captured_at)"
        " VALUES($1,$2,$3,'recording_notice_ack',$4,$5,now())"
    >>,
    elib_pg:execute(C, Sql, Params).
