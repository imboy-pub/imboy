-module(enterprise_internal_signature_repo).
-moduledoc "Cross-node, atomic single-use request nonces. No process-local replay cache.".
-export([reserve_tx/4]).

reserve_tx(Conn, CredentialId, Nonce, Seconds) ->
    %% Bound cleanup work; a nonce is retained beyond the full acceptance window.
    %% The uniqueness constraint serializes concurrent copies across all nodes.
    Cleanup = <<
        "DELETE FROM enterprise_internal_request_nonce WHERE ctid IN "
        "(SELECT ctid FROM enterprise_internal_request_nonce "
        "WHERE expires_at<clock_timestamp() LIMIT 1000)"
    >>,
    case elib_pg:execute(Conn, Cleanup, []) of
        {ok, _} -> reserve_current_tx(Conn, CredentialId, Nonce, Seconds);
        {error, _} -> {error, security_gate_closed}
    end.

reserve_current_tx(Conn, CredentialId, Nonce, Seconds) ->
    Sql = <<
        "INSERT INTO enterprise_internal_request_nonce(credential_id,nonce,expires_at) "
        "SELECT $1,$2,to_timestamp($3::bigint+301) "
        "WHERE abs(extract(epoch FROM clock_timestamp())-$3::bigint)<=300 "
        "ON CONFLICT DO NOTHING RETURNING credential_id"
    >>,
    case elib_pg:query(Conn, Sql, [CredentialId, Nonce, Seconds]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, invalid_credential};
        {error, _} -> {error, security_gate_closed}
    end.
