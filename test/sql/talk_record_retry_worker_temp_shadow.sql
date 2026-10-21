-- Each independent case must run in its own psql process. No worker SELECT grant.
-- Controller: run each sibling *_claim.sql / *_ack.sql / *_reaper.sql separately.
-- The warehouse runner reports each terminal, including mutant expected rejection.
\echo 'Use run_talk_record_retry_shadow_checks.py; no combined early-abort case.'
\quit 2
