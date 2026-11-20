CREATE INDEX i_c2c_action_receipt_accepted_revoke
    ON public.c2c_action_receipt (original_msg_id)
    WHERE action = 'revoke';
