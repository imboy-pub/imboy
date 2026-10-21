"""Static invariants only, not PostgreSQL syntax, privilege or concurrency proof."""
from pathlib import Path
import unittest
ROOT=Path(__file__).resolve().parents[2]
SQL=(ROOT/'priv/migrations/00000221_talk_record_cleanup_retry.up.sql').read_text()

class CleanupRetryContractTests(unittest.TestCase):
    def test_budgets_agree(self):
        self.assertIn('q.attempts-q.attempt_base < 8',SQL)
        self.assertIn('WHERE attempts-attempt_base < 8',SQL)
        self.assertIn('SELECT job_state, attempts-attempt_base INTO',SQL)
        self.assertIn('q.attempts-q.attempt_base >= 8',SQL)
        projection=(ROOT/'src/repo/talk_record_delete_repo.erl').read_text()
        self.assertIn('q.attempts-q.attempt_base >= 8',projection)
        self.assertNotIn('SET attempts=0',SQL)
    def test_authority_and_protection(self):
        for term in ['NOLOGIN NOINHERIT NOSUPERUSER','pg_catalog,public,pg_temp',
                     "current_user<>'talk_record_retry_definer'",'FROM PUBLIC,v5_worker,v5_proof',
                     "role NOT IN ('owner','admin')", "r.owner_user_id<>p_actor",
                     'p_revision','p_generation','prior.payload_hash<>p_hash',
                     "i.action='delete_intent'",'storage_physical_key',
                     'fn_talk_record_war_blocks','user_collect','moment_post',
                     "pr.state IN ('prepared','running','unknown')",
                     "state<>'complete'",'jsonb_array_length(resources)=0']:
            self.assertIn(term,SQL)
        # Every eligibility pass completes before any queue is rearmed.
        self.assertLess(SQL.index('jsonb_array_length(resources)=0'),
                        SQL.index('SET retry_generation=retry_generation+1'))
        self.assertNotIn('set_config',SQL)
    def test_retained_history_and_atomic_job(self):
        for term in ['retry audit is immutable','PRIMARY KEY(record_id,actor_user_id,request_key)',
                     'UNIQUE(record_id,generation)','attempt_base=attempts',
                     "OLD.completed_at IS NOT NULL",'INSERT INTO public.talk_record_job',
                     'INSERT INTO public.talk_record_cleanup_retry']:
            self.assertIn(term,SQL)
        self.assertNotIn('DELETE FROM attachment_object_deletion',SQL)
    def test_no_temp_relation_resolution_in_protocol_or_rollback(self):
        import re
        down=(ROOT/'priv/migrations/00000221_talk_record_cleanup_retry.down.sql').read_text()
        for text in [SQL,down]:
            self.assertNotRegex(text,r'SET search_path\s*=\s*public\b')
            for table in ['attachment_object_deletion','attachment_lifecycle_v5_audit',
                          'workspace_attachment_retention','enterprise_attachment_retention']:
                self.assertNotRegex(text,r'(?:FROM|UPDATE|INTO)\s+'+table+r'\b')
            self.assertIn('CREATE OR REPLACE FUNCTION public.fn_talk_record_war_blocks',text)
            self.assertIn('FROM public.workspace_attachment_retention',text)
            self.assertIn('FROM public.enterprise_attachment_retention',text)

    def test_caps_and_post_share_readonly_plan(self):
        self.assertEqual(SQL.count('plan:=public.fn_talk_record_retry_plan(p_record);'),2)
        self.assertIn('protected_count:=protected_count+1; CONTINUE;',SQL)
        plan=SQL.split('CREATE FUNCTION public.fn_talk_record_retry_plan',1)[1].split('CREATE FUNCTION public.fn_talk_record_cleanup_retry_capability',1)[0]
        for write in ['UPDATE public.','INSERT INTO public.','FOR UPDATE']:
            self.assertNotIn(write,plan)
        self.assertIn("'partial_protection'",plan)
        self.assertIn('has_queue:=FOUND',plan)

    def test_route_and_request(self):
        doc=(ROOT/'api/paths/api/v1/talk-records/cleanup_retry.yaml').read_text()
        self.assertIn('required: [cleanup_generation]',doc)
        self.assertIn('minimum: 1',doc)
        self.assertIn("'202':",doc)
        self.assertIn('additionalProperties: false',doc)

if __name__=='__main__': unittest.main()
