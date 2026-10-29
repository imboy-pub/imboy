import hashlib
import json
from pathlib import Path
import tempfile
import sys
import time
import unittest
from datetime import datetime, timedelta, timezone
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from scripts.test.attachment_lifecycle_v5_pg_guard import authorized_database, owned_container


class PgGuardTests(unittest.TestCase):
    def test_database_requires_exact_receipt_entry_and_unambiguous_slot(self):
        allow = {'scratch_postgres': ['attv5_slot1_pool', 'attv5_chain_slot1_w403',
                                      'attv5_slot2_pool']}
        slots = ['slot1', 'slot2']
        self.assertEqual('attv5_slot1_pool', authorized_database(allow, slots, 'slot1', 'attv5_slot1_pool'))
        self.assertEqual('attv5_chain_slot1_w403', authorized_database(allow, slots, 'slot1', 'attv5_chain_slot1_w403'))
        for slot, database, receipt in [
            ('slot1', 'attv5_slot2_pool', allow),
            ('slot1', 'attv5_slot1_missing', allow),
            ('slot1', 'postgres', allow),
            ('slot1', 'attv5_slot1_pool;DROP', allow),
            ('slot1', 'attv5_slot1_pool', {'scratch_postgres': ['attv5_slot1_pool'] * 2}),
            ('slot1', 'attv5_slot1_pool', {'scratch_postgres': ['attv5_slot1_pool', None]}),
        ]:
            with self.subTest(slot=slot, database=database), self.assertRaises(ValueError):
                authorized_database(receipt, slots, slot, database)
        with self.assertRaises(ValueError):
            authorized_database({'scratch_postgres': ['attv5_slot1_x_pool']},
                                ['slot1', 'slot1_x'], 'slot1_x', 'attv5_slot1_x_pool')
        self.assertEqual('attv5_worker_a_pool', authorized_database(
            {'scratch_postgres': ['attv5_worker_a_pool']}, ['worker_a'],
            'worker_a', 'attv5_worker_a_pool'))

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name) / 'run1'
        (self.root / 'resources').mkdir(parents=True)
        self.auth = {
            'issued_at': (datetime.now(timezone.utc) - timedelta(minutes=1)).isoformat(),
            'expires_at': (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat(),
            'implementation_allowed': True,
            'plan_manifest_sha256': 'digest',
            'resource_allowlist': {'postgres_container': ['attv5-run1-pg']},
        }
        self.state = {
            'authorization_path': str(self.root / 'authorization.json'),
            'mode': 'IMPLEMENTATION', 'run_id': 'run1', 'owner_token': 'owner',
            'lease_until': time.time() + 120, 'plan_dir': str(self.root / 'plan'),
            'plan_manifest_sha256': 'digest',
        }
        self.resource = {
            'owner_run_id': 'run1', 'allowed_slots': ['slot1'],
            'container': 'attv5-run1-pg', 'container_id': 'a' * 64, 'image_id': 'image',
            'host_binding': [{'HostIp': '127.0.0.1', 'HostPort': '54321'}],
        }
        self.actual = {
            'id': 'a' * 64, 'image': 'image', 'running': True,
            'labels': {'codex.run': 'run1', 'codex.role': 'attachment-lifecycle-pg'},
            'ports': {'5432/tcp': self.resource['host_binding']},
        }
        self.runner = patch('attachment_lifecycle_v5_runner.verify_plan_manifest',
                            return_value=(True, 'digest'))
        self.docker = patch('subprocess.check_output', return_value=json.dumps(self.actual))
        self.runner.start()
        self.docker.start()
        self.addCleanup(self.runner.stop)
        self.addCleanup(self.docker.stop)
        self.addCleanup(self.tmp.cleanup)

    def write(self):
        auth = json.dumps(self.auth).encode()
        (self.root / 'authorization.json').write_bytes(auth)
        self.state['authorization_sha256'] = hashlib.sha256(auth).hexdigest()
        (self.root / 'state.json').write_text(json.dumps(self.state))
        (self.root / 'resources/postgres.json').write_text(json.dumps(self.resource))

    def test_valid_receipt_and_rejected_conditions(self):
        self.write()
        self.assertEqual(owned_container(self.root, 'slot1'), ('a' * 64, '127.0.0.1', '54321'))
        changes = [
            ('auth', 'implementation_allowed', False),
            ('auth', 'expires_at', '2000-01-01T00:00:00Z'),
            ('auth', 'expires_at', (datetime.now(timezone.utc) + timedelta(days=8)).isoformat()),
            ('auth', 'issued_at', (datetime.now(timezone.utc) + timedelta(minutes=1)).isoformat()),
            ('auth', 'issued_at', 'invalid'),
            ('auth', 'plan_manifest_sha256', 'wrong'),
            ('state', 'lease_until', time.time() - 1),
            ('state', 'lease_until', float('nan')),
            ('state', 'mode', 'DESIGN_ONLY'),
            ('state', 'owner_token', ''),
            ('state', 'plan_manifest_sha256', 'wrong'),
        ]
        for obj, key, value in changes:
            with self.subTest(obj=obj, key=key):
                target = getattr(self, obj)
                before = target[key]
                target[key] = value
                self.write()
                with self.assertRaises(ValueError):
                    owned_container(self.root, 'slot1')
                target[key] = before
        self.write()
        with self.assertRaises(ValueError):
            owned_container(self.root, 'slot2')
        for slots in ('slot1x', ['slot1x'], ['slot1', 'slot1'], [], None):
            with self.subTest(slots=slots):
                self.resource['allowed_slots'] = slots
                self.write()
                with self.assertRaises(ValueError):
                    owned_container(self.root, 'slot1')
        self.resource['allowed_slots'] = ['slot1']
        self.resource['container_id'] = 'a' * 12
        self.write()
        with self.assertRaises(ValueError):
            owned_container(self.root, 'slot1')
        self.resource['container_id'] = 'a' * 64
        self.write()
        for malformed in ([], None, 'not-a-map'):
            with self.subTest(malformed=malformed):
                self.auth['resource_allowlist'] = malformed
                self.write()
                with self.assertRaises(ValueError):
                    owned_container(self.root, 'slot1')
        self.auth['resource_allowlist'] = {'postgres_container': ['attv5-run1-pg']}
        self.write()
        self.runner.stop()
        self.runner = patch('attachment_lifecycle_v5_runner.verify_plan_manifest',
                            return_value=(False, 'digest'))
        self.runner.start()
        self.addCleanup(self.runner.stop)
        with self.assertRaises(ValueError):
            owned_container(self.root, 'slot1')


if __name__ == '__main__':
    unittest.main()
