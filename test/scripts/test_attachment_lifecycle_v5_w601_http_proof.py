"""Request identities must refer to the actual leaf actions, not control requests."""
import importlib.util
from pathlib import Path
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/test"))
SPEC = importlib.util.spec_from_file_location(
    "w601_real_adapter", ROOT / "scripts/test/attachment_lifecycle_v5_adapter_w6_01_real_pg_garage_disk.py")
adapter = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(adapter)


class HttpProofTests(unittest.TestCase):
    def measured(self, method="GET", route="/api/v1/attachment/presign",
                 presign_handler="attach_handler", put_handler="attachment_storage_gateway_handler",
                 put_route="/api/v1/attachment/gateway/12345678-1234-1234-1234-123456789abc"):
        def request(identity, verb, path, status, handler, action):
            return dict(request_id=identity, method=verb, route=path, status=status,
                        handler=handler, action=action, object_key="owned/key",
                        server_process="<1.2.3>", connection_id="<1.2.4>")
        return dict(http_requests=[request("denied", method, route, 403, presign_handler, "presign"),
                                   request("registered", method, route, 200, presign_handler, "presign"),
                                   request("put", "PUT", put_route, 410, put_handler, "put")],
                    presign_request_id="registered", put_request_id="put", put_status=410,
                    object_key="owned/key", pin_sha256="a" * 64,
                    client_upload_id="12345678-1234-1234-1234-123456789abc",
                    driver_window_expired=True, provider_bytes_absent=True)

    def test_selects_exact_registered_and_put_requests(self):
        m = self.measured()
        a, b = adapter._expiry_proof(m, "ordinary")
        self.assertEqual(("registered", "put"), (a["request_id"], b["request_id"]))

    def test_rejects_denied_request_wrong_surface_and_mismatched_put(self):
        m = self.measured()
        m["presign_request_id"] = "denied"
        with self.assertRaises(ValueError):
            adapter._expiry_proof(m, "ordinary")
        m = self.measured()
        with self.assertRaises(ValueError):
            adapter._expiry_proof(m, "enterprise_internal")
        m["put_status"] = 409
        with self.assertRaises(ValueError):
            adapter._expiry_proof(m, "ordinary")

    def test_rejects_duplicate_ids_and_missing_real_object_state(self):
        m = self.measured()
        m["http_requests"].append(dict(m["http_requests"][1]))
        with self.assertRaises(ValueError):
            adapter._expiry_proof(m, "ordinary")
        m = self.measured()
        del m["object_key"]
        with self.assertRaises(ValueError):
            adapter._expiry_proof(m, "ordinary")

    def test_all_surface_routes(self):
        gateway = "/api/v1/attachment/gateway/12345678-1234-1234-1234-123456789abc"
        for name, method, route, handler, put_handler, put_route in (
            ("enterprise_internal", "POST", "/api/internal/v1/files/presign", "enterprise_asset_handler", "attachment_storage_gateway_handler", gateway),
            ("enterprise_seat", "POST", "/api/v1/seat/enterprise/organizations/12/assets/presign", "eb_tenant_handler", "eb_tenant_handler", "/api/v1/seat/enterprise/organizations/12/assets/presign"),
            ("cs_widget", "POST", "/api/v1/cs/widget/sessions/34/assets/presign", "cs_widget_handler", "cs_widget_handler", "/api/v1/cs/widget/sessions/34/assets/upload"),
        ):
            with self.subTest(name=name):
                adapter._expiry_proof(self.measured(method, route, handler, put_handler, put_route), name)

    def test_rejects_unrelated_put_wrong_handler_success_and_other_object(self):
        for field, value in (("route", "/unrelated"), ("handler", "wrong_handler"),
                             ("status", 200), ("object_key", "other/object")):
            with self.subTest(field=field):
                m = self.measured()
                m["http_requests"][2][field] = value
                if field == "status":
                    m["put_status"] = value
                with self.assertRaises(ValueError):
                    adapter._expiry_proof(m, "ordinary")


if __name__ == "__main__":
    unittest.main()
