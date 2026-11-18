"""Offline independent SigV4 verification of the community /s3 proxy contract."""
import hashlib
import hmac
import os
from pathlib import Path
import subprocess
import unittest
from urllib.parse import parse_qs, urlsplit

EBIN = os.environ.get('W09_TEST_EBIN', str(Path(__file__).resolve().parents[2] / 'ebin'))

def sign_url(method):
    source = ("C=#{access_key => <<\"test-access\">>,secret_key => <<\"test-secret\">>,region => <<\"garage\">>},"
              "N={{2026,11,12},{1,2,3}},E = <<\"https://api.example.test/s3\">>,"
              "H=[{<<\"content-type\">>,<<\"image/png\">>},{<<\"x-amz-checksum-sha256\">>,<<\"test-checksum\">>}],"
              "U=case <<\"" + method + "\">> of <<\"PUT\">> -> elib_s3_sign:presign_signed_url_at(<<\"PUT\">>,E,<<\"b\">>,<<\"folder/file\">>,H,N,600,C);"
              "_ -> elib_s3_sign:presign_url_at(<<\"GET\">>,E,<<\"b\">>,<<\"folder/file\">>,<<\"\">>,N,600,C) end,"
              "io:format(\"~s\",[U]),halt().")
    # Separate owned crash-dump target; no network or DB connection.
    env = dict(os.environ, ERL_CRASH_DUMP=str(Path(EBIN) / ('proxy-v2-' + method + '.dump')))
    return subprocess.check_output(['erl', '-noshell', '-pa', EBIN, '-eval', source], env=env, text=True)


def expected_signature(url, method, upstream_path, host, headers):
    parsed = urlsplit(url)
    fields = parse_qs(parsed.query)
    query = '&'.join(part for part in parsed.query.split('&') if not part.startswith('X-Amz-Signature='))
    signed_names = fields['X-Amz-SignedHeaders'][0]
    values = dict(headers, host=host)
    canonical_headers = ''.join(name + ':' + values[name] + '\n' for name in signed_names.split(';'))
    canonical = '\n'.join((method, upstream_path, query, canonical_headers, signed_names, 'UNSIGNED-PAYLOAD'))
    amz_date = fields['X-Amz-Date'][0]
    scope = fields['X-Amz-Credential'][0].split('/', 1)[1]
    signing = '\n'.join(('AWS4-HMAC-SHA256', amz_date, scope, hashlib.sha256(canonical.encode()).hexdigest()))
    def mac(key, text):
        return hmac.new(key, text.encode(), hashlib.sha256).digest()
    date, region, service, _ = scope.split('/')
    key = mac(mac(mac(mac(b'AWS4test-secret', date), region), service), 'aws4_request')
    return hmac.new(key, signing.encode(), hashlib.sha256).hexdigest()


class CommunityProxySigV4Test(unittest.TestCase):
    def test_get_and_put_signature_after_proxy_strips_s3(self):
        for method in ('GET', 'PUT'):
            with self.subTest(method=method):
                url = sign_url(method)
                parsed = urlsplit(url)
                self.assertEqual(parsed.netloc, 'api.example.test')
                self.assertEqual(parsed.path, '/s3/b/folder/file')
                headers = {} if method == 'GET' else {'content-type': 'image/png', 'x-amz-checksum-sha256': 'test-checksum'}
                supplied = parse_qs(parsed.query)['X-Amz-Signature'][0]
                self.assertEqual(supplied, expected_signature(url, method, '/b/folder/file', parsed.netloc, headers))
                self.assertNotEqual(supplied, expected_signature(url, method, parsed.path, parsed.netloc, headers))
                self.assertNotEqual(supplied, expected_signature(url, method, '/b/folder/file', 'wrong.example.test', headers))

if __name__ == '__main__':
    unittest.main()
