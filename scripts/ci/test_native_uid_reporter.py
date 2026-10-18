#!/usr/bin/env python3
"""Minimal real-OTP reporter regression; no app, DB, service or UID operation.

Usage: python3 scripts/ci/test_native_uid_reporter.py /path/to/otp/bin/erl
Only the generator is a synthetic fixture. The actual CI adapter and proof
extractor are used unchanged in the corrected-path reproduction.
"""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
STUB = '''-module(attachment_lifecycle_v5_w403_driver_matrix_tests).
-export([w403_driver_matrix_test_/0]).
w403_driver_matrix_test_() ->
    {setup, fun() -> fixture_only end, fun(fixture_only) -> ok end,
     fun(fixture_only) ->
        Names = ["ordinary/disk/provider_bypass_writer_denied",
                 "enterprise_internal/disk/provider_bypass_writer_denied",
                 "enterprise_seat/disk/provider_bypass_writer_denied",
                 "cs_widget/disk/provider_bypass_writer_denied"],
        [{Name, {timeout, 180, fun() ->
            io:format("before <&>~nW403_PROVIDER_BYPASS ~s~nafter~n",
                [json:encode(#{leaf => list_to_binary(Name), owner_uid => 0,
                               writer_uid => 65534, unprivileged_writer_exit => 2})])
         end}} || Name <- Names] ++
        [{"ordinary/garage/provider_bypass_writer_denied",
          fun() -> erlang:error(unselected_fixture_must_not_run) end}]
     end}.
'''


def main():
    erl = Path(sys.argv[1]).resolve()
    adapter = (HERE/'native_eunit_uid_preflight.erl').read_text()
    named = '[{"native_uid_preflight", Tests}]'
    assert adapter.count(named) == 1
    summary = {}
    with tempfile.TemporaryDirectory(prefix='native-uid-reporter-') as temp:
        base = Path(temp)
        env = dict(PATH=str(erl.parent)+':'+os.defpath, HOME=temp,
                   LC_ALL='C.UTF-8', ERL_FLAGS='+S 2:2 +A 2')
        version = subprocess.run([str(erl), '-noshell', '-boot', 'no_dot_erlang', '-eval',
            '{ok,V}=file:read_file(filename:join([code:root_dir(),"releases",erlang:system_info(otp_release),"OTP_VERSION"])),io:format("~s",[V]),halt().'],
            cwd=temp, env=env, text=True, capture_output=True, check=True, timeout=15).stdout.strip()
        assert version == '29.1.2', 'Reproduction requires audited OTP29.1.2'
        summary['otp_version'] = version
        for mode in ('original_anonymous', 'corrected_named'):
            work = base/mode; work.mkdir()
            xml = work/'xml'; xml.mkdir()
            source = work/'native_eunit_uid_preflight.erl'
            source.write_text(adapter.replace(named, 'Tests') if mode == 'original_anonymous' else adapter)
            stub = work/'attachment_lifecycle_v5_w403_driver_matrix_tests.erl'; stub.write_text(STUB)
            subprocess.run([str(erl.parent/'erlc'), '-o', str(work), str(stub), str(source)],
                cwd=work, env=env, capture_output=True, check=True, timeout=15)
            run_env = dict(env, NATIVE_UID_XML=str(xml))
            result = subprocess.run([str(erl), '-noshell', '-boot', 'no_dot_erlang', '-pa', str(work),
                '-s', 'native_eunit_uid_preflight', 'run'], cwd=work, env=run_env,
                text=True, capture_output=True, timeout=15)
            assert result.returncode == 0 and 'All 4 tests passed.' in result.stdout
            output = work/'proof.json'
            proof = subprocess.run([sys.executable, str(HERE/'native-eunit-uid-proof.py'), str(xml), str(output)],
                text=True, capture_output=True, timeout=15)
            if mode == 'original_anonymous':
                assert not list(xml.glob('*.xml')) and proof.returncode != 0 and not output.exists()
                summary[mode] = dict(eunit_exit=0, tests_passed=4, xml_reports=0, proof_rejected=True)
            else:
                assert proof.returncode == 0 and len(json.loads(output.read_text())) == 4
                report = xml/'TEST-native_uid_preflight.xml'
                assert len(list(xml.glob('*.xml'))) == 1
                summary[mode] = dict(eunit_exit=0, tests_passed=4, xml_reports=1,
                    proof_rows=4, xml_sha256=hashlib.sha256(report.read_bytes()).hexdigest())
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
