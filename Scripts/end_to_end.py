#!/usr/bin/env python3
import subprocess

from common import *

def run():
    print(f'--> running')
    lines = subprocess.run(['dotnet', DLL, '-p', 'online', 'learn', BENCHMARK, '-k', '10', '-n', '4'],
            check=True, stdout=subprocess.PIPE, cwd='../EqFix/bin/Debug/netcoreapp2.0'
    ).stdout.decode('utf8').split('\n')
    return parse_log(lines)


if __name__ == '__main__':
    raw, _ = run()
    pretty_matrix(stat_solving_results({(4, 'E'): raw}))
