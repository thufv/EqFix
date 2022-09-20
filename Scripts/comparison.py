#!/usr/bin/env python3
import subprocess

from common import *

# config: 1 | 2 | 3 | 4 (i.e., C1 | C2 | C3 | C4 as defined by the paper)
def run_EqFix(config: int):
    print(f'--> running EqFix under C{config}')
    lines = subprocess.run(['dotnet', DLL, '-p', 'bench', BENCHMARK, '-k', '10', '-n', str(config)],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    return parse_log(lines)

# config: 1 | 2 | 3 | 4 (i.e., C1 | C2 | C3 | C4 as defined by the paper)
def run_FlashFill(config: int):
    print(f'--> running FlashFill under C{config}')
    lines = subprocess.run(['dotnet', DLL, '-p', 'bench', BENCHMARK, '-k', '10', '-n', str(config), '--flashfill'],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    return parse_log(lines)

def stat_synthesis_time(raw: dict[(int, str), dict[int, float]]):
    pass

if __name__ == '__main__':
    result_data = {}
    time_data = {}
    for config in range(1, 5): # C1..C4
        result_data[(config, 'E')], time_data[(config, 'E')] = run_EqFix(config)
        result_data[(config, 'F')], time_data[(config, 'F')] = run_FlashFill(config)

    pretty_matrix(stat_solving_results(result_data))