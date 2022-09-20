#!/usr/bin/env python3
import subprocess

from common import *

COMMAND_FILE = os.path.abspath('command.txt')

def gen_command_file():
    with open(COMMAND_FILE, 'w') as f:
        f.writelines([
            f'train {BENCHMARK} -k 10 -n 4\n',
            f'test {BENCHMARK}\n'
        ])

def run():
    print(f'--> running')
    lines = subprocess.run(['dotnet', DLL, '-p', 'batch', COMMAND_FILE],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    return parse_log(lines)


if __name__ == '__main__':
    gen_command_file()
    raw, _ = run()
    pretty_matrix(stat_solving_results({(4, 'E'): raw}))
