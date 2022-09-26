#!/usr/bin/env python3
import subprocess

from common import *

COMMAND_FILE = os.path.abspath('cmd_')
RULE_FILE = '1.xml'

def gen_command_file_1():
    with open(COMMAND_FILE + '1.txt', 'w') as f:
        f.writelines([
            f'train {BENCHMARK} -k 10 -n 4\n',
            f'save {RULE_FILE}\n'
        ])

def gen_command_file_2():
    with open(COMMAND_FILE + '2.txt', 'w') as f:
        f.writelines([
            f'load {RULE_FILE}\n',
            f'test {BENCHMARK}\n'
        ])

def run():
    print(f'--> training')
    gen_command_file_1()
    lines = subprocess.run(['dotnet', DLL, '-p', 'batch', COMMAND_FILE + '1.txt'],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    with open('training.log', 'w') as f:
        f.writelines([line + '\n' for line in lines])

    print(f'--> testing')
    gen_command_file_2()
    lines = subprocess.run(['dotnet', DLL, '-p', 'batch', COMMAND_FILE + '2.txt'],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    with open('testing.log', 'w') as f:
        f.writelines([line + '\n' for line in lines])
    return parse_log(lines)


if __name__ == '__main__':
    if os.path.exists('testing.log'):
        with open('testing.log', 'r') as f:
            print('Loading testing.log')
            raw, _ = parse_log(f.readlines())
    else:
        raw, _ = run()

    gen_csv(stat_solving_results({(4, 'E'): raw}), 'end_to_end_test.csv')
    print('Generated: end_to_end_test.csv')
