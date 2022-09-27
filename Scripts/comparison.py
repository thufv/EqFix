#!/usr/bin/env python3
import subprocess
import matplotlib.pyplot as plt
import matplotlib

from common import *

# config: 1 | 2 | 3 | 4 (i.e., C1 | C2 | C3 | C4 as defined by the paper)
def run_EqFix(config: int):
    print(f'--> running EqFix under C{config}')
    lines = subprocess.run(['dotnet', DLL, '-p', 'bench', BENCHMARK, '-k', '10', '-n', str(config)],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    with open(f'run_EqFix_C{config}.log', 'w') as f:
        f.writelines([line + '\n' for line in lines])
    return parse_log(lines)

# config: 1 | 2 | 3 | 4 (i.e., C1 | C2 | C3 | C4 as defined by the paper)
def run_FlashFill(config: int):
    print(f'--> running FlashFill under C{config}')
    lines = subprocess.run(['dotnet', DLL, '-p', 'bench', BENCHMARK, '-k', '10', '-n', str(config), '--flashfill'],
        check=True, stdout=subprocess.PIPE, cwd=DLL_DIR
    ).stdout.decode('utf8').split('\n')
    with open(f'run_FlashFill_C{config}.log', 'w') as f:
        f.writelines([line + '\n' for line in lines])
    return parse_log(lines)

# global setting of plotting figures
font = {'family' : 'Sans',
        'weight' : 'normal',
        'size'   : 14}
matplotlib.rc('font', **font)

def plot_solved(raw: dict[(int, str), dict[int, Optional[int]]]):
    # data
    x = ['C1', 'C2', 'C3', 'C4']
    yE = [len([1 for v in raw[(config, 'E')].values() if v is not None]) for config in range(1, 5)]
    yF = [len([1 for v in raw[(config, 'F')].values() if v is not None]) for config in range(1, 5)]

    # plot
    plt.figure()
    plt.xlabel('Training configuration')
    plt.ylabel('# Solved')
    delta = 4
    plt.ylim(0 - delta, 80 + delta)
    plt.yticks(range(0, 80, 20))
    plt.plot(x, yE, '-s', color='green', label='EqFix')
    plt.plot(x, yF, '-^', color='purple', label='FlashFill')
    plt.legend()
    annot_opt = {'textcoords': 'offset points', 'xytext': (0, 10), 'ha': 'center'}
    for i in range(4):
        plt.annotate(yE[i], (x[i], yE[i]), **annot_opt)
        plt.annotate(yF[i], (x[i], yF[i]), **annot_opt)

    # save
    plt.savefig('solved.eps')

def accumulate(times: list[float]) -> list[float]:
    acc = 0.0
    ret = []
    for t in times:
        acc += t
        ret.append(acc)

    return ret

def plot_time(raw: dict[(int, str), dict[int, float]]):
    # data
    x = list(range(1, 90))
    yE = accumulate([t / 1000.0 for t in raw[(4, 'E')].values()]) # as seconds
    yF = accumulate([t / 1000.0 for t in raw[(4, 'F')].values()]) # as seconds

    # plot
    plt.figure()
    plt.xlabel('# Groups of examples')
    plt.xlim(0, 91)
    plt.xticks(range(0, 91, 10))
    plt.ylabel('Cumulative time (s)')
    plt.plot(x, yE, color='green', label='EqFix')
    plt.plot(x, yF, color='purple', label='FlashFill')
    plt.legend()
    
    # save
    plt.savefig('time.eps')


if __name__ == '__main__':
    result_data = {}
    time_data = {}
    for config in range(1, 5): # C1..C4
        if os.path.exists(f'run_EqFix_C{config}.log'):
            with open(f'run_EqFix_C{config}.log', 'r') as f:
                print(f'Loading run_EqFix_C{config}.log')
                result_data[(config, 'E')], time_data[(config, 'E')] = parse_log(f.readlines())
        else:
            result_data[(config, 'E')], time_data[(config, 'E')] = run_EqFix(config)

        if os.path.exists(f'run_FlashFill_C{config}.log'):
            with open(f'run_FlashFill_C{config}.log', 'r') as f:
                print(f'Loading run_FlashFill_C{config}.log')
                result_data[(config, 'F')], time_data[(config, 'F')] = parse_log(f.readlines())
        else:
            result_data[(config, 'F')], time_data[(config, 'F')] = run_FlashFill(config)

    gen_csv(stat_solving_results(result_data), 'comparison.csv')
    print('Generated: comparison.csv')

    plot_solved(result_data)
    print('Generated: solved.eps')

    plot_time(time_data)
    print('Generated: time.eps')
