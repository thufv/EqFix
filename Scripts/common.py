import os
import re
from typing import Any, List, Optional, Tuple

DLL = 'EqFix.dll'
DLL_DIR = '../EqFix/bin/Release/netcoreapp2.0'
BENCHMARK = os.path.abspath('../Benchmarks/Full.json')

INT_REGEX = re.compile(r'(?P<number>\d+) .+')
TIME_REGEX = re.compile(r'(?P<number>\d+(.\d+)?) ms.*')

# returns a mapping from benchmark id to (result, time)
#   where result is: k -> number of rules attempted
#                    None -> failure (no rules synthesized/applicable)
def parse_log(lines: List[str]) -> Tuple[dict[int, Optional[int]], dict[int, float]]:
    current_id = -1
    current_time = 0
    result_data = {}
    time_data = {}
    for line in lines:
        if line.startswith('[Info] Running benchmark #'):
            m = re.match(INT_REGEX, line[len('[Info] Running benchmark #'):])
            current_id = int(m.group('number'))
            current_time = 0.0
        elif line.startswith('[Info] ST synthesis time: '):
            m = re.match(TIME_REGEX, line[len('[Info] ST synthesis time: '):])
            current_time += float(m.group('number'))
        elif line.startswith('[Info] Rule synthesis time: '):
            m = re.match(TIME_REGEX, line[len('[Info] Rule synthesis time: '):])
            current_time += float(m.group('number'))
        elif line.startswith('[Info] Success: '):
            m = re.match(INT_REGEX, line[len('[Info] Success: '):])
            result_data[current_id] = int(m.group('number'))
            time_data[current_id] = current_time
        elif line.startswith('[Info] Failure: '):
            result_data[current_id] = None
            time_data[current_id] = current_time

    return (result_data, time_data)

def stat_solving_results(raw: dict[(int, str), dict[int, Optional[int]]]) -> list[list[Any]]:
    first_row = ['#']
    max_id = 0
    for (config, tool) in sorted(raw):
        first_row.append(f'C{config} {tool}')
        max_id = max(max_id, max(raw[(config, tool)].keys()))

    rows = [first_row]
    for i in range(1, max_id + 1):
        current_row = [i]
        for key in sorted(raw):
            if i in raw[key]:
                if raw[key][i]:
                    current_row.append(raw[key][i])
                else:
                    current_row.append('✗')
            else:
                current_row.append('-')
        rows.append(current_row)

    overall_row = ['A']
    for key in sorted(raw):
        solved = [1 for i in raw[key] if raw[key][i] is not None]
        overall_row.append(len(solved))
    rows.append(overall_row)

    return rows
    
# ref: https://stackoverflow.com/questions/13214809/pretty-print-2d-list
def pretty_matrix(matrix: list[list[Any]]):
    s = [[str(e) for e in row] for row in matrix]
    lens = [max(map(len, col)) for col in zip(*s)]
    fmt = '\t'.join('{{:{}}}'.format(x) for x in lens)
    table = [fmt.format(*row) for row in s]
    print('\n'.join(table))
