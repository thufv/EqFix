# Replication Instructions

This document introduces how to replicate the evaluation made by the paper entitled "EqFix: Fixing LaTeX Equation Errors by Examples".

## Preparation

Follow [README](README.md) to build the project and make sure:
1. The release build is successful: `EqFix/bin/Release/netcoreapp2.0/EqFix.dll` is correctly generated.
2. The DLL is executable: in directory `EqFix/bin/Release/netcoreapp2.0/`, type `dotnet EqFix.dll` and you should see the CLI help information.
3. The test cases are passed: `dotnet test EqFix.Tests` should output `Total tests: 6. Passed: 6. Failed: 0. Skipped: 0.`

The experiment scripts are written in Python with `matplotlib`. Make sure you have Python 3 (version >= 3.9 recommended) and the latest `matplotlib` installed (e.g., via `python3 -m pip install -U matplotlib`).

## Step-by-Step Instructions

Section 5 of the paper conducts two experiments: one is the comparison with FlashFill disabling rule library, and the other is a testing of EqFix's rule library (the last paragraph of Section 5).

To replicate the comparison experiment, change directory to `Scripts/` and execute the `comparison.py` script:
```sh
cd Scripts/
./comparison.py
```

This script will invoke EqFix and FlashFill under the four configurations C1 -- C4 as mentioned in the paper. It can take a while (a couple of minutes). When completes, the following files will be generated in the current directory (`Scripts/`):
- `comparison.csv` contains the number of attempted rules, i.e., Table 3
- `solved.eps` is the figure plotting the overall number of solved test cases, i.e., Fig. 6
- `time.eps` is the figure plotting the cumulative synthesis time in seconds, i.e., Fig. 7

To replicate the second experiment, execute the `end_to_end.py` script in the current directory (`Scripts/`):
```sh
./end_to_end.py
```

This script will test EqFix's rule library and the serialization/deserialization of rule library files (in XML format). Technically, this script invokes EqFix twice (you can see there are two calls of `subprocess.run` in the script):
For the first time, it invokes EqFix to train a set of rules from the training set (configuration C4) and then to save the rules in a rule library file (`1.xml`).
For the second time, it invokes EqFix to load the rules from this rule library file and then test against the testing set (the last example of each example group).
Note that the script uses EqFix's CLI subcommand `batch <FILE>` to instruct EqFix to accept user-input commands from `<FILE>` in the interactive mode (so that you don't have to input them manually).
The commands are using the absolute path and we generate temporary `cmd_*.txt` files.
The test result -- the number of attempted rules for each test case -- is recorded in the generated file `end_to_end_test.csv`.
You can see one more test case (#71) is solved as mentioned in the paper.
