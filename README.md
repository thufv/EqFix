# EqFix

Repairing LaTeX equations.

## System Requirements

Install .Net SDK (https://www.microsoft.com/net/download) for your platform.
You must have .NET Core 2.x installed to correctly run [PROSE](https://github.com/microsoft/prose)'s DSL compiler.

Note: you may build this project using a higher version .Net SDK (such as 6.x, the latest); but 2.x must be one of your installed version. Type `dotnet --list-sdks` to check if 2.0.x/2.1.x/2.2.x is on the list.

## Build

In the project root directory:

```bash
dotnet build --configuration Release EqFix  # Build EqFix
dotnet test EqFix.Tests                     # Test
```

## Interactive Mode

Change to the binary directory `EqFix/bin/Release/netcoreapp2.0`.
Type `dotnet EqFix.dll`.
EqFix REPL should shortly launch like the following:

```
EqFix interactive REPL
> 
```

Type `--help` to show usage. Type `q` to quit.

### Training

You use the command `train` to learn fixing rules from groups of examples.
The typical usage is:

```
> train <DATA_FILE> -k <K> -n <N>
```

The argument `<DATA_FILE>` is the *absolute path* to a `.json` file containing the example groups.
As demonstrated in [our data set file](Benchmarks/Full.json), this should be a JSON array of objects

```json
{
  "id": 1,
  "examples": [
    {
      "eq": "$x^10$",
      "err": "10 is a superscript.",
      "fix": "$x^{10}$"
    },
    ...
  ]
}
```

that consists of two fields:
- `id` is an integer value that provides a file-wide unique ID
- `examples` is a JSON array of input-output examples and each you have to provide the erroneous equation (`eq`), the error message (`err`) and the fixed equation (`fix`)

The argument `<K>` for the option `-k` specifies to synthesize top-`<K>` programs (default 1).
The argument `<N>` for the option `-n` specifies the number of examples used for synthesis in each example group (default 1).

### Testing

You use the command `test` to test the learned fixing rules against some groups of examples.
The typical usage is:

```
> test <DATA_FILE> -i <I>
```

The `<DATA_FILE>` is the same as above.
The argument `<I>` for the option `-i` indicates that the `I`-th example *from last* in each group is used for testing (default 1, meaning the last example is the test case).

### Fixing

You use the command `fix` to apply the learned fixing rules on an erroneous equation and error message.
If any rule is applicable, the fixed equation will be displayed.
The typical usage is simply:

```
> fix
```

Then input your erroneous equation and error message following the prompts.

## Replication

See `Scripts/`.