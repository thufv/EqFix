using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Diagnostics;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Compiler;
using Microsoft.ProgramSynthesis.Specifications;
using Microsoft.ProgramSynthesis.VersionSpace;
using Microsoft.ProgramSynthesis.Learning;
using Microsoft.ProgramSynthesis.Learning.Strategies;
using Microsoft.ProgramSynthesis.Learning.Logging;
using Microsoft.ProgramSynthesis.Features;
using Microsoft.ProgramSynthesis.Utils;
using EqFix.Lib.Transformer.StringLang;
using EqFix.Lib.Logging;

namespace EqFix.Lib.Transformer
{
    using STInput = IEnumerable<string>;

    /// <summary>
    /// A concrete string transformer developed by our own.
    /// This class wraps some useful methods to interactive with the synthesizer.
    /// See the base class <see ref="TransformerExample"/> for the functionality of the methods.
    /// </summary>
    public static class STLangTransformer
    {
        private static Logger Log = Logger.Instance;

        private static SynthesisEngine _engine;

        private static RankingScore _scorer;
        
        public static Grammar DSL;

        public static Symbol InputSymbol;

        public static void Init()
        {
            if (DSL == null) { // not yet compiled
                // compile grammar
                DSL = LoadGrammar("StringLang.grammar",
                    CompilerReference.FromAssemblyFiles(typeof(Semantics).GetTypeInfo().Assembly,
                                                        typeof(Record).GetTypeInfo().Assembly));
                if (DSL == null) {
                    Log.Error("ST: DSL not compiled.");
                    return;
                }
                
                InputSymbol = DSL.InputSymbol; // set input symbol
            }

            // set up engine
            var witnessFunctions = new WitnessFunctions(DSL);
            _scorer = new RankingScore(DSL);
            _engine = new SynthesisEngine(DSL, new SynthesisEngine.Config
            {
                Strategies = new ISynthesisStrategy[]
                {
                    new DeductiveSynthesis(witnessFunctions)
                },
                UseThreads = false
            });

            Log.Debug("ST: STLang synthesis engine is setup.");
        }

        public static object TransformInput(STInput input)
        {
            return input.ToArray();
        }

        /// <summary>
        /// Synthesize candidate programs by examples.
        /// </summary>
        /// <param name="k">Maximal number of candidate programs to be synthesized.
        /// Default 3.</param>
        /// <returns>The synthesized programs.</returns>
        public static List<STProgram> Synthesize(IEnumerable<STExample> examples, int k = 1)
        {
            Log.Debug("ST examples: {0}",
                      String.Concat(examples.Select(e => "\n" + e.ToString())));

            // synthesis
            var stopwatch = new Stopwatch();
            stopwatch.Start();
            ProgramNode[] programs = LearnPrograms(examples, k);
            stopwatch.Stop();
            Log.Info("ST synthesis time: {0} ms", stopwatch.Elapsed.Milliseconds);
            if (!programs.Any()) {
                Log.Error("ST synthesis failed: no program(s) synthesized.");
            }

            List<STProgram> progs = new List<STProgram>();
            int rank = 0;
            foreach (var programNode in programs) {
                rank++;
                progs.Add(new STProgram(programNode, rank, "STLang"));
            }
            return progs;
        }

        private static ProgramNode[] LearnPrograms(IEnumerable<STExample> examples, int k)
        {
            // examples
            var constraints = examples.ToDictionary(
                e => State.CreateForLearning(InputSymbol, TransformInput(e.input)),
                e => (object) e.output
            );
            Spec spec = new ExampleSpec(constraints);

            // learn
            ProgramSet consistentPrograms = _engine.LearnGrammar(spec);
            var programs = consistentPrograms.TopK(_scorer, k).Take(k).ToArray();
            Log.Debug("ST: {0} program(s) synthesized.", programs.Length);
            
            return programs;
        }

        private static Grammar LoadGrammar(string grammarFile, IReadOnlyList<CompilerReference> assemblyReferences)
        {
            var compilationResult = DSLCompiler.Compile(new CompilerOptions() {
                InputGrammarText = File.ReadAllText(grammarFile),
                References = assemblyReferences
            });

            if (compilationResult.HasErrors)
            {
                compilationResult.TraceDiagnostics();
                // Console.WriteLine(compilationResult.TraceDiagnostics);
                return null;
            }
            if (compilationResult.Diagnostics.Count > 0)
            {
                // Console.WriteLine("has Diagnostics");
                // foreach (var d in compilationResult.Diagnostics) {
                    // Console.WriteLine(d.ToString());
                // }
                // Console.WriteLine(compilationResult.TraceDiagnostics);
            }

            return compilationResult.Value;
        }
    }
}