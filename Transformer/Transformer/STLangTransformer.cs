using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Compiler;
using Microsoft.ProgramSynthesis.Specifications;
using Microsoft.ProgramSynthesis.VersionSpace;
using Microsoft.ProgramSynthesis.Learning;
using Microsoft.ProgramSynthesis.Learning.Strategies;
using Microsoft.ProgramSynthesis.Learning.Logging;
using Microsoft.ProgramSynthesis.Utils;
using EqFix.Lib.Transformer.StringLang;

namespace EqFix.Lib.Transformer
{
    using STInput = IEnumerable<string>;

    /// <summary>
    /// A concrete string transformer developed by our own.
    /// This class wraps some useful methods to interactive with the synthesizer.
    /// See the base class <see ref="TransformerExample"/> for the functionality of the methods.
    /// </summary>
    public class STLangTransformer : StringTransformer
    {
        private SynthesisEngine _engine { get; set; }

        public STLangTransformer() : base()
        {
            // compile grammar
            var grammar = LoadGrammar("StringLang.grammar",
                CompilerReference.FromAssemblyFiles(typeof(Semantics).GetTypeInfo().Assembly,
                                                    typeof(Record).GetTypeInfo().Assembly));
            if (grammar == null) {
                Log.Error("ST: Grammar not compiled.");
                return;
            }
            _inputSymbol = grammar.InputSymbol; // set input symbol

            // set up engine
            var witnessFunctions = new WitnessFunctions(grammar);
            _scorer = new RankingScore(grammar);
            _engine = new SynthesisEngine(grammar, new SynthesisEngine.Config
            {
                Strategies = new ISynthesisStrategy[]
                {
                    // new EnumerativeSynthesis(),
                    new DeductiveSynthesis(witnessFunctions)
                },
                UseThreads = false,
                LogListener = new LogListener(LogInfo.Witness),
            });

            Log.Debug("ST: EqFix synthesis engine is setup.");
        }

        override public object TransformInput(STInput input)
        {
            return input.ToArray();
        }

        override protected ProgramNode[] LearnPrograms(IEnumerable<STExample> examples, int k)
        {
            // examples
            var constraints = examples.ToDictionary(
                e => State.CreateForLearning(_inputSymbol, TransformInput(e.input)),
                e => (object) e.output
            );
            Spec spec = new ExampleSpec(constraints);

            // learn
            ProgramSet consistentPrograms = _engine.LearnGrammar(spec);
            _engine.Configuration.LogListener.SaveLogToXML("learning.log.xml");
            var programs = consistentPrograms.TopK(_scorer, k).Take(k).ToArray();
            Log.Debug("ST: {0} program(s) synthesized.", programs.Length);
            
            return programs;
        }

        public static Grammar LoadGrammar(string grammarFile, IReadOnlyList<CompilerReference> assemblyReferences)
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