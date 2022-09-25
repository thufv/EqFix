using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Transformation.Text;
using Microsoft.ProgramSynthesis.Wrangling;
using EqFix.Lib.Logging;

namespace EqFix.Lib.Transformer
{
    using STInput = IEnumerable<string>;

    /// <summary>
    /// A concrete string transformer developed by PROSE.
    /// This class wraps some useful methods to interactive with the synthesizer.
    /// See the base class <see ref="TransformerExample"/> for the functionality of the methods.
    /// </summary>
    public static class PROSETextTransformer
    {
        private static Logger Log = Logger.Instance;

        public static Symbol InputSymbol;

        public static void Init()
        {
            if (InputSymbol == null) { // not yet set
                var constraints = new[]
                {
                    new Example(new InputRow("foo"), "foo")
                };
                ProgramNode p = Learner.Instance.Learn(constraints).ProgramNode;
                Debug.Assert(p != null);
                InputSymbol = p.Grammar.InputSymbol;
            }

            Log.Debug("ST: PROSE synthesis engine is setup.");
        }

        public static object TransformInput(STInput input)
        {
            return new InputRow(input);
        }

        /// <summary>
        /// Synthesize candidate programs by examples.
        /// </summary>
        /// <param name="k">Maximal number of candidate programs to be synthesized.
        /// Default 3.</param>
        /// <returns>The synthesized programs.</returns>
        public static List<STProgram> Synthesize(IEnumerable<STExample> examples, int k = 1)
        {
            Init();
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
                progs.Add(new STProgram(programNode, rank, "PROSE"));
            }
            return progs;
        }

        private static ProgramNode[] LearnPrograms(IEnumerable<STExample> examples, int k)
        {
            var constraints = examples.Select(e => new Example(new InputRow(e.input), e.output));

            if (k == 1) {
                ProgramNode p = Learner.Instance.Learn(constraints).ProgramNode;
                Log.Debug("ST: {0} program(s) synthesized.", p == null ? 0 : 1);

                if (p == null) return new ProgramNode[] {};
                
                InputSymbol = p.Grammar.InputSymbol; // set input symbol
                return new[] { p };
            }

            var programs = Learner.Instance.LearnAll(constraints).AllElements.Take(k).ToArray();
            Log.Debug("ST: {0} program(s) synthesized.", programs.Length);
            if (programs.Any()) {
                InputSymbol = programs[0].Grammar.InputSymbol; // set input symbol
            }

            return programs;
        }
    }
}
