using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Features;
using EqFix.Lib.Logging;

namespace EqFix.Lib.Transformer
{
    /// <summary>
    /// The general type for a input, say a string list.
    /// </summary>
    using STInput = IEnumerable<string>;
    
    /// <summary>
    /// The general type for an input-output example, say a string list (as input),
    /// with a string (as output).
    /// </summary>
    public struct STExample
    {
        public STInput input;
        public string output;

        public STExample(STInput i, string o)
        {
            input = i;
            output = o;
        }

        public override string ToString()
        {
            return String.Format("{0} -> \"{1}\"", 
                                 String.Join(", ", input.Select(e => "\"" + e + "\"")), output);
        }
    }

    /// <summary>
    /// A wrapped type for the internal <code>ProgramNode</code>.
    /// </summary>
    /// <typeparam name="/summary"></typeparam>
    public sealed class STProgram
    {   
        private static Logger Log = Logger.Instance;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="prog">The program node.</param>
        public STProgram(ProgramNode prog, int rank, StringTransformer trans)
        {
            _program = prog;
            _t = trans;
            _rank = rank;
        }

        /// <summary>
        /// Internal <code>ProgramNode</code> representation.
        /// </summary>
        private ProgramNode _program { get; set; }

        /// <summary>
        /// Reference to the <code>StringTransformer</code>.
        /// </summary>
        /// <value>The <code>StringTransformer</code> instance.</value>
        private StringTransformer _t;
        
        /// <summary>
        /// Rank of this candidate program.
        /// </summary>
        private int _rank;

        public string apply(STInput input)
        {
            if (_t._scorer != null) {
                Log.Debug("ST #{0} ({1:F3}): {2}", _rank, 
                          _program.GetFeatureValue(_t._scorer), ToString());
            } else {
                Log.Debug("ST #{0}: {1}", _rank, ToString());
            }

            var internalInput = _t.TransformInput(input);
            var inputState = State.CreateForExecution(_t._inputSymbol, internalInput);
            var output = _program.Invoke(inputState);
            if (output == null) {
                return "";
            }

            var o = output.ToString();
            Log.Debug("ST output: \"{0}\"", o);
            return o;
        }

        override public string ToString()
        {
            return _program.PrintAST(ASTSerializationFormat.HumanReadable);
        }
    }

    /// <summary>
    /// Base class of a string transformer: string list -> string.
    /// </summary>
    public abstract class StringTransformer
    {
        protected static Logger Log = Logger.Instance;

        /// <summary>
        /// The synthesizer requires the users to provide input-output examples.
        /// </summary>
        /// <value>Input-output examples.</value>
        protected IEnumerable<STExample> _examples { get; set; }

        /// <summary>
        /// Candidate programs (internal representation).
        /// </summary>
        /// <value>All candidate programs synthesized.</value>
        protected ProgramNode[] _programs { get; set; }

        /// <summary>
        /// Input symbol of the grammar.
        /// </summary>
        /// <value>Input symbol.</value>
        public Symbol _inputSymbol { get; set; }

        public IFeature _scorer { get; set; }

        /// <summary>
        /// Constructor.
        /// </summary>
        public StringTransformer()
        {
        }

        /// <summary>
        /// Transform a general input, of type <code>TransformerInput</code> into an internal input,
        /// of type <code>I</code>.
        /// </summary>
        /// <param name="input">The general input of type <code>TransformerInput</code>.</param>
        /// <returns>The interal input of type <code>I</code>.</returns>
        abstract public object TransformInput(STInput input);

        /// <summary>
        /// Launch the internal learner to synthesis candidate programs.
        /// NOTE: this function shall set property <code>_programs</code> 
        /// if any candidate programs are found.
        /// </summary>
        /// <param name="examples">Input-output examples of internal example type.</param>
        /// <param name="k">Maximal number of candidate programs to be synthesized.
        /// 1 means that only the top-ranked program is considered.</param>
        /// <returns><code>true</code> if succeeds, <code>false</code> if fails.</returns>
        abstract protected ProgramNode[] LearnPrograms(IEnumerable<STExample> examples, int k);

        /// <summary>
        /// Run a candidate program on some input.
        /// </summary>
        /// <param name="input">The test input.</param>
        /// <param name="k">Execute the <code>k</code>-th candidate program.</param>
        /// <returns>The output.</returns>
        private string RunProgram(STInput input, int k)
        {
            var inputState = State.CreateForExecution(_inputSymbol, TransformInput(input));
            return _programs[k - 1].Invoke(inputState) as string;
        }

        /// <summary>
        /// Synthesize candidate programs by examples.
        /// </summary>
        /// <param name="k">Maximal number of candidate programs to be synthesized.
        /// Default 3.</param>
        /// <returns>The synthesized programs.</returns>
        public List<STProgram> Synthesize(IEnumerable<STExample> examples, int k = 1)
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
                progs.Add(new STProgram(programNode, rank, this));
            }
            return progs;
        }

        /// <summary>
        /// Test a single input on the <code>k</code>-th candidate program.
        /// </summary>
        /// <param name="input">The test input.</param>
        /// <param name="k">Run the <code>k</code>-th candidate program. 
        /// ASSUME: <code>1 <= k <= _maxK</code>. Default the top-ranked program.</param>
        /// <returns>The output.</returns>
        public string TestInput(STInput input, int k = 1)
        {
            var output = RunProgram(input, k);
            Log.Info("{0} => \"{1}\"", input, output);
            return output;
        }

        /// <summary>
        /// Test a group of inputs on the <code>k</code>-th candidate program.
        /// </summary>
        /// <param name="inputs">The test inputs.</param>
        /// <param name="k">Run the <code>k</code>-th candidate program. 
        /// ASSUME: <code>1 <= k <= _maxK</code>. Default the top-ranked program.</param>
        /// <returns>The output results for each input.</returns>
        public IEnumerable<string> TestInputs(IEnumerable<STInput> inputs, int k = 1)
        {
            foreach (var input in inputs) {
                var output = RunProgram(input, k);
                Log.Info("{0} => \"{1}\"", input, output);
                yield return output;
            }
        }

        /// <summary>
        /// Similar to <see cref="Transformer.TestInputs" />.
        /// But the expected output is also provided with the test inputs.
        /// </summary>
        /// <param name="cases">The test cases, say the test inputs with expected outputs.</param>
        /// <param name="k">Run the <code>k</code>-th candidate program. 
        /// ASSUME: <code>1 <= k <= _maxK</code>. Default the top-ranked program.</param>
        /// <returns>The test results for each test case: 
        /// <code>true</code>if passed, or <code>false</code> otherwise.</returns>
        public IEnumerable<bool> TestCases(IEnumerable<STExample> cases, int k = 1)
        {
            var tests = cases.Select(p => (input: p.input, output: p.output));
            foreach (var p in tests) {
                var output = RunProgram(p.input, k);
                var result = output == p.output;
                if (!result) {
                    Log.Failure("\"{0}\" /= \"{1}\"", output, p.output);
                }
                yield return result;
            }
        }

        /// <summary>
        /// Find the first candidate program that passes all the test cases.
        /// If all candidate programs fail, then <code>-1</code> is returned.
        /// Similar to <see cref="Transformer.TestCases" />.
        /// </summary>
        /// <param name="cases">The test cases.</param>
        /// <returns>The index of the first candidate program that passes all the test cases.
        /// If not exist, then <code>-1</code> is returned.</returns>
        public int TestCasesWithLeastK(IEnumerable<STExample> cases)
        {
            for (int k = 1; k <= _programs.Length; k++) {
                var results = TestCases(cases, k);
                if (Contract.ForAll(results, r => r)) { // all PASS
                    Log.Success("Tests PASS with program #{0}.", k);
                    return k;
                }
            }

            Log.Failure("No candidate program passes all tests.");
            return -1;
        }

        /// <summary>
        /// F# interface. 
        /// Return an instance of string transformer synthesizer.
        /// </summary>
        /// <param name="name">The name of the ST, 
        /// either <code>PROSE</code> or <code>STLang</code>.</param>
        /// <returns></returns>
        public static StringTransformer SetupST(string name) {
            switch (name) {
                case "PROSE": return new PROSETextTransformer();
                case "STLang": return new STLangTransformer();
            }

            // default
            Log.Warning("Invalid ST name: {0}, STLang used as default.", name);
            return new STLangTransformer();
        }
    }
}