using System.Collections.Generic;
using System.Linq;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Transformation.Text;
using Microsoft.ProgramSynthesis.Wrangling;

namespace EqFix.Lib.Transformer
{
    using STInput = IEnumerable<string>;

    /// <summary>
    /// A concrete string transformer developed by PROSE.
    /// This class wraps some useful methods to interactive with the synthesizer.
    /// See the base class <see ref="TransformerExample"/> for the functionality of the methods.
    /// </summary>
    public class PROSETextTransformer : StringTransformer
    {
        public PROSETextTransformer() : base()
        {
            Log.Debug("ST: PROSE synthesis engine is setup.");
        }

        override public object TransformInput(STInput input)
        {
            return new InputRow(input);
        }

        override protected ProgramNode[] LearnPrograms(IEnumerable<STExample> examples, int k)
        {
            var constriants = examples.Select(e => new Example(new InputRow(e.input), e.output));

            if (k == 1) {
                ProgramNode p = Learner.Instance.Learn(constriants).ProgramNode;
                Log.Debug("ST: {0} program(s) synthesized.", p == null ? 0 : 1);

                if (p == null) return new ProgramNode[] {};
                
                _inputSymbol = p.Grammar.InputSymbol; // set input symbol
                return new[] { p };
            }

            var programs = Learner.Instance.LearnAll(constriants).AllElements.Take(k).ToArray();
            Log.Debug("ST: {0} program(s) synthesized.", programs.Length);
            if (programs.Any()) {
                _inputSymbol = programs[0].Grammar.InputSymbol; // set input symbol
            }

            return programs;
        }
    }
}
