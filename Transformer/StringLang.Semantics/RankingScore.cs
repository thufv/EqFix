using System;
using System.Diagnostics;
using System.Linq;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Features;

using MyLogger = EqFix.Lib.Logging.Logger;

namespace EqFix.Lib.Transformer.StringLang
{
    public class RankingScore : Feature<double>
    {
        public RankingScore(Grammar grammar) : base(grammar, "Score") { }

        private static MyLogger Log = MyLogger.Instance;

        protected override double GetFeatureValueForVariable(VariableNode variable) => 0;

        // The larger the score, the higher the rank.
        [FeatureCalculator(nameof(Semantics.Concat))]
        public static double Concat(double s1, double s2) => s1 * s2 * 0.5;

        [FeatureCalculator(nameof(Semantics.ConstStr))]
        public static double ConstStr(double s) => s; // <= 1

        [FeatureCalculator(nameof(Semantics.JustChooseInput))]
        public static double JustChooseInput(double v, double i) => 3; // 3

        [FeatureCalculator(nameof(Semantics.SubStr))]
        public static double SubStr(double x, double posPair) => posPair; // <= 2

        [FeatureCalculator(nameof(Semantics.ChooseInput))]
        public static double ChooseInput(double v, double i) => i; // = 1

        [FeatureCalculator("PosPair")]
        public static double PosPair(double pos1, double pos2) { // <= 2
            if (pos1 == pos2) return pos1 * pos2 * 0.25;

            return pos1 * pos2 * 0.5;
        }

        [FeatureCalculator(nameof(Semantics.AbsPos))]
        public static double AbsPos(double x, double k) => k; // <= 1

        [FeatureCalculator(nameof(Semantics.RelPos))]
        public static double RelPos(double x, double t, double indexPair) => t * indexPair; // <= 2

        [FeatureCalculator("IndexPair")]
        public static double IndexPair(double j, double k) => j * k; // <= 1

        [FeatureCalculator("k", Method = CalculationMethod.FromLiteral)]
        public static double K(int k) => (k >= 0) ? 1.0 / (1 + k) : 1.0 / (1.1 - k); // <= 1

        [FeatureCalculator("i", Method = CalculationMethod.FromLiteral)]
        public static double I(int i) => 0;

        [FeatureCalculator("j", Method = CalculationMethod.FromLiteral)]
        public static double J(int j) => 1.0 / Math.Abs(j); // <= 1

        [FeatureCalculator("s", Method = CalculationMethod.FromLiteral)]
        public static double S(string s) {
            if (s.Any(Char.IsDigit)) return 0;

            return 1.0 / (1 + s.Length);
        } // <= 1

        [FeatureCalculator("t", Method = CalculationMethod.FromLiteral)]
        public static double T(Token t) => t.Score; // <= 2
    }
}