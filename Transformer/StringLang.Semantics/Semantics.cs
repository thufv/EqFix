using System;
using Microsoft.ProgramSynthesis.Utils;

namespace EqFix.Lib.Transformer.StringLang {
    public static class Semantics {
        public static string Concat(string s1, string s2) {
            return s1 + s2;
        }

        public static string ConstStr(string s) {
            return s;
        }

        // posPair = [p1, p2], both included.
        public static string SubStr(string x, Record<int?, int?>? posPair) {
            if (posPair == null) return null;
            int? left = posPair.Value.Item1;
            int? right = posPair.Value.Item2;

            if (left == null || right == null || left > right || right >= x.Length) {
                return null;
            }

            return x.Slice(left, right + 1); // x[left..right]
        }

        public static string JustChooseInput(string[] v, int i) {
            if (i >= v.Length) {
                return null;
            }
            
            return v[i];
        }

        public static string ChooseInput(string[] v, int i) {
            if (i >= v.Length) {
                return null;
            }
            
            return v[i];
        }

        public static int? AbsPos(string x, int k) {
            int n = x.Length;
            if (k < -n || k >= n) {
                return null;
            }

            // -n <= k < n
            return (k >= 0) ? k : (n + k);
        }

        public static int? RelPos(string x, Token t, Record<int, int>? indexPair) {
            if (indexPair == null) return null;
            int j = indexPair.Value.Item1;
            int k = indexPair.Value.Item2;

            var matches = t.FindMatches(x);
            int n = matches.Length;
            if (j < -n || j > n) { // j >= 1 or j <= -1
                return null;
            }

            var match = matches[j > 0 ? (j - 1) : (n + j)];
            // k > 0: k characters after the last character of the match
            // k = 0: the first character of the match
            // k < 0: -k characters before the first character of the match
            return (k > 0) ? (match.Last + k) : (match.First - k);
        }
    }
}
