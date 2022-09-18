using System.Collections.Generic;

namespace EqFix.Lib.Transformer.StringLang
{
    public struct Match // [first..last]
    {
        public Match(int pos) {
            First = pos;
            Last = pos;
        }

        public Match(int first, int last) {
            First = first;
            Last = last;
        }

        public int First { get; set; }
        public int Last { get; set; }
    }

    public abstract class Token
    {
        public Token(string representation, double score) {
            Representation = representation;
            Score = score;
        }

        public abstract Match[] FindMatches(string s);

        public abstract bool HasMatch(string s);

        public abstract bool BoundedHasMatch(string s, int bound, int target);

        // The larger the score, the higher the rank.
        public double Score { get; set; }

        public string Representation { get; set; }

        public override string ToString() {
            return Representation;
        }
    }

    public class TokenChar : Token
    {
        public TokenChar(char c, double score) : base(c.ToString(), score) {
            _c = c;
        }

        private char _c;

        override public bool BoundedHasMatch(string s, int bound, int target) {
            for (int i = target - bound; i <= target + bound; i++) {
                if (i < 0 || i >= s.Length) break;

                if (s[i] == _c) {
                    return true;
                }
            }

            return false;
        }

        override public Match[] FindMatches(string s) {
            var matches = new List<Match>();
            for (int first = s.IndexOf(Representation); first >= 0;
                 first = s.IndexOf(Representation, first + 1)) {
                matches.Add(new Match(first));
            }

            return matches.ToArray();
        }

        override public bool HasMatch(string s) {
            return s.Contains(Representation);
        }
    }
}