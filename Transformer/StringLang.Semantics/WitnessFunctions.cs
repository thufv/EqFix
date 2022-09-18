using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.Rules;
using Microsoft.ProgramSynthesis.Specifications;
using Microsoft.ProgramSynthesis.Learning;
using Microsoft.ProgramSynthesis.Utils;

using MyLogger = EqFix.Lib.Logging.Logger;

namespace EqFix.Lib.Transformer.StringLang {
    public class WitnessFunctions : DomainLearningLogic {
        private static MyLogger Log = MyLogger.Instance;

        private static void ShowList(List<int> list) {
            Log.Fine("Candidates: " + String.Join(", ", list.Select(x => x.ToString())));
        }

        private static void ShowList(List<Record<int?, int?>> list) {
            Log.Fine("Candidates: " + String.Join(", ", list.Select(x => x.ToString())));
        }

        private static void ShowList(List<Record<int, int>> list) {
            Log.Fine("Candidates: " + String.Join(", ", list.Select(x => x.ToString())));
        }

        private static void ShowList(List<string> list) {
            Log.Fine("Candidates: " + String.Join(", ", list));
        }

        private static void ShowList(List<Token> list) {
            Log.Fine("Candidates: " + String.Join(", ", list));
        }

        private static Token[] UsefulTokens = new[] {
            new TokenChar('$', 2),
            new TokenChar('{', 1.5),
            new TokenChar('}', 2),
            new TokenChar('(', 1.5),
            new TokenChar(')', 2),
            new TokenChar('^', 1.5),
            new TokenChar('\\', 1)
        };

        public WitnessFunctions(Grammar grammar) : base(grammar) { }

        [WitnessFunction(nameof(Semantics.Concat), 0)]
        public DisjunctiveExamplesSpec GenConcatHead(GrammarRule rule, ExampleSpec spec) {
            // Find all s1 s.t. Concat(s1, s2) = s.
            Log.Fine("GenConcatHead with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.Examples) {
                State inputState = example.Key;
                var s = example.Value as string;
                var candidates = new List<string>();

                for (int len = 1; len < s.Length; len++) {
                    candidates.Add(s.Substring(0, len));
                }

                ShowList(candidates);
                if (candidates.Count == 0) { 
                    return null;
                }
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.Concat), 1, DependsOnParameters = new []{0})]
        public ExampleSpec GenConcatTail(GrammarRule rule, ExampleSpec spec, ExampleSpec headSpec) {
            // Find the s2 s.t. Concat(s1, s2) = s, given s1.
            Log.Fine("GenConcatTail with {0}, dep {1}", spec, headSpec);

            var results = new Dictionary<State, object>();

            foreach (var example in spec.Examples) {
                State inputState = example.Key;
                var s = example.Value as string;
                var candidates = new List<string>();
                
                var s1 = headSpec.Examples[inputState] as string;
                if (s == null || s1 == null || !s.StartsWith(s1)) {
                    return null;
                }

                // ASSERT s1 is a prefix of s.
                var s2 = s.Substring(s1.Length);
                candidates.Add(s2);
                ShowList(candidates);
                results[inputState] = s2;
            }

            return new ExampleSpec(results);
        }

        [WitnessFunction(nameof(Semantics.ConstStr), 0)]
        public DisjunctiveExamplesSpec GenConstStr(GrammarRule rule, DisjunctiveExamplesSpec spec) {
            // Find all s0 s.t. ConstStr(s0) = s.
            Log.Fine("GenConstStr with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var candidates = new List<string>();

                foreach (string s in example.Value) {
                    candidates.Add(s);
                }

                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction("LetSubStr", 0)]
        public DisjunctiveExamplesSpec GenLetSubStr(LetRule rule, DisjunctiveExamplesSpec spec) {
            // Find all x s.t. SubStr(x, posPair) = s,
            // where x = v[i] for some i.
            // NOTE cases where `x = s` should be excluded, which is covered with `JustChooseInput`.
            Log.Fine("GenLetSubStr with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var chooseInput = rule.Value.DependentRules.First(); // ChooseInput
                var v = inputState[chooseInput.Body[0]] as string[];
                var candidates = new List<string>();

                foreach (string s in example.Value) {
                    for (int i = 0; i < v.Length; i++) {
                        if (v[i].Contains(s) && v[i] != s) {
                            candidates.Add(v[i]);
                        }
                    }
                }

                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.JustChooseInput), 1)]
        public DisjunctiveExamplesSpec GenJustChooseInput(GrammarRule rule, 
                                                          DisjunctiveExamplesSpec spec) {
            // Find all i s.t. ChooseInput(v, i) = x.
            // Log.Fine("GenChooseInput with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var v = inputState[rule.Body[0]] as string[];
                var candidates = new List<int>();

                foreach (string s in example.Value) {
                    for (int i = 0; i < v.Length; i++) {
                        if (v[i] == s) {
                            candidates.Add(i);
                        }
                    }
                }

                // ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.ChooseInput), 1)]
        public DisjunctiveExamplesSpec GenChooseInput(GrammarRule rule, 
                                                      DisjunctiveExamplesSpec spec) {
            // Find all i s.t. ChooseInput(v, i) = x.
            // Log.Fine("GenChooseInput with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var v = inputState[rule.Body[0]] as string[];
                var candidates = new List<int>();

                foreach (string s in example.Value) {
                    for (int i = 0; i < v.Length; i++) {
                        if (v[i] == s) {
                            candidates.Add(i);
                        }
                    }
                }

                // ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.SubStr), 1)]
        public DisjunctiveExamplesSpec GenSubStrPosPair(GrammarRule rule, DisjunctiveExamplesSpec spec) {
            // Find all posPair s.t. SubStr(x, posPair) = s.
            // ASSUME x contains s.
            Log.Fine("GenSubStrPosPair with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var x = inputState[rule.Body[0]] as string;
                var n = x.Length;
                var candidates = new List<Record<int?, int?>>();

                foreach (string s in example.Value) {
                    for (int left = x.IndexOf(s); left >= 0; ) {
                        var posPair = Record.Create((int?) left, (int?) left + s.Length - 1);
                        candidates.Add(posPair);

                        if (left + 1 < n) {
                            left = x.IndexOf(s, left + 1);
                        } else break;
                    }
                }

                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.AbsPos), 1)]
        public DisjunctiveExamplesSpec GenAbsPosK(GrammarRule rule, DisjunctiveExamplesSpec spec) {
            // Find all k s.t. AbsPos(x, k) = pos.
            Log.Fine("GenAbsPosK with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();
            
            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var x = inputState[rule.Body[0]] as string;
                var n = x.Length;
                var candidates = new List<int>();

                foreach (int pos in example.Value) {
                    candidates.Add(pos);
                    var negPos = pos - n; // n + negPos = pos
                    if (negPos < 0) {
                        candidates.Add(negPos);
                    }
                }

                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        private static int bound = 1; // only allow offset be -1, 0, 1

        [WitnessFunction(nameof(Semantics.RelPos), 1)]
        public DisjunctiveExamplesSpec GenRelPosToken(GrammarRule rule, 
                                                      DisjunctiveExamplesSpec spec) {
            // Find all t s.t. RelPos(x, t, (j, k)) = pos.
            Log.Fine("GenRelPosToken with {0}", spec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var x = inputState[rule.Body[0]] as string;
                var candidates = new List<Token>();

                foreach (int pos in example.Value) {
                    candidates.AddRange(UsefulTokens.Where(t => t.BoundedHasMatch(x, bound, pos)));
                }
                
                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }

        [WitnessFunction(nameof(Semantics.RelPos), 2, DependsOnParameters = new[] {1})]
        public DisjunctiveExamplesSpec GenRelPosIndex(GrammarRule rule, 
                                                      DisjunctiveExamplesSpec spec,
                                                      ExampleSpec tokenSpec) {
            // Find all (j, k) s.t. RelPos(x, t, (j, k)) = pos, given t.
            // ASSUME t has match in x.
            Log.Fine("GenRelPosIndex with {0} dep {1}", spec, tokenSpec);

            var results = new Dictionary<State, IEnumerable<object>>();

            foreach (var example in spec.DisjunctiveExamples) {
                State inputState = example.Key;
                var x = inputState[rule.Body[0]] as string;
                var t = tokenSpec.Examples[inputState] as Token;
                var matches = t.FindMatches(x);
                int n = matches.Length;
                var candidates = new List<Record<int, int>>();
                
                foreach (int pos in example.Value) {
                    for (int j = 1; j <= n; j++) { // the j-th occurence
                        var match = matches[j - 1];
                        if (pos <= match.First && match.First - pos <= bound) {
                            candidates.Add(Record.Create(j, pos - match.First));
                            candidates.Add(Record.Create(j - n - 1, pos - match.First));
                        } else if (pos > match.Last && pos - match.Last <= bound) {
                            candidates.Add(Record.Create(j, pos - match.Last));
                            candidates.Add(Record.Create(j - n - 1, pos - match.Last));
                        }
                    }
                }

                ShowList(candidates);
                if (candidates.Count == 0) return null;
                results[inputState] = candidates.Cast<object>();
            }

            return new DisjunctiveExamplesSpec(results);
        }
    }
}