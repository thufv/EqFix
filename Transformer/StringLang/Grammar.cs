using System.IO;
using System.Reflection;

namespace EqFix.Lib.Transformer.StringLang {
    public static class GrammarText
    {
        public static string Get()
        {
            var assembly = typeof(GrammarText).GetTypeInfo().Assembly;            
            using (var stream = assembly.GetManifestResourceStream("EqFix.Lib.Transformer.StringLang.StringLang.grammar"))
            using (var reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
        }
    }
}
