using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;
using System.Runtime.Serialization;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using EqFix.Lib.Transformer.StringLang;
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
    [Serializable]
    public sealed class STProgram : ISerializable
    {
        private static Logger Log = Logger.Instance;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="prog">The program node.</param>
        public STProgram(ProgramNode prog, int rank, string trans)
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
        /// Name of the <code>StringTransformer</code>.
        /// </summary>
        /// <value>The <code>StringTransformer</code> name.</value>
        private string _t;
        
        /// <summary>
        /// Rank of this candidate program.
        /// </summary>
        private int _rank;

        public string apply(STInput input)
        {
            Log.Debug("ST #{0}: {1}", _rank, ToString());

            var internalInput = StringTransformer.TransformInput(_t, input);
            var inputState = State.CreateForExecution(StringTransformer.GetInputSymbol(_t), internalInput);
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

        /// <summary>
        /// Serialization
        /// </summary>
        public void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            XElement xml;
            try
            {
                var serializer = StringTransformer.GetASTSerializer(_t);
                xml = serializer.PrintXML(_program);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.Message);
                Console.Error.WriteLine(ex.StackTrace);
                return;
            }

            info.AddValue("ast", xml.ToString(SaveOptions.DisableFormatting), typeof(string));
            info.AddValue("rank", _rank, typeof(int));
            info.AddValue("dsl", _t, typeof(string));
        }

        /// <summary>
        /// The special constructor for deserialization.
        /// </summary>
        public STProgram(SerializationInfo info, StreamingContext context)
        {
            var xml = (string) info.GetValue("ast", typeof(string));
            var rank = (int) info.GetValue("rank", typeof(int));
            var dsl = (string) info.GetValue("dsl", typeof(string));

            _rank = rank;
            _t = dsl;

            try
            {
                var serializer = StringTransformer.GetASTSerializer(_t);
                _program = serializer.Parse(XElement.Parse(xml));
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("Loading: " + xml);
                Console.Error.WriteLine(ex.Message);
                Console.Error.WriteLine(ex.StackTrace);
                Environment.Exit(1);
            }
        }
    }

    public static class StringTransformer
    {
        public static List<STProgram> Synthesize(string name, IEnumerable<STExample> examples, int k)
        {
            switch (name) {
                case "PROSE":
                    PROSETextTransformer.Init();
                    return PROSETextTransformer.Synthesize(examples, k);
                case "STLang":
                    STLangTransformer.Init();
                    return STLangTransformer.Synthesize(examples, k);
            }

            throw new InvalidOperationException("Unknown st name: " + name);
        }

        public static object TransformInput(string name, STInput input)
        {
            switch (name) {
                case "PROSE":
                    PROSETextTransformer.Init();
                    return PROSETextTransformer.TransformInput(input);
                case "STLang":
                    STLangTransformer.Init();
                    return STLangTransformer.TransformInput(input);
            }

            throw new InvalidOperationException("Unknown st name: " + name);
        }

        public static Symbol GetInputSymbol(string name)
        {
            switch (name) {
                case "PROSE":
                    PROSETextTransformer.Init();
                    return PROSETextTransformer.InputSymbol;
                case "STLang":
                    STLangTransformer.Init();
                    return STLangTransformer.InputSymbol;
            }

            throw new InvalidOperationException("Unknown st name: " + name);
        }

        public static Grammar GetGrammar(string name)
        {
            switch (name) {
                case "PROSE":
                    PROSETextTransformer.Init();
                    return PROSETextTransformer.DSL;
                case "STLang":
                    STLangTransformer.Init();
                    return STLangTransformer.DSL;
            }

            throw new InvalidOperationException("Unknown st name: " + name);
        }

        public static ASTSerializer GetASTSerializer(string name)
        {
            return new ASTSerializer(GetGrammar(name));
        }
    }
}