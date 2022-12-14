using System;
using System.Linq;
using System.Xml.Linq;
using Microsoft.ProgramSynthesis;
using Microsoft.ProgramSynthesis.AST;
using Microsoft.ProgramSynthesis.Rules;
using EqFix.Lib.Logging;

// Based on: https://github.com/xurongchen/ASTSerialization
namespace EqFix.Lib.Transformer.StringLang
{
    public interface ASTSerializable
    {
        XElement serialize();
        // Deserialization needs a constructor of signature: #ctor(XElement)
    }

    public class ASTSerializer
    {
        // public static ASTSerializer instance = new ASTSerializer();
        private static Logger Log = Logger.Instance;

        private Grammar grammar;

        public ASTSerializer(Grammar _grammar)
        {
            grammar = _grammar;
        }
        
        public XElement PrintXML(ProgramNode node)
        {
            switch(node.GetType().FullName.ToString())
            {
                case "Microsoft.ProgramSynthesis.AST.TerminalNode":
                    throw(new TypeAccessException("TerminalNode is abstract node!"));
                case "Microsoft.ProgramSynthesis.AST.NonterminalNode":
                {
                    XElement xe = new XElement("NonterminalNode");
                    var nonterminalNode = node as NonterminalNode;
                    xe.SetAttributeValue("rule", nonterminalNode.Rule.ToString());
                    foreach(var child in nonterminalNode.Children)
                    {
                        xe.Add(PrintXML(child));
                    }
                    return xe;
                }
                case "Microsoft.ProgramSynthesis.AST.Hole":
                    throw(new NotImplementedException("Hole is not supported yet!"));
                case "Microsoft.ProgramSynthesis.AST.LiteralNode":
                {
                    XElement xe = new XElement("LiteralNode");
                    var literalNode = node as LiteralNode;
                    xe.SetAttributeValue("symbol",literalNode.Symbol.ToString());
                    fillXElement(literalNode.Value,xe);
                    return xe;
                }
                case "Microsoft.ProgramSynthesis.AST.VariableNode":
                {
                    XElement xe = new XElement("VariableNode");
                    var variableNode = node as VariableNode;
                    xe.SetAttributeValue("symbol",variableNode.Symbol.ToString());
                    return xe;
                }
                case "Microsoft.ProgramSynthesis.AST.LambdaNode":
                    throw(new NotImplementedException("LambdaNode is not supported yet!"));
                case "Microsoft.ProgramSynthesis.AST.LetNode":
                {
                    XElement xe = new XElement("LetNode");
                    var letNode = node as LetNode;
                    if (letNode.LetRule.Id == null) {
                        throw(new FormatException("LetNode must set 'id'!"));
                    }
                    xe.SetAttributeValue("rule", letNode.LetRule.Id);
                    xe.Add(PrintXML(letNode.Children[0]));
                    xe.Add(PrintXML(letNode.Children[1]));
                    return xe;
                }
                default:
                    throw(new TypeAccessException("Type "+node.GetType().FullName.ToString()+" is invalid."));
            }
        }

        public ProgramNode Parse(XElement xe)
        {
            switch(xe.Name.ToString())
            {
                case "TerminalNode":
                    throw(new FormatException("TerminalNode is abstract node!"));
                case "NonterminalNode":
                {
                    var ruleName = xe.Attribute("rule").Value;
                    var grammarRule = (NonterminalRule) grammar.Rule(ruleName);
                    var children = new ProgramNode[xe.Elements().Count()];
                    int index = 0;
                    foreach(var childXe in xe.Elements())
                    {
                        children[index] = Parse(childXe);
                        index++;
                    }
                    var node = new NonterminalNode(grammarRule,children);
                    return node;
                }
                case "Hole":
                    throw(new NotImplementedException("Hole is not supported yet!"));
                case "LiteralNode":
                {
                    var symbolName = xe.Attribute("symbol").Value;
                    var node = new LiteralNode(grammar.Symbol(symbolName),makeObject(xe));
                    return node;
                }
                case "VariableNode":
                {
                    var symbolName = xe.Attribute("symbol").Value;
                    var node = new VariableNode(grammar.Symbol(symbolName));
                    return node;
                }
                case "LambdaNode":
                    throw(new NotImplementedException("LambdaNode is not supported yet!"));
                case "LetNode":
                {
                    var ruleName = xe.Attribute("rule").Value;
                    var letRule = (LetRule) grammar.Rule(ruleName);
                    if (letRule == null) {
                        throw(new InvalidOperationException("Let rule not found: " + ruleName));
                    }
                    if (xe.Elements().Count() != 2) {
                        throw(new InvalidOperationException("LetNode should have only 2 children!"));
                    }
                    var children = new ProgramNode[2];
                    int index = 0;
                    foreach(var childXe in xe.Elements())
                    {
                        children[index] = Parse(childXe);
                        index++;
                    }
                    var node = new LetNode(letRule, children[0], children[1]);
                    return node;
                }
                default:
                    throw(new FormatException("Unknown XML node label!"));
            }
        }

        public object makeObject(XElement xe)
        {
            var typeName = xe.Attribute("type").Value;
            Type type = Type.GetType(typeName);
            if(type==null)
                throw(new TypeLoadException("Type " + typeName + " not found!"));
            Object obj = null;
            var objFullName = type.FullName.ToString();
            switch(objFullName)
            {
                case "System.Int32":
                    obj = Int32.Parse(xe.Value);
                    break;
                case "System.Int16":
                    obj = Int16.Parse(xe.Value);
                    break;
                case "System.Int64":
                    obj = Int64.Parse(xe.Value);
                    break;
                case "System.UInt16":
                    obj = UInt16.Parse(xe.Value);
                    break;
                case "System.UInt32":
                    obj = UInt32.Parse(xe.Value);
                    break;
                case "System.UInt64":
                    obj = UInt64.Parse(xe.Value);
                    break;
                case "System.Single":
                    obj = Single.Parse(xe.Value);
                    break;
                case "System.Double":
                    obj = Double.Parse(xe.Value);
                    break;
                case "System.Decimal":
                    obj = Decimal.Parse(xe.Value);
                    break;
                case "System.String":
                    obj = xe.Value;
                    break;
                case "System.Boolean":
                    obj = Boolean.Parse(xe.Value);
                    break;
                case "System.Char":
                    obj = Char.Parse(xe.Value);
                    break;
                case "System.Byte":
                    obj = Byte.Parse(xe.Value);
                    break;
                case "System.SByte":
                    obj = SByte.Parse(xe.Value);
                    break;
                default:
                {
                    // Support more common non-basic class
                    var ListNamePrefix = "System.Collections.Generic.List";
                    if(objFullName.Length>=ListNamePrefix.Length && objFullName.Substring(0,ListNamePrefix.Length)==ListNamePrefix)
                    {
                        try
                        {
                            obj = Activator.CreateInstance(type);
                        }
                        catch(MissingMethodException)
                        {
                            throw(new MissingMethodException("Deserialization error happened when processing List"));
                        }
                        dynamic lobj = obj;
                        if(lobj==null)
                            throw(new InvalidCastException("List object convert error."));
                        foreach(var child in xe.Elements("Attr-ListElement"))
                        {
                            dynamic childObj = makeObject(child);
                            lobj.Add(childObj);
                        }
                        break;
                    }
                    // class ProgramNode
                    if(objFullName==typeof(ProgramNode).FullName.ToString())
                    {
                        if(grammar == null)
                            throw(new NullReferenceException("Deserialization of ProgramNode must call 'grammar.Setter' first."));
                        obj = Parse(xe.FirstNode as XElement);
                        break;
                    }
                    // Other class type
                    if(type==null)
                        throw(new TypeAccessException("Type " + typeName + " was not found!"));
                    var serializedObjct = xe.FirstNode as XElement;
                    try
                    {
                        obj = Activator.CreateInstance(type,serializedObjct);
                    }
                    catch(MissingMethodException)
                    {
                        throw(new MissingMethodException(type.GetType().FullName.ToString()
                            + " does not implement the construction fuction with XElement for deserialization."));
                    }
                    break;
                }
            }
            return obj;
        }

        public void fillXElement(object obj,XElement xe)
        {
            var objFullName = obj.GetType().FullName.ToString();
            switch(objFullName)
            {
                case "System.Int32":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Int16":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Int64":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.UInt16":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.UInt32":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.UInt64":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Single":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Double":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Decimal":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.String":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj);
                    break;
                case "System.Boolean":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Char":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.Byte":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                case "System.SByte":
                    xe.SetAttributeValue("type",obj.GetType().FullName.ToString());
                    xe.Add(obj.ToString());
                    break;
                default:
                {
                    // Support more common non-basic class
                    var ListNamePrefix = "System.Collections.Generic.List";
                    if(objFullName.Length>=ListNamePrefix.Length && objFullName.Substring(0,ListNamePrefix.Length)==ListNamePrefix)
                    {
                        xe.SetAttributeValue("type",objFullName);
                        dynamic lobj = obj;
                        foreach(var item in lobj)
                        {
                            XElement child = new XElement("Attr-ListElement");
                            fillXElement(item,child);
                            xe.Add(child);
                        }
                        break;
                    }
                    // class ProgramNode
                    if(typeof(ProgramNode).IsInstanceOfType(obj))
                    {
                        xe.SetAttributeValue("type",typeof(ProgramNode).AssemblyQualifiedName.ToString());
                        xe.Add(PrintXML(obj as ProgramNode));
                        break;
                    }
                    // Other class type
                    var iobj = obj as ASTSerializable;
                    if(iobj==null)
                        throw(new NotSupportedException("Type " + obj.GetType().FullName.ToString() 
                            + " does not implement interface 'ASTSerializable'."));
                    xe.SetAttributeValue("type",iobj.GetType().AssemblyQualifiedName.ToString());
                    xe.Add(iobj.serialize());
                    break;
                }
            }
        }
    }
}