/*
  Literals definitions
*/
{
  acdk.aci.parser.WhiteSpaceParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("WS").getScannerTokenId());
  
  acdk.aci.parser.LineCommentParseNodeParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("LINECOMMENT").getScannerTokenId());
  
  acdk.aci.parser.BlockCommentParseNode.registerParseNode(compiler);
  compiler.getParseEnv().addIgnoreToken(compiler.getParseNode("BLOCKCOMMENT").getScannerTokenId());
  
  acdk.aci.parser.IdentifierParseNode.registerParseNode(compiler);
  compiler.registerTerminal(new acdk.aci.parser.StringTerminalParseNode("STRING_LITERAL"));
  
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("DEC_LITERAL", "^([0-9])+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("SIGNED_DEC_LITERAL", "^([\\+\\-])?([0-9])+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("HEX_LITERAL", "^0[xX][0-9a-fA-F]+[LISB]?"));
  compiler.registerTerminal(new acdk.aci.parser.DecimalLiteralParseNode("OCTAL_LITERAL", "^0[0-7]+"));
  compiler.registerTerminal(new acdk.aci.parser.FloatLiteralParseNode("FLOAT_LITERAL", "^([\\+\\-])?([0-9])+\\.([0-9])+"));
  
}

CodeText: Literal*;

Literal
: BOOLEAN_LITERAL 
| DEC_LITERAL 
| SIGNED_DEC_LITERAL 
| HEX_LITERAL 
| FLOAT_LITERAL 
| OCTAL_LITERAL
| STRING_LITERAL
;
