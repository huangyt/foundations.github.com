/* just include the orignal aal defintion file */
{
  acdk.io.File aalasd = new acdk.io.File(acdk.lang.System.getAcdkHome() + "/aal/cfgs/aal/aal.asd");
  String s = aalasd.getReader().getCharReader().readString();
  acdk.aci.parser.SyntaxParseNode.parseSyntaxText(compiler, s, aalasd.getCanonicalPath());
}

