// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_doc_urlify/src/acdk/doc/urlify/urlifiy_acdk_doc.cpp,v 1.20 2004/11/21 21:56:33 kommer Exp $
//
// $Log: urlifiy_acdk_doc.cpp,v $
// Revision 1.20  2004/11/21 21:56:33  kommer
// changed copyright date to 2005
//
// Revision 1.19  2004/11/19 08:29:31  kommer
// panta rei
//
// Revision 1.18  2003/10/12 13:10:21  kommer
// panta rei
//
// Revision 1.17  2003/06/28 10:07:23  kommer
// panta rei
//
// Revision 1.16  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.15  2003/06/19 13:17:24  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.14.2.2  2003/06/03 12:26:27  kommer
// panta rei
//
// Revision 1.14.2.1  2003/05/13 13:47:23  kommer
// panta rei
//
// Revision 1.14  2002/09/09 13:39:35  kommer
// dos2unix
//
// Revision 1.13  2002/07/12 09:55:58  kommer
// panta rei
//
// Revision 1.12  2001/12/16 22:28:07  kommer
// panta rei
//
// Revision 1.11  2001/12/09 02:40:10  kommer
// introduced IN() for method parameter
//
// Revision 1.10  2001/12/01 18:46:53  kommer
// panta rei
//
// Revision 1.9  2001/09/30 17:44:15  kommer
// panta rei
//
// Revision 1.8  2001/09/02 10:06:06  kommer
// panta rei
//
// Revision 1.7  2001/08/12 15:25:00  kommer
// dos2unix
//
// Revision 1.6  2001/08/02 15:13:14  kommer
// typo and struct
//
// Revision 1.5  2001/08/02 15:12:08  kommer
// *** empty log message ***
//
// Revision 1.4  2001/07/29 12:59:23  kommer
// ms compaitility
//
// Revision 1.3  2001/07/28 23:07:27  kommer
// panta rei
//
// Revision 1.2  2001/05/18 12:04:06  kommer
// panta rei
//
// Revision 1.1  2001/03/03 20:11:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:21  roger
// initial acdk sources
//
// Revision 1.3  2000/06/03 18:48:32  roger
// panta rei
//
// Revision 1.2  2000/05/08 21:37:57  roger
// panta rei
//
// Revision 1.1  2000/04/25 11:57:26  roger
// initial revision
//
// Revision 1.9  2000/02/14 22:09:33  roger
// new ACDK_DECL_CLASS
//
// Revision 1.8  2000/02/14 09:29:37  roger
// panta rei
//
// Revision 1.7  2000/02/08 16:29:39  roger
// RefHolder and Arrays changed
//
// Revision 1.6  1999/10/25 19:26:51  roger
// panta rei
//
// Revision 1.5  1999/10/24 15:18:52  roger
// panta rei
//
// Revision 1.4  1999/10/24 11:57:02  roger
// panta rei
//
// Revision 1.3  1999/10/23 15:28:16  roger
// panta rei
//
// Revision 1.2  1999/10/22 19:14:12  roger
// panta rei
//
// Revision 1.1  1999/10/22 19:04:15  roger
// initial revision
//

#include <acdk.h>
#include "HTMLStreamTokenizer.h"
#include <acdk/lang/System.h>
#include <acdk/io/File.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/BufferedWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/io/ByteToCharReader.h>

#include <acdk/locale/ByteAsciiEncoding.h>

namespace acdk {
namespace doc {
namespace urlify {

using namespace acdk::lang;
using namespace acdk::io;
using namespace acdk::util;

ACDK_DECL_CLASS(Urlify);


enum LogLevel
{
  Trace,
  Warn,
  Err
};

#define LOG(ll, stream) log(ll, SBSTR(stream))

void log(LogLevel ll, IN(RString) str)
{
  if (ll == Trace)
    System::out->print("  * ");
  else if (ll == Warn)
    System::out->print(" ** ");
  else
    System::out->print("*** ");
  System::out->println(str);
}

class Urlify 
: public acdk::lang::Object 
{
  RString _symbolsDir;
  RString _outputDir;
  RString _urlDir;
  RStringArray _args;
  bool _symbolsLoaded;
  RHashMap _symbols;
public:
  Urlify(IN(RString) sdir, IN(RString) urldir, IN(RString) odir, IN(RObjectArrayImpl<RString>) args)
  : Object(),
    _symbolsDir(File(sdir).getCanonicalPath()),
    _outputDir(File(odir).getCanonicalPath()),
    _urlDir(File(urldir).getCanonicalPath()),
    _args(args),
    _symbolsLoaded(false),
    _symbols(Nil)
  {
    _symbols = new HashMap();
    LOG(Trace, "SymbolDirectory: " << _symbolsDir << "\n"
        "URLDirectory: " << _urlDir << "\n"
        "OutputDir: " << _outputDir << "\n"
        "Files: " << _args->toString());
  }
  int go();
  int goRecursive(IN(RString) outputdir);
  void loadSymbtable();
  void patchFile(IN(RString) fname);
  void patchFile(IN(RString) ofile, IN(RString) destdir);
  bool patchFile(HTMLStreamTokenizer& in, CharWriter& out, IN(RString) urlsource);
  RString getUrl(IN(RString) targetdir, IN(RString) targetfile);
  static int acdkmain(RStringArray args);
  static void usage();
};

//static 
void 
Urlify::usage()
{
  System::out->println("patch_acdk_doc symboldir urldir destdir sourcefiles\n"
                       );
}
#define DOUT(msg) System::out->println(msg)


void 
Urlify::loadSymbtable()
{
  if (_symbolsLoaded == true)
    return;

  File dir(_symbolsDir);
  LOG(Trace, "load symbols in: " << dir.getAbsolutePath());
  RObjectArrayImpl<RString> files = dir.list();
  for (int i = 0; i < files->length(); i++) 
  {
    RString cf = files[i];
    bool isStruct = false;
    if ((cf->startsWith("class_") == false && cf->startsWith("struct_") == false) || 
        cf->endsWith(".html") == false || 
        cf->indexOf("-members") != -1 || 
        cf->indexOf("-include") != -1) 
      continue;
    cf = cf->replace(".html", "");
    if (cf->startsWith("class_") == true)
    {
      cf = cf->substr(String("class_").length())->replace("__", "::");
    }
    else
    {
      cf = cf->substr(String("struct_").length())->replace("__", "::");
      isStruct = true;
    }
    if (cf->equals("RString") == false) 
    {
      _symbols->put(&cf, &files[i]);
      RString cfa = cf + "Array";
      _symbols->put(&cfa, &files[i]);
      //System::out->println(cf + ": " + files[i]);
    }
    if (cf->indexOf("::") != -1) 
    {
      RString cn = cf->substr(cf->lastIndexOf("::") + 2);
      RString ns = cf->substr(0, cf->lastIndexOf("::"));
      if (cn->equals("RString") == true)
        continue;
      if (_symbols->containsKey(&cn) == false)
        _symbols->put(&cn, &files[i]);
      RString cfa = cf + "Array";
      if (_symbols->containsKey(&cfa) == false)
        _symbols->put(&cfa, &files[i]);
      if (isStruct == false)
      {
        RString ts = "R" + cn;
        if (_symbols->containsKey(&ts) == false)
          _symbols->put(&ts, &files[i]);
        cfa = "R" + cn + "Array";
        if (_symbols->containsKey(&cfa) == false)
          _symbols->put(&cfa, &files[i]);
      
        _symbols->put(&RString(ns + "::R" + cn), &files[i]);
        cfa = ns + "::R" + cn + "Array";
        _symbols->put(&cfa, &files[i]);
      }
    } else
      _symbols->put(&cf, &RString(files[i]));
  }
  _symbolsLoaded = true;
  DOUT("Symbols loaded");
}


RString 
mergePath(IN(RString) dir, IN(RString) file)
{
  if (file->length() == 0)
    return dir;
  if (dir->length() == 0)
    return file;
  if (dir->endsWith("/") == true || file->startsWith("/") || dir->endsWith("\\") == true || file->startsWith("\\"))
    return dir + file;
  return dir + File::separator() + file;
}


void
Urlify::patchFile(IN(RString) ofile, IN(RString) destdir)
{
  try {
    FileReader tf(ofile);
    File tofile(ofile);
    RString fn = tofile.getName();
    RString sourceurl = tofile.getParent();
    RString dstfilename = mergePath(destdir, fn);
    LOG(Trace, "Writing file: " << dstfilename);
    FileWriter fout(dstfilename);
    BufferedWriter bufout(&fout, 4000);
    
    CharToByteWriter out(&bufout, new acdk::locale::ByteAsciiEncoder(Nil));
    //PrintWriter pw(&out);
    //
    ByteToCharReader b2cr(&tf, new acdk::locale::ByteAsciiDecoder(Nil));
    HTMLStreamTokenizer in(&b2cr);
    patchFile(in, out, dstfilename);
    bufout.flush();
    tf.close();
    out.close();
  } catch (RThrowable ex) {
    ex->printStackTrace();
    System::err->println(ex->getMessage());
    return;
  }
}

void
Urlify::patchFile(IN(RString) fname) 
{
  LOG(Trace, "urlify: " << fname);
  RString nfname = fname + ".new.html";
  try {
    FileReader tf(fname);
    ByteToCharReader b2cr(&tf, new acdk::locale::ByteAsciiDecoder(Nil));
    FileWriter fout(nfname);
    BufferedWriter bufout(&fout, 4000);
    CharToByteWriter out(&bufout, new acdk::locale::ByteAsciiEncoder(Nil));
    HTMLStreamTokenizer in(&b2cr);
    
    if (patchFile(in, out, File(fname).getParentFile()->getCanonicalPath()) == false) 
    {
      bufout.flush();
      tf.close();
      out.close();
      File f(nfname);
      if (f.deleteFile() == false)
        LOG(Err, "Cannot deleteFile:" << f.getAbsolutePath());
      return;
    }
    tf.close();
    bufout.flush();
    out.close();
  } catch (RThrowable ex) {
    ex->printStackTrace();
    LOG(Err, "Exception in patchFile: " << ex->getMessage());
    return;
  }
  File f(fname);
  if (f.deleteFile() == false) {
    LOG(Err, "Cannot deleteFile:" << f.getAbsolutePath());
    return;
  }
  File nf(nfname);
  if (nf.renameTo(&f) == false)
    LOG(Err, "Cannot remove File from:" << nf.getAbsolutePath() << " to " << f.getAbsolutePath());
}


void dprint(IN(RString) txt)
{
  System::out->println(txt);
}
//#define DPRINT(msg) dprint(msg)
#define DPRINT(msg) 

bool 
Urlify::patchFile(HTMLStreamTokenizer& in, CharWriter& out, IN(RString) urlsource)
{
  bool fileChanged = false;
  in.wantWhiteSpace(true);
  in.wantNewline(true);
  in.wantComments(true);
  int c = 0;
  bool inAnchor = false;
  bool inNoURL = false;
  while (in.eof() == false) 
  {
    c = in.nextToken();
    if (c == StreamTokenizer::TT_EOF)
      return fileChanged;
    if (c == HTMLStreamTokenizer::TT_TAG_A) {
      if (in.sval->indexOf("href=\"") != -1)
        inAnchor = true;
      DPRINT("TT_TAG_A: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (c == HTMLStreamTokenizer::TT_TAG_A_END) {
      inAnchor = false;
      DPRINT("TT_TAG_A_END: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (c == HTMLStreamTokenizer::TT_TAG_H) {
      inNoURL = true;
      DPRINT("TT_TAG_H: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (c == HTMLStreamTokenizer::TT_TAG_H_END) {
      inNoURL = false;
      DPRINT("TT_TAG_H_END: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (inAnchor == true || inNoURL == true) {
      DPRINT("InAnchor: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (c == StreamTokenizer::TT_NUMBER) {
      out.writeString(in.sval);
      continue;
    }
    if (c == StreamTokenizer::TT_STRING) {
      DPRINT("TT_STRING: " + in.sval);
      out.writeString(in.sval);
      continue;
    }
    if (c == StreamTokenizer::TT_WORD) {
      DPRINT("TT_WORD: " + in.sval);
      if (in.sval->equals("RObject") == true || in.sval->equals("Object") == true) {
        in.sval = "acdk::lang::Object";
      }
      if (in.sval->equals("Constructor") == true) {
        //don't do it 
      } else if (_symbols->containsKey(&in.sval) == true) {
        
        RString val = File(_symbolsDir, RString(_symbols->get(&in.sval))).getCanonicalPath();
        
        //RString val = RString(_symbols->get(in.sval));
        RString sdir = urlsource;
        File f(urlsource);
        if (f.isFile() == true)
          sdir = f.getParent();
        RString trelurl = getUrl(sdir, val);
        RString url = "<a href=\"" + trelurl + "\">" + in.sval + "</a>";
        LOG(Trace, "[" <<  in.lineno() << "]" << in.sval << "=[" << val << "]; trel[" << trelurl << "]");
        out.writeString(url);
        fileChanged = true;
      } else {
        out.writeString(in.sval);
      }
      continue;
    }
    out.writeString(in.sval);
  }
  return true;
}


int 
Urlify::go()
{
  loadSymbtable();
  
  if (true || _args->length() < 5) {
    File f(_outputDir);
    if (f.exists() == false) {
      DOUT(_outputDir + " does not exists");
      return 1;
    }
    RStringArray files = f.list();
    LOG(Trace, "File: " << files);
    if (files == Nil) {
      DOUT(_outputDir + " is empty");
      return 1;
    }
    LOG(Trace, "Processing in dir: [" << _outputDir << "]: " << files->length());
    for (int j = 0; j < files->length(); j++) 
    {
      try {
        RString fname = files[j];
        //LOG(Trace, "patch file1: " << fname);
        if (fname->equals(".") == true || fname->equals("..") == true)
          continue;
        //LOG(Trace, "patch file1a: " << fname << "indexOf html: " <<  fname->indexOf(".html") << " indexof html: " << fname->indexOf(".htm"));
        if (fname->indexOf(".html") == -1 && fname->indexOf(".htm") == -1)
          continue; 
        //LOG(Trace, "patch file2: " << mergePath(f.getAbsolutePath(), fname));
        patchFile(mergePath(f.getAbsolutePath(), fname));  
      } catch (RThrowable ex) {
        LOG(Err, "Throwable in go(): " << ex->getMessage());
      }
    }
    return 0;
  }
  return 0;
  /*
  for (int i = 4; i < _args->length(); i++) {
    File f(_args[i]);
    if (f.isDirectory() == true) {
      System::out->println("scan: " + f.getCanonicalPath());
      RObjectArrayImpl<RString> files = f.list();
      for (int j = 0; j < files->length(); j++) {
        RString fname = files[j];
        if (fname->equals(RString(".")) == true || fname->equals(RString("..")) == true || 
            (fname->indexOf(".html") == -1 && fname->indexOf(".htm") == -1))
          continue;  
        patchFile(mergePath(f.getAbsolutePath(), fname), _outputDir);  
      }
      continue;
    } else {
      patchFile(_args[i], _outputDir);
    }
  }
  return 0;*/
}



int
Urlify::goRecursive(IN(RString) outputdir)
{
  _outputDir = outputdir;
  DOUT("outputdir: " + outputdir);
  go();
  File f(outputdir);
  DOUT("outputdir fq: " + f.getCanonicalPath());
  RFileArray sa = f.listFiles();
  //LOG(Trace, "Files: " << sa);
  if (sa == Nil) {
      DOUT("outputdir doesn't exists or contains no files: " + outputdir);
      return 1;
  }
  for (int i = 0; i < sa->length(); ++i) 
  {
    if (sa[i]->isDirectory() == true)
      goRecursive(outputdir + "/" + sa[i]->getName());
  }
  return 0;
}


RString 
getRelURL2(IN(RString) targetdir, IN(RString) sourcedir)
{
  RString target = targetdir->replace('\\', '/');
  RString source = sourcedir->replace('\\', '/');
  if (target->equals(source) == true)
    return "";
  int minlen = Math::min(target->length(), source->length());
  int i = 0;
  for (; i < minlen; i++) {
    if (target->charAt(i) != source->charAt(i))
      break;
  }
  target = target->substr(i);
  source = source->substr(i);
  if (target->length() == 0 && source->length() == 0)
    return "";
  int upcount = source->elementCount('/');
  if (source->length() > 0)
    ++upcount;
  StringBuffer sb(source->length() + 10);

  while (--upcount > 0) 
    sb.append("../");
  if (target->length() > 0 && target->charAt(0) == '/')
    target = target->substr(1);
  RString erg = sb.append(target)->toString();
  // System::out->println("RELURL: [" + source + " = " + target + "] => " + erg);
  return erg;
}
 

RString 
Urlify::getUrl(IN(RString) targetdir, IN(RString) targetfile)
{
  return mergePath(getRelURL2(_urlDir, targetdir), File(targetfile).getName());
}


void
testCtrlDInFile()
{
  RString fname = "./test.txt";
  FileReader rin(fname);
  int c;
  while ((c = rin.read()) != -1) {
    System::out->print((char)c);    
     System::out->flush();
  }
}


//static 
int 
Urlify::acdkmain(RStringArray args)
{
  /*
  //testCtrlDInFile();
  RString str;
  const char* ptr;
  str = getRelURL2("bla\\blub\\dir", "bla\\blub\\dir");
  ptr = str->c_str(); // ""
  str = getRelURL2("bla\\blub\\dir", "bla\\blub2\\dir");
  ptr = str->c_str(); // "..\\blub\\dir"
  */
  try {
    if (args->length() < 4) {
      System::err->println("Usage: urlyfi [SymbolsDir] [URLDir] [OutputDir]\n");
      return 1;
    }
    int startidx = 1;
    if (args[startidx]->equals("-r") == false)  {
      RUrlify urlify = new Urlify(args[startidx], args[startidx + 1], args[startidx+ 2], args);
      return urlify->go();
    }
    ++startidx;
    System::out->println(args[startidx] + " | " + args[startidx + 1]  + " | " + args[startidx + 2]);
    RUrlify urlify = new Urlify(args[startidx], args[startidx + 1], args[startidx + 2], args);
    return urlify->goRecursive(args[startidx+ 2]);
  } catch (RThrowable ex) {
    ex->printStackTrace();
    LOG(Err, "Throwable in main: " << ex->getMessage());
    
  }
  return 1;
}



} // namespace urlify 
} // namespace doc
} //namespace acdk



int
main(int argc, char* argv[], char** envptr)
{
  int erg = acdk::lang::System::main(acdk::doc::urlify::Urlify::acdkmain, argc, argv, envptr);
  return erg;  

}
