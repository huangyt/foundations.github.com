// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Runtime_Test.cpp,v 1.21 2005/04/18 19:47:50 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Runtime.h>
#include <acdk/io/StringWriter.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Runtime_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( timout )
  DECLARE_TEST( signalEnventHandler )
  DECLARE_TEST( stopResume )
END_DECLARE_TEST( Runtime_Test  )

BEGIN_DEFINE_TEST( Runtime_Test )
  ADD_TEST( Runtime_Test, standard ) 
  ADD_TEST( Runtime_Test, timout ) 
  ADD_TEST( Runtime_Test, signalEnventHandler ) 
  ADD_TEST( Runtime_Test, stopResume )
END_DEFINE_TEST( Runtime_Test )

using namespace acdk::lang;


ACDK_DECL_CLASS(ProcessOutReaderThread);

class ProcessOutReaderThread
: extends ::acdk::lang::Thread
{
public:
  StringBuffer _sb;
  ::acdk::io::RCharReader _in;
  ::acdk::io::RCharWriter _out;
  
  ProcessOutReaderThread(IN(::acdk::io::RCharReader) in, IN(::acdk::io::RCharWriter) out = Nil)
  : _in(in)
  , _out(out)
  {
  }
  RString toString() { return _sb.toString(); }
  void run()
  {
    try  {
      int buf;
      while ((buf = _in->readChar()) != -1)  
      {
        _sb.append((uc2char)buf);
        if (_out != Nil)
        {
          _out->writeChar((ucchar)buf);
          if (buf == '\n')
            _out->flush();
        }
      } 
    } catch (::acdk::io::RIOException ex) {
      //System::out->println("PORTH ex: " + ex->getMessage());
    }
    //sys::coreout << "PORTH exit" << sys::eofl;
  }
};


void 
Runtime_Test::standard()
{
  RString acdkHome = System::getAcdkHome();
#if defined(ACDK_OS_WIN32)
  //RString cmd = "cmd /c \"dir \\\"*.*\\\" \\\"*.cpp\\\"\"";
  //RString cmd = "cmd /c \"dir \\\"*.*\\\"\"";
  RString cmd = "cmd /c \"dir *.*\"";
#else
  //RString cmd = "sh -e \"ls \"";
  RString cmd = "ls  " + acdkHome;
#endif
  System::out->println("starting: " + cmd);
  RProcess process = Runtime::exec(cmd);
  
  //::acdk::io::StringWriter output;
  //::acdk::io::StringWriter errput;
  RProcessOutReaderThread outreader = new ProcessOutReaderThread(process->getOutputStream()->getCharReader());
  RProcessOutReaderThread errreader = new ProcessOutReaderThread(process->getErrorStream()->getCharReader());
  outreader->start();
  errreader->start();
  Thread::sleep(300);
  process->waitFor();
  outreader->join();
  errreader->join();
  
  //process->getOutputStream()->trans(output.getWriter());
  //process->getErrorStream()->trans(errput.getWriter());
  
  RString s = outreader->toString();
  RString es = errreader->toString();
  System::out->println("Exec " + cmd + " std: " + s);
  System::out->println("Exec " + cmd + " err: " + es);

}

void
Runtime_Test::timout()
{
  
  RString cfgInterpreter = System::getAcdkHome() + "/bin/acdkcfgscript";
#if defined(_MSC_VER)
  cfgInterpreter = cfgInterpreter + "_d";
#endif
  RString cfgscript = " -e \"while(true) { out.println((new acdk.util.SysDate()).toString()); Thread.sleep(250); }\"";
  RString cmd = cfgInterpreter + cfgscript;
  System::out->println("start cmd: " + cmd);
  /*
#if defined(ACDK_OS_WIN32)
  RString cmd = "cmd /c pause";
#else
  RString cmd = "/bin/sh -c \"while true; do sleep 1; echo ..; done\"";
#endif
  System::out->println("starting: " + cmd);
*/
  RProcess process = Runtime::exec(cmd);
  
  //::acdk::io::StringWriter output;
  //::acdk::io::StringWriter errput;
  RProcessOutReaderThread outreader = new ProcessOutReaderThread(process->getOutputStream()->getCharReader());
  RProcessOutReaderThread errreader = new ProcessOutReaderThread(process->getErrorStream()->getCharReader());
  outreader->start();
  errreader->start();
  Thread::sleep(300);
  int ret = process->waitFor(3000); // process should never stop
  //ret = process->waitFor(500);
  System::out->println(SBSTR("process->waitFor(3000): " << ret));
  process->destroy();
  outreader->join();
  errreader->join();
  testAssert(ret == ProcessStillRunning);
  ret = process->waitFor();
  
  //process->getOutputStream()->trans(output.getWriter());
  //process->getErrorStream()->trans(errput.getWriter());
  
  RString s = outreader->toString();
  RString es = errreader->toString();
  System::out->println("Exec " + cmd + " std: " + s);
  System::out->println("Exec " + cmd + " err: " + es);
  System::out->println(SBSTR("exit status: " << ret));
  
}

ACDK_DECL_CLASS(TestSignalEventHandler);

class TestSignalEventHandler
: extends ::acdk::lang::Object
, implements ::acdk::lang::SignalEventHandler
{
public:
  bool handleEvent(SignalEventType event)
  {
    if (event == CtrlC_Event)
      System::out->println("CtrlC_Event");
    else if (event == CtrlBreak_Event)
      System::out->println("CtrlBreak_Event");
    return true;
  }
  bool equals(IN(RSignalEventHandler) other) 
  {
    return this == other->_getObjectPtr();
  }
};

void Runtime_Test::signalEnventHandler()
{

  TestSignalEventHandler tseh;
  Runtime::registerSignalEventHandler(&tseh);
  bool interactive = false;
  if (interactive == true)
  {
    System::out->println("type in something");
    System::in->readLine();
  }

  Runtime::unregisterSignalEventHandler(&tseh);

}

void
Runtime_Test::stopResume()
{

  RString cfgInterpreter = System::getAcdkHome() + "/bin/acdkcfgscript";
#if defined(_MSC_VER)
  cfgInterpreter = cfgInterpreter + "_d";
#endif
  RString cfgscript = " -e \"for (int i = 0; i < 10; ++i) { out.println((new acdk.util.SysDate()).toString()); Thread.sleep(250); }\"";
  System::out->println("start cmd: " + cfgInterpreter + cfgscript);
  RProcess process = Runtime::exec(cfgInterpreter + cfgscript);
  RProcessOutReaderThread outreader = new ProcessOutReaderThread(process->getOutputStream()->getCharReader());
  RProcessOutReaderThread errreader = new ProcessOutReaderThread(process->getErrorStream()->getCharReader());
  outreader->start();
  errreader->start();

  Thread::sleep(2000);
  process->suspend();
  System::out->println("Thread now suspended for 2 seconds");
  Thread::sleep(2000);
  System::out->println("Thread now resume");
  process->resume();
  int ret = process->waitFor();
  outreader->join();
  errreader->join();
  
  //process->getOutputStream()->trans(output.getWriter());
  //process->getErrorStream()->trans(errput.getWriter());
  
  RString s = outreader->toString();
  RString es = errreader->toString();
  System::out->println("Exec  std: " + s);
  System::out->println("Exec  err: " + es);

}


} // namespace lang 
} //namespace acdk 
} //namespace tests 



