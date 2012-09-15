// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/tools/acidbg/AciDbgFrame.h,v 1.5 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_tools_acidbg_AciDbgFrame_h
#define acdk_tools_acidbg_AciDbgFrame_h

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/wx/App.h>
#include <acdk/wx/Frame.h>
#include <acdk/wx/TextCtrl.h>
#include <acdk/wx/StaticText.h>
#include <acdk/wx/Button.h>
#include <acdk/wx/ToolBar.h>
#include <acdk/wx/Notebook.h>
#include <acdk/wx/HtmlWindow.h>
#include <acdk/wx/SplitterWindow.h>
#include <acdk/wx/FileDialog.h>

#include "AciDbgFrame.h"
#include <acdk/aci/guidbg/CodeTreeCtrl.h>
#include <acdk/aci/guidbg/SyntaxTreeCtrl.h>

#include <acdk/aci/Compiler.h>

namespace acdk {
namespace tools {
namespace acidbg {

using namespace acdk::aci::guidbg;
using namespace acdk::wx;


enum Ids
{
  FirstUserId = 1001,
  MenuQuitId,
  MenuAboutId,
  MenuParseCodeId,
  MenuParsePostCodeId,
  MenuParseEmitId,
  MenuParseExecuteId,
  MenuExpandAllId,
  MenuLoadSyntaxId,
  CodeTreeId,
  SourceCodeTextCtrlId,
  SyntaxTreeId
};

class AciDbgFrame
: extends acdk::wx::Frame
{
  RCodeTreeCtrl _codeTree;
  RSyntaxTreeCtrl _syntaxTree;
  RTextCtrl _sourceCodeCtrl;
  RCompiler _comp;
public:
  AciDbgFrame(IN(RString) title) 
  : Frame(Nil, -1, title, new Point(100, 100), new Size(800, 600))
  {

    RMenuBar menuBar = new MenuBar();
    
    RMenu menuFile = new Menu();
    menuFile->append(MenuLoadSyntaxId, "Load Syntax...", "Load Syntax definition file");
    menuFile->appendSeparator();
    menuFile->append(MenuQuitId, "E&xit\tAlt-X", "Quit this program");
    
    RMenu menuParse = new Menu();

    menuParse->append(MenuParseCodeId, "ParseCode", "Parse the Code in the Code Window"); 
    menuParse->append(MenuParsePostCodeId, "ParsePostCode", "Parse and PostParse the Code in the Code Window"); 
    menuParse->append(MenuParseEmitId, "ParsePostEmitCode", "Parse, PostParse and Emit the Code in the Code Window"); 
    menuParse->append(MenuParseExecuteId, "Execute", "Parse, PostParse and Emit and execute the Code in the Code Window"); 
    
    menuBar->append(menuFile, "&File");
    menuBar->append(menuParse, "&Parse");
    RMenu viewMenu = new Menu();
    viewMenu->append(MenuExpandAllId, "Expand &All");
    menuBar->append(viewMenu, "&View");

    RMenu helpMenu = new Menu();
    helpMenu->append(MenuAboutId, "&About...\tCtrl-A", "Show about dialog");
    menuBar->append(helpMenu, "&Help");
    setMenuBar(menuBar);
    
    connect(CommandEvent::EvtCommandMenuSelected, MenuLoadSyntaxId, (ObjectEventFunction)&AciDbgFrame::onMenuLoadSyntax);
    connect(CommandEvent::EvtCommandMenuSelected, MenuParseCodeId, (ObjectEventFunction)&AciDbgFrame::onMenuParseCode);
    connect(CommandEvent::EvtCommandMenuSelected, MenuParsePostCodeId, (ObjectEventFunction)&AciDbgFrame::onMenuParseCode);
    connect(CommandEvent::EvtCommandMenuSelected, MenuParseEmitId, (ObjectEventFunction)&AciDbgFrame::onMenuParseCode);
    connect(CommandEvent::EvtCommandMenuSelected, MenuParseExecuteId, (ObjectEventFunction)&AciDbgFrame::onMenuParseCode);
    
    connect(CommandEvent::EvtCommandMenuSelected, MenuExpandAllId, (ObjectEventFunction)&AciDbgFrame::onMenuViewExpandAll);
    connect(CommandEvent::EvtCommandMenuSelected, MenuQuitId, (ObjectEventFunction)&AciDbgFrame::onMenuQuit);
    connect(CommandEvent::EvtCommandMenuSelected, MenuAboutId, (ObjectEventFunction)&AciDbgFrame::onMenuAbout);
    
    /*
    connect(CommandEvent::EvtCommandMenuSelected, MenuAboutId, (ObjectEventFunction)&AciDbgFrame::onMenuAbout);
    connect(CommandEvent::EvtCommandMenuSelected, ClearLogId,  (ObjectEventFunction)&AciDbgFrame::onMenuClear);
    
    */

    RSplitterWindow splitter = new SplitterWindow(this);
    RString initText = 
      "int add(int i, int j)\n"
      "{\n"
      "  return i + j;\n"
      "}\n"
      "int k = add(3, 2);\n"
      ;

    RNotebook notebook = new Notebook(&splitter, -1);

    _codeTree = new CodeTreeCtrl(&notebook, CodeTreeId, new Point(0, 0), new Size(200, 200));
    _syntaxTree = new SyntaxTreeCtrl(&notebook, SyntaxTreeId, new Point(0, 0), new Size(200, 200));
    _sourceCodeCtrl = new TextCtrl(&splitter, SourceCodeTextCtrlId, initText, Point::defaultPosition(), Size::defaultSize(), 
                            TeMultiline | TeAutoScroll | TeDontwrap);
    notebook->addPage(&_syntaxTree, "Syntax");
    notebook->addPage(&_codeTree, "Code");
    splitter->splitVertically(&notebook, &_sourceCodeCtrl, 300);
    _comp = new acdk::aci::Compiler();
    ::acdk::io::File f("c:\\d\\artefaktur\\acdk\\aal\\cfgs\\aal\\aal.asd");
    RString syntax = f.getReader()->getCharReader()->readString();
    ::acdk::aci::parser::SyntaxParseNode::parseSyntaxText(&_comp, syntax, f.getCanonicalPath());
  
    _syntaxTree->setCompiler(_comp);

  }
  void onMenuParseCode(IN(RCommandEvent) event)
  {
    try {
    int cmdId = event->getId();
    RString text = _sourceCodeCtrl->getValue();
    RString initalEl = "CodeText";
    
    _comp->scanner->setInBuffer(text);
    //compiler.printSyntax(System::out);
    acdk::aci::ast::RAstNode codetext =  _comp->parseComplete(initalEl);
    if (codetext == Nil)
      System::out->println("Cannot Parse [" + text + "] begins with [" + initalEl + "]");
    else
    {
      /*
      if (cmdId == MenuParsePostCodeId || cmdId == MenuParseEmitId || cmdId == MenuParseExecuteId)
         codetext->postParse(&compiler);
      if (cmdId == MenuParseEmitId || cmdId == MenuParseExecuteId)
      {
        RExecutableArray oca = new ExecutableArray(0);
        codetext->emitOpCode(&compiler, oca);
        for (int i = 0; i < oca->length(); ++i)
        {
          oca[i]->printOpCode(acdk::lang::System::out);
        }
        if (cmdId == MenuParseExecuteId)
          compiler.execute(oca);
      }*/
      _codeTree->setRootNode(codetext);
      /*
      System::out->println("Parsed: [" + text + "] to");
      codetext->printCodeTree(::acdk::lang::System::out, "");
      
      codetext->printCodeTree(::acdk::lang::System::out, "");
      RExecutableArray oca = new ExecutableArray(0);
      codetext->emitOpCode(&compiler, oca);
      for (int i = 0; i < oca->length(); ++i)
      {
        System::out->print(RString("") + i + ": ");
        oca[i]->printOpCode(System::out);
      }
      compiler.execute(oca);

    //codetext->printOpCode(::acdk::lang::System::out);
    //EvalEnv env(compiler);
    //oca->execute(env);
    */
  }
    } catch(RThrowable ex) {
      messageBox("Exception: " + ex->getMessage());
    }
  }
  void onMenuQuit(IN(RCommandEvent) event)
  {
    close(true);
  }
  void onMenuAbout(IN(RCommandEvent) event)
  {
    messageBox("ACI debugger\nCopyright 2004 by Roger Rene Kommer, artefaktur");
  }
  void onMenuViewExpandAll(IN(RCommandEvent) event)
  {
    _codeTree->expandAll();
  }
  void onMenuLoadSyntax(IN(RCommandEvent) event)
  {
    RFileDialog dlg = new FileDialog(this, "Open a file", "", "", "*.asd", FDFOpen);
    if (dlg->showModal() != IdOk)
      return;
    RString s = dlg->getPath();
    ::acdk::io::File f(s);
    RString syntax = f.getReader()->getCharReader()->readString();
    ::acdk::aci::parser::SyntaxParseNode::parseSyntaxText(&_comp, syntax);
    _syntaxTree->setCompiler(_comp);  
  }
};


} // namespace acidbg
} // namespace tools 
} // namespace acdk 


#endif //acdk_tools_acidbg_AciDbgFrame_h

