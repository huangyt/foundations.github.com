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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/KeyListener.h,v 1.5 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_event_KeyListener_h
#define acdk_java_awt_event_KeyListener_h

#include <acdk.h>
#include <acdk/java/Config.h>
#include <acdk/java/JavaObject.h>
#include "KeyEvent.h"

namespace acdk {
namespace java {
namespace awt {
namespace event {

const int CHAR_UNDEFINED = 65535;
const int KEY_FIRST = 400;
const int KEY_LAST = 402;
const int KEY_LOCATION_LEFT = 2;
const int KEY_LOCATION_NUMPAD = 4;
const int KEY_LOCATION_RIGHT = 3;
const int KEY_LOCATION_STANDARD = 1;
const int KEY_LOCATION_UNKNOWN = 0;
const int KEY_PRESSED = 401;
const int KEY_RELEASED = 402;
const int KEY_TYPED = 400;
const int AWT_VK_0 = 48;
const int AWT_VK_1 = 49;
const int AWT_VK_2 = 50;
const int AWT_VK_3 = 51;
const int AWT_VK_4 = 52;
const int AWT_VK_5 = 53;
const int AWT_VK_6 = 54;
const int AWT_VK_7 = 55;
const int AWT_VK_8 = 56;
const int AWT_VK_9 = 57;
const int AWT_VK_A = 65;
const int AWT_VK_ACCEPT = 30;
const int AWT_VK_ADD = 107;
const int AWT_VK_AGAIN = 65481;
const int AWT_VK_ALL_CANDIDATES = 256;
const int AWT_VK_ALPHANUMERIC = 240;
const int AWT_VK_ALT = 18;
const int AWT_VK_ALT_GRAPH = 65406;
const int AWT_VK_AMPERSAND = 150;
const int AWT_VK_ASTERISK = 151;
const int AWT_VK_AT = 512;
const int AWT_VK_B = 66;
const int AWT_VK_BACK_QUOTE = 192;
const int AWT_VK_BACK_SLASH = 92;
const int AWT_VK_BACK_SPACE = 8;
const int AWT_VK_BRACELEFT = 161;
const int AWT_VK_BRACERIGHT = 162;
const int AWT_VK_C = 67;
const int AWT_VK_CANCEL = 3;
const int AWT_VK_CAPS_LOCK = 20;
const int AWT_VK_CIRCUMFLEX = 514;
const int AWT_VK_CLEAR = 12;
const int AWT_VK_CLOSE_BRACKET = 93;
const int AWT_VK_CODE_INPUT = 258;
const int AWT_VK_COLON = 513;
const int AWT_VK_COMMA = 44;
const int AWT_VK_COMPOSE = 65312;
const int AWT_VK_CONTROL = 17;
const int AWT_VK_CONVERT = 28;
const int AWT_VK_COPY = 65485;
const int AWT_VK_CUT = 65489;
const int AWT_VK_D = 68;
const int AWT_VK_DEAD_ABOVEDOT = 134;
const int AWT_VK_DEAD_ABOVERING = 136;
const int AWT_VK_DEAD_ACUTE = 129;
const int AWT_VK_DEAD_BREVE = 133;
const int AWT_VK_DEAD_CARON = 138;
const int AWT_VK_DEAD_CEDILLA = 139;
const int AWT_VK_DEAD_CIRCUMFLEX = 130;
const int AWT_VK_DEAD_DIAERESIS = 135;
const int AWT_VK_DEAD_DOUBLEACUTE = 137;
const int AWT_VK_DEAD_GRAVE = 128;
const int AWT_VK_DEAD_IOTA = 141;
const int AWT_VK_DEAD_MACRON = 132;
const int AWT_VK_DEAD_OGONEK = 140;
const int AWT_VK_DEAD_SEMIVOICED_SOUND = 143;
const int AWT_VK_DEAD_TILDE = 131;
const int AWT_VK_DEAD_VOICED_SOUND = 142;
const int AWT_VK_DECIMAL = 110;
const int AWT_VK_DELETE = 127;
const int AWT_VK_DIVIDE = 111;
const int AWT_VK_DOLLAR = 515;
const int AWT_VK_DOWN = 40;
const int AWT_VK_E = 69;
const int AWT_VK_END = 35;
const int AWT_VK_ENTER = 10;
const int AWT_VK_EQUALS = 61;
const int AWT_VK_ESCAPE = 27;
const int AWT_VK_EURO_SIGN = 516;
const int AWT_VK_EXCLAMATION_MARK = 517;
const int AWT_VK_F = 70;
const int AWT_VK_F1 = 112;
const int AWT_VK_F10 = 121;
const int AWT_VK_F11 = 122;
const int AWT_VK_F12 = 123;
const int AWT_VK_F13 = 61440;
const int AWT_VK_F14 = 61441;
const int AWT_VK_F15 = 61442;
const int AWT_VK_F16 = 61443;
const int AWT_VK_F17 = 61444;
const int AWT_VK_F18 = 61445;
const int AWT_VK_F19 = 61446;
const int AWT_VK_F2 = 113;
const int AWT_VK_F20 = 61447;
const int AWT_VK_F21 = 61448;
const int AWT_VK_F22 = 61449;
const int AWT_VK_F23 = 61450;
const int AWT_VK_F24 = 61451;
const int AWT_VK_F3 = 114;
const int AWT_VK_F4 = 115;
const int AWT_VK_F5 = 116;
const int AWT_VK_F6 = 117;
const int AWT_VK_F7 = 118;
const int AWT_VK_F8 = 119;
const int AWT_VK_F9 = 120;
const int AWT_VK_FINAL = 24;
const int AWT_VK_FIND = 65488;
const int AWT_VK_FULL_WIDTH = 243;
const int AWT_VK_G = 71;
const int AWT_VK_GREATER = 160;
const int AWT_VK_H = 72;
const int AWT_VK_HALF_WIDTH = 244;
const int AWT_VK_HELP = 156;
const int AWT_VK_HIRAGANA = 242;
const int AWT_VK_HOME = 36;
const int AWT_VK_I = 73;
const int AWT_VK_INPUT_METHOD_ON_OFF = 263;
const int AWT_VK_INSERT = 155;
const int AWT_VK_INVERTED_EXCLAMATION_MARK = 518;
const int AWT_VK_J = 74;
const int AWT_VK_JAPANESE_HIRAGANA = 260;
const int AWT_VK_JAPANESE_KATAKANA = 259;
const int AWT_VK_JAPANESE_ROMAN = 261;
const int AWT_VK_K = 75;
const int AWT_VK_KANA = 21;
const int AWT_VK_KANA_LOCK = 262;
const int AWT_VK_KANJI = 25;
const int AWT_VK_KATAKANA = 241;
const int AWT_VK_KP_DOWN = 225;
const int AWT_VK_KP_LEFT = 226;
const int AWT_VK_KP_RIGHT = 227;
const int AWT_VK_KP_UP = 224;
const int AWT_VK_L = 76;
const int AWT_VK_LEFT = 37;
const int AWT_VK_LEFT_PARENTHESIS = 519;
const int AWT_VK_LESS = 153;
const int AWT_VK_M = 77;
const int AWT_VK_META = 157;
const int AWT_VK_MINUS = 45;
const int AWT_VK_MODECHANGE = 31;
const int AWT_VK_MULTIPLY = 106;
const int AWT_VK_N = 78;
const int AWT_VK_NONCONVERT = 29;
const int AWT_VK_NUM_LOCK = 144;
const int AWT_VK_NUMBER_SIGN = 520;
const int AWT_VK_NUMPAD0 = 96;
const int AWT_VK_NUMPAD1 = 97;
const int AWT_VK_NUMPAD2 = 98;
const int AWT_VK_NUMPAD3 = 99;
const int AWT_VK_NUMPAD4 = 100;
const int AWT_VK_NUMPAD5 = 101;
const int AWT_VK_NUMPAD6 = 102;
const int AWT_VK_NUMPAD7 = 103;
const int AWT_VK_NUMPAD8 = 104;
const int AWT_VK_NUMPAD9 = 105;
const int AWT_VK_O = 79;
const int AWT_VK_OPEN_BRACKET = 91;
const int AWT_VK_P = 80;
const int AWT_VK_PAGE_DOWN = 34;
const int AWT_VK_PAGE_UP = 33;
const int AWT_VK_PASTE = 65487;
const int AWT_VK_PAUSE = 19;
const int AWT_VK_PERIOD = 46;
const int AWT_VK_PLUS = 521;
const int AWT_VK_PREVIOUS_CANDIDATE = 257;
const int AWT_VK_PRINTSCREEN = 154;
const int AWT_VK_PROPS = 65482;
const int AWT_VK_Q = 81;
const int AWT_VK_QUOTE = 222;
const int AWT_VK_QUOTEDBL = 152;
const int AWT_VK_R = 82;
const int AWT_VK_RIGHT = 39;
const int AWT_VK_RIGHT_PARENTHESIS = 522;
const int AWT_VK_ROMAN_CHARACTERS = 245;
const int AWT_VK_S = 83;
const int AWT_VK_SCROLL_LOCK = 145;
const int AWT_VK_SEMICOLON = 59;
const int AWT_VK_SEPARATER = 108;
const int AWT_VK_SEPARATOR = 108;
const int AWT_VK_SHIFT = 16;
const int AWT_VK_SLASH = 47;
const int AWT_VK_SPACE = 32;
const int AWT_VK_STOP = 65480;
const int AWT_VK_SUBTRACT = 109;
const int AWT_VK_T = 84;
const int AWT_VK_TAB = 9;
const int AWT_VK_U = 85;
const int AWT_VK_UNDEFINED = 0;
const int AWT_VK_UNDERSCORE = 523;
const int AWT_VK_UNDO = 65483;
const int AWT_VK_UP = 38;
const int AWT_VK_V = 86;
const int AWT_VK_W = 87;
const int AWT_VK_X = 88;
const int AWT_VK_Y = 89;
const int AWT_VK_Z = 90;




ACDK_DECL_INTERFACE(KeyListener);

class ACDK_ACDK_JAVA_PUBLIC KeyListener
      ACDK_INTERFACEBASE
{
public:
  virtual void keyPressed(IN(RKeyEvent) event) = 0;
  virtual void keyReleased(IN(RKeyEvent) event) = 0;
  virtual void keyTyped(IN(RKeyEvent) event) = 0;
};

} // namespace event 
} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_KeyListener_h
