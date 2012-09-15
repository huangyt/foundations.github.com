
#ifndef acdk_wx_ide_StyledTextCtrl_h
#define acdk_wx_ide_StyledTextCtrl_h

#include "ide.h"
#if defined(ACDK_SCINTILLA_INTERN)
#include "../../../ext/wxscintilla/wxscintilla.h"
#else
# define WXUSING_STC_DLL
# include <wx/stc/stc.h>
#endif
#include <acdk/wx/Control.h>
#include <acdk/cfgscript/Props.h>

//typedef wxStyledTextCtrl wxScintella; 

namespace acdk {
namespace wx {
namespace ide {


enum SciStyles
{
  SciStyleDefault = wxSTC_STYLE_DEFAULT,  // wxSTC_STYLE_DEFAULT 32
  SciStyleLinenumber = wxSTC_STYLE_LINENUMBER,  // wxSTC_STYLE_LINENUMBER 33
  SciStyleBracelight = wxSTC_STYLE_BRACELIGHT,  // wxSTC_STYLE_BRACELIGHT 34
  SciStyleBracebad = wxSTC_STYLE_BRACEBAD,  // wxSTC_STYLE_BRACEBAD 35
  SciStyleControlchar = wxSTC_STYLE_CONTROLCHAR,  // wxSTC_STYLE_CONTROLCHAR 36
  SciStyleIndentguide = wxSTC_STYLE_INDENTGUIDE,  // wxSTC_STYLE_INDENTGUIDE 37
  SciStyleLastpredefined = wxSTC_STYLE_LASTPREDEFINED,  // wxSTC_STYLE_LASTPREDEFINED 39
  SciStyleMax = wxSTC_STYLE_MAX  // wxSTC_STYLE_MAX 127
};


/** 
  Symbolic key codes and modifier flags.
  ASCII and other printable characters below 256.
  Extended keys above 300.
*/
enum SciKeyCodes
{
  SciKeyDown = wxSTC_KEY_DOWN,  // wxSTC_KEY_DOWN 300
  SciKeyUp = wxSTC_KEY_UP,  // wxSTC_KEY_UP 301
  SciKeyLeft = wxSTC_KEY_LEFT,  // wxSTC_KEY_LEFT 302
  SciKeyRight = wxSTC_KEY_RIGHT,  // wxSTC_KEY_RIGHT 303
  SciKeyHome = wxSTC_KEY_HOME,  // wxSTC_KEY_HOME 304
  SciKeyEnd = wxSTC_KEY_END,  // wxSTC_KEY_END 305
  SciKeyPrior = wxSTC_KEY_PRIOR,  // wxSTC_KEY_PRIOR 306
  SciKeyNext = wxSTC_KEY_NEXT,  // wxSTC_KEY_NEXT 307
  SciKeyDelete = wxSTC_KEY_DELETE,  // wxSTC_KEY_DELETE 308
  SciKeyInsert = wxSTC_KEY_INSERT,  // wxSTC_KEY_INSERT 309
  SciKeyEscape = wxSTC_KEY_ESCAPE,  // wxSTC_KEY_ESCAPE 7
  SciKeyBack = wxSTC_KEY_BACK,  // wxSTC_KEY_BACK 8
  SciKeyTab = wxSTC_KEY_TAB,  // wxSTC_KEY_TAB 9
  SciKeyReturn = wxSTC_KEY_RETURN,  // wxSTC_KEY_RETURN 13
  SciKeyAdd = wxSTC_KEY_ADD,  // wxSTC_KEY_ADD 310
  SciKeySubtract = wxSTC_KEY_SUBTRACT,  // wxSTC_KEY_SUBTRACT 311
  SciKeyDivide = wxSTC_KEY_DIVIDE,  // wxSTC_KEY_DIVIDE 312
  //SciScmodNull = wxSTC_SCMOD_NULL,  // wxSTC_SCMOD_NULL 0
  SciScmodShift = wxSTC_SCMOD_SHIFT,  // wxSTC_SCMOD_SHIFT 1
  SciScmodCtrl = wxSTC_SCMOD_CTRL,  // wxSTC_SCMOD_CTRL 2
  SciScmodAlt = wxSTC_SCMOD_ALT  // wxSTC_SCMOD_ALT 4
};

enum SciLexers
{
  SciLexContainer = wxSTC_LEX_CONTAINER,  // wxSTC_LEX_CONTAINER 0
  SciLexNull = wxSTC_LEX_NULL,  // wxSTC_LEX_NULL 1
  SciLexPython = wxSTC_LEX_PYTHON,  // wxSTC_LEX_PYTHON 2
  SciLexCpp = wxSTC_LEX_CPP,  // wxSTC_LEX_CPP 3
  SciLexHtml = wxSTC_LEX_HTML,  // wxSTC_LEX_HTML 4
  SciLexXml = wxSTC_LEX_XML,  // wxSTC_LEX_XML 5
  SciLexPerl = wxSTC_LEX_PERL,  // wxSTC_LEX_PERL 6
  SciLexSql = wxSTC_LEX_SQL,  // wxSTC_LEX_SQL 7
  SciLexVb = wxSTC_LEX_VB,  // wxSTC_LEX_VB 8
  SciLexProperties = wxSTC_LEX_PROPERTIES,  // wxSTC_LEX_PROPERTIES 9
  SciLexErrorlist = wxSTC_LEX_ERRORLIST,  // wxSTC_LEX_ERRORLIST 10
  SciLexMakefile = wxSTC_LEX_MAKEFILE,  // wxSTC_LEX_MAKEFILE 11
  SciLexBatch = wxSTC_LEX_BATCH,  // wxSTC_LEX_BATCH 12
  SciLexXcode = wxSTC_LEX_XCODE,  // wxSTC_LEX_XCODE 13
  SciLexLatex = wxSTC_LEX_LATEX,  // wxSTC_LEX_LATEX 14
  SciLexLua = wxSTC_LEX_LUA,  // wxSTC_LEX_LUA 15
  SciLexDiff = wxSTC_LEX_DIFF,  // wxSTC_LEX_DIFF 16
  SciLexConf = wxSTC_LEX_CONF,  // wxSTC_LEX_CONF 17
  SciLexPascal = wxSTC_LEX_PASCAL,  // wxSTC_LEX_PASCAL 18
  SciLexAve = wxSTC_LEX_AVE,  // wxSTC_LEX_AVE 19
  SciLexAda = wxSTC_LEX_ADA,  // wxSTC_LEX_ADA 20
  SciLexLisp = wxSTC_LEX_LISP,  // wxSTC_LEX_LISP 21
  SciLexRuby = wxSTC_LEX_RUBY,  // wxSTC_LEX_RUBY 22
  SciLexEiffel = wxSTC_LEX_EIFFEL,  // wxSTC_LEX_EIFFEL 23
  SciLexEiffelkw = wxSTC_LEX_EIFFELKW,  // wxSTC_LEX_EIFFELKW 24
  SciLexTcl = wxSTC_LEX_TCL,  // wxSTC_LEX_TCL 25
  SciLexNncrontab = wxSTC_LEX_NNCRONTAB,  // wxSTC_LEX_NNCRONTAB 26
  SciLexBullant = wxSTC_LEX_BULLANT,  // wxSTC_LEX_BULLANT 27
  SciLexVbscript = wxSTC_LEX_VBSCRIPT,  // wxSTC_LEX_VBSCRIPT 28
  SciLexAsp = wxSTC_LEX_ASP,  // wxSTC_LEX_ASP 29
  SciLexPhp = wxSTC_LEX_PHP,  // wxSTC_LEX_PHP 30
  SciLexBaan = wxSTC_LEX_BAAN,  // wxSTC_LEX_BAAN 31
  SciLexMatlab = wxSTC_LEX_MATLAB,  // wxSTC_LEX_MATLAB 32
  SciLexScriptol = wxSTC_LEX_SCRIPTOL,  // wxSTC_LEX_SCRIPTOL 33
  SciLexAsm = wxSTC_LEX_ASM,  // wxSTC_LEX_ASM 34
  SciLexCppnocase = wxSTC_LEX_CPPNOCASE,  // wxSTC_LEX_CPPNOCASE 35
  SciLexFortran = wxSTC_LEX_FORTRAN,  // wxSTC_LEX_FORTRAN 36
  SciLexF77 = wxSTC_LEX_F77,  // wxSTC_LEX_F77 37
  SciLexCss = wxSTC_LEX_CSS,  // wxSTC_LEX_CSS 38
  SciLexPov = wxSTC_LEX_POV,  // wxSTC_LEX_POV 39
  SciLexLout = wxSTC_LEX_LOUT,  // wxSTC_LEX_LOUT 40
  SciLexEscript = wxSTC_LEX_ESCRIPT,  // wxSTC_LEX_ESCRIPT 41
  SciLexPs = wxSTC_LEX_PS,  // wxSTC_LEX_PS 42
  SciLexNsis = wxSTC_LEX_NSIS,  // wxSTC_LEX_NSIS 43
  SciLexMmixal = wxSTC_LEX_MMIXAL  // wxSTC_LEX_MMIXAL 44
  /*
  SciLexClw = wxSTC_LEX_CLW,  // wxSTC_LEX_CLW 45
  SciLexClwnocase = wxSTC_LEX_CLWNOCASE,  // wxSTC_LEX_CLWNOCASE 46
  SciLexLot = wxSTC_LEX_LOT,  // wxSTC_LEX_LOT 47
  SciLexYaml = wxSTC_LEX_YAML,  // wxSTC_LEX_YAML 48
  SciLexTex = wxSTC_LEX_TEX,  // wxSTC_LEX_TEX 49
  SciLexMetapost = wxSTC_LEX_METAPOST,  // wxSTC_LEX_METAPOST 50
  SciLexPowerbasic = wxSTC_LEX_POWERBASIC,  // wxSTC_LEX_POWERBASIC 51
  SciLexForth = wxSTC_LEX_FORTH,  // wxSTC_LEX_FORTH 52
  SciLexErlang = wxSTC_LEX_ERLANG,  // wxSTC_LEX_ERLANG 53
  SciLexOctave = wxSTC_LEX_OCTAVE,  // wxSTC_LEX_OCTAVE 54
  SciLexMssql = wxSTC_LEX_MSSQL,  // wxSTC_LEX_MSSQL 55
  SciLexVerilog = wxSTC_LEX_VERILOG,  // wxSTC_LEX_VERILOG 56
  SciLexKix = wxSTC_LEX_KIX,  // wxSTC_LEX_KIX 57
  SciLexGui4cli = wxSTC_LEX_GUI4CLI,  // wxSTC_LEX_GUI4CLI 58
  SciLexSpecman = wxSTC_LEX_SPECMAN,  // wxSTC_LEX_SPECMAN 59
  SciLexAu3 = wxSTC_LEX_AU3,  // wxSTC_LEX_AU3 60
  SciLexApdl = wxSTC_LEX_APDL,  // wxSTC_LEX_APDL 61
  SciLexBash = wxSTC_LEX_BASH,  // wxSTC_LEX_BASH 62
  */
// When a lexer specifies its language as SCLEX_AUTOMATIC it receives a
// value assigned in sequence from SCLEX_AUTOMATIC+1.
  //SciLexAutomatic = wxSTC_LEX_AUTOMATIC  // wxSTC_LEX_AUTOMATIC 1000
};

enum SciStatesCpp
{
  SciCDefault = wxSTC_C_DEFAULT,  // wxSTC_C_DEFAULT 0
  SciCComment = wxSTC_C_COMMENT,  // wxSTC_C_COMMENT 1
  SciCCommentline = wxSTC_C_COMMENTLINE,  // wxSTC_C_COMMENTLINE 2
  SciCCommentdoc = wxSTC_C_COMMENTDOC,  // wxSTC_C_COMMENTDOC 3
  SciCNumber = wxSTC_C_NUMBER,  // wxSTC_C_NUMBER 4
  SciCWord = wxSTC_C_WORD,  // wxSTC_C_WORD 5
  SciCString = wxSTC_C_STRING,  // wxSTC_C_STRING 6
  SciCCharacter = wxSTC_C_CHARACTER,  // wxSTC_C_CHARACTER 7
  SciCUuid = wxSTC_C_UUID,  // wxSTC_C_UUID 8
  SciCPreprocessor = wxSTC_C_PREPROCESSOR,  // wxSTC_C_PREPROCESSOR 9
  SciCOperator = wxSTC_C_OPERATOR,  // wxSTC_C_OPERATOR 10
  SciCIdentifier = wxSTC_C_IDENTIFIER,  // wxSTC_C_IDENTIFIER 11
  SciCStringeol = wxSTC_C_STRINGEOL,  // wxSTC_C_STRINGEOL 12
  SciCVerbatim = wxSTC_C_VERBATIM,  // wxSTC_C_VERBATIM 13
  SciCRegex = wxSTC_C_REGEX,  // wxSTC_C_REGEX 14
  SciCCommentlinedoc = wxSTC_C_COMMENTLINEDOC,  // wxSTC_C_COMMENTLINEDOC 15
  SciCWord2 = wxSTC_C_WORD2,  // wxSTC_C_WORD2 16
  SciCCommentdockeyword = wxSTC_C_COMMENTDOCKEYWORD,  // wxSTC_C_COMMENTDOCKEYWORD 17
  SciCCommentdockeyworderror = wxSTC_C_COMMENTDOCKEYWORDERROR,  // wxSTC_C_COMMENTDOCKEYWORDERROR 18
  SciCGlobalclass = wxSTC_C_GLOBALCLASS  // wxSTC_C_GLOBALCLASS 19
};

enum StcMarginType
{
  StcMarginSymbol = wxSTC_MARGIN_SYMBOL,  // wxSTC_MARGIN_SYMBOL 0
  StcMarginNumber = wxSTC_MARGIN_NUMBER  // wxSTC_MARGIN_NUMBER 1
};

enum StcMarkerFlags
{
  StcMarkerMax = wxSTC_MARKER_MAX,  // wxSTC_MARKER_MAX 31
  StcMarkCircle = wxSTC_MARK_CIRCLE,  // wxSTC_MARK_CIRCLE 0
  StcMarkRoundrect = wxSTC_MARK_ROUNDRECT,  // wxSTC_MARK_ROUNDRECT 1
  StcMarkArrow = wxSTC_MARK_ARROW,  // wxSTC_MARK_ARROW 2
  StcMarkSmallrect = wxSTC_MARK_SMALLRECT,  // wxSTC_MARK_SMALLRECT 3
  StcMarkShortarrow = wxSTC_MARK_SHORTARROW,  // wxSTC_MARK_SHORTARROW 4
  StcMarkEmpty = wxSTC_MARK_EMPTY,  // wxSTC_MARK_EMPTY 5
  StcMarkArrowdown = wxSTC_MARK_ARROWDOWN,  // wxSTC_MARK_ARROWDOWN 6
  StcMarkMinus = wxSTC_MARK_MINUS,  // wxSTC_MARK_MINUS 7
  StcMarkPlus = wxSTC_MARK_PLUS,  // wxSTC_MARK_PLUS 8

// Shapes used for outlining column.
  StcMarkVline = wxSTC_MARK_VLINE,  // wxSTC_MARK_VLINE 9
  StcMarkLcorner = wxSTC_MARK_LCORNER,  // wxSTC_MARK_LCORNER 10
  StcMarkTcorner = wxSTC_MARK_TCORNER,  // wxSTC_MARK_TCORNER 11
  StcMarkBoxplus = wxSTC_MARK_BOXPLUS,  // wxSTC_MARK_BOXPLUS 12
  StcMarkBoxplusconnected = wxSTC_MARK_BOXPLUSCONNECTED,  // wxSTC_MARK_BOXPLUSCONNECTED 13
  StcMarkBoxminus = wxSTC_MARK_BOXMINUS,  // wxSTC_MARK_BOXMINUS 14
  StcMarkBoxminusconnected = wxSTC_MARK_BOXMINUSCONNECTED,  // wxSTC_MARK_BOXMINUSCONNECTED 15
  StcMarkLcornercurve = wxSTC_MARK_LCORNERCURVE,  // wxSTC_MARK_LCORNERCURVE 16
  StcMarkTcornercurve = wxSTC_MARK_TCORNERCURVE,  // wxSTC_MARK_TCORNERCURVE 17
  StcMarkCircleplus = wxSTC_MARK_CIRCLEPLUS,  // wxSTC_MARK_CIRCLEPLUS 18
  StcMarkCircleplusconnected = wxSTC_MARK_CIRCLEPLUSCONNECTED,  // wxSTC_MARK_CIRCLEPLUSCONNECTED 19
  StcMarkCircleminus = wxSTC_MARK_CIRCLEMINUS,  // wxSTC_MARK_CIRCLEMINUS 20
  StcMarkCircleminusconnected = wxSTC_MARK_CIRCLEMINUSCONNECTED,  // wxSTC_MARK_CIRCLEMINUSCONNECTED 21

// Invisible mark that only sets the line background color.
  StcMarkBackground = wxSTC_MARK_BACKGROUND,  // wxSTC_MARK_BACKGROUND 22
  StcMarkDotdotdot = wxSTC_MARK_DOTDOTDOT,  // wxSTC_MARK_DOTDOTDOT 23
  StcMarkArrows = wxSTC_MARK_ARROWS,  // wxSTC_MARK_ARROWS 24
  StcMarkPixmap = wxSTC_MARK_PIXMAP,  // wxSTC_MARK_PIXMAP 25
  StcMarkCharacter = wxSTC_MARK_CHARACTER,  // wxSTC_MARK_CHARACTER 10000

// Markers used for outlining column.
  StcMarknumFolderend = wxSTC_MARKNUM_FOLDEREND,  // wxSTC_MARKNUM_FOLDEREND 25
  StcMarknumFolderopenmid = wxSTC_MARKNUM_FOLDEROPENMID,  // wxSTC_MARKNUM_FOLDEROPENMID 26
  StcMarknumFoldermidtail = wxSTC_MARKNUM_FOLDERMIDTAIL,  // wxSTC_MARKNUM_FOLDERMIDTAIL 27
  StcMarknumFoldertail = wxSTC_MARKNUM_FOLDERTAIL,  // wxSTC_MARKNUM_FOLDERTAIL 28
  StcMarknumFoldersub = wxSTC_MARKNUM_FOLDERSUB,  // wxSTC_MARKNUM_FOLDERSUB 29
  StcMarknumFolder = wxSTC_MARKNUM_FOLDER,  // wxSTC_MARKNUM_FOLDER 30
  StcMarknumFolderopen = wxSTC_MARKNUM_FOLDEROPEN,  // wxSTC_MARKNUM_FOLDEROPEN 31
  StcMaskFolders = wxSTC_MASK_FOLDERS  // wxSTC_MASK_FOLDERS 0xFE000000
};

enum StcCharsets
{
// Character set identifiers are used in StyleSetCharacterSet.
// The values are the same as the Windows *_CHARSET values.
  StcCharsetAnsi = wxSTC_CHARSET_ANSI,  // wxSTC_CHARSET_ANSI 0
  StcCharsetDefault = wxSTC_CHARSET_DEFAULT,  // wxSTC_CHARSET_DEFAULT 1
  StcCharsetBaltic = wxSTC_CHARSET_BALTIC,  // wxSTC_CHARSET_BALTIC 186
  StcCharsetChinesebig5 = wxSTC_CHARSET_CHINESEBIG5,  // wxSTC_CHARSET_CHINESEBIG5 136
  StcCharsetEasteurope = wxSTC_CHARSET_EASTEUROPE,  // wxSTC_CHARSET_EASTEUROPE 238
  StcCharsetGb2312 = wxSTC_CHARSET_GB2312,  // wxSTC_CHARSET_GB2312 134
  StcCharsetGreek = wxSTC_CHARSET_GREEK,  // wxSTC_CHARSET_GREEK 161
  StcCharsetHangul = wxSTC_CHARSET_HANGUL,  // wxSTC_CHARSET_HANGUL 129
  StcCharsetMac = wxSTC_CHARSET_MAC,  // wxSTC_CHARSET_MAC 77
  StcCharsetOem = wxSTC_CHARSET_OEM,  // wxSTC_CHARSET_OEM 255
  StcCharsetRussian = wxSTC_CHARSET_RUSSIAN,  // wxSTC_CHARSET_RUSSIAN 204
  StcCharsetShiftjis = wxSTC_CHARSET_SHIFTJIS,  // wxSTC_CHARSET_SHIFTJIS 128
  StcCharsetSymbol = wxSTC_CHARSET_SYMBOL,  // wxSTC_CHARSET_SYMBOL 2
  StcCharsetTurkish = wxSTC_CHARSET_TURKISH,  // wxSTC_CHARSET_TURKISH 162
  StcCharsetJohab = wxSTC_CHARSET_JOHAB,  // wxSTC_CHARSET_JOHAB 130
  StcCharsetHebrew = wxSTC_CHARSET_HEBREW,  // wxSTC_CHARSET_HEBREW 177
  StcCharsetArabic = wxSTC_CHARSET_ARABIC,  // wxSTC_CHARSET_ARABIC 178
  StcCharsetVietnamese = wxSTC_CHARSET_VIETNAMESE,  // wxSTC_CHARSET_VIETNAMESE 163
  StcCharsetThai = wxSTC_CHARSET_THAI,  // wxSTC_CHARSET_THAI 222
  StcCaseMixed = wxSTC_CASE_MIXED,  // wxSTC_CASE_MIXED 0
  StcCaseUpper = wxSTC_CASE_UPPER,  // wxSTC_CASE_UPPER 1
  StcCaseLower = wxSTC_CASE_LOWER,  // wxSTC_CASE_LOWER 2
  StcIndicMax = wxSTC_INDIC_MAX,  // wxSTC_INDIC_MAX 7
  StcIndicPlain = wxSTC_INDIC_PLAIN,  // wxSTC_INDIC_PLAIN 0
  StcIndicSquiggle = wxSTC_INDIC_SQUIGGLE,  // wxSTC_INDIC_SQUIGGLE 1
  StcIndicTt = wxSTC_INDIC_TT,  // wxSTC_INDIC_TT 2
  StcIndicDiagonal = wxSTC_INDIC_DIAGONAL,  // wxSTC_INDIC_DIAGONAL 3
  StcIndicStrike = wxSTC_INDIC_STRIKE,  // wxSTC_INDIC_STRIKE 4
  StcIndicHidden = wxSTC_INDIC_HIDDEN,  // wxSTC_INDIC_HIDDEN 5
  StcIndicBox = wxSTC_INDIC_BOX,  // wxSTC_INDIC_BOX 6
  StcIndic0Mask = wxSTC_INDIC0_MASK,  // wxSTC_INDIC0_MASK 0x20
  StcIndic1Mask = wxSTC_INDIC1_MASK,  // wxSTC_INDIC1_MASK 0x40
  StcIndic2Mask = wxSTC_INDIC2_MASK,  // wxSTC_INDIC2_MASK 0x80
  StcIndicsMask = wxSTC_INDICS_MASK  // wxSTC_INDICS_MASK 0xE0
};

enum StcFindFlags
{
  StcFindWholeword = wxSTC_FIND_WHOLEWORD,  // wxSTC_FIND_WHOLEWORD 2
  StcFindMatchcase = wxSTC_FIND_MATCHCASE,  // wxSTC_FIND_MATCHCASE 4
  StcFindWordstart = wxSTC_FIND_WORDSTART,  // wxSTC_FIND_WORDSTART 0x00100000
  StcFindRegexp = wxSTC_FIND_REGEXP,  // wxSTC_FIND_REGEXP 0x00200000
  StcFindPosix = wxSTC_FIND_POSIX  // wxSTC_FIND_POSIX 0x00400000
};

/**
 Commands that can be bound to keystrokes
*/
enum SciCommands
{
/// Redoes the next action on the undo history.
  SciCmdRedo = wxSTC_CMD_REDO,  // wxSTC_CMD_REDO 2011

/// Select all the text in the document.
  SciCmdSelectall = wxSTC_CMD_SELECTALL,  // wxSTC_CMD_SELECTALL 2013

/// Undo one action in the undo history.
  SciCmdUndo = wxSTC_CMD_UNDO,  // wxSTC_CMD_UNDO 2176

/// Cut the selection to the clipboard.
  SciCmdCut = wxSTC_CMD_CUT,  // wxSTC_CMD_CUT 2177

/// Copy the selection to the clipboard.
  SciCmdCopy = wxSTC_CMD_COPY,  // wxSTC_CMD_COPY 2178

/// Paste the contents of the clipboard into the document replacing the selection.
  SciCmdPaste = wxSTC_CMD_PASTE,  // wxSTC_CMD_PASTE 2179

/// Clear the selection.
  SciCmdClear = wxSTC_CMD_CLEAR,  // wxSTC_CMD_CLEAR 2180

/// Move caret down one line.
  SciCmdLinedown = wxSTC_CMD_LINEDOWN,  // wxSTC_CMD_LINEDOWN 2300

/// Move caret down one line extending selection to new caret position.
  SciCmdLinedownextend = wxSTC_CMD_LINEDOWNEXTEND,  // wxSTC_CMD_LINEDOWNEXTEND 2301

/// Move caret up one line.
  SciCmdLineup = wxSTC_CMD_LINEUP,  // wxSTC_CMD_LINEUP 2302

/// Move caret up one line extending selection to new caret position.
  SciCmdLineupextend = wxSTC_CMD_LINEUPEXTEND,  // wxSTC_CMD_LINEUPEXTEND 2303

/// Move caret left one character.
  SciCmdCharleft = wxSTC_CMD_CHARLEFT,  // wxSTC_CMD_CHARLEFT 2304

/// Move caret left one character extending selection to new caret position.
  SciCmdCharleftextend = wxSTC_CMD_CHARLEFTEXTEND,  // wxSTC_CMD_CHARLEFTEXTEND 2305

/// Move caret right one character.
  SciCmdCharright = wxSTC_CMD_CHARRIGHT,  // wxSTC_CMD_CHARRIGHT 2306

/// Move caret right one character extending selection to new caret position.
  SciCmdCharrightextend = wxSTC_CMD_CHARRIGHTEXTEND,  // wxSTC_CMD_CHARRIGHTEXTEND 2307

/// Move caret left one word.
  SciCmdWordleft = wxSTC_CMD_WORDLEFT,  // wxSTC_CMD_WORDLEFT 2308

/// Move caret left one word extending selection to new caret position.
  SciCmdWordleftextend = wxSTC_CMD_WORDLEFTEXTEND,  // wxSTC_CMD_WORDLEFTEXTEND 2309

/// Move caret right one word.
  SciCmdWordright = wxSTC_CMD_WORDRIGHT,  // wxSTC_CMD_WORDRIGHT 2310

/// Move caret right one word extending selection to new caret position.
  SciCmdWordrightextend = wxSTC_CMD_WORDRIGHTEXTEND,  // wxSTC_CMD_WORDRIGHTEXTEND 2311

/// Move caret to first position on line.
  SciCmdHome = wxSTC_CMD_HOME,  // wxSTC_CMD_HOME 2312

/// Move caret to first position on line extending selection to new caret position.
  SciCmdHomeextend = wxSTC_CMD_HOMEEXTEND,  // wxSTC_CMD_HOMEEXTEND 2313

/// Move caret to last position on line.
  SciCmdLineend = wxSTC_CMD_LINEEND,  // wxSTC_CMD_LINEEND 2314

/// Move caret to last position on line extending selection to new caret position.
  SciCmdLineendextend = wxSTC_CMD_LINEENDEXTEND,  // wxSTC_CMD_LINEENDEXTEND 2315

/// Move caret to first position in document.
  SciCmdDocumentstart = wxSTC_CMD_DOCUMENTSTART,  // wxSTC_CMD_DOCUMENTSTART 2316

/// Move caret to first position in document extending selection to new caret position.
  SciCmdDocumentstartextend = wxSTC_CMD_DOCUMENTSTARTEXTEND,  // wxSTC_CMD_DOCUMENTSTARTEXTEND 2317

/// Move caret to last position in document.
  SciCmdDocumentend = wxSTC_CMD_DOCUMENTEND,  // wxSTC_CMD_DOCUMENTEND 2318

/// Move caret to last position in document extending selection to new caret position.
  SciCmdDocumentendextend = wxSTC_CMD_DOCUMENTENDEXTEND,  // wxSTC_CMD_DOCUMENTENDEXTEND 2319

/// Move caret one page up.
  SciCmdPageup = wxSTC_CMD_PAGEUP,  // wxSTC_CMD_PAGEUP 2320

/// Move caret one page up extending selection to new caret position.
  SciCmdPageupextend = wxSTC_CMD_PAGEUPEXTEND,  // wxSTC_CMD_PAGEUPEXTEND 2321

/// Move caret one page down.
  SciCmdPagedown = wxSTC_CMD_PAGEDOWN,  // wxSTC_CMD_PAGEDOWN 2322

/// Move caret one page down extending selection to new caret position.
  SciCmdPagedownextend = wxSTC_CMD_PAGEDOWNEXTEND,  // wxSTC_CMD_PAGEDOWNEXTEND 2323

/// Switch from insert to overtype mode or the reverse.
  SciCmdEdittoggleovertype = wxSTC_CMD_EDITTOGGLEOVERTYPE,  // wxSTC_CMD_EDITTOGGLEOVERTYPE 2324

/// Cancel any modes such as call tip or auto-completion list display.
  SciCmdCancel = wxSTC_CMD_CANCEL,  // wxSTC_CMD_CANCEL 2325

/// Delete the selection or if no selection, the character before the caret.
  SciCmdDeleteback = wxSTC_CMD_DELETEBACK,  // wxSTC_CMD_DELETEBACK 2326

/// If selection is empty or all on one line replace the selection with a tab character.
/// If more than one line selected, indent the lines.
  SciCmdTab = wxSTC_CMD_TAB,  // wxSTC_CMD_TAB 2327

/// Dedent the selected lines.
  SciCmdBacktab = wxSTC_CMD_BACKTAB,  // wxSTC_CMD_BACKTAB 2328

/// Insert a new line, may use a CRLF, CR or LF depending on EOL mode.
  SciCmdNewline = wxSTC_CMD_NEWLINE,  // wxSTC_CMD_NEWLINE 2329

/// Insert a Form Feed character.
  SciCmdFormfeed = wxSTC_CMD_FORMFEED,  // wxSTC_CMD_FORMFEED 2330

/// Move caret to before first visible character on line.
/// If already there move to first character on line.
  SciCmdVchome = wxSTC_CMD_VCHOME,  // wxSTC_CMD_VCHOME 2331

/// Like VCHome but extending selection to new caret position.
  SciCmdVchomeextend = wxSTC_CMD_VCHOMEEXTEND,  // wxSTC_CMD_VCHOMEEXTEND 2332

/// Magnify the displayed text by increasing the sizes by 1 point.
  SciCmdZoomin = wxSTC_CMD_ZOOMIN,  // wxSTC_CMD_ZOOMIN 2333

/// Make the displayed text smaller by decreasing the sizes by 1 point.
  SciCmdZoomout = wxSTC_CMD_ZOOMOUT,  // wxSTC_CMD_ZOOMOUT 2334

/// Delete the word to the left of the caret.
  SciCmdDelwordleft = wxSTC_CMD_DELWORDLEFT,  // wxSTC_CMD_DELWORDLEFT 2335

/// Delete the word to the right of the caret.
  SciCmdDelwordright = wxSTC_CMD_DELWORDRIGHT,  // wxSTC_CMD_DELWORDRIGHT 2336

/// Cut the line containing the caret.
  SciCmdLinecut = wxSTC_CMD_LINECUT,  // wxSTC_CMD_LINECUT 2337

/// Delete the line containing the caret.
  SciCmdLinedelete = wxSTC_CMD_LINEDELETE,  // wxSTC_CMD_LINEDELETE 2338

/// Switch the current line with the previous.
  SciCmdLinetranspose = wxSTC_CMD_LINETRANSPOSE,  // wxSTC_CMD_LINETRANSPOSE 2339

/// Duplicate the current line.
  SciCmdLineduplicate = wxSTC_CMD_LINEDUPLICATE,  // wxSTC_CMD_LINEDUPLICATE 2404

/// Transform the selection to lower case.
  SciCmdLowercase = wxSTC_CMD_LOWERCASE,  // wxSTC_CMD_LOWERCASE 2340

/// Transform the selection to upper case.
  SciCmdUppercase = wxSTC_CMD_UPPERCASE,  // wxSTC_CMD_UPPERCASE 2341

/// Scroll the document down, keeping the caret visible.
  SciCmdLinescrolldown = wxSTC_CMD_LINESCROLLDOWN,  // wxSTC_CMD_LINESCROLLDOWN 2342

/// Scroll the document up, keeping the caret visible.
  SciCmdLinescrollup = wxSTC_CMD_LINESCROLLUP,  // wxSTC_CMD_LINESCROLLUP 2343

/// Delete the selection or if no selection, the character before the caret.
/// Will not delete the character before at the start of a line.
  SciCmdDeletebacknotline = wxSTC_CMD_DELETEBACKNOTLINE,  // wxSTC_CMD_DELETEBACKNOTLINE 2344

/// Move caret to first position on display line.
  SciCmdHomedisplay = wxSTC_CMD_HOMEDISPLAY,  // wxSTC_CMD_HOMEDISPLAY 2345

/// Move caret to first position on display line extending selection to
/// new caret position.
  SciCmdHomedisplayextend = wxSTC_CMD_HOMEDISPLAYEXTEND,  // wxSTC_CMD_HOMEDISPLAYEXTEND 2346

/// Move caret to last position on display line.
  SciCmdLineenddisplay = wxSTC_CMD_LINEENDDISPLAY,  // wxSTC_CMD_LINEENDDISPLAY 2347

/// Move caret to last position on display line extending selection to new
/// caret position.
  SciCmdLineenddisplayextend = wxSTC_CMD_LINEENDDISPLAYEXTEND,  // wxSTC_CMD_LINEENDDISPLAYEXTEND 2348

/// These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
/// except they behave differently when word-wrap is enabled:
/// They go first to the start / end of the display line, like (Home|LineEnd)Display
/// The difference is that, the cursor is already at the point, it goes on to the start
/// or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
  SciCmdHomewrap = wxSTC_CMD_HOMEWRAP,  // wxSTC_CMD_HOMEWRAP 2349
  SciCmdHomewrapextend = wxSTC_CMD_HOMEWRAPEXTEND,  // wxSTC_CMD_HOMEWRAPEXTEND 2450
  SciCmdLineendwrap = wxSTC_CMD_LINEENDWRAP,  // wxSTC_CMD_LINEENDWRAP 2451
  SciCmdLineendwrapextend = wxSTC_CMD_LINEENDWRAPEXTEND,  // wxSTC_CMD_LINEENDWRAPEXTEND 2452
  SciCmdVchomewrap = wxSTC_CMD_VCHOMEWRAP,  // wxSTC_CMD_VCHOMEWRAP 2453
  SciCmdVchomewrapextend = wxSTC_CMD_VCHOMEWRAPEXTEND,  // wxSTC_CMD_VCHOMEWRAPEXTEND 2454

/// Copy the line containing the caret.
  // not supported by STC SciCmdLinecopy = wxSTC_CMD_LINECOPY,  // wxSTC_CMD_LINECOPY 2455

/// Move to the previous change in capitalisation.
  SciCmdWordpartleft = wxSTC_CMD_WORDPARTLEFT,  // wxSTC_CMD_WORDPARTLEFT 2390

/// Move to the previous change in capitalisation extending selection
/// to new caret position.
  SciCmdWordpartleftextend = wxSTC_CMD_WORDPARTLEFTEXTEND,  // wxSTC_CMD_WORDPARTLEFTEXTEND 2391

/// Move to the change next in capitalisation.
  SciCmdWordpartright = wxSTC_CMD_WORDPARTRIGHT,  // wxSTC_CMD_WORDPARTRIGHT 2392

/// Move to the next change in capitalisation extending selection
/// to new caret position.
  SciCmdWordpartrightextend = wxSTC_CMD_WORDPARTRIGHTEXTEND,  // wxSTC_CMD_WORDPARTRIGHTEXTEND 2393

/// Delete back from the current position to the start of the line.
  SciCmdDellineleft = wxSTC_CMD_DELLINELEFT,  // wxSTC_CMD_DELLINELEFT 2395

/// Delete forwards from the current position to the end of the line.
  SciCmdDellineright = wxSTC_CMD_DELLINERIGHT,  // wxSTC_CMD_DELLINERIGHT 2396

/// Move caret between paragraphs (delimited by empty lines).
  SciCmdParadown = wxSTC_CMD_PARADOWN,  // wxSTC_CMD_PARADOWN 2413
  SciCmdParadownextend = wxSTC_CMD_PARADOWNEXTEND,  // wxSTC_CMD_PARADOWNEXTEND 2414
  SciCmdParaup = wxSTC_CMD_PARAUP,  // wxSTC_CMD_PARAUP 2415
  SciCmdParaupextend = wxSTC_CMD_PARAUPEXTEND,  // wxSTC_CMD_PARAUPEXTEND 2416
/* not supported from here from STC
/// Move caret down one line, extending rectangular selection to new caret position.
  // not supported by STC SciCmdLinedownrectextend = wxSTC_CMD_LINEDOWNRECTEXTEND,  // wxSTC_CMD_LINEDOWNRECTEXTEND 2426

/// Move caret up one line, extending rectangular selection to new caret position.
 SciCmdLineuprectextend = wxSTC_CMD_LINEUPRECTEXTEND,  // wxSTC_CMD_LINEUPRECTEXTEND 2427

/// Move caret left one character, extending rectangular selection to new caret position.
  SciCmdCharleftrectextend = wxSTC_CMD_CHARLEFTRECTEXTEND,  // wxSTC_CMD_CHARLEFTRECTEXTEND 2428

/// Move caret right one character, extending rectangular selection to new caret position.
  SciCmdCharrightrectextend = wxSTC_CMD_CHARRIGHTRECTEXTEND,  // wxSTC_CMD_CHARRIGHTRECTEXTEND 2429

/// Move caret to first position on line, extending rectangular selection to new caret position.
  SciCmdHomerectextend = wxSTC_CMD_HOMERECTEXTEND,  // wxSTC_CMD_HOMERECTEXTEND 2430

/// Move caret to before first visible character on line.
/// If already there move to first character on line.
/// In either case, extend rectangular selection to new caret position.
  SciCmdVchomerectextend = wxSTC_CMD_VCHOMERECTEXTEND,  // wxSTC_CMD_VCHOMERECTEXTEND 2431

/// Move caret to last position on line, extending rectangular selection to new caret position.
  SciCmdLineendrectextend = wxSTC_CMD_LINEENDRECTEXTEND,  // wxSTC_CMD_LINEENDRECTEXTEND 2432

/// Move caret one page up, extending rectangular selection to new caret position.
  SciCmdPageuprectextend = wxSTC_CMD_PAGEUPRECTEXTEND,  // wxSTC_CMD_PAGEUPRECTEXTEND 2433

/// Move caret one page down, extending rectangular selection to new caret position.
  SciCmdPagedownrectextend = wxSTC_CMD_PAGEDOWNRECTEXTEND,  // wxSTC_CMD_PAGEDOWNRECTEXTEND 2434

/// Move caret to top of page, or one page up if already at top of page.
  SciCmdStutteredpageup = wxSTC_CMD_STUTTEREDPAGEUP,  // wxSTC_CMD_STUTTEREDPAGEUP 2435

/// Move caret to top of page, or one page up if already at top of page, extending selection to new caret position.
  SciCmdStutteredpageupextend = wxSTC_CMD_STUTTEREDPAGEUPEXTEND,  // wxSTC_CMD_STUTTEREDPAGEUPEXTEND 2436

/// Move caret to bottom of page, or one page down if already at bottom of page.
  SciCmdStutteredpagedown = wxSTC_CMD_STUTTEREDPAGEDOWN,  // wxSTC_CMD_STUTTEREDPAGEDOWN 2437

/// Move caret to bottom of page, or one page down if already at bottom of page, extending selection to new caret position.
  SciCmdStutteredpagedownextend = wxSTC_CMD_STUTTEREDPAGEDOWNEXTEND,  // wxSTC_CMD_STUTTEREDPAGEDOWNEXTEND 2438

/// Move caret left one word, position cursor at end of word.
  SciCmdWordleftend = wxSTC_CMD_WORDLEFTEND,  // wxSTC_CMD_WORDLEFTEND 2439

/// Move caret left one word, position cursor at end of word, extending selection to new caret position.
  SciCmdWordleftendextend = wxSTC_CMD_WORDLEFTENDEXTEND,  // wxSTC_CMD_WORDLEFTENDEXTEND 2440

/// Move caret right one word, position cursor at end of word.
  SciCmdWordrightend = wxSTC_CMD_WORDRIGHTEND,  // wxSTC_CMD_WORDRIGHTEND 2441

/// Move caret right one word, position cursor at end of word, extending selection to new caret position.
  SciCmdWordrightendextend = wxSTC_CMD_WORDRIGHTENDEXTEND  // wxSTC_CMD_WORDRIGHTENDEXTEND 2442
*/
};

enum StcCache 
{
  StcCacheNone = wxSTC_CACHE_NONE,  // wxSTC_CACHE_NONE 0
  StcCacheCaret = wxSTC_CACHE_CARET,  // wxSTC_CACHE_CARET 1
  StcCachePage = wxSTC_CACHE_PAGE,  // wxSTC_CACHE_PAGE 2
  StcCacheDocument = wxSTC_CACHE_DOCUMENT  // wxSTC_CACHE_DOCUMENT 3
};

/**
  see StyledTextCtrl::setModEventMask
*/
enum StcContainerNotifications
{
  StcModInserttext = wxSTC_MOD_INSERTTEXT,  // wxSTC_MOD_INSERTTEXT 0x1
  StcModDeletetext = wxSTC_MOD_DELETETEXT,  // wxSTC_MOD_DELETETEXT 0x2
  StcModChangestyle = wxSTC_MOD_CHANGESTYLE,  // wxSTC_MOD_CHANGESTYLE 0x4
  StcModChangefold = wxSTC_MOD_CHANGEFOLD,  // wxSTC_MOD_CHANGEFOLD 0x8
  StcPerformedUser = wxSTC_PERFORMED_USER,  // wxSTC_PERFORMED_USER 0x10
  StcPerformedUndo = wxSTC_PERFORMED_UNDO,  // wxSTC_PERFORMED_UNDO 0x20
  StcPerformedRedo = wxSTC_PERFORMED_REDO,  // wxSTC_PERFORMED_REDO 0x40
  StcLaststepinundoredo = wxSTC_LASTSTEPINUNDOREDO,  // wxSTC_LASTSTEPINUNDOREDO 0x100
  StcModChangemarker = wxSTC_MOD_CHANGEMARKER,  // wxSTC_MOD_CHANGEMARKER 0x200
  StcModBeforeinsert = wxSTC_MOD_BEFOREINSERT,  // wxSTC_MOD_BEFOREINSERT 0x400
  StcModBeforedelete = wxSTC_MOD_BEFOREDELETE,  // wxSTC_MOD_BEFOREDELETE 0x800
  StcModeventmaskall = wxSTC_MODEVENTMASKALL  // wxSTC_MODEVENTMASKALL 0xF77
};

#if defined (ACDK_OS_WIN32)
# define EXT_TEXTMARFUNC(call) call
#else
# define EXT_TEXTMARFUNC(call) sys::coreout << #call << " not supported on this platform" << sys::eofl
#endif

using namespace acdk::wx;

ACDK_DECL_CLASS(StyledTextCtrl);

/**
  see wxStyledTextCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/21 09:52:29 $
*/
class ACDK_WX_IDE_PUBLIC StyledTextCtrl
: extends Control
{
  ACDK_WITH_METAINFO(StyledTextCtrl)
  
  static acdk::cfgscript::RProps _globalProps;
public:
  /// wxStyledTextCtrl
  ACDK_WX_STD_MEMBERS(StyledTextCtrl, Control)
  StyledTextCtrl(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
           int style = 0, IN(RString) name = "SCIwindow")
  : Control(new wxStyledTextCtrl(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)))
  {
  }
  /**
    initialize the control with given property set
    Please refer to acdk/acdk_wx/cfg/csf/acdk/wx/ide/StyledTextCtrConfig.csf
  */
  void initFromProps(IN(::acdk::cfgscript::RProps) props);
  foreign int _initState(IN(::acdk::cfgscript::RProps) props, int state);
  /**
    try to load $(ACDK_TOOLS_HOME)/acdk_wx/cfg/csf/acdk/wx/ide/StyledTextCtrConfig.csf
  */
  static void loadStdConfig();
  static void loadConfig(IN(RString) csfFile);
  /**
    use the previous load configuration to setup syntax hilighting
  */
  void initFromTextFile(IN(RString) fileName);
  //void AddText (const wxString& text);
  inline void addText(IN(RString)  text) { getWx()->AddText(S2WXS(text)); }
    //void AddText (const int length, const wxString& text);
  // not supported inline void addText(int length, IN(RString)  text) { getWx()->AddText(length, S2WXS(text)); }

    // Add array of cells to document.
    //void AddStyledText (const wxMemoryBuffer& data);
  // ### TODO inline void addStyledText(IN(RMemoryBuffer) data) { getWx()->AddStyledText(CLS2WXREF(data)); }

    // Insert string at a position.
    //void InsertText (int pos, const wxString& text);
  inline void insertText(int pos, IN(RString)  text) { getWx()->InsertText(pos, S2WXS(text)); }

    // Delete all text in the document.
    //void ClearAll();
  inline void clearAll() { getWx()->ClearAll(); }

    // Set all style bytes to 0, remove all folding information.
    //void ClearDocumentStyle();
  inline void clearDocumentStyle() { getWx()->ClearDocumentStyle(); }

    // The number of characters in the document.
    //int GetLength();
  inline int getLength() { return getWx()->GetLength(); }

    // Returns the character byte at the position.
    //int GetCharAt (int pos);
  inline int getCharAt(int pos) { return getWx()->GetCharAt(pos); }

    // Returns the position of the caret.
    //int GetCurrentPos();
  inline int getCurrentPos() { return getWx()->GetCurrentPos(); }

    // Returns the position of the opposite end of the selection to the caret.
    //int GetAnchor();
  inline int getAnchor() { return getWx()->GetAnchor(); }

    // Returns the style byte at the position.
    //int GetStyleAt (int pos);
  inline int getStyleAt(int pos) { return getWx()->GetStyleAt(pos); }

    // Redoes the next action on the undo history.
    //void Redo();
  inline void redo() { getWx()->Redo(); }

    // Choose between collecting actions into the undo
    // history and discarding them.
    //void SetUndoCollection (bool collectUndo);
  inline void setUndoCollection(bool collectUndo) { getWx()->SetUndoCollection(collectUndo); }

    // Select all the text in the document.
    //void SelectAll();
  inline void selectAll() { getWx()->SelectAll(); }

    // Remember the current position in the undo history as the position
    // at which the document was saved.
    //void SetSavePoint();
  inline void setSavePoint() { getWx()->SetSavePoint(); }

    // Retrieve a buffer of cells.
    //wxMemoryBuffer GetStyledText (int startPos, int endPos);
  // ### TODO inline RMemoryBuffer getStyledText(int startPos, int endPos) { return WXVAL2CLS(MemoryBuffer, getWx()->GetStyledText(startPos, endPos)); }

    // Are there any redoable actions in the undo history?
    //bool CanRedo();
  inline bool canRedo() { return getWx()->CanRedo(); }

    // Retrieve the line number at which a particular marker is located.
    //int MarkerLineFromHandle (int handle);
  inline int markerLineFromHandle(int handle) { return getWx()->MarkerLineFromHandle(handle); }

    // Delete a marker.
    //void MarkerDeleteHandle (int handle);
  inline void markerDeleteHandle(int handle) { getWx()->MarkerDeleteHandle(handle); }

    // Is undo history being collected?
    //bool GetUndoCollection();
  inline bool getUndoCollection() { return getWx()->GetUndoCollection(); }

    // Are white space characters currently visible?
    // Returns one of SCWS_* constants.
    //int GetViewWhiteSpace();
  inline int getViewWhiteSpace() { return getWx()->GetViewWhiteSpace(); }

    // Make white space characters invisible, always visible or visible outside indentation.
    //void SetViewWhiteSpace (int viewWS);
  inline void setViewWhiteSpace(int viewWS) { getWx()->SetViewWhiteSpace(viewWS); }

    // Find the position from a point within the window.
    //int PositionFromPoint (wxPoint pt);
  inline int positionFromPoint(IN(RPoint) pt) { return getWx()->PositionFromPoint(CLS2WXREF(pt)); }

    // Find the position from a point within the window but return
    // INVALID_POSITION if not close to text.
    //int PositionFromPointClose (int x, int y);
  inline int positionFromPointClose(int x, int y) { return getWx()->PositionFromPointClose(x, y); }

    // Set caret to start of a line and ensure it is visible.
    //void GotoLine (int line);
  inline void gotoLine(int line) { getWx()->GotoLine(line); }

    // Set caret to a position and ensure it is visible.
    //void GotoPos (int pos);
  inline void gotoPos(int pos) { getWx()->GotoPos(pos); }

    // Set the selection anchor to a position. The anchor is the opposite
    // end of the selection from the caret.
    //void SetAnchor (int posAnchor);
  inline void setAnchor(int posAnchor) { getWx()->SetAnchor(posAnchor); }

    // Retrieve the text of the line containing the caret.
    // Returns the index of the caret on the line.
    //wxString GetCurLine (int* linePos=NULL);
  inline RString getCurLine(OUT(int) linePos) { return WXS2S(getWx()->GetCurLine(&linePos)); }
  inline RString getCurLine() { return WXS2S(getWx()->GetCurLine(0)); }

    // Retrieve the position of the last correctly styled character.
    //int GetEndStyled();
  inline int getEndStyled() { return getWx()->GetEndStyled(); }

    // Convert all line endings in the document to one mode.
    //void ConvertEOLs (int eolMode);
    inline void convertEOLs(int eolMode) { getWx()->ConvertEOLs(eolMode); }

    // Retrieve the current end of line mode - one of CRLF, CR, or LF.
    //int GetEOLMode();
    inline int getEOLMode() { return getWx()->GetEOLMode(); }

    // Set the current end of line mode.
    //void SetEOLMode (int eolMode);
    inline void setEOLMode(int eolMode) { getWx()->SetEOLMode(eolMode); }

    // Set the current styling position to pos and the styling mask to mask.
    // The styling mask can be used to protect some bits in each styling byte from modification.
    //void StartStyling (int pos, int mask);
    inline void startStyling(int pos, int mask) { getWx()->StartStyling(pos, mask); }

    // Change style from current styling position for length characters to a style
    // and move the current styling position to after this newly styled segment.
    //void SetStyling (int length, int style);
    inline void setStyling(int length, int style) { getWx()->SetStyling(length, style); }

    // Is drawing done first into a buffer or direct to the screen?
    //bool GetBufferedDraw();
    inline bool getBufferedDraw() { return getWx()->GetBufferedDraw(); }

    // If drawing is buffered then each line of text is drawn into a bitmap buffer
    // before drawing it to the screen to avoid flicker.
    //void SetBufferedDraw (bool buffered);
    inline void setBufferedDraw(bool buffered) { getWx()->SetBufferedDraw(buffered); }

    // Change the visible size of a tab to be a multiple of the width of a space character.
    //void SetTabWidth (int tabWidth);
    inline void setTabWidth(int tabWidth) { getWx()->SetTabWidth(tabWidth); }

    // Retrieve the visible size of a tab.
    //int GetTabWidth();
    inline int getTabWidth() { return getWx()->GetTabWidth(); }

    // Set the code page used to interpret the bytes of the document as characters.
    //void SetCodePage (int codePage);
    inline void setCodePage(int codePage) { getWx()->SetCodePage(codePage); }

    // Set the symbol used for a particular marker number,
    //void MarkerDefine (int markerNumber, int markerSymbol);
    inline void markerDefine(int markerNumber, int markerSymbol) { getWx()->MarkerDefine(markerNumber, markerSymbol); }

    // Set the foreground colour used for a particular marker number.
    //void MarkerSetForeground (int markerNumber, const wxColour& fore);
    inline void markerSetForeground(int markerNumber, IN(RColour) fore) { getWx()->MarkerSetForeground(markerNumber, CLS2WXREF(fore)); }

    // Set the background colour used for a particular marker number.
    //void MarkerSetBackground (int markerNumber, const wxColour& back);
    inline void markerSetBackground(int markerNumber, IN(RColour) back) { getWx()->MarkerSetBackground(markerNumber, CLS2WXREF(back)); }

    // Add a marker to a line, returning an ID which can be used to find or delete the marker.
    //int MarkerAdd (int line, int markerNumber);
    inline int markerAdd(int line, int markerNumber) { return getWx()->MarkerAdd(line, markerNumber); }

    // Delete a marker from a line.
    //void MarkerDelete (int line, int markerNumber);
    inline void markerDelete(int line, int markerNumber) { getWx()->MarkerDelete(line, markerNumber); }

    // Delete a marker with a particular number from all lines.
    //void MarkerDeleteAll (int markerNumber);
    inline void markerDeleteAll(int markerNumber) { getWx()->MarkerDeleteAll(markerNumber); }

    // Get a bit mask of all the markers set on a line.
    //int MarkerGet (int line);
    inline int markerGet(int line) { return getWx()->MarkerGet(line); }

    // Find the next line after lineStart that includes a marker in mask.
    //int MarkerNext (int lineStart, int markerMask);
    inline int markerNext(int lineStart, int markerMask) { return getWx()->MarkerNext(lineStart, markerMask); }

    // Find the previous line before lineStart that includes a marker in mask.
    //int MarkerPrevious (int lineStart, int markerMask);
    inline int markerPrevious(int lineStart, int markerMask) { return getWx()->MarkerPrevious(lineStart, markerMask); }

    // Define a marker from a bitmap
    //void MarkerDefineBitmap (int markerNumber, const wxBitmap& bmp);
    inline void markerDefineBitmap(int markerNumber, IN(RBitmap) bmp) { getWx()->MarkerDefineBitmap(markerNumber, CLS2WXREF(bmp)); }

    // Set a margin to be either numeric or symbolic.
    //void SetMarginType (int margin, int marginType);
    inline void setMarginType(int margin, int marginType) { getWx()->SetMarginType(margin, marginType); }

    // Retrieve the type of a margin.
    //int GetMarginType (int margin);
    inline int getMarginType(int margin) { return getWx()->GetMarginType(margin); }

    // Set the width of a margin to a width expressed in pixels.
    //void SetMarginWidth (int margin, int pixels);
    inline void setMarginWidth(int margin, int pixels) { getWx()->SetMarginWidth(margin, pixels); }

    // Retrieve the width of a margin in pixels.
    //int GetMarginWidth (int margin);
    inline int getMarginWidth(int margin) { return getWx()->GetMarginWidth(margin); }

    // Set a mask that determines which markers are displayed in a margin.
    //void SetMarginMask (int margin, int mask);
    inline void setMarginMask(int margin, int mask) { getWx()->SetMarginMask(margin, mask); }

    // Retrieve the marker mask of a margin.
    //int GetMarginMask (int margin);
    inline int getMarginMask(int margin) { return getWx()->GetMarginMask(margin); }

    // Make a margin sensitive or insensitive to mouse clicks.
    //void SetMarginSensitive (int margin, bool sensitive);
    inline void setMarginSensitive(int margin, bool sensitive) { getWx()->SetMarginSensitive(margin, sensitive); }

    // Retrieve the mouse click sensitivity of a margin.
    //bool GetMarginSensitive (int margin);
    inline bool getMarginSensitive(int margin) { return getWx()->GetMarginSensitive(margin); }

    // Clear all the styles and make equivalent to the global default style.
    //void StyleClearAll();
    inline void styleClearAll() { getWx()->StyleClearAll(); }

    // Set the foreground colour of a style.
    //void StyleSetForeground (int style, const wxColour& fore);
    inline void styleSetForeground(int style, IN(RColour) fore) { getWx()->StyleSetForeground(style, CLS2WXREF(fore)); }

    // Set the background colour of a style.
    //void StyleSetBackground (int style, const wxColour& back);
    inline void styleSetBackground(int style, IN(RColour) back) { getWx()->StyleSetBackground(style, CLS2WXREF(back)); }

    // Set a style to be bold or not.
    //void StyleSetBold (int style, bool bold);
    inline void styleSetBold(int style, bool bold) { getWx()->StyleSetBold(style, bold); }

    // Set a style to be italic or not.
    //void StyleSetItalic (int style, bool italic);
    inline void styleSetItalic(int style, bool italic) { getWx()->StyleSetItalic(style, italic); }

    // Set the size of characters of a style.
    //void StyleSetSize (int style, int sizePoints);
    inline void styleSetSize(int style, int sizePoints) { getWx()->StyleSetSize(style, sizePoints); }

    // Set the font of a style.
    //void StyleSetFaceName(int style, const wxString& fontName);
    inline void styleSetFaceName(int style, IN(RString)  fontName) { getWx()->StyleSetFaceName(style, S2WXS(fontName)); }

    // Set a style to have its end of line filled or not.
    //void StyleSetEOLFilled (int style, bool filled);
    inline void styleSetEOLFilled(int style, bool filled) { getWx()->StyleSetEOLFilled(style, filled); }

    // Reset the default style to its state at startup
    //void StyleResetDefault();
    inline void styleResetDefault() { getWx()->StyleResetDefault(); }

    // Set a style to be underlined or not.
    //void StyleSetUnderline (int style, bool underline);
    inline void styleSetUnderline(int style, bool underline) { getWx()->StyleSetUnderline(style, underline); }

    // Set a style to be mixed case, or to force upper or lower case.
    //void StyleSetCase (int style, int caseMode);
    inline void styleSetCase(int style, int caseMode) { getWx()->StyleSetCase(style, caseMode); }

    // Set the character set of the font in a style.
    //void StyleSetCharacterSet (int style, int characterSet);
    inline void styleSetCharacterSet(int style, int characterSet) { getWx()->StyleSetCharacterSet(style, characterSet); }

    // Set a style to be a hotspot or not.
    //void StyleSetHotSpot (int style, bool hotspot);
    inline void styleSetHotSpot(int style, bool hotspot) { getWx()->StyleSetHotSpot(style, hotspot); }

    // Set the foreground colour of the selection and whether to use this setting.
    //void SetSelForeground (bool useSetting, const wxColour& fore);
    inline void setSelForeground(bool useSetting, IN(RColour) fore) { getWx()->SetSelForeground(useSetting, CLS2WXREF(fore)); }

    // Set the background colour of the selection and whether to use this setting.
    //void SetSelBackground (bool useSetting, const wxColour& back);
    inline void setSelBackground(bool useSetting, IN(RColour) back) { getWx()->SetSelBackground(useSetting, CLS2WXREF(back)); }

    // Set the foreground colour of the caret.
    //void SetCaretForeground (const wxColour& fore);
    inline void setCaretForeground(IN(RColour) fore) { getWx()->SetCaretForeground(CLS2WXREF(fore)); }

    // When key+modifier combination km is pressed perform msg.
    //void CmdKeyAssign (int key, int modifiers, int cmd);
    inline void cmdKeyAssign(int key, int modifiers, int cmd) { getWx()->CmdKeyAssign(key, modifiers, cmd); }

    // When key+modifier combination km is pressed do nothing.
    //void CmdKeyClear (int key, int modifiers);
    inline void cmdKeyClear(int key, int modifiers) { getWx()->CmdKeyClear(key, modifiers); }

    // Drop all key mappings.
    //void CmdKeyClearAll();
    inline void cmdKeyClearAll() { getWx()->CmdKeyClearAll(); }

    // Set the styles for a segment of the document.
    //void SetStyleBytes (int length, char* styleBytes);
    //### TODO inline void setStyleBytes(int length, char styleBytes) { getWx()->SetStyleBytes(length, styleBytes); }

    // Set a style to be visible or not.
    //void StyleSetVisible (int style, bool visible);
    inline void styleSetVisible(int style, bool visible) { getWx()->StyleSetVisible(style, visible); }

    // Get the time in milliseconds that the caret is on and off.
    //int GetCaretPeriod();
    inline int getCaretPeriod() { return getWx()->GetCaretPeriod(); }

    // Get the time in milliseconds that the caret is on and off. 0 = steady on.
    //void SetCaretPeriod (int milliseconds);
    inline void setCaretPeriod(int milliseconds) { getWx()->SetCaretPeriod(milliseconds); }

    // Set the set of characters making up words for when moving or selecting by word.
    // First sets deaults like SetCharsDefault.
    //void SetWordChars (const wxString& characters);
    inline void setWordChars(IN(RString)  characters) { getWx()->SetWordChars(S2WXS(characters)); }

    // Start a sequence of actions that is undone and redone as a unit.
    // May be nested.
    //void BeginUndoAction();
    inline void beginUndoAction() { getWx()->BeginUndoAction(); }

    // End a sequence of actions that is undone and redone as a unit.
    //void EndUndoAction();
    inline void endUndoAction() { getWx()->EndUndoAction(); }

    // Set an indicator to plain, squiggle or TT.
    //void IndicatorSetStyle (int indic, int style);
    inline void indicatorSetStyle(int indic, int style) { getWx()->IndicatorSetStyle(indic, style); }

    // Retrieve the style of an indicator.
    //int IndicatorGetStyle (int indic);
    inline int indicatorGetStyle(int indic) { return getWx()->IndicatorGetStyle(indic); }

    // Set the foreground colour of an indicator.
    //void IndicatorSetForeground (int indic, const wxColour& fore);
    inline void indicatorSetForeground(int indic, IN(RColour) fore) { getWx()->IndicatorSetForeground(indic, CLS2WXREF(fore)); }

    // Retrieve the foreground colour of an indicator.
    //wxColour IndicatorGetForeground (int indic);
    inline RColour indicatorGetForeground(int indic) { return WXVAL2CLS(Colour, getWx()->IndicatorGetForeground(indic)); }

    // Set the foreground colour of all whitespace and whether to use this setting.
    //void SetWhitespaceForeground (bool useSetting, const wxColour& fore);
    inline void setWhitespaceForeground(bool useSetting, IN(RColour) fore) { getWx()->SetWhitespaceForeground(useSetting, CLS2WXREF(fore)); }

    // Set the background colour of all whitespace and whether to use this setting.
    //void SetWhitespaceBackground (bool useSetting, const wxColour& back);
    inline void setWhitespaceBackground(bool useSetting, IN(RColour) back) { getWx()->SetWhitespaceBackground(useSetting, CLS2WXREF(back)); }

    // Divide each styling byte into lexical class bits (default: 5) and indicator
    // bits (default: 3). If a lexer requires more than 32 lexical states, then this
    // is used to expand the possible states.
    //void SetStyleBits (int bits);
    inline void setStyleBits(int bits) { getWx()->SetStyleBits(bits); }

    // Retrieve number of bits in style bytes used to hold the lexical state.
    //int GetStyleBits ();
    inline int getStyleBits() { return getWx()->GetStyleBits(); }

    // Used to hold extra styling information for each line.
    //void SetLineState (int line, int state);
    inline void setLineState(int line, int state) { getWx()->SetLineState(line, state); }

    // Retrieve the extra styling information for a line.
    //int GetLineState (int line);
    inline int getLineState(int line) { return getWx()->GetLineState(line); }

    // Retrieve the last line number that has line state.
    //int GetMaxLineState();
    inline int getMaxLineState() { return getWx()->GetMaxLineState(); }

    // Is the background of the line containing the caret in a different colour?
    //bool GetCaretLineVisible();
    inline bool getCaretLineVisible() { return getWx()->GetCaretLineVisible(); }

    // Display the background of the line containing the caret in a different colour.
    //void SetCaretLineVisible (bool show);
    inline void setCaretLineVisible(bool show) { getWx()->SetCaretLineVisible(show); }

    // Get the colour of the background of the line containing the caret.
    //wxColour GetCaretLineBackground();
    // not supported by stcinline RColour getCaretLineBackground() { return WXVAL2CLS(Colour, getWx()->GetCaretLineBackground()); }

    // Set the colour of the background of the line containing the caret.
    //void SetCaretLineBackground (const wxColour& back);
    //not supported by stc inline void setCaretLineBackground(IN(RColour) back) { getWx()->SetCaretLineBackground(CLS2WXREF(back)); }

    // Set a style to be changeable or not (read only).
    // Experimental feature, currently buggy.
    //void StyleSetChangeable (int style, bool changeable);
    inline void styleSetChangeable(int style, bool changeable) { getWx()->StyleSetChangeable(style, changeable); }

    // Display a auto-completion list.
    // The lenEntered parameter indicates how many characters before
    // the caret should be used to provide context.
    //void AutoCompShow (int lenEntered, const wxString& itemList);
    inline void autoCompShow(int lenEntered, IN(RString)  itemList) { getWx()->AutoCompShow(lenEntered, S2WXS(itemList)); }

    // Remove the auto-completion list from the screen.
    //void AutoCompCancel();
    inline void autoCompCancel() { getWx()->AutoCompCancel(); }

    // Is there an auto-completion list visible?
    //bool AutoCompActive();
    inline bool autoCompActive() { return getWx()->AutoCompActive(); }

    // Retrieve the position of the caret when the auto-completion list was displayed.
    //int AutoCompPosStart();
    inline int autoCompPosStart() { return getWx()->AutoCompPosStart(); }

    // User has selected an item so remove the list and insert the selection.
    //void AutoCompComplete();
    inline void autoCompComplete() { getWx()->AutoCompComplete(); }

    // Define a set of character that when typed cancel the auto-completion list.
    //void AutoCompStops (const wxString& characterSet);
    inline void autoCompStops(IN(RString)  characterSet) { getWx()->AutoCompStops(S2WXS(characterSet)); }

    // Change the separator character in the string setting up an auto-completion list.
    //// Default is space but can be changed if items contain space.
    //void AutoCompSetSeparator (int separatorCharacter);
    inline void autoCompSetSeparator(int separatorCharacter) { getWx()->AutoCompSetSeparator(separatorCharacter); }

    // Retrieve the auto-completion list separator character.
    //int AutoCompGetSeparator();
    inline int autoCompGetSeparator() { return getWx()->AutoCompGetSeparator(); }


    // Select the item in the auto-completion list that starts with a string.
    //void AutoCompSelect (const wxString& text);
    inline void autoCompSelect(IN(RString)  text) { getWx()->AutoCompSelect(S2WXS(text)); }

    // Should the auto-completion list be cancelled if the user backspaces to a
    // position before where the box was created.
    //void AutoCompSetCancelAtStart (bool cancel);
    inline void autoCompSetCancelAtStart(bool cancel) { getWx()->AutoCompSetCancelAtStart(cancel); }

    // Retrieve whether auto-completion cancelled by backspacing before start.
    //bool AutoCompGetCancelAtStart();
    inline bool autoCompGetCancelAtStart() { return getWx()->AutoCompGetCancelAtStart(); }

    // Define a set of characters that when typed will cause the autocompletion to
    // choose the selected item.
    //void AutoCompSetFillUps (const wxString& characterSet);
    inline void autoCompSetFillUps(IN(RString)  characterSet) { getWx()->AutoCompSetFillUps(S2WXS(characterSet)); }

    // Should a single item auto-completion list automatically choose the item.
    //void AutoCompSetChooseSingle (bool chooseSingle);
    inline void autoCompSetChooseSingle(bool chooseSingle) { getWx()->AutoCompSetChooseSingle(chooseSingle); }

    // Retrieve whether a single item auto-completion list automatically choose the item.
    //bool AutoCompGetChooseSingle();
    inline bool autoCompGetChooseSingle() { return getWx()->AutoCompGetChooseSingle(); }

    // Set whether case is significant when performing auto-completion searches.
    //void AutoCompSetIgnoreCase (bool ignoreCase);
    inline void autoCompSetIgnoreCase(bool ignoreCase) { getWx()->AutoCompSetIgnoreCase(ignoreCase); }

    // Retrieve state of ignore case flag.
    //bool AutoCompGetIgnoreCase();
    inline bool autoCompGetIgnoreCase() { return getWx()->AutoCompGetIgnoreCase(); }

    // Display a list of strings and send notification when user chooses one.
    //void UserListShow (int listType, const wxString& itemList);
    inline void userListShow(int listType, IN(RString)  itemList) { getWx()->UserListShow(listType, S2WXS(itemList)); }

    // Set whether or not autocompletion is hidden automatically when nothing matches.
    //void AutoCompSetAutoHide (bool autoHide);
    inline void autoCompSetAutoHide(bool autoHide) { getWx()->AutoCompSetAutoHide(autoHide); }

    // Retrieve whether or not autocompletion is hidden automatically when nothing matches.
    //bool AutoCompGetAutoHide();
    inline bool autoCompGetAutoHide() { return getWx()->AutoCompGetAutoHide(); }

    // Set whether or not autocompletion deletes any word characters
    // after the inserted text upon completion.
    //void AutoCompSetDropRestOfWord (bool dropRestOfWord);
    inline void autoCompSetDropRestOfWord(bool dropRestOfWord) { getWx()->AutoCompSetDropRestOfWord(dropRestOfWord); }

    // Retrieve whether or not autocompletion deletes any word characters
    // after the inserted text upon completion.
    //bool AutoCompGetDropRestOfWord();
    inline bool autoCompGetDropRestOfWord() { return getWx()->AutoCompGetDropRestOfWord(); }

    // Register an image for use in autocompletion lists.
    //void RegisterImage (int type, const wxBitmap& bmp);
    inline void registerImage(int type, IN(RBitmap) bmp) { getWx()->RegisterImage(type, CLS2WXREF(bmp)); }
//
    // Clear all the registered images.
    //void ClearRegisteredImages();
    inline void clearRegisteredImages() { getWx()->ClearRegisteredImages(); }


    // Retrieve the auto-completion list type-separator character.
    //int AutoCompGetTypeSeparator();
    inline int autoCompGetTypeSeparator() { return getWx()->AutoCompGetTypeSeparator(); }

    // Change the type-separator character in the string setting up an auto-completion list.
    // Default is '?' but can be changed if items contain '?'.
    //void AutoCompSetTypeSeparator (int separatorCharacter);
    inline void autoCompSetTypeSeparator(int separatorCharacter) { getWx()->AutoCompSetTypeSeparator(separatorCharacter); }

    // Set the number of spaces used for one level of indentation.
    //void SetIndent (int indentSize);
    inline void setIndent(int indentSize) { getWx()->SetIndent(indentSize); }

    // Retrieve indentation size.
    //int GetIndent();
    inline int getIndent() { return getWx()->GetIndent(); }

    // Indentation will only use space characters if useTabs is false, otherwise
    // it will use a combination of tabs and spaces.
    //void SetUseTabs (bool useTabs);
    inline void setUseTabs(bool useTabs) { getWx()->SetUseTabs(useTabs); }

    // Retrieve whether tabs will be used in indentation.
    //bool GetUseTabs();
    inline bool getUseTabs() { return getWx()->GetUseTabs(); }

    // Change the indentation of a line to a number of columns.
    //void SetLineIndentation (int line, int indentSize);
    inline void setLineIndentation(int line, int indentSize) { getWx()->SetLineIndentation(line, indentSize); }

    // Retrieve the number of columns that a line is indented.
    //int GetLineIndentation (int line);
    inline int getLineIndentation(int line) { return getWx()->GetLineIndentation(line); }

    // Retrieve the position before the first non indentation character on a line.
    //int GetLineIndentPosition (int line);
    inline int getLineIndentPosition(int line) { return getWx()->GetLineIndentPosition(line); }

    // Retrieve the column number of a position, taking tab width into account.
    //int GetColumn (int pos);
    inline int getColumn(int pos) { return getWx()->GetColumn(pos); }

    // Returns the position of a column on a line taking the width of tabs into account.
    //int FindColumn (int line, int column);
    // not supported by STC inline int findColumn(int line, int column) { return getWx()->FindColumn(line, column); }

    // Show or hide the horizontal scroll bar.
    //void SetUseHorizontalScrollBar (bool show);
    inline void setUseHorizontalScrollBar(bool show) { getWx()->SetUseHorizontalScrollBar(show); }

    // Is the horizontal scroll bar visible?
    //bool GetUseHorizontalScrollBar();
    inline bool getUseHorizontalScrollBar() { return getWx()->GetUseHorizontalScrollBar(); }

    // Show or hide indentation guides.
    //void SetIndentationGuides (bool show);
    inline void setIndentationGuides(bool show) { getWx()->SetIndentationGuides(show); }

    // Are the indentation guides visible?
    //bool GetIndentationGuides();
    inline bool getIndentationGuides() { return getWx()->GetIndentationGuides(); }

    // Set the highlighted indentation guide column.
    // 0 = no highlighted guide.
    //void SetHighlightGuide (int column);
    inline void setHighlightGuide(int column) { getWx()->SetHighlightGuide(column); }

    // Get the highlighted indentation guide column.
    //int GetHighlightGuide();
    inline int getHighlightGuide() { return getWx()->GetHighlightGuide(); }

    // Get the position after the last visible characters on a line.
    //int GetLineEndPosition (int line);
    inline int getLineEndPosition(int line) { return getWx()->GetLineEndPosition(line); }

    // Get the code page used to interpret the bytes of the document as characters.
    //int GetCodePage();
    inline int getCodePage() { return getWx()->GetCodePage(); }

    // Get the foreground colour of the caret.
    //wxColour GetCaretForeground();
    inline RColour getCaretForeground() { return WXVAL2CLS(Colour, getWx()->GetCaretForeground()); }

    // In read-only mode?
    //bool GetReadOnly();
    inline bool getReadOnly() { return getWx()->GetReadOnly(); }

    // Sets the position of the caret.
    //void SetCurrentPos (int pos);
    inline void setCurrentPos(int pos) { getWx()->SetCurrentPos(pos); }

    // Sets the position that starts the selection - this becomes the anchor.
    //void SetSelectionStart (int pos);
    inline void setSelectionStart(int pos) { getWx()->SetSelectionStart(pos); }

    // Returns the position at the start of the selection.
    //int GetSelectionStart();
    inline int getSelectionStart() { return getWx()->GetSelectionStart(); }

    // Sets the position that ends the selection - this becomes the currentPosition.
    //void SetSelectionEnd (int pos);
    inline void setSelectionEnd(int pos) { getWx()->SetSelectionEnd(pos); }

    // Returns the position at the end of the selection.
    //int GetSelectionEnd();
    inline int getSelectionEnd() { return getWx()->GetSelectionEnd(); }

    // Sets the print magnification added to the point size of each style for printing.
    //void SetPrintMagnification (int magnification);
    inline void setPrintMagnification(int magnification) { getWx()->SetPrintMagnification(magnification); }

    // Returns the print magnification.
    //int GetPrintMagnification();
    inline int getPrintMagnification() { return getWx()->GetPrintMagnification(); }

    // Modify colours when printing for clearer printed text.
    //void SetPrintColourMode (int mode);
    inline void setPrintColourMode(int mode) { getWx()->SetPrintColourMode(mode); }

    // Returns the print colour mode.
    //int GetPrintColourMode();
    inline int getPrintColourMode() { return getWx()->GetPrintColourMode(); }

    // Find some text in the document.
    //int FindText (int minPos, int maxPos, const wxString& text, int flags=0);
    inline int findText(int minPos, int maxPos, IN(RString)  text, int flags = 0) { return getWx()->FindText(minPos, maxPos, S2WXS(text), flags); }

    // On Windows, will draw the document into a display context such as a printer.
    //int FormatRange (bool doDraw, int startPos, int endPos, wxDC* draw, wxDC* target, wxRect renderRect, wxRect pageRect);
    inline int formatRange(bool doDraw, int startPos, int endPos, IN(RDC) draw, IN(RDC) target, IN(RRect) renderRect, IN(RRect) pageRect) 
    { return getWx()->FormatRange(doDraw, startPos, endPos, CLS2WXPTR(draw), CLS2WXPTR(target), CLS2WXREF(renderRect), CLS2WXREF(pageRect)); }

    // Retrieve the display line at the top of the display.
    //int GetFirstVisibleLine();
    inline int getFirstVisibleLine() { return getWx()->GetFirstVisibleLine(); }

    // Retrieve the contents of a line.
    //wxString GetLine (int line);
    inline RString getLine(int line) { return WXS2S(getWx()->GetLine(line)); }
    
    // Returns the number of lines in the document. There is always at least one.
    //int GetLineCount();
    inline int getLineCount() { return getWx()->GetLineCount(); }

    // Sets the size in pixels of the left margin.
    //void SetMarginLeft (int pixels);
    inline void setMarginLeft(int pixels) { getWx()->SetMarginLeft(pixels); }

    // Returns the size in pixels of the left margin.
    //int GetMarginLeft();
    inline int getMarginLeft() { return getWx()->GetMarginLeft(); }

    // Sets the size in pixels of the right margin.
    //void SetMarginRight (int pixels);
    inline void setMarginRight(int pixels) { getWx()->SetMarginRight(pixels); }

    // Returns the size in pixels of the right margin.
    //int GetMarginRight();
    inline int getMarginRight() { return getWx()->GetMarginRight(); }

    // Is the document different from when it was last saved?
    //bool GetModify();
    inline bool getModify() { return getWx()->GetModify(); }

    // Select a range of text.
    //void SetSelection (int startPos, int endPos);
    inline void setSelection(int startPos, int endPos) { getWx()->SetSelection(startPos, endPos); }

    // Retrieve the selected text.
    //wxString GetSelectedText();
    inline RString getSelectedText() { return WXS2S(getWx()->GetSelectedText()); }

    // Retrieve a range of text.
    //wxString GetTextRange (int startPos, int endPos);
    inline RString getTextRange(int startPos, int endPos) { return WXS2S(getWx()->GetTextRange(startPos, endPos)); }

    // Draw the selection in normal style or with selection highlighted.
    //void HideSelection (bool hide);
    inline void hideSelection(bool hide) { getWx()->HideSelection(hide); }

    // Retrieve the line containing a position.
    //int LineFromPosition (int pos);
    inline int lineFromPosition(int pos) { return getWx()->LineFromPosition(pos); }

    // Retrieve the position at the start of a line.
    //int PositionFromLine (int line);
    inline int positionFromLine(int line) { return getWx()->PositionFromLine(line); }

    // Scroll horizontally and vertically.
    //void LineScroll (int columns, int lines);
    inline void lineScroll(int columns, int lines) { getWx()->LineScroll(columns, lines); }

    // Ensure the caret is visible.
    //void EnsureCaretVisible();
    inline void ensureCaretVisible() { getWx()->EnsureCaretVisible(); }

    // Replace the selected text with the argument text.
    //void ReplaceSelection (const wxString& text);
    inline void replaceSelection(IN(RString)  text) { getWx()->ReplaceSelection(S2WXS(text)); }

    // Set to read only or read write.
    //void SetReadOnly (bool readOnly);
    inline void setReadOnly(bool readOnly) { getWx()->SetReadOnly(readOnly); }

    // Will a paste succeed?
    //bool CanPaste();
    inline bool canPaste() { return getWx()->CanPaste(); }

    // Are there any undoable actions in the undo history?
    //bool CanUndo();
    inline bool canUndo() { return getWx()->CanUndo(); }

    // Delete the undo history.
    //void EmptyUndoBuffer();
    inline void emptyUndoBuffer() { getWx()->EmptyUndoBuffer(); }

    // Undo one action in the undo history.
    //void Undo();
    inline void undo() { getWx()->Undo(); }

    // Cut the selection to the clipboard.
    //void Cut();
    inline void cut() { getWx()->Cut(); }

    // Copy the selection to the clipboard.
    //void Copy();
    inline void copy() { getWx()->Copy(); }

    // Paste the contents of the clipboard into the document replacing the selection.
    //void Paste();
    inline void paste() { getWx()->Paste(); }

    // Clear the selection.
    //void Clear();
    inline void clear() { getWx()->Clear(); }

    // Replace the contents of the document with the argument text.
    //void SetText (const wxString& text);
    inline void setText(IN(RString)  text) { getWx()->SetText(S2WXS(text)); }

    // Retrieve all the text in the document.
    //wxString GetText();
    inline RString getText() { return WXS2S(getWx()->GetText()); }

    // Retrieve the number of characters in the document.
    //int GetTextLength();
    inline int getTextLength() { return getWx()->GetTextLength(); }

    // Set to overtype (true) or insert mode.
    //void SetOvertype (bool overtype);
    inline void setOvertype(bool overtype) { getWx()->SetOvertype(overtype); }

    // Returns true if overtype mode is active otherwise false is returned.
    //bool GetOvertype();
    inline bool getOvertype() { return getWx()->GetOvertype(); }

    // Set the width of the insert mode caret.
    //void SetCaretWidth (int pixels);
    inline void setCaretWidth(int pixels) { getWx()->SetCaretWidth(pixels); }

    // Returns the width of the insert mode caret.
    //int GetCaretWidth();
    inline int getCaretWidth() { return getWx()->GetCaretWidth(); }

    // Sets the position that starts the target which is used for updating the
    //// document without affecting the scroll position.
    //void SetTargetStart (int pos);
    inline void setTargetStart(int pos) { getWx()->SetTargetStart(pos); }


    // Get the position that starts the target.
    //int GetTargetStart();
    inline int getTargetStart() { return getWx()->GetTargetStart(); }

    // Sets the position that ends the target which is used for updating the
    // document without affecting the scroll position.
    //void SetTargetEnd (int pos);
    inline void setTargetEnd(int pos) { getWx()->SetTargetEnd(pos); }

    // Get the position that ends the target.
    //int GetTargetEnd();
    inline int getTargetEnd() { return getWx()->GetTargetEnd(); }

    // Replace the target text with the argument text.
    // Text is counted so it can contain NULs.
    // Returns the length of the replacement text.
    //int ReplaceTarget (const wxString& text);
    inline int replaceTarget(IN(RString)  text) { return getWx()->ReplaceTarget(S2WXS(text)); }

    // Replace the target text with the argument text after \d processing.
    // Text is counted so it can contain NULs.
    // Looks for \d where d is between 1 and 9 and replaces these with the strings
    // matched in the last search operation which were surrounded by \( and \).
    // Returns the length of the replacement text including any change
    // caused by processing the \d patterns.
    //int ReplaceTargetRE (const wxString& text);
    inline int replaceTargetRE(IN(RString)  text) { return getWx()->ReplaceTargetRE(S2WXS(text)); }

    // Search for a counted string in the target and set the target to the found
    // range. Text is counted so it can contain NULs.
    // Returns length of range or -1 for failure in which case target is not moved.
    //int SearchInTarget (const wxString& text);
    inline int searchInTarget(IN(RString)  text) { return getWx()->SearchInTarget(S2WXS(text)); }

    // Set the search flags used by SearchInTarget.
    //void SetSearchFlags (int flags);
    inline void setSearchFlags(int flags) { getWx()->SetSearchFlags(flags); }

    // Get the search flags used by SearchInTarget.
    //int GetSearchFlags();
    inline int getSearchFlags() { return getWx()->GetSearchFlags(); }

    // Show a call tip containing a definition near position pos.
    //void CallTipShow (int pos, const wxString& definition);
    inline void callTipShow(int pos, IN(RString)  definition) { getWx()->CallTipShow(pos, S2WXS(definition)); }

    // Remove the call tip from the screen.
    //void CallTipCancel();
    inline void callTipCancel() { getWx()->CallTipCancel(); }

    // Is there an active call tip?
    //bool CallTipActive();
    inline bool callTipActive() { return getWx()->CallTipActive(); }

    // Retrieve the position where the caret was before displaying the call tip.
    //int CallTipPosAtStart();
    inline int callTipPosAtStart() { return getWx()->CallTipPosAtStart(); }

    // Highlight a segment of the definition.
    //void CallTipSetHighlight (int startPos, int endPos);
    inline void callTipSetHighlight(int startPos, int endPos) { getWx()->CallTipSetHighlight(startPos, endPos); }

    // Set the background colour for the call tip.
    //void CallTipSetBackground (const wxColour& back);
    inline void callTipSetBackground(IN(RColour) back) { getWx()->CallTipSetBackground(CLS2WXREF(back)); }

    // Set the foreground colour for the call tip.
    //void CallTipSetForeground (const wxColour& fore);
    inline void callTipSetForeground(IN(RColour) fore) { getWx()->CallTipSetForeground(CLS2WXREF(fore)); }

    // Set the foreground colour for the highlighted part of the call tip.
    //void CallTipSetForegroundHighlight (const wxColour& fore);
    inline void callTipSetForegroundHighlight(IN(RColour) fore) { getWx()->CallTipSetForegroundHighlight(CLS2WXREF(fore)); }

    // Find the display line of a document line taking hidden lines into account.
    //int VisibleFromDocLine (int line);
    inline int visibleFromDocLine(int line) { return getWx()->VisibleFromDocLine(line); }

    // Find the document line of a display line taking hidden lines into account.
    //int DocLineFromVisible (int lineDisplay);
    inline int docLineFromVisible(int lineDisplay) { return getWx()->DocLineFromVisible(lineDisplay); }

    // Set the fold level of a line.
    // This encodes an integer level along with flags indicating whether the
    // line is a header and whether it is effectively white space.
    //void SetFoldLevel (int line, int level);
    inline void setFoldLevel(int line, int level) { getWx()->SetFoldLevel(line, level); }

    // Retrieve the fold level of a line.
    //int GetFoldLevel (int line);
    inline int getFoldLevel(int line) { return getWx()->GetFoldLevel(line); }

    // Find the last child line of a header line.
    //int GetLastChild (int line, int level);
    inline int getLastChild(int line, int level) { return getWx()->GetLastChild(line, level); }

    // Find the parent line of a child line.
    //int GetFoldParent (int line);
    inline int getFoldParent(int line) { return getWx()->GetFoldParent(line); }

    // Make a range of lines visible.
    //void ShowLines (int lineStart, int lineEnd);
    inline void showLines(int lineStart, int lineEnd) { getWx()->ShowLines(lineStart, lineEnd); }

    // Make a range of lines invisible.
    //void HideLines (int lineStart, int lineEnd);
    inline void hideLines(int lineStart, int lineEnd) { getWx()->HideLines(lineStart, lineEnd); }

    // Is a line visible?
    //bool GetLineVisible (int line);
    inline bool getLineVisible(int line) { return getWx()->GetLineVisible(line); }

    // Show the children of a header line.
    //void SetFoldExpanded (int line, bool expanded);
    inline void setFoldExpanded(int line, bool expanded) { getWx()->SetFoldExpanded(line, expanded); }

    // Is a header line expanded?
    //bool GetFoldExpanded (int line);
    inline bool getFoldExpanded(int line) { return getWx()->GetFoldExpanded(line); }

    // Switch a header line between expanded and contracted.
    //void ToggleFold (int line);
    inline void toggleFold(int line) { getWx()->ToggleFold(line); }

    // Ensure a particular line is visible by expanding any header line hiding it.
    //void EnsureVisible (int line);
    inline void ensureVisible(int line) { getWx()->EnsureVisible(line); }

    // Set some style options for folding.
    //void SetFoldFlags (int flags);
    inline void setFoldFlags(int flags) { getWx()->SetFoldFlags(flags); }

    // Ensure a particular line is visible by expanding any header line hiding it.
    // Use the currently set visibility policy to determine which range to display.
    //void EnsureVisibleEnforcePolicy (int line);
    inline void ensureVisibleEnforcePolicy(int line) { getWx()->EnsureVisibleEnforcePolicy(line); }

    // Sets whether a tab pressed when caret is within indentation indents.
    //void SetTabIndents (bool tabIndents);
    inline void setTabIndents(bool tabIndents) { getWx()->SetTabIndents(tabIndents); }

    // Does a tab pressed when caret is within indentation indent?
    //bool GetTabIndents();
    inline bool getTabIndents() { return getWx()->GetTabIndents(); }

    // Sets whether a backspace pressed when caret is within indentation unindents.
    //void SetBackSpaceUnIndents (bool bsUnIndents);
    inline void setBackSpaceUnIndents(bool bsUnIndents) { getWx()->SetBackSpaceUnIndents(bsUnIndents); }

    // Does a backspace pressed when caret is within indentation unindent?
    //bool GetBackSpaceUnIndents();
    inline bool getBackSpaceUnIndents() { return getWx()->GetBackSpaceUnIndents(); }

    // Sets the time the mouse must sit still to generate a mouse dwell event.
    //void SetMouseDwellTime (int periodMilliseconds);
    inline void setMouseDwellTime(int periodMilliseconds) { getWx()->SetMouseDwellTime(periodMilliseconds); }

    // Retrieve the time the mouse must sit still to generate a mouse dwell event.
    //int GetMouseDwellTime();
    inline int getMouseDwellTime() { return getWx()->GetMouseDwellTime(); }

    // Get position of start of word.
    //int WordStartPosition (int pos, bool onlyWordCharacters);
    inline int wordStartPosition(int pos, bool onlyWordCharacters) { return getWx()->WordStartPosition(pos, onlyWordCharacters); }

    // Get position of end of word.
    //int WordEndPosition (int pos, bool onlyWordCharacters);
    inline int wordEndPosition(int pos, bool onlyWordCharacters) { return getWx()->WordEndPosition(pos, onlyWordCharacters); }

    // Sets whether text is word wrapped.
    //void SetWrapMode (int mode);
    inline void setWrapMode(int mode) { getWx()->SetWrapMode(mode); }

    // Retrieve whether text is word wrapped.
    //int GetWrapMode();
    inline int getWrapMode() { return getWx()->GetWrapMode(); }

    // Set the display mode of visual flags for wrapped lines.
    //void SetWrapVisualFlags (int wrapVisualFlags);
    // not supported by STC inline void setWrapVisualFlags(int wrapVisualFlags) { getWx()->SetWrapVisualFlags(wrapVisualFlags); }

    // Retrive the display mode of visual flags for wrapped lines.
    //int GetWrapVisualFlags();
    // not supported by STC inline int getWrapVisualFlags() { return getWx()->GetWrapVisualFlags(); }

    // Set the location of visual flags for wrapped lines.
    //void SetWrapVisualFlagsLocation (int wrapVisualFlagsLocation);
    // not supported by STC inline void setWrapVisualFlagsLocation(int wrapVisualFlagsLocation) { getWx()->SetWrapVisualFlagsLocation(wrapVisualFlagsLocation); }

    // Retrive the location of visual flags for wrapped lines.
    //int GetWrapVisualFlagsLocation();
    // not supported by STC inline int getWrapVisualFlagsLocation() { return getWx()->GetWrapVisualFlagsLocation(); }

    // Set the start indent for wrapped lines.
    //void SetWrapStartIndent (int indent);
    // not supported by STC inline void setWrapStartIndent(int indent) { getWx()->SetWrapStartIndent(indent); }

    // Retrive the start indent for wrapped lines.
    //int GetWrapStartIndent();
    // not supported by STC inline int getWrapStartIndent() { return getWx()->GetWrapStartIndent(); }

    // Sets the degree of caching of layout information.
    //void SetLayoutCache (int mode);
    /**
      see StcCache
    */
    inline void setLayoutCache(int mode) { getWx()->SetLayoutCache(mode); }

    // Retrieve the degree of caching of layout information.
    //int GetLayoutCache();
    inline int getLayoutCache() { return getWx()->GetLayoutCache(); }

    // Sets the document width assumed for scrolling.
    //void SetScrollWidth (int pixels);
    inline void setScrollWidth(int pixels) { getWx()->SetScrollWidth(pixels); }

    // Retrieve the document width assumed for scrolling.
    //int GetScrollWidth();
    inline int getScrollWidth() { return getWx()->GetScrollWidth(); }

    // Measure the pixel width of some text in a particular style.
    // NUL terminated text argument.
    // Does not handle tab or control characters.
    //int TextWidth (int style, const wxString& text);
    inline int textWidth(int style, IN(RString)  text) { return getWx()->TextWidth(style, S2WXS(text)); }

    // Sets the scroll range so that maximum scroll position has
    // the last line at the bottom of the view (default).
    // Setting this to false allows scrolling one page below the last line.
    //void SetEndAtLastLine (bool endAtLastLine);
    inline void setEndAtLastLine(bool endAtLastLine) { getWx()->SetEndAtLastLine(endAtLastLine); }

    // Retrieve whether the maximum scroll position has the last
    // line at the bottom of the view.
    //int GetEndAtLastLine();
    inline int getEndAtLastLine() { return getWx()->GetEndAtLastLine(); }

    // Retrieve the height of a particular line of text in pixels.
    //int TextHeight (int line);
    inline int textHeight(int line) { return getWx()->TextHeight(line); }

    // Show or hide the vertical scroll bar.
    //void SetUseVerticalScrollBar (bool show);
    inline void setUseVerticalScrollBar(bool show) { getWx()->SetUseVerticalScrollBar(show); }

    // Is the vertical scroll bar visible?
    //bool GetUseVerticalScrollBar();
    inline bool getUseVerticalScrollBar() { return getWx()->GetUseVerticalScrollBar(); }

    // Append a string to the end of the document without changing the selection.
        
    inline void appendText(IN(RString)  text) 
    { 
#if ACDK_CHECK_WX_VERSION(2, 6)
      getWx()->AppendText(S2WXS(text)); 
#else
      getWx()->AppendText(text->length(), S2WXS(text)); 
#endif
    }

    // Is drawing done in two phases with backgrounds drawn before foregrounds?
    //bool GetTwoPhaseDraw();
    inline bool getTwoPhaseDraw() { return getWx()->GetTwoPhaseDraw(); }

    // In twoPhaseDraw mode, drawing is performed in two phases, first the background
    // and then the foreground. This avoids chopping off characters that overlap the next run.
    //void SetTwoPhaseDraw (bool twoPhase);
    inline void setTwoPhaseDraw(bool twoPhase) { getWx()->SetTwoPhaseDraw(twoPhase); }

    // Make the target range start and end be the same as the selection range start and end.
    //void TargetFromSelection();
    inline void targetFromSelection() { getWx()->TargetFromSelection(); }

    // Join the lines in the target.
    //void LinesJoin();
    inline void linesJoin() { getWx()->LinesJoin(); }

    // Split the lines in the target into lines that are less wide than pixelWidth
    // where possible.
    //void LinesSplit (int pixels);
    inline void linesSplit(int pixels) { getWx()->LinesSplit(pixels); }

    // Set the colours used as a chequerboard pattern in the fold margin
    //void SetFoldMarginColour (bool useSetting, const wxColour& back);
    inline void setFoldMarginColour(bool useSetting, IN(RColour) back) { getWx()->SetFoldMarginColour(useSetting, CLS2WXREF(back)); }
    //void SetFoldMarginHiColour (bool useSetting, const wxColour& fore);
    inline void setFoldMarginHiColour(bool useSetting, IN(RColour) fore) { getWx()->SetFoldMarginHiColour(useSetting, CLS2WXREF(fore)); }

    // Move caret down one line.
    //void LineDown();
    inline void lineDown() { getWx()->LineDown(); }

    // Move caret down one line extending selection to new caret position.
    //void LineDownExtend();
    // not supported by STC  inline void lineDownExtend() { getWx()->LineDownExtend(); }

    // Move caret up one line.
    //void LineUp();
    inline void lineUp() { getWx()->LineUp(); }

    // Move caret up one line extending selection to new caret position.
    //void LineUpExtend();
    inline void lineUpExtend() { EXT_TEXTMARFUNC(getWx()->LineUpExtend()); }

    // Move caret left one character.
    //void CharLeft();
    inline void charLeft() { EXT_TEXTMARFUNC(getWx()->CharLeft()); }

    // Move caret left one character extending selection to new caret position.
    //void CharLeftExtend();
    inline void charLeftExtend() { EXT_TEXTMARFUNC(getWx()->CharLeftExtend()); }

    // Move caret right one character.
    //void CharRight();
    inline void charRight() { EXT_TEXTMARFUNC(getWx()->CharRight()); }

    // Move caret right one character extending selection to new caret position.
    //void CharRightExtend();
    inline void charRightExtend() { EXT_TEXTMARFUNC(getWx()->CharRightExtend()); }

    // Move caret left one word.
    //void WordLeft();
    inline void wordLeft() { EXT_TEXTMARFUNC(getWx()->WordLeft()); }

    // Move caret left one word extending selection to new caret position.
    //void WordLeftExtend();
    inline void wordLeftExtend() { EXT_TEXTMARFUNC(getWx()->WordLeftExtend()); }

    // Move caret right one word.
    //void WordRight();
    inline void wordRight() { EXT_TEXTMARFUNC(getWx()->WordRight()); }

    // Move caret right one word extending selection to new caret position.
    //void WordRightExtend();
    inline void wordRightExtend() { EXT_TEXTMARFUNC(getWx()->WordRightExtend()); }

    // Move caret to first position on line.
    //void Home();
    inline void home() { EXT_TEXTMARFUNC(getWx()->Home()); }

    // Move caret to first position on line extending selection to new caret position.
    //void HomeExtend();
    inline void homeExtend() { EXT_TEXTMARFUNC(getWx()->HomeExtend()); }

    // Move caret to last position on line.
    //void LineEnd();
    inline void lineEnd() { EXT_TEXTMARFUNC(getWx()->LineEnd()); }

    // Move caret to last position on line extending selection to new caret position.
    //void LineEndExtend();
    inline void lineEndExtend() { EXT_TEXTMARFUNC(getWx()->LineEndExtend()); }

    // Move caret to first position in document.
    //void DocumentStart();
    inline void documentStart() { EXT_TEXTMARFUNC(getWx()->DocumentStart()); }

    // Move caret to first position in document extending selection to new caret position.
    //void DocumentStartExtend();
    inline void documentStartExtend() { EXT_TEXTMARFUNC(getWx()->DocumentStartExtend()); }

    // Move caret to last position in document.
    //void DocumentEnd();
    inline void documentEnd() { EXT_TEXTMARFUNC(getWx()->DocumentEnd()); }

    // Move caret to last position in document extending selection to new caret position.
    //void DocumentEndExtend();
    inline void documentEndExtend() { EXT_TEXTMARFUNC(getWx()->DocumentEndExtend()); }

    // Move caret one page up.
    //void PageUp();
    inline void pageUp() { getWx()->PageUp(); }

    // Move caret one page up extending selection to new caret position.
    //void PageUpExtend();
    inline void pageUpExtend() { EXT_TEXTMARFUNC(getWx()->PageUpExtend()); }

    // Move caret one page down.
    //void PageDown();
    inline void pageDown() { getWx()->PageDown(); }

    // Move caret one page down extending selection to new caret position.
    //void PageDownExtend();
    inline void pageDownExtend() { EXT_TEXTMARFUNC(getWx()->PageDownExtend()); }

    // Switch from insert to overtype mode or the reverse.
    //void EditToggleOvertype();
    inline void editToggleOvertype() { EXT_TEXTMARFUNC(getWx()->EditToggleOvertype()); }

    // Cancel any modes such as call tip or auto-completion list display.
    //void Cancel();
    inline void cancel() { EXT_TEXTMARFUNC(getWx()->Cancel()); }

    // Delete the selection or if no selection, the character before the caret.
    //void DeleteBack();
    inline void deleteBack() { EXT_TEXTMARFUNC(getWx()->DeleteBack()); }

    // If selection is empty or all on one line replace the selection with a tab character.
    // If more than one line selected, indent the lines.
    //void Tab();
    inline void tab() { EXT_TEXTMARFUNC(getWx()->Tab()); }

    // Dedent the selected lines.
    //void BackTab();
    inline void backTab() { EXT_TEXTMARFUNC(getWx()->BackTab()); }

    // Insert a new line, may use a CRLF, CR or LF depending on EOL mode.
    //void NewLine();
    inline void newLine() { EXT_TEXTMARFUNC(getWx()->NewLine()); }

    // Insert a Form Feed character.
    //void FormFeed();
    inline void formFeed() { EXT_TEXTMARFUNC(getWx()->FormFeed()); }

    // Move caret to before first visible character on line.
    // If already there move to first character on line.
    //void VCHome();
    inline void vCHome() { EXT_TEXTMARFUNC(getWx()->VCHome()); }

    // Like VCHome but extending selection to new caret position.
    //void VCHomeExtend();
    inline void vCHomeExtend() { EXT_TEXTMARFUNC(getWx()->VCHomeExtend()); }

    // Magnify the displayed text by increasing the sizes by 1 point.
    //void ZoomIn();
    inline void zoomIn() { EXT_TEXTMARFUNC(getWx()->ZoomIn()); }

    // Make the displayed text smaller by decreasing the sizes by 1 point.
    //void ZoomOut();
    inline void zoomOut() { EXT_TEXTMARFUNC(getWx()->ZoomOut()); }

    // Delete the word to the left of the caret.
    //void DelWordLeft();
    inline void delWordLeft() { EXT_TEXTMARFUNC(getWx()->DelWordLeft()); }

    // Delete the word to the right of the caret.
    //void DelWordRight();
    inline void delWordRight() { EXT_TEXTMARFUNC(getWx()->DelWordRight()); }

    // Cut the line containing the caret.
    //void LineCut();
    inline void lineCut() { EXT_TEXTMARFUNC(getWx()->LineCut()); }

    // Delete the line containing the caret.
    //void LineDelete();
    inline void lineDelete() { EXT_TEXTMARFUNC(getWx()->LineDelete()); }

    // Switch the current line with the previous.
    //void LineTranspose();
    inline void lineTranspose() { EXT_TEXTMARFUNC(getWx()->LineTranspose()); }

    // Duplicate the current line.
    //void LineDuplicate();
    inline void lineDuplicate() { getWx()->LineDuplicate(); }

    // Transform the selection to lower case.
    //void LowerCase();
    inline void lowerCase() { EXT_TEXTMARFUNC(getWx()->LowerCase()); }

    // Transform the selection to upper case.
    //void UpperCase();
    inline void upperCase() { EXT_TEXTMARFUNC(getWx()->UpperCase()); }

    // Scroll the document down, keeping the caret visible.
    //void LineScrollDown();
    inline void lineScrollDown() { EXT_TEXTMARFUNC(getWx()->LineScrollDown()); }

    //// Scroll the document up, keeping the caret visible.
    //void LineScrollUp();
    inline void lineScrollUp() { EXT_TEXTMARFUNC(getWx()->LineScrollUp()); }


    // Delete the selection or if no selection, the character before the caret.
    // Will not delete the character before at the start of a line.
    //void DeleteBackNotLine();
    inline void deleteBackNotLine() { EXT_TEXTMARFUNC(getWx()->DeleteBackNotLine()); }

    // Move caret to first position on display line.
    //void HomeDisplay();
    inline void homeDisplay() { getWx()->HomeDisplay(); }

    // Move caret to first position on display line extending selection to
    // new caret position.
    //void HomeDisplayExtend();
    inline void homeDisplayExtend() { getWx()->HomeDisplayExtend(); }

    // Move caret to last position on display line.
    //void LineEndDisplay();
    inline void lineEndDisplay() { getWx()->LineEndDisplay(); }

    // Move caret to last position on display line extending selection to new
    // caret position.
    //void LineEndDisplayExtend();
    inline void lineEndDisplayExtend() { getWx()->LineEndDisplayExtend(); }

    // These are like their namesakes Home(Extend)?, LineEnd(Extend)?, VCHome(Extend)?
    // except they behave differently when word-wrap is enabled:
    // They go first to the start / end of the display line, like (Home|LineEnd)Display
    // The difference is that, the cursor is already at the point, it goes on to the start
    // or end of the document line, as appropriate for (Home|LineEnd|VCHome)(Extend)?.
    //void HomeWrap();
    inline void homeWrap() { EXT_TEXTMARFUNC(getWx()->HomeWrap()); }
    //void HomeWrapExtend();
    inline void homeWrapExtend() { EXT_TEXTMARFUNC(getWx()->HomeWrapExtend()); }
    //void LineEndWrap();
    inline void lineEndWrap() { EXT_TEXTMARFUNC(getWx()->LineEndWrap()); }
    //void LineEndWrapExtend();
    inline void lineEndWrapExtend() { EXT_TEXTMARFUNC(getWx()->LineEndWrapExtend()); }
    //void VCHomeWrap();
    inline void vCHomeWrap() { EXT_TEXTMARFUNC(getWx()->VCHomeWrap()); }
    //void VCHomeWrapExtend();
    inline void vCHomeWrapExtend() { EXT_TEXTMARFUNC(getWx()->VCHomeWrapExtend()); }

    // Copy the line containing the caret.
    //void LineCopy();
    inline void lineCopy() { getWx()->LineCopy(); }

    // Move the caret inside current view if it's not there already.
    //void MoveCaretInsideView();
    inline void moveCaretInsideView() { getWx()->MoveCaretInsideView(); }

    // How many characters are on a line, not including end of line characters?
    //int LineLength (int line);
    inline int lineLength(int line) { return getWx()->LineLength(line); }

    // Highlight the characters at two positions.
    //void BraceHighlight (int pos1, int pos2);
    inline void braceHighlight(int pos1, int pos2) { getWx()->BraceHighlight(pos1, pos2); }

    // Highlight the character at a position indicating there is no matching brace.
    //void BraceBadLight (int pos);
    inline void braceBadLight(int pos) { getWx()->BraceBadLight(pos); }

    // Find the position of a matching brace or INVALID_POSITION if no match.
    //int BraceMatch (int pos);
    inline int braceMatch(int pos) { return getWx()->BraceMatch(pos); }

    // Are the end of line characters visible?
    //bool GetViewEOL();
    inline bool getViewEOL() { return getWx()->GetViewEOL(); }

    // Make the end of line characters visible or invisible.
    //void SetViewEOL (bool visible);
    inline void setViewEOL(bool visible) { getWx()->SetViewEOL(visible); }

    // Retrieve a pointer to the document object.
    //void* GetDocPointer();
    // ### TODO inline Rid getDocPointer() { getWx()->GetDocPointer(); }

    // Change the document object used.
    //void SetDocPointer (void* docPointer);
    // ### TODO inline void setDocPointer(void* docPointer) { getWx()->SetDocPointer(docPointer); }

    // Set which document modification events are sent to the container.
    //void SetModEventMask (int mask);
    /**
      combination of StcContainerNotifications
    */
    inline void setModEventMask(int mask) { getWx()->SetModEventMask(mask); }

    // Retrieve the column number which text should be kept within.
    //int GetEdgeColumn();
    inline int getEdgeColumn() { return getWx()->GetEdgeColumn(); }

    // Set the column number of the edge.
    // If text goes past the edge then it is highlighted.
    //void SetEdgeColumn (int column);
    inline void setEdgeColumn(int column) { getWx()->SetEdgeColumn(column); }

    // Retrieve the edge highlight mode.
    //int GetEdgeMode();
    inline int getEdgeMode() { return getWx()->GetEdgeMode(); }

    // The edge may be displayed by a line (EDGE_LINE) or by highlighting text that
    // goes beyond it (EDGE_BACKGROUND) or not displayed at all (EDGE_NONE).
    //void SetEdgeMode (int mode);
    inline void setEdgeMode(int mode) { getWx()->SetEdgeMode(mode); }

    // Retrieve the colour used in edge indication.
    //wxColour GetEdgeColour();
    inline RColour getEdgeColour() { return WXVAL2CLS(Colour, getWx()->GetEdgeColour()); }

    // Change the colour used in edge indication.
    //void SetEdgeColour (const wxColour& colour);
    inline void setEdgeColour(IN(RColour) colour) { getWx()->SetEdgeColour(CLS2WXREF(colour)); }

    // Sets the current caret position to be the search anchor.
    //void SearchAnchor();
    inline void searchAnchor() { getWx()->SearchAnchor(); }

    // Find some text starting at the search anchor.
    // Does not ensure the selection is visible.
    //int SearchNext (int flags, const wxString& text);
    inline int searchNext(int flags, IN(RString)  text) { return getWx()->SearchNext(flags, S2WXS(text)); }

    // Find some text starting at the search anchor and moving backwards.
    // Does not ensure the selection is visible.
    //int SearchPrev (int flags, const wxString& text);
    inline int searchPrev(int flags, IN(RString)  text) { return getWx()->SearchPrev(flags, S2WXS(text)); }

    // Retrieves the number of lines completely visible.
    //int LinesOnScreen();
    inline int linesOnScreen() { return getWx()->LinesOnScreen(); }

    // Set whether a pop up menu is displayed automatically when the user presses
    // the wrong mouse button.
    //void UsePopUp (bool allowPopUp);
    inline void usePopUp(bool allowPopUp) { getWx()->UsePopUp(allowPopUp); }

    // Is the selection rectangular? The alternative is the more common stream selection.
    //bool SelectionIsRectangle();
    inline bool selectionIsRectangle() { return getWx()->SelectionIsRectangle(); }

    // Set the zoom level. This number of points is added to the size of all fonts.
    // It may be positive to magnify or negative to reduce.
    //void SetZoom (int zoom);
    inline void setZoom(int zoom) { getWx()->SetZoom(zoom); }

    // Retrieve the zoom level.
    //int GetZoom();
    inline int getZoom() { return getWx()->GetZoom(); }

    // Create a new document object.
    // Starts with reference count of 1 and not selected into editor.
    // ### TODO void* CreateDocument();

    // Extend life of document.
    // ### TODO void AddRefDocument (void* docPointer);

    // Release a reference to the document, deleting document if it fades to black.
    // ### TODO void ReleaseDocument (void* docPointer);

    // Get which document modification events are sent to the container.
    //int GetModEventMask();
    inline int getModEventMask() { return getWx()->GetModEventMask(); }

    // Change internal focus flag.
    //void SetSCIFocus (bool focus);
    // not supported by STC inline void setSCIFocus(bool focus) { getWx()->SetSCIFocus(focus); }

    // Get internal focus flag.
    //bool GetSCIFocus();
    // not supported by STC inline bool getSCIFocus() { return getWx()->GetSCIFocus(); }

    // Change error status - 0 = OK.
    //void SetStatus (int status);
    inline void setStatus(int status) { getWx()->SetStatus(status); }

    // Get error status.
    //int GetStatus();
    inline int getStatus() { return getWx()->GetStatus(); }

    // Set whether the mouse is captured when its button is pressed.
    //void SetMouseDownCaptures (bool captures);
    inline void setMouseDownCaptures(bool captures) { getWx()->SetMouseDownCaptures(captures); }

    // Get whether mouse gets captured.
    //bool GetMouseDownCaptures();
    inline bool getMouseDownCaptures() { return getWx()->GetMouseDownCaptures(); }

    // Sets the cursor to one of the SC_CURSOR* values.
    //void SetCursorType (int cursorType);
    // not supported by STC inline void setCursorType(int cursorType) { getWx()->SetCursorType(cursorType); }

    // Get cursor type.
    //int GetCursorType();
    // not supported by STC inline int getCursorType() { return getWx()->GetCursorType(); }

    // Change the way control characters are displayed:
    // If symbol is < 32, keep the drawn way, else, use the given character.
    //void SetControlCharSymbol (int symbol);
    inline void setControlCharSymbol(int symbol) { getWx()->SetControlCharSymbol(symbol); }

    // Get the way control characters are displayed.
    //int GetControlCharSymbol();
    inline int getControlCharSymbol() { return getWx()->GetControlCharSymbol(); }

    // Move to the previous change in capitalisation.
    //void WordPartLeft();
    inline void wordPartLeft() { getWx()->WordPartLeft(); }

    // Move to the previous change in capitalisation extending selection
    // to new caret position.
    //void WordPartLeftExtend();
    inline void wordPartLeftExtend() { getWx()->WordPartLeftExtend(); }

    // Move to the change next in capitalisation.
    //void WordPartRight();
    inline void wordPartRight() { getWx()->WordPartRight(); }

    // Move to the next change in capitalisation extending selection
    // to new caret position.
    //void WordPartRightExtend();
    inline void wordPartRightExtend() { getWx()->WordPartRightExtend(); }

    // Set the way the display area is determined when a particular line
    // is to be moved to by Find, FindNext, GotoLine, etc.
    //void SetVisiblePolicy (int visiblePolicy, int visibleSlop);
    inline void setVisiblePolicy(int visiblePolicy, int visibleSlop) { getWx()->SetVisiblePolicy(visiblePolicy, visibleSlop); }

    // Delete back from the current position to the start of the line.
    //void DelLineLeft();
    inline void delLineLeft() { getWx()->DelLineLeft(); }

    // Delete forwards from the current position to the end of the line.
    //void DelLineRight();
    inline void delLineRight() { getWx()->DelLineRight(); }

    // Get and Set the xOffset (ie, horizonal scroll position).
    //void SetXOffset (int newOffset);
    inline void setXOffset(int newOffset) { getWx()->SetXOffset(newOffset); }
    //int GetXOffset();
    inline int getXOffset() { return getWx()->GetXOffset(); }

    // Set the last x chosen value to be the caret x position.
    //void ChooseCaretX();
    inline void chooseCaretX() { getWx()->ChooseCaretX(); }

    // Set the way the caret is kept visible when going sideway.
    // The exclusion zone is given in pixels.
    //void SetXCaretPolicy (int caretPolicy, int caretSlop);
    inline void setXCaretPolicy(int caretPolicy, int caretSlop) { getWx()->SetXCaretPolicy(caretPolicy, caretSlop); }

    // Set the way the line the caret is on is kept visible.
    // The exclusion zone is given in lines.
    //void SetYCaretPolicy (int caretPolicy, int caretSlop);
    inline void setYCaretPolicy(int caretPolicy, int caretSlop) { getWx()->SetYCaretPolicy(caretPolicy, caretSlop); }

    // Set printing to line wrapped (SC_WRAP_WORD) or not line wrapped (SC_WRAP_NONE).
    //void SetPrintWrapMode (int mode);
    inline void setPrintWrapMode(int mode) { getWx()->SetPrintWrapMode(mode); }

    // Is printing line wrapped?
    //int GetPrintWrapMode();
    inline int getPrintWrapMode() { return getWx()->GetPrintWrapMode(); }

    // Set a fore colour for active hotspots.
    //void SetHotspotActiveForeground (bool useSetting, const wxColour& fore);
    inline void setHotspotActiveForeground(bool useSetting, IN(RColour) fore) { getWx()->SetHotspotActiveForeground(useSetting, CLS2WXREF(fore)); }

    // Set a back colour for active hotspots.
    //void SetHotspotActiveBackground (bool useSetting, const wxColour& back);
    inline void setHotspotActiveBackground(bool useSetting, IN(RColour) back) { getWx()->SetHotspotActiveBackground(useSetting, CLS2WXREF(back)); }

    // Enable / Disable underlining active hotspots.
    //void SetHotspotActiveUnderline (bool underline);
    inline void setHotspotActiveUnderline(bool underline) { getWx()->SetHotspotActiveUnderline(underline); }

    // Limit hotspots to single line so hotspots on two lines don't merge.
    //void SetHotspotSingleLine (bool singleLine);
    inline void setHotspotSingleLine(bool singleLine) { EXT_TEXTMARFUNC(getWx()->SetHotspotSingleLine(singleLine)); }

    // Move caret between paragraphs (delimited by empty lines).
    //void ParaDown();
    inline void paraDown() { EXT_TEXTMARFUNC(getWx()->ParaDown()); }
    //void ParaDownExtend();
    inline void paraDownExtend() { EXT_TEXTMARFUNC(getWx()->ParaDownExtend()); }
    //void ParaUp();
    inline void paraUp() { EXT_TEXTMARFUNC(getWx()->ParaUp()); }
    //void ParaUpExtend();
    inline void paraUpExtend() { EXT_TEXTMARFUNC(getWx()->ParaUpExtend()); }

    // Given a valid document position, return the previous position taking code
    // page into account. Returns 0 if passed 0.
    //int PositionBefore (int pos);
    inline int positionBefore(int pos) { return getWx()->PositionBefore(pos); }

    // Given a valid document position, return the next position taking code
    // page into account. Maximum value returned is the last position in the document.
    //int PositionAfter (int pos);
    inline int positionAfter(int pos) { return getWx()->PositionAfter(pos); }

    // Copy a range of text to the clipboard. Positions are clipped into the document.
    //void CopyRange (int startPos, int endPos);
    inline void copyRange(int startPos, int endPos) { getWx()->CopyRange(startPos, endPos); }

    // Copy argument text to the clipboard.
    //void CopyText (int length, const wxString& text);
    inline void copyText(int length, IN(RString)  text) { getWx()->CopyText(length, S2WXS(text)); }

    // Set the selection mode to stream (SC_SEL_STREAM=1) or rectangular (SC_SEL_RECTANGLE=2) or
    // by lines (SC_SEL_LINES=3).
    //void SetSelectionMode (int mode);
    inline void setSelectionMode(int mode) { EXT_TEXTMARFUNC(getWx()->SetSelectionMode(mode)); }

    // Get the mode of the current selection.
    //int GetSelectionMode();
    // not supported on linux inline int getSelectionMode() { return getWx()->GetSelectionMode(); }

    // Retrieve the position of the start of the selection at the given line (INVALID_POSITION if no selection on this line).
    //int GetLineSelStartPosition (int line);
    // not supported on linux inline int getLineSelStartPosition(int line) { return getWx()->GetLineSelStartPosition(line); }

    // Retrieve the position of the end of the selection at the given line (INVALID_POSITION if no selection on this line).
    //int GetLineSelEndPosition (int line);
    // not supported on unix inline int getLineSelEndPosition(int line) { return getWx()->GetLineSelEndPosition(line); }

    // Move caret down one line, extending rectangular selection to new caret position.
    //void LineDownRectExtend();
    inline void lineDownRectExtend() { EXT_TEXTMARFUNC(getWx()->LineDownRectExtend()); }

    // Move caret up one line, extending rectangular selection to new caret position.
    //void LineUpRectExtend();
    inline void lineUpRectExtend() { EXT_TEXTMARFUNC(getWx()->LineUpRectExtend()); }

    // Move caret left one character, extending rectangular selection to new caret position.
    //void CharLeftRectExtend();
    inline void charLeftRectExtend() { EXT_TEXTMARFUNC(getWx()->CharLeftRectExtend()); }

    // Move caret right one character, extending rectangular selection to new caret position.
    //void CharRightRectExtend();
    inline void charRightRectExtend() { EXT_TEXTMARFUNC(getWx()->CharRightRectExtend()); }

    // Move caret to first position on line, extending rectangular selection to new caret position.
    //void HomeRectExtend();
    inline void homeRectExtend() { EXT_TEXTMARFUNC(getWx()->HomeRectExtend()); }

    // Move caret to before first visible character on line.
    // If already there move to first character on line.
    // In either case, extend rectangular selection to new caret position.
    //void VCHomeRectExtend();
    inline void vCHomeRectExtend() { EXT_TEXTMARFUNC(getWx()->VCHomeRectExtend()); }

    // Move caret to last position on line, extending rectangular selection to new caret position.
    //void LineEndRectExtend();
    inline void lineEndRectExtend() { EXT_TEXTMARFUNC(getWx()->LineEndRectExtend()); }

    // Move caret one page up, extending rectangular selection to new caret position.
    //void PageUpRectExtend();
    inline void pageUpRectExtend() { EXT_TEXTMARFUNC(getWx()->PageUpRectExtend()); }

    // Move caret one page down, extending rectangular selection to new caret position.
    //void PageDownRectExtend();
    inline void pageDownRectExtend() { EXT_TEXTMARFUNC(getWx()->PageDownRectExtend()); }

    // Move caret to top of page, or one page up if already at top of page.
    //void StutteredPageUp();
    inline void stutteredPageUp() { EXT_TEXTMARFUNC(getWx()->StutteredPageUp()); }

    // Move caret to top of page, or one page up if already at top of page, extending selection to new caret position.
    //void StutteredPageUpExtend();
    inline void stutteredPageUpExtend() { EXT_TEXTMARFUNC(getWx()->StutteredPageUpExtend()); }

    // Move caret to bottom of page, or one page down if already at bottom of page.
    //void StutteredPageDown();
    inline void stutteredPageDown() { EXT_TEXTMARFUNC(getWx()->StutteredPageDown()); }

    // Move caret to bottom of page, or one page down if already at bottom of page, extending selection to new caret position.
    //void StutteredPageDownExtend();
    inline void stutteredPageDownExtend() { EXT_TEXTMARFUNC(getWx()->StutteredPageDownExtend()); }

    // Move caret left one word, position cursor at end of word.
    //void WordLeftEnd();
    inline void wordLeftEnd() { EXT_TEXTMARFUNC(getWx()->WordLeftEnd()); }

    // Move caret left one word, position cursor at end of word, extending selection to new caret position.
    //void WordLeftEndExtend();
    inline void wordLeftEndExtend() { EXT_TEXTMARFUNC(getWx()->WordLeftEndExtend()); }

    // Move caret right one word, position cursor at end of word.
    //void WordRightEnd();
    inline void wordRightEnd() { EXT_TEXTMARFUNC(getWx()->WordRightEnd()); }

    // Move caret right one word, position cursor at end of word, extending selection to new caret position.
    //void WordRightEndExtend();
    inline void wordRightEndExtend() { EXT_TEXTMARFUNC(getWx()->WordRightEndExtend()); }

    // Set the set of characters making up whitespace for when moving or selecting by word.
    // Should be called after SetWordChars.
    //void SetWhitespaceChars (const wxString& characters);
    inline void setWhitespaceChars(IN(RString)  characters) { EXT_TEXTMARFUNC(getWx()->SetWhitespaceChars(S2WXS(characters))); }

    // Reset the set of characters for whitespace and word characters to the defaults.
    //void SetCharsDefault();
    inline void setCharsDefault() { EXT_TEXTMARFUNC(getWx()->SetCharsDefault()); }

    // Get currently selected item position in the auto-completion list
    //int AutoCompGetCurrent();
    // not supported at gtk inline int autoCompGetCurrent() { return getWx()->AutoCompGetCurrent(); }

    // Enlarge the document to a particular size of text bytes.
    //void Allocate (int bytes);
    // not supported by gtk? inline void allocate(int bytes) { getWx()->Allocate(bytes); }

    // Start notifying the container of all key presses and commands.
    //void StartRecord();
    inline void startRecord() { getWx()->StartRecord(); }

    // Stop notifying the container of all key presses and commands.
    //void StopRecord();
    inline void stopRecord() { getWx()->StopRecord(); }

    // Set the lexing language of the document.
    //void SetLexer (int lexer);
    inline void setLexer(int lexer) { getWx()->SetLexer(lexer); }

    // Retrieve the lexing language of the document.
    //int GetLexer();
    inline int getLexer() { return getWx()->GetLexer(); }

    // Colourise a segment of the document using the current lexing language.
    //void Colourise (int startPos, int endPos);
    inline void colourise(int startPos, int endPos) { getWx()->Colourise(startPos, endPos); }

    // Set up a value that may be used by a lexer for some optional feature.
    //void SetProperty (const wxString& key, const wxString& value);
    inline void setProperty(IN(RString)  key, IN(RString)  value) { getWx()->SetProperty(S2WXS(key), S2WXS(value)); }

    // Set up the key words used by the lexer.
    //void SetKeyWords (int keywordSet, const wxString& keyWords);
    inline void setKeyWords(int keywordSet, IN(RString)  keyWords) { getWx()->SetKeyWords(keywordSet, S2WXS(keyWords)); }

    // Set the lexing language of the document based on string name.
    //void SetLexerLanguage (const wxString& language);
    inline void setLexerLanguage(IN(RString)  language) { getWx()->SetLexerLanguage(S2WXS(language)); }



    // Returns the line number of the line with the caret.
    //int GetCurrentLine();
    inline int getCurrentLine() { return getWx()->GetCurrentLine(); }

    // Extract style settings from a spec-string which is composed of one or
    // more of the following comma separated elements:
    //
    //      bold                    turns on bold
    //      italic                  turns on italics
    //      fore:[name or #RRGGBB]  sets the foreground colour
    //      back:[name or #RRGGBB]  sets the background colour
    //      face:[facename]         sets the font face name to use
    //      size:[num]              sets the font size in points
    //      eol                     turns on eol filling
    //      underline               turns on underlining
    //
    //void StyleSetSpec(int styleNum, const wxString& spec);
    inline void styleSetSpec(int styleNum, IN(RString)  spec) { getWx()->StyleSetSpec(styleNum, S2WXS(spec)); }



    // Set style size, face, bold, italic, and underline attributes from
    // a wxFont's attributes.
    //void StyleSetFont (int styleNum, wxFont& font);
    inline void styleSetFont(int styleNum, IN(RFont) font) { getWx()->StyleSetFont(styleNum, CLS2WXREF(font)); }

    // Set all font style attributes at once.
    //void StyleSetFontAttr(int styleNum, int size,const wxString& faceName, bool bold, bool italic, bool underline);
    inline void styleSetFontAttr(int styleNum, int size, IN(RString)  faceName, bool bold, bool italic, bool underline) { getWx()->StyleSetFontAttr(styleNum, size, S2WXS(faceName), bold, italic, underline); }

    // Perform one of the operations defined by the wxSTC_CMD_* constants.
    //void CmdKeyExecute(int cmd);
    inline void cmdKeyExecute(int cmd) { getWx()->CmdKeyExecute(cmd); }

    // Set the left and right margin in the edit area, measured in pixels.
    //void SetMargins(int left, int right);
    inline void setMargins(int left, int right) { getWx()->SetMargins(left, right); }


    // Retrieve the start and end positions of the current selection.
    //void GetSelection(int* startPos, int* endPos);
    inline void getSelection(OUT(int) startPos, OUT(int) endPos) { getWx()->GetSelection(&startPos, &endPos); }

    // Retrieve the point in the window where a position is displayed.
    //wxPoint PointFromPosition(int pos);
    inline RPoint pointFromPosition(int pos) { return WXVAL2CLS(Point, getWx()->PointFromPosition(pos)); }


    // Scroll enough to make the given line visible
    //void ScrollToLine(int line);
    inline void scrollToLine(int line) { getWx()->ScrollToLine(line); }


    // Scroll enough to make the given column visible
    //void ScrollToColumn(int column);
    inline void scrollToColumn(int column) { getWx()->ScrollToColumn(column); }


    // Send a message to StyledTextCtrl
    // not supported long SendMsg(int msg, long wp=0, long lp=0);


    // Set the vertical scrollbar to use instead of the ont that's built-in.
    //void SetVScrollBar (wxScrollBar* bar);
    // ### TODO inline void setVScrollBar(IN(RScrollBar) bar) { getWx()->SetVScrollBar(CLS2WXPTR(bar)); }


    // Set the horizontal scrollbar to use instead of the ont that's built-in.
    //void SetHScrollBar (wxScrollBar* bar);
    // ### TODO inline void setHScrollBar(IN(RScrollBar) bar) { getWx()->SetHScrollBar(CLS2WXPTR(bar)); }

    // Can be used to prevent the EVT_CHAR handler from adding the char
    //bool GetLastKeydownProcessed() { return m_lastKeyDownConsumed; }
    inline bool getLastKeydownProcessed() { return getWx()->GetLastKeydownProcessed(); }
    //void SetLastKeydownProcessed(bool val) { m_lastKeyDownConsumed = val; }
    inline void setLastKeydownProcessed(bool val) { getWx()->SetLastKeydownProcessed(val); }

    // Write the contents of the editor to filename
    //bool SaveFile(const wxString& filename);
    inline bool saveFile(IN(RString)  filename) { return getWx()->SaveFile(S2WXS(filename)); }

    // Load the contents of filename into the editor
    //bool LoadFile(const wxString& filename);
    inline bool loadFile(IN(RString)  filename) { return getWx()->LoadFile(S2WXS(filename)); }

#ifdef SCI_USE_DND
    // Allow for simulating a DnD DragOver
    // ### TODO wxDragResult DoDragOver (wxCoord x, wxCoord y, wxDragResult def);

    // Allow for simulating a DnD DropText
    // ### TODO bool DoDropText(long x, long y, const wxString& data);

    // Allow for simulating a DnD DragEnter
    // ### TODO wxDragResult DoDragEnter (wxCoord x, wxCoord y, wxDragResult def);

    // Allow for simulating a DnD DragEnter
    // ### TODO void DoDragLeave ();

#endif

    // Specify whether anti-aliased fonts should be used.  Will have no effect
    // on some platforms, but on some (wxMac for example) can greatly improve
    // performance.
    //void SetUseAntiAliasing(bool useAA);
    inline void setUseAntiAliasing(bool useAA) { EXT_TEXTMARFUNC(getWx()->SetUseAntiAliasing(useAA)); }

    // Returns the current UseAntiAliasing setting.
    //bool GetUseAntiAliasing();
    // not supported by gtk? inline bool getUseAntiAliasing() { return getWx()->GetUseAntiAliasing(); }
    
  
};

ACDK_DECL_CLASS(StyledTextEvent);

/**
  see wxStyledTextCtrlEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/04/21 09:52:29 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_IDE_PUBLIC StyledTextEvent
: extends CommandEvent
{
  ACDK_WITH_METAINFO(StyledTextEvent)
public:
  // wxStyledTextEvent
  ACDK_WX_STD_EVENT_MEMBERS(StyledTextEvent, CommandEvent)

  StyledTextEvent(int eventType = EvtNull, int id = 0)
  : CommandEvent(new wxStyledTextEvent(eventType, id))
  {
  }
  //void SetPosition(int pos)             { m_position = pos; }
  inline void setPosition(int pos) { getWx()->SetPosition(pos); }
    //void SetKey(int k)                    { m_key = k; }
  inline void setKey(int k) { getWx()->SetKey(k); }
    //void SetModifiers(int m)              { m_modifiers = m; }
  inline void setModifiers(int m) { getWx()->SetModifiers(m); }
    //void SetModificationType(int t)       { m_modificationType = t; }
  inline void setModificationType(int t) { getWx()->SetModificationType(t); }
    //void SetText(const wxString& t)       { m_text = t; }
  inline void setText(IN(RString)  t) { getWx()->SetText(S2WXS(t)); }
    //void SetLength(int len)               { m_length = len; }
  inline void setLength(int len) { getWx()->SetLength(len); }
    //void SetLinesAdded(int num)           { m_linesAdded = num; }
  inline void setLinesAdded(int num) { getWx()->SetLinesAdded(num); }
    //void SetLine(int val)                 { m_line = val; }
  inline void setLine(int val) { getWx()->SetLine(val); }
    //void SetFoldLevelNow(int val)         { m_foldLevelNow = val; }
  inline void setFoldLevelNow(int val) { getWx()->SetFoldLevelNow(val); }
    //void SetFoldLevelPrev(int val)        { m_foldLevelPrev = val; }
  inline void setFoldLevelPrev(int val) { getWx()->SetFoldLevelPrev(val); }
    //void SetMargin(int val)               { m_margin = val; }
  inline void setMargin(int val) { getWx()->SetMargin(val); }
    //void SetMessage(int val)              { m_message = val; }
  inline void setMessage(int val) { getWx()->SetMessage(val); }
    //void SetWParam(int val)               { m_wParam = val; }
  inline void setWParam(int val) { getWx()->SetWParam(val); }
    //void SetLParam(int val)               { m_lParam = val; }
  inline void setLParam(int val) { getWx()->SetLParam(val); }
    //void SetListType(int val)             { m_listType = val; }
  inline void setListType(int val) { getWx()->SetListType(val); }
    //void SetX(int val)                    { m_x = val; }
  inline void setX(int val) { getWx()->SetX(val); }
    //void SetY(int val)                    { m_y = val; }
  inline void setY(int val) { getWx()->SetY(val); }
    //void SetDragText(const wxString& val) { m_dragText = val; }
  inline void setDragText(IN(RString)  val) { getWx()->SetDragText(S2WXS(val)); }
    //void SetDragAllowMove(bool val)       { m_dragAllowMove = val; }
  inline void setDragAllowMove(bool val) { getWx()->SetDragAllowMove(val); }
//#ifdef  SCI_USE_DND
    //void SetDragResult(wxDragResult val)  { m_dragResult = val; }
  // ### TODO inline void setDragResult(wxDragResult val) { getWx()->SetDragResult(val); }
//#endif

    //int  GetPosition() const         { return m_position; }
  inline int getPosition() const { return getWx()->GetPosition(); }
    //int  GetKey()  const             { return m_key; }
  inline int getKey() const { return getWx()->GetKey(); }
    //int  GetModifiers() const        { return m_modifiers; }
  inline int getModifiers() const { return getWx()->GetModifiers(); }
    //int  GetModificationType() const { return m_modificationType; }
  inline int getModificationType() const { return getWx()->GetModificationType(); }
    //wxString GetText() const         { return m_text; }
  inline RString getText() const { return WXS2S(getWx()->GetText()); }
    //int  GetLength() const           { return m_length; }
  inline int getLength() const { return getWx()->GetLength(); }
    //int  GetLinesAdded() const       { return m_linesAdded; }
  inline int getLinesAdded() const { return getWx()->GetLinesAdded(); }
    //int  GetLine() const             { return m_line; }
  inline int getLine() const { return getWx()->GetLine(); }
    //int  GetFoldLevelNow() const     { return m_foldLevelNow; }
  inline int getFoldLevelNow() const { return getWx()->GetFoldLevelNow(); }
    //int  GetFoldLevelPrev() const    { return m_foldLevelPrev; }
  inline int getFoldLevelPrev() const { return getWx()->GetFoldLevelPrev(); }
    //int  GetMargin() const           { return m_margin; }
  inline int getMargin() const { return getWx()->GetMargin(); }
    //int  GetMessage() const          { return m_message; }
  inline int getMessage() const { return getWx()->GetMessage(); }
    //int  GetWParam() const           { return m_wParam; }
  inline int getWParam() const { return getWx()->GetWParam(); }
    //int  GetLParam() const           { return m_lParam; }
  inline int getLParam() const { return getWx()->GetLParam(); }
    //int  GetListType() const         { return m_listType; }
  inline int getListType() const { return getWx()->GetListType(); }
    //int  GetX() const                { return m_x; }
  inline int getX() const { return getWx()->GetX(); }
    //int  GetY() const                { return m_y; }
  inline int getY() const { return getWx()->GetY(); }
    //wxString GetDragText()           { return m_dragText; }
  inline RString getDragText() { return WXS2S(getWx()->GetDragText()); }
    //bool GetDragAllowMove()          { return m_dragAllowMove; }
  inline bool getDragAllowMove() { return getWx()->GetDragAllowMove(); }
//#ifdef SCI_USE_DND
    //wxDragResult GetDragResult()     { return m_dragResult; }
  // ### TODO inline RDragResult getDragResult() { return WXVAL2CLS(DragResult, getWx()->GetDragResult()); }
//#endif

    //bool GetShift() const;
  inline bool getShift() const { return getWx()->GetShift(); }
    //bool GetControl() const;
  inline bool getControl() const { return getWx()->GetControl(); }
    //bool GetAlt() const;
  inline bool getAlt() const { return getWx()->GetAlt(); }

  static int EvtSciChange;
  static int EvtSciStyleneeded; // wxEVT_SCI_STYLENEEDED,
  static int EvtSciCharadded; // wxEVT_SCI_CHARADDED,
  static int EvtSciSavepointreached; // wxEVT_SCI_SAVEPOINTREACHED,
  static int EvtSciSavepointleft; // wxEVT_SCI_SAVEPOINTLEFT,
  static int EvtSciRomodifyattempt; // wxEVT_SCI_ROMODIFYATTEMPT,
  static int EvtSciKey; // wxEVT_SCI_KEY,
  static int EvtSciDoubleclick; // wxEVT_SCI_DOUBLECLICK,
  static int EvtSciUpdateui; // wxEVT_SCI_UPDATEUI,
  static int EvtSciModified; // wxEVT_SCI_MODIFIED,
  static int EvtSciMacrorecord; // wxEVT_SCI_MACRORECORD,
  static int EvtSciMarginclick; // wxEVT_SCI_MARGINCLICK,
  static int EvtSciNeedshown; // wxEVT_SCI_NEEDSHOWN,
  static int EvtSciPainted; // wxEVT_SCI_PAINTED,
  static int EvtSciUserlistselection; // wxEVT_SCI_USERLISTSELECTION,
  static int EvtSciUridropped; // wxEVT_SCI_URIDROPPED,
  static int EvtSciDwellstart; // wxEVT_SCI_DWELLSTART,
  static int EvtSciDwellend; // wxEVT_SCI_DWELLEND,
  static int EvtSciStartDrag; // wxEVT_SCI_START_DRAG,
  static int EvtSciDragOver; // wxEVT_SCI_DRAG_OVER,
  static int EvtSciDoDrop; // wxEVT_SCI_DO_DROP,
  static int EvtSciZoom; // wxEVT_SCI_ZOOM,
  static int EvtSciHotspotClick; // wxEVT_SCI_HOTSPOT_CLICK,
  static int EvtSciHotspotDclick; // wxEVT_SCI_HOTSPOT_DCLICK,
  static int EvtSciCalltipClick; // wxEVT_SCI_CALLTIP_CLICK
};

} // namespace ide
} //namespace wx
} // namespace acdk

#endif //acdk_wx_ide_StyledTextCtrl_h
