// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "AciCodeAttributeData.h"
#include "ast.h"
#include "AstNode.h"
#include "AstNodeVisitor.h"
#include "ast_metainf.h"
#include "EofTerminal.h"
#include "Expression.h"
#include "Identifier.h"
#include "Keyword.h"
#include "Literal.h"
#include "Statement.h"
#include "Terminal.h"
#include "Whitespace.h"

namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _AstNode_super_acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AstNode_interfaces[] =
{
  &_AstNode_super_acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* AstNode::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AstNode", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AstNode_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    AstNode::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AstNode(AstNode::clazzInfo());

::acdk::lang::RClass
AstNode::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AstNode::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_astProps._ref_this()); // acdk::cfgscript::RProps _astProps 
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _AstNodeFromParseNode_super_AstNode =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  AstNode::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AstNodeFromParseNode_interfaces[] =
{
  &_AstNodeFromParseNode_super_AstNode,
  0
};

::acdk::lang::dmi::ClazzInfo* AstNodeFromParseNode::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AstNodeFromParseNode", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AstNodeFromParseNode_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 2, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    AstNodeFromParseNode::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AstNodeFromParseNode(AstNodeFromParseNode::clazzInfo());

::acdk::lang::RClass
AstNodeFromParseNode::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AstNodeFromParseNode::getCollectableFields(FieldReferences& fields)
{
  AstNode::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_nodeName._ref_this()); // RString _nodeName 
  fields.push_back((::acdk::lang::RObject*)this->_parseNode._ref_this()); // acdk::aci::parser::RParseNode _parseNode 
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _AstNodeWithChilds_super_AstNodeFromParseNode =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  AstNodeFromParseNode::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AstNodeWithChilds_interfaces[] =
{
  &_AstNodeWithChilds_super_AstNodeFromParseNode,
  0
};

::acdk::lang::dmi::ClazzInfo* AstNodeWithChilds::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AstNodeWithChilds", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AstNodeWithChilds_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    AstNodeWithChilds::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AstNodeWithChilds(AstNodeWithChilds::clazzInfo());

::acdk::lang::RClass
AstNodeWithChilds::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AstNodeWithChilds::getCollectableFields(FieldReferences& fields)
{
  AstNodeFromParseNode::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_childs._ref_this()); // RAstNodeArray _childs 
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 


::acdk::lang::dmi::ClazzEnumValueInfo ParseState_PSSyntax = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "PSSyntax", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::PSSyntax, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo ParseState_PSSemantic = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "PSSemantic", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::PSSemantic, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo ParseState_PSOpCode = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "PSOpCode", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::PSOpCode, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* ParseState_enumValues[] = {
  &ParseState_PSSyntax,
  &ParseState_PSSemantic,
  &ParseState_PSOpCode,
0
};

::acdk::lang::dmi::ClazzEnumInfo* ParseStateMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo ParseState_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "ParseState", // name
  -1, // hashCode
  "acdk/aci/ast", // ns
  0, // _scopeParent
  0, // _nextSibling
  ParseState_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_ParseState(&ParseState_enumInfo);

  return &ParseState_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_ParseStateEnumInfo(ParseStateMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 


::acdk::lang::dmi::ClazzEnumValueInfo TraverseResult_TraverseContinue = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TraverseContinue", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::TraverseContinue, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo TraverseResult_TraverseSkipSiblings = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TraverseSkipSiblings", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::TraverseSkipSiblings, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo TraverseResult_TraverseSkipChilds = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TraverseSkipChilds", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::aci::ast::TraverseSkipChilds, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* TraverseResult_enumValues[] = {
  &TraverseResult_TraverseContinue,
  &TraverseResult_TraverseSkipSiblings,
  &TraverseResult_TraverseSkipChilds,
0
};

::acdk::lang::dmi::ClazzEnumInfo* TraverseResultMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo TraverseResult_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "TraverseResult", // name
  -1, // hashCode
  "acdk/aci/ast", // ns
  0, // _scopeParent
  0, // _nextSibling
  TraverseResult_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_TraverseResult(&TraverseResult_enumInfo);

  return &TraverseResult_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_TraverseResultEnumInfo(TraverseResultMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo* _Expression_interfaces[] =
{
  0
};

::acdk::lang::dmi::ClazzInfo* Expression::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiInterface | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "Expression", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Expression_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 0, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    Expression::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Expression(Expression::clazzInfo());

::acdk::lang::RClass
Expression::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Expression::getCollectableFields(FieldReferences& fields)
{
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _Identifier_super_Terminal =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Terminal::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Identifier_interfaces[] =
{
  &_Identifier_super_Terminal,
  0
};

::acdk::lang::dmi::ClazzInfo* Identifier::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "Identifier", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Identifier_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    Identifier::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Identifier(Identifier::clazzInfo());

::acdk::lang::RClass
Identifier::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Identifier::getCollectableFields(FieldReferences& fields)
{
  Terminal::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_identifier._ref_this()); // RString _identifier 
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _Keyword_super_Terminal =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Terminal::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Keyword_interfaces[] =
{
  &_Keyword_super_Terminal,
  0
};

::acdk::lang::dmi::ClazzInfo* Keyword::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "Keyword", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Keyword_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    Keyword::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Keyword(Keyword::clazzInfo());

::acdk::lang::RClass
Keyword::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Keyword::getCollectableFields(FieldReferences& fields)
{
  Terminal::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_keyword._ref_this()); // RString _keyword 
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _Literal_super_Terminal =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Terminal::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo _Literal_super_Expression =
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiCiInterface | ::acdk::lang::dmi::MiCiThrowable,
  0, //AttributesRes
  Expression::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Literal_interfaces[] =
{
  &_Literal_super_Terminal,
  &_Literal_super_Expression,
  0
};

::acdk::lang::dmi::ClazzInfo* Literal::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "Literal", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Literal_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 0, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    Literal::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Literal(Literal::clazzInfo());

::acdk::lang::RClass
Literal::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Literal::getCollectableFields(FieldReferences& fields)
{
  Terminal::getCollectableFields(fields);
}


} // namespace acdk
} // namespace aci
} // namespace ast


namespace acdk { 
namespace aci { 
namespace ast { 

::acdk::lang::dmi::ClazzSuperInfo _Terminal_super_AstNodeFromParseNode =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  AstNodeFromParseNode::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Terminal_interfaces[] =
{
  &_Terminal_super_AstNodeFromParseNode,
  0
};

::acdk::lang::dmi::ClazzInfo* Terminal::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "Terminal", // name of class
  -1, // hashCode
  "acdk/aci/ast", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Terminal_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 2, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    Terminal::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Terminal(Terminal::clazzInfo());

::acdk::lang::RClass
Terminal::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Terminal::getCollectableFields(FieldReferences& fields)
{
  AstNodeFromParseNode::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_codeLocation._ref_this()); // acdk::aci::util::RCodeLocation _codeLocation 
  fields.push_back((::acdk::lang::RObject*)this->_terminalParseNode._ref_this()); // acdk::aci::parser::RTerminalParseNode _terminalParseNode 
}


} // namespace acdk
} // namespace aci
} // namespace ast

