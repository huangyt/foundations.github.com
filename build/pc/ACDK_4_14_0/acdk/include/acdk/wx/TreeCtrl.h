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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TreeCtrl.h,v 1.22 2005/03/11 11:11:50 kommer Exp $

#ifndef acdk_wx_TreeCtrl_h
#define acdk_wx_TreeCtrl_h

#include "Control.h"
#include "TextCtrl.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {

/**
  see TreeCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
enum TreeCtrlFlags
{
  TrNoButtons = wxTR_NO_BUTTONS,              /* 0x0000     // for convenience */
  TrHasButtons             = wxTR_HAS_BUTTONS            ,  // wxTR_HAS_BUTTONS             0x0001     // generates a +/- button
  TrTwistButtons           = wxTR_TWIST_BUTTONS          ,  // wxTR_TWIST_BUTTONS           0x0002     // generates a twister button
  TrNoLines                = wxTR_NO_LINES               ,  // wxTR_NO_LINES                0x0004     // don't generate level connectors
  TrLinesAtRoot           = wxTR_LINES_AT_ROOT          ,  // wxTR_LINES_AT_ROOT           0x0008     // connect top-level nodes
  TrMacButtons             = wxTR_MAC_BUTTONS            ,  // wxTR_MAC_BUTTONS             wxTR_TWIST_BUTTONS // backward compatibility
  TrAquaButtons            = wxTR_AQUA_BUTTONS           ,  // wxTR_AQUA_BUTTONS            0x0010     // used internally

  TrSingle                  = wxTR_SINGLE                 ,  // wxTR_SINGLE                  0x0000     // for convenience
  TrMultiple                = wxTR_MULTIPLE               ,  // wxTR_MULTIPLE                0x0020     // can select multiple items
  TrExtended                = wxTR_EXTENDED               ,  // wxTR_EXTENDED                0x0040     // TODO: allow extended selection
  TrFullRowHighlight      = wxTR_FULL_ROW_HIGHLIGHT     ,  // wxTR_FULL_ROW_HIGHLIGHT      0x2000     // highlight full horizontal space

  TrEditLabels             = wxTR_EDIT_LABELS            ,  // wxTR_EDIT_LABELS             0x0200     // can edit item labels
  TrRowLines               = wxTR_ROW_LINES              ,  // wxTR_ROW_LINES               0x0400     // put border around items
  TrHideRoot               = wxTR_HIDE_ROOT              ,  // wxTR_HIDE_ROOT               0x0800     // don't display root node
  TrHasVariableRowHeight = wxTR_HAS_VARIABLE_ROW_HEIGHT,  // wxTR_HAS_VARIABLE_ROW_HEIGHT 0x0080     // what it says
  TrDefaultStyle = wxTR_DEFAULT_STYLE,  // wxTR_DEFAULT_STYLE
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, TreeCtrlFlags);

/**
  see TreeCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
enum TreeItemIcon
{
  TreeitemiconNormal = wxTreeItemIcon_Normal,  // wxTreeItemIcon_Normal,              // not selected, not expanded
  TreeitemiconSelected = wxTreeItemIcon_Selected,  // wxTreeItemIcon_Selected,            //     selected, not expanded
  TreeitemiconExpanded = wxTreeItemIcon_Expanded,  // wxTreeItemIcon_Expanded,            // not selected,     expanded
  TreeitemiconSelectedexpanded = wxTreeItemIcon_SelectedExpanded,  // wxTreeItemIcon_SelectedExpanded,    //     selected,     expanded
  TreeitemiconMax = wxTreeItemIcon_Max  // wxTreeItemIcon_Max
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, TreeItemIcon);

ACDK_DECL_CLASS(TreeItemId);

typedef WxValStruct<wxTreeItemId> TreeItemIdSuper;

/**
  see wxTreeItemId
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC TreeItemId
: extends TreeItemIdSuper
{
  ACDK_WITH_METAINFO(TreeItemId)
public:
  typedef WxValStruct<wxTreeItemId> Super;
  foreign TreeItemId(const wxTreeItemId& id, bool owns = false) : Super(id) {}
  TreeItemId() : Super(wxTreeItemId()) {}
  TreeItemId(int lItem) : Super(wxTreeItemId(lItem)) {}
  //bool IsOk() const { return m_pItem != 0; }
  inline bool isOk() const { return getWx()->IsOk(); }
  int treeItemIdValue() const 
  { 
#if defined(ACDK_OS_WIN32)
    return (int)(long)_wxObject;
#else
    return (int)_wxObject.m_pItem;
#endif
  }

  // acdk standard:
  RString toString() { return String::valueOf(treeItemIdValue()); }
  bool equals(IN(RTreeItemId) obj) 
  {
    if (obj == Nil)
      return false;
    return treeItemIdValue() == obj->treeItemIdValue();
  }
  bool equals(IN(RObject) obj) 
  {
    if (instanceof(obj, TreeItemId) == false)
      return false;
    return equals(RTreeItemId(obj));
  }
  int hashCode() { return (int)getWx()->m_pItem; }
};


typedef WxNonCopyStruct<wxTreeItemData> TreeItemDataSuper;

foreign
class wxAcdkTreeItemData
: public wxTreeItemData
{
private:
  RObject _data;
public:
  wxAcdkTreeItemData() {}
  wxAcdkTreeItemData(IN(RObject) data) : _data(data) {}
  RObject GetData() { return _data; }
  void SetData(IN(RObject) data) { _data = data; }
};



ACDK_DECL_CLASS(TreeItemData);
/**
  see wxTreeItemData
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC TreeItemData
: extends TreeItemDataSuper
{
  ACDK_WITH_METAINFO(TreeItemData)
public:
  foreign TreeItemData(wxTreeItemData* data, bool owns = false) : TreeItemDataSuper(data, owns) { }
  TreeItemData(IN(RObject) data) : TreeItemDataSuper(new wxAcdkTreeItemData(data), false) { }
  //const wxTreeItemId& GetId() const { return m_pItem; }
  inline RTreeItemId getId() const { return WXVAL2CLS(TreeItemId, getWx()->GetId()); }
    //void SetId(const wxTreeItemId& id) { m_pItem = id; }
  inline void setId(IN(RTreeItemId) id) { getWx()->SetId(CLS2WXREF(id)); }
  inline RObject getData() 
  { 
    wxAcdkTreeItemData* wxd = dynamic_cast<wxAcdkTreeItemData*>(getWx());
    if (wxd == 0)
      return Nil;
    return wxd->GetData(); 
  }
  inline void setData(IN(RObject) data ) 
  { 
    wxAcdkTreeItemData* wxd = dynamic_cast<wxAcdkTreeItemData*>(getWx());
    if (wxd == 0)
    {
      // ### @todo warn here
    }
    wxd->SetData(data); 
  }
  RString toString()
  {
    RObject data = getData();
    return SBSTR(getId()->toString() << ": " << (data == Nil ? RString("Nil") : data->toString()));
  }
  bool equals(IN(RTreeItemData) other)
  {
    if (other == Nil)
      return Nil;
    return getId()->equals(other->getId());
  }
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, TreeItemData) == false)
      return false;
    return equals(RTreeItemData(other));
  }
};



ACDK_DECL_CLASS(TreeEvent);
/**
  see wxTreeEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC TreeEvent
: extends NotifyEvent
{
  ACDK_WITH_METAINFO(TreeEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(TreeEvent, NotifyEvent)
  TreeEvent(int commandType = EvtNull, int id = 0)
  : NotifyEvent(new wxTreeEvent(commandType, id))
  {
  }
  //wxTreeItemId GetItem() const { return m_item; }
  inline RTreeItemId getItem() const { return WXVAL2CLS(TreeItemId, getWx()->GetItem()); }
    //void SetItem(const wxTreeItemId& item) { m_item = item; }
  inline void setItem(IN(RTreeItemId) item) { getWx()->SetItem(CLS2WXREF(item)); }
//
        // for wxEVT_COMMAND_TREE_SEL_CHANGED/ING events, get the previously
        // selected item
    //wxTreeItemId GetOldItem() const { return m_itemOld; }
  inline RTreeItemId getOldItem() const { return WXVAL2CLS(TreeItemId, getWx()->GetOldItem()); }
    //void SetOldItem(const wxTreeItemId& item) { m_itemOld = item; }
  inline void setOldItem(IN(RTreeItemId) item) { getWx()->SetOldItem(CLS2WXREF(item)); }


        // the point where the mouse was when the drag operation started (for
        // wxEVT_COMMAND_TREE_BEGIN_(R)DRAG events only) or click position
    //wxPoint GetPoint() const { return m_pointDrag; }
  inline RPoint getPoint() const { return WXVAL2CLS(Point, getWx()->GetPoint()); }
    //void SetPoint(const wxPoint& pt) { m_pointDrag = pt; }
  inline void setPoint(IN(RPoint) pt) { getWx()->SetPoint(CLS2WXREF(pt)); }

        // keyboard data (for wxEVT_COMMAND_TREE_KEY_DOWN only)
    //const wxKeyEvent& GetKeyEvent() const { return m_evtKey; }
  inline RKeyEvent getKeyEvent() const { return WXVAL2CLS(KeyEvent, getWx()->GetKeyEvent()); }
    //int GetKeyCode() const { return m_evtKey.GetKeyCode(); }
  inline int getKeyCode() const { return getWx()->GetKeyCode(); }
    //void SetKeyEvent(const wxKeyEvent& evt) { m_evtKey = evt; }
  inline void setKeyEvent(IN(RKeyEvent) evt) { getWx()->SetKeyEvent(CLS2WXREF(evt)); }

        // label (for EVT_TREE_{BEGIN|END}_LABEL_EDIT only)
    //const wxString& GetLabel() const { return m_label; }
  inline RString getLabel() const { return WXS2S(getWx()->GetLabel()); }
    //void SetLabel(const wxString& label) { m_label = label; }
  inline void setLabel(IN(RString)  label) { getWx()->SetLabel(S2WXS(label)); }

        // edit cancel flag (for EVT_TREE_{BEGIN|END}_LABEL_EDIT only)
    //bool IsEditCancelled() const { return m_editCancelled; }
  inline bool isEditCancelled() const { return getWx()->IsEditCancelled(); }
    //void SetEditCanceled(bool editCancelled) { m_editCancelled = editCancelled; }
  inline void setEditCanceled(bool editCancelled) { getWx()->SetEditCanceled(editCancelled); }
  
  
  static int EvtCommandTreeBeginDrag; // wxEVT_COMMAND_TREE_BEGIN_RDRAG, 601)
  static int EvtCommandTreeBeginRDrag; 
  static int EvtCommandTreeBeginLabelEdit; // wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT, 602)
  static int EvtCommandTreeEndLabelEdit; // wxEVT_COMMAND_TREE_END_LABEL_EDIT, 603)
  static int EvtCommandTreeDeleteItem; // wxEVT_COMMAND_TREE_DELETE_ITEM, 604)
  static int EvtCommandTreeGetInfo; // wxEVT_COMMAND_TREE_GET_INFO, 605)
  static int EvtCommandTreeSetInfo; // wxEVT_COMMAND_TREE_SET_INFO, 606)
  static int EvtCommandTreeItemExpanded; // wxEVT_COMMAND_TREE_ITEM_EXPANDED, 607)
  static int EvtCommandTreeItemExpanding; // wxEVT_COMMAND_TREE_ITEM_EXPANDING, 608)
  static int EvtCommandTreeItemCollapsed; // wxEVT_COMMAND_TREE_ITEM_COLLAPSED, 609)
  static int EvtCommandTreeItemCollapsing; // wxEVT_COMMAND_TREE_ITEM_COLLAPSING, 610)
  static int EvtCommandTreeSelChanged; // wxEVT_COMMAND_TREE_SEL_CHANGED, 611)
  static int EvtCommandTreeSelChanging; // wxEVT_COMMAND_TREE_SEL_CHANGING, 612)
  static int EvtCommandTreeKeyDown; // wxEVT_COMMAND_TREE_KEY_DOWN, 613)
  static int EvtCommandTreeItemActivated; // wxEVT_COMMAND_TREE_ITEM_ACTIVATED, 614)
  static int EvtCommandTreeItemRightClick; // wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK, 615)
  static int EvtCommandTreeItemMiddleClick; // wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK, 616)
  static int EvtCommandTreeEndDrag; // wxEVT_COMMAND_TREE_END_DRAG, 617)
  
};

ACDK_DECL_CLASS(TreeCtrl);


# define TREECTRL_BASE Window

/**
  see wxTreeCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC TreeCtrl
: extends Window
{
  ACDK_WITH_METAINFO(TreeCtrl)
public:
  // wxTreeCtrl
  ACDK_WX_STD_MEMBERS(TreeCtrl, TREECTRL_BASE)
  TreeCtrl() : TREECTRL_BASE(new wxTreeCtrl()) {  }

  //void TreeControl(wxWindow *parent, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxTR_DEFAULT_STYLE, const wxValidator &validator = wxDefaultValidator, const wxString& name = wxTreeCtrlNameStr)
  TreeCtrl( IN(RWindow) parent, int id = -1,  IN(RPoint) pos = Point::defaultPosition(), 
            IN(RSize) size = Size::defaultSize(), int style = TrDefaultStyle, 
            IN(RValidator) validator = Validator::defaultValidator(),  IN(RString)  name = "treectrl");

    
  
  /**
    get the total number of items in the control
  */
    //size_t GetCount() const;
  inline int getCount() const { return (int)getWx()->GetCount(); }

  /**
    indent is the number of pixels the children are indented relative to
    the parents position. SetIndent() also redraws the control
    immediately.
  */
  inline int getIndent() const { return getWx()->GetIndent(); }

  /// Sets the indentation for the tree control
  inline void setIndent(int indent) { getWx()->SetIndent(indent); }

   /// spacing is the number of pixels between the start and the Text
   //unsigned int GetSpacing() const { return m_spacing; }
  inline int getSpacing() const { return getWx()->GetSpacing(); }
    //void SetSpacing(int spacing); 
  inline void setSpacing(int spacing) { getWx()->SetSpacing(spacing); }

        // image list: these functions allow to associate an image list with
        // the control and retrieve it. Note that when assigned with
        // SetImageList, the control does _not_ delete
        // the associated image list when it's deleted in order to allow image
        // lists to be shared between different controls. If you use
        // AssignImageList, the control _does_ delete the image list.
        //
        // The normal image list is for the icons which correspond to the
        // normal tree item state (whether it is selected or not).
        // Additionally, the application might choose to show a state icon
        // which corresponds to an app-defined item state (for example,
        // checked/unchecked) which are taken from the state image list.
    //wxImageList *GetImageList() const;
  inline RImageList getImageList() const { RETURN_WXPTR2CLS(ImageList, getWx()->GetImageList()); }
  //wxImageList *GetStateImageList() const;
  inline RImageList getStateImageList() const { RETURN_WXPTR2CLS(ImageList, getWx()->GetStateImageList()); }
  //wxImageList *GetButtonsImageList() const;
  //not supported on win32: inline RImageList getButtonsImageList() const { RETURN_WXPTR2CLS(ImageList, getWx()->GetButtonsImageList()); }

  //void SetImageList(wxImageList *imageList);
  inline void setImageList(IN(RImageList) imageList) { getWx()->SetImageList(CLS2WXPTR(imageList)); }
  //void SetStateImageList(wxImageList *imageList);
  inline void setStateImageList(IN(RImageList) imageList) { getWx()->SetStateImageList(CLS2WXPTR(imageList)); }
  //void SetButtonsImageList(wxImageList *imageList);
  //not supported on win32: inline void setButtonsImageList(IN(RImageList) imageList) { getWx()->SetButtonsImageList(CLS2WXPTR(imageList)); }
  //void AssignImageList(wxImageList *imageList);
  inline void assignImageList(IN(RImageList) imageList) { getWx()->AssignImageList(CLS2WXPTR(imageList)); }
  //void AssignStateImageList(wxImageList *imageList);
  inline void assignStateImageList(IN(RImageList) imageList) { getWx()->AssignStateImageList(CLS2WXPTR(imageList)); }
  //void AssignButtonsImageList(wxImageList *imageList);
  //not supported on win32: inline void assignButtonsImageList(IN(RImageList) imageList) { getWx()->AssignButtonsImageList(CLS2WXPTR(imageList)); }

    // Functions to work with tree ctrl items.

    // accessors
    // ---------

     /// retrieve item's label
    //wxString GetItemText(const wxTreeItemId& item) const;
    inline RString getItemText(IN(RTreeItemId) item) const { return WXS2S(getWx()->GetItemText(CLS2WXREF(item))); }

    /// get one of the images associated with the item (TreeitemiconNormal by default)
    inline int getItemImage(IN(RTreeItemId) item, TreeItemIcon which = TreeitemiconNormal) const { return getWx()->GetItemImage(CLS2WXREF(item), (wxTreeItemIcon)which); }

        // get the data associated with the item
    //wxTreeItemData *GetItemData(const wxTreeItemId& item) const;
    inline RTreeItemData getItemData(IN(RTreeItemId) item) const { RETURN_WXPTR2CLS(TreeItemData, getWx()->GetItemData(CLS2WXREF(item))); }
    inline RObject getItemDataObject(IN(RTreeItemId) item) const 
    { 
      wxTreeItemData* itmd = getWx()->GetItemData(CLS2WXREF(item));
      if (itmd == 0)
        return Nil;
      wxAcdkTreeItemData* aitd = dynamic_cast<wxAcdkTreeItemData*>(itmd);
      if (aitd == 0)
        return Nil;
      return aitd->GetData();
    }

        // get the item's text colour
    //wxColour GetItemTextColour(const wxTreeItemId& item) const;
    inline RColour getItemTextColour(IN(RTreeItemId) item) const { return WXVAL2CLS(Colour, getWx()->GetItemTextColour(CLS2WXREF(item))); }

        // get the item's background colour
    //wxColour GetItemBackgroundColour(const wxTreeItemId& item) const;
    inline RColour getItemBackgroundColour(IN(RTreeItemId) item) const { return WXVAL2CLS(Colour, getWx()->GetItemBackgroundColour(CLS2WXREF(item))); }

        // get the item's font
    //wxFont GetItemFont(const wxTreeItemId& item) const;
    inline RFont getItemFont(IN(RTreeItemId) item) const { return WXVAL2CLS(Font, getWx()->GetItemFont(CLS2WXREF(item))); }

    // modifiers
    // ---------

        // set item's label
    //void SetItemText(const wxTreeItemId& item, const wxString& text);
    inline void setItemText(IN(RTreeItemId) item, IN(RString)  text) { getWx()->SetItemText(CLS2WXREF(item), S2WXS(text)); }
        // get one of the images associated with the item (normal by default)
    
    //void SetItemImage(const wxTreeItemId& item, int image, wxTreeItemIcon which = wxTreeItemIcon_Normal);
    inline void setItemImage(IN(RTreeItemId) item, int image, TreeItemIcon which = TreeitemiconNormal) { getWx()->SetItemImage(CLS2WXREF(item), image, (wxTreeItemIcon)which); }

        // associate some data with the item
    //void SetItemData(const wxTreeItemId& item, wxTreeItemData *data);
    inline void setItemData(IN(RTreeItemId) item, IN(RTreeItemData) data) { getWx()->SetItemData(CLS2WXREF(item), CLS2WXPTR(data)); }
    inline void setItemDataObject(IN(RTreeItemId) item, IN(RObject) data) { getWx()->SetItemData(CLS2WXREF(item), new wxAcdkTreeItemData(data)); }

        // force appearance of [+] button near the item. This is useful to
        // allow the user to expand the items which don't have any children now
        // - but instead add them only when needed, thus minimizing memory
        // usage and loading time.
    //void SetItemHasChildren(const wxTreeItemId& item, bool has = TRUE);
    inline void setItemHasChildren(IN(RTreeItemId) item, bool has = true) { getWx()->SetItemHasChildren(CLS2WXREF(item), has); }

        // the item will be shown in bold
    //void SetItemBold(const wxTreeItemId& item, bool bold = TRUE);
    inline void setItemBold(IN(RTreeItemId) item, bool bold = true) { getWx()->SetItemBold(CLS2WXREF(item), bold); }

        // set the item's text colour
    //void SetItemTextColour(const wxTreeItemId& item, const wxColour& col);
    inline void setItemTextColour(IN(RTreeItemId) item, IN(RColour) col) { getWx()->SetItemTextColour(CLS2WXREF(item), CLS2WXREF(col)); }

        // set the item's background colour
    //void SetItemBackgroundColour(const wxTreeItemId& item, const wxColour& col);
    inline void setItemBackgroundColour(IN(RTreeItemId) item, IN(RColour) col) { getWx()->SetItemBackgroundColour(CLS2WXREF(item), CLS2WXREF(col)); }

        // set the item's font (should be of the same height for all items)
    //void SetItemFont(const wxTreeItemId& item, const wxFont& font);
    inline void setItemFont(IN(RTreeItemId) item, IN(RFont) font) { getWx()->SetItemFont(CLS2WXREF(item), CLS2WXREF(font)); }

        // set the window font
    //virtual bool SetFont( const wxFont &font );
    inline virtual bool setFont(IN(RFont) font) { return getWx()->SetFont(CLS2WXREF(font)); }

       // set the styles.  No need to specify a GetWindowStyle here since
       // the base wxWindow member function will do it for us
    //void SetWindowStyle(const long styles);
    inline void setWindowStyle(int styles) { getWx()->SetWindowStyle(styles); }

    // item status inquiries
    // ---------------------

        // is the item visible (it might be outside the view or not expanded)?
    //bool IsVisible(const wxTreeItemId& item) const;
    inline bool isVisible(IN(RTreeItemId) item) const { return getWx()->IsVisible(CLS2WXREF(item)); }
        // does the item has any children?
    
      
    //bool ItemHasChildren(const wxTreeItemId& item) const;
    inline bool itemHasChildren(IN(RTreeItemId) item) const { return getWx()->ItemHasChildren(CLS2WXREF(item)); }
        // is the item expanded (only makes sense if HasChildren())?
    //bool IsExpanded(const wxTreeItemId& item) const;
    inline bool isExpanded(IN(RTreeItemId) item) const { return getWx()->IsExpanded(CLS2WXREF(item)); }
        // is this item currently selected (the same as has focus)?
    //bool IsSelected(const wxTreeItemId& item) const;
    inline bool isSelected(IN(RTreeItemId) item) const { return getWx()->IsSelected(CLS2WXREF(item)); }
        // is item text in bold font?
    //bool IsBold(const wxTreeItemId& item) const;
    inline bool isBold(IN(RTreeItemId) item) const { return getWx()->IsBold(CLS2WXREF(item)); }
        // does the layout include space for a button?

    // number of children
    // ------------------

        // if 'recursively' is FALSE, only immediate children count, otherwise
        // the returned number is the number of all items in this branch
    //size_t GetChildrenCount(const wxTreeItemId& item, bool recursively = TRUE);
    inline int getChildrenCount(IN(RTreeItemId) item, bool recursively = true) { return getWx()->GetChildrenCount(CLS2WXREF(item), recursively); }

    // navigation
    // ----------

    // wxTreeItemId.IsOk() will return FALSE if there is no such item

        // get the root tree item
    //wxTreeItemId GetRootItem() const { return m_anchor; }
    inline RTreeItemId getRootItem() const { return WXVAL2CLS(TreeItemId, getWx()->GetRootItem()); }

        // get the item currently selected (may return NULL if no selection)
    //wxTreeItemId GetSelection() const { return m_current; }
    inline RTreeItemId getSelection() const { return WXVAL2CLS(TreeItemId, getWx()->GetSelection()); }

        // get the items currently selected, return the number of such item
    //size_t GetSelections(wxArrayTreeItemIds&) const;
    //int GetSelections(wxArrayTreeItemIds& ids) const;
    /**
      expects an initialized != Nil Array
    */
    inline int getSelections(IN(RTreeItemIdArray) tids) const 
    { 
      wxArrayTreeItemIds wids;
      int erg = getWx()->GetSelections(wids);
      
      for (int i = 0; i < erg; ++i)
      {
        tids->append(new TreeItemId(wids.Item(i)));
      }
      return erg;
    }

        // get the parent of this item (may return NULL if root)
    //wxTreeItemId GetItemParent(const wxTreeItemId& item) const;
    inline RTreeItemId getItemParent(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetItemParent(CLS2WXREF(item))); }

        // for this enumeration function you must pass in a "cookie" parameter
        // which is opaque for the application but is necessary for the library
        // to make these functions reentrant (i.e. allow more than one
        // enumeration on one and the same object simultaneously). Of course,
        // the "cookie" passed to GetFirstChild() and GetNextChild() should be
        // the same!

        // get the first child of this item
    //wxTreeItemId GetFirstChild(const wxTreeItemId& item, long& cookie) const;
    inline RTreeItemId getFirstChild(IN(RTreeItemId) item, OUT(jlong) cookie) const 
    { 
      wxTreeItemIdValue* tid = (wxTreeItemIdValue*)&cookie;
      return WXVAL2CLS(TreeItemId, getWx()->GetFirstChild(CLS2WXREF(item), *tid)); 
    }
        // get the next child
    //wxTreeItemId GetNextChild(const wxTreeItemId& item, long& cookie) const;
    inline RTreeItemId getNextChild(IN(RTreeItemId) item, jlong cookie) const 
    { return WXVAL2CLS(TreeItemId, getWx()->GetNextChild(CLS2WXREF(item), *((wxTreeItemIdValue*)&cookie))); }
        // get the last child of this item - this method doesn't use cookies
    //wxTreeItemId GetLastChild(const wxTreeItemId& item) const;
    inline RTreeItemId getLastChild(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetLastChild(CLS2WXREF(item))); }

        // get the next sibling of this item
    //wxTreeItemId GetNextSibling(const wxTreeItemId& item) const;
    inline RTreeItemId getNextSibling(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetNextSibling(CLS2WXREF(item))); }
        // get the previous sibling
    //wxTreeItemId GetPrevSibling(const wxTreeItemId& item) const;
    inline RTreeItemId getPrevSibling(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetPrevSibling(CLS2WXREF(item))); }

        // get first visible item
    //wxTreeItemId GetFirstVisibleItem() const;
    inline RTreeItemId getFirstVisibleItem() const { return WXVAL2CLS(TreeItemId, getWx()->GetFirstVisibleItem()); }
        // get the next visible item: item must be visible itself!
        // see IsVisible() and wxTreeCtrl::GetFirstVisibleItem()
    //wxTreeItemId GetNextVisible(const wxTreeItemId& item) const;
    inline RTreeItemId getNextVisible(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetNextVisible(CLS2WXREF(item))); }
        // get the previous visible item: item must be visible itself!
    //wxTreeItemId GetPrevVisible(const wxTreeItemId& item) const;
    inline RTreeItemId getPrevVisible(IN(RTreeItemId) item) const { return WXVAL2CLS(TreeItemId, getWx()->GetPrevVisible(CLS2WXREF(item))); }
    

    // operations
    // ----------

        // add the root node to the tree
    //wxTreeItemId AddRoot(const wxString& text, int image = -1, int selectedImage = -1, wxTreeItemData *data = NULL);
    inline RTreeItemId addRoot(IN(RString)  text, int image = -1, int selectedImage = -1, IN(RTreeItemData) data = Nil) { return WXVAL2CLS(TreeItemId, getWx()->AddRoot(S2WXS(text), image, selectedImage, CLS2WXPTR(data))); }

        // insert a new item in as the first child of the parent
    //wxTreeItemId PrependItem(const wxTreeItemId& parent, const wxString& text, int image = -1, int selectedImage = -1, wxTreeItemData *data = NULL);
    inline RTreeItemId prependItem(IN(RTreeItemId) parent, IN(RString)  text, int image = -1, int selectedImage = -1, IN(RTreeItemData) data = Nil) { return WXVAL2CLS(TreeItemId, getWx()->PrependItem(CLS2WXREF(parent), S2WXS(text), image, selectedImage, CLS2WXPTR(data))); }

        // insert a new item after a given one
    //wxTreeItemId InsertItem(const wxTreeItemId& parent, const wxTreeItemId& idPrevious, const wxString& text, int image = -1, int selectedImage = -1, wxTreeItemData *data = NULL);
    inline RTreeItemId insertItem(IN(RTreeItemId) parent, IN(RTreeItemId) idPrevious, IN(RString)  text, int image = -1, int selectedImage = -1, IN(RTreeItemData) data = Nil) { return WXVAL2CLS(TreeItemId, getWx()->InsertItem(CLS2WXREF(parent), CLS2WXREF(idPrevious), S2WXS(text), image, selectedImage, CLS2WXPTR(data))); }

        // insert a new item before the one with the given index
    //wxTreeItemId InsertItem(const wxTreeItemId& parent, size_t index, const wxString& text, int image = -1, int selectedImage = -1, wxTreeItemData *data = NULL);
    inline RTreeItemId insertItem(IN(RTreeItemId) parent, int index, IN(RString)  text, int image = -1, int selectedImage = -1, IN(RTreeItemData) data = Nil) { return WXVAL2CLS(TreeItemId, getWx()->InsertItem(CLS2WXREF(parent), index, S2WXS(text), image, selectedImage, CLS2WXPTR(data))); }

        // insert a new item in as the last child of the parent
    //wxTreeItemId AppendItem(const wxTreeItemId& parent, const wxString& text, int image = -1, int selectedImage = -1, wxTreeItemData *data = NULL);
    inline RTreeItemId appendItem(IN(RTreeItemId) parent, IN(RString)  text, int image = -1, int selectedImage = -1, IN(RTreeItemData) data = Nil) { return WXVAL2CLS(TreeItemId, getWx()->AppendItem(CLS2WXREF(parent), S2WXS(text), image, selectedImage, CLS2WXPTR(data))); }

        // delete this item and associated data if any
    //void Delete(const wxTreeItemId& item);
    inline void deleteItem(IN(RTreeItemId) item) { getWx()->Delete(CLS2WXREF(item)); }
        // delete all children (but don't delete the item itself)
        // NB: this won't send wxEVT_COMMAND_TREE_ITEM_DELETED events
    //void DeleteChildren(const wxTreeItemId& item);
    inline void deleteChildren(IN(RTreeItemId) item) { getWx()->DeleteChildren(CLS2WXREF(item)); }
        // delete all items from the tree
        // NB: this won't send wxEVT_COMMAND_TREE_ITEM_DELETED events
    //void DeleteAllItems();
    inline void deleteAllItems() { getWx()->DeleteAllItems(); }

        // expand this item
    //void Expand(const wxTreeItemId& item);
    inline void expand(IN(RTreeItemId) item) { getWx()->Expand(CLS2WXREF(item)); }
        // expand this item and all subitems recursively
    //void ExpandAll(const wxTreeItemId& item);
    //deprecated inline void expandAll(IN(RTreeItemId) item) { getWx()->ExpandAll(CLS2WXREF(item)); }
        // collapse the item without removing its children
    //void Collapse(const wxTreeItemId& item);
    inline void collapse(IN(RTreeItemId) item) { getWx()->Collapse(CLS2WXREF(item)); }
        // collapse the item and remove all children
    //void CollapseAndReset(const wxTreeItemId& item);
    inline void collapseAndReset(IN(RTreeItemId) item) { getWx()->CollapseAndReset(CLS2WXREF(item)); }
        // toggles the current state
    //void Toggle(const wxTreeItemId& item);
    inline void toggle(IN(RTreeItemId) item) { getWx()->Toggle(CLS2WXREF(item)); }

        // remove the selection from currently selected item (if any)
    //void Unselect();
    inline void unselect() { getWx()->Unselect(); }
    //void UnselectAll();
    inline void unselectAll() { getWx()->UnselectAll(); }
        // select this item
    //void SelectItem(const wxTreeItemId& item, bool unselect_others=TRUE, bool extended_select=FALSE);
    inline void selectItem(IN(RTreeItemId) item) { getWx()->SelectItem(CLS2WXREF(item)); }
        // make sure this item is visible (expanding the parent item and/or
        // scrolling to this item if necessary)
    //void EnsureVisible(const wxTreeItemId& item);
    inline void ensureVisible(IN(RTreeItemId) item) { getWx()->EnsureVisible(CLS2WXREF(item)); }
        // scroll to this item (but don't expand its parent)
    //void ScrollTo(const wxTreeItemId& item);
    inline void scrollTo(IN(RTreeItemId) item) { getWx()->ScrollTo(CLS2WXREF(item)); }
    
        // The first function is more portable (because easier to implement
        // on other platforms), but the second one returns some extra info.
    //wxTreeItemId HitTest(const wxPoint& point) { int dummy; return HitTest(point, dummy); }
    inline RTreeItemId hitTest(IN(RPoint) point) { return WXVAL2CLS(TreeItemId, getWx()->HitTest(CLS2WXREF(point))); }
    //wxTreeItemId HitTest(const wxPoint& point, int& flags);
    inline RTreeItemId hitTest(IN(RPoint) point, OUT(int) flags) { return WXVAL2CLS(TreeItemId, getWx()->HitTest(CLS2WXREF(point), flags)); }

        // get the bounding rectangle of the item (or of its label only)
    //bool GetBoundingRect(const wxTreeItemId& item, wxRect& rect, bool textOnly = FALSE) const;
    inline bool getBoundingRect(IN(RTreeItemId) item, OUT(RRect) rect, bool textOnly = false) const { return getWx()->GetBoundingRect(CLS2WXREF(item), CLS2WXOUTREF(rect), textOnly); }

        // Start editing the item label: this (temporarily) replaces the item
        // with a one line edit control. The item will be selected if it hadn't
        // been before.
    //void EditLabel( const wxTreeItemId& item ) { Edit( item ); }
    inline void editLabel(IN(RTreeItemId) item) { getWx()->EditLabel(CLS2WXREF(item)); }
    
        // returns a pointer to the text edit control if the item is being
        // edited, NULL otherwise (it's assumed that no more than one item may
        // be edited simultaneously)
    //wxTextCtrl* GetEditControl() const;
    inline RTextCtrl getEditControl() const { RETURN_WXPTR2CLS(TextCtrl, getWx()->GetEditControl()); }

    // sorting
        // this function is called to compare 2 items and should return -1, 0
        // or +1 if the first item is less than, equal to or greater than the
        // second one. The base class version performs alphabetic comparaison
        // of item labels (GetText)
    //virtual int OnCompareItems(const wxTreeItemId& item1, const wxTreeItemId& item2);
    inline virtual int onCompareItems(IN(RTreeItemId) item1, IN(RTreeItemId) item2) { return getWx()->OnCompareItems(CLS2WXREF(item1), CLS2WXREF(item2)); }
        // sort the children of this item using OnCompareItems
        //
        // NB: this function is not reentrant and not MT-safe (FIXME)!
    //void SortChildren(const wxTreeItemId& item);
    inline void sortChildren(IN(RTreeItemId) item) { getWx()->SortChildren(CLS2WXREF(item)); }
    
};

inline
TreeCtrl::TreeCtrl( IN(RWindow) parent, int id,  IN(RPoint) pos, 
            IN(RSize) size, int style, 
            IN(RValidator) validator,  IN(RString) name) 
#if defined(ACDK_OS_WIN32)
    : TREECTRL_BASE(new wxTreeCtrl(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, CLS2WXREF(validator), S2WXS(name))) 
#else
    : TREECTRL_BASE(new wxTreeCtrl(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size)))
#endif
    {
      ownsWxObject(false);
      // ### TODO handle  style, CLS2WXREF(validator), S2WXS(name) if !defined(ACDK_OS_WIN32)
    }

} // wx
} // acdk

#endif //acdk_wx_TreeCtrl_h
