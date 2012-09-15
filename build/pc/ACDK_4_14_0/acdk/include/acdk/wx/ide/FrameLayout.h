
#ifndef acdk_wx_ide_FrameLayout_h
#define acdk_wx_ide_FrameLayout_h

#include "ide.h"
#include <acdk/wx/ToolBar.h>

#include <wx/fl/controlbar.h>
// extra plugins
#include <wx/fl/barhintspl.h>    // bevel for bars with "X"s and grooves
#include <wx/fl/rowdragpl.h>     // NC-look with draggable rows
#include <wx/fl/cbcustom.h>      // customization plugin
#include <wx/fl/hintanimpl.h>

// beauty-care
#include <wx/fl/gcupdatesmgr.h>  // smooth d&d
#include <wx/fl/antiflickpl.h>   // double-buffered repaint of decorations
#include <wx/fl/dyntbar.h>       // auto-layout toolbar
#include <wx/fl/dyntbarhnd.h>    // control-bar dimension handler for it

#include <wx/fl/dyntbar.h>    // dynamic 
#include <wx/fl/dyntbarhnd.h>

// to make it macro compatible
typedef   cbBarDimHandlerBase wxBarDimHandlerBase;

namespace acdk {
namespace wx {
namespace ide {


enum CtrlBarStates
{
  CbarDockedHorizontally = wxCBAR_DOCKED_HORIZONTALLY,  // wxCBAR_DOCKED_HORIZONTALLY 0
  CbarDockedVertically   = wxCBAR_DOCKED_VERTICALLY  ,  // wxCBAR_DOCKED_VERTICALLY   1
  CbarFloating            = wxCBAR_FLOATING           ,  // wxCBAR_FLOATING            2
  CbarHidden              = wxCBAR_HIDDEN               // wxCBAR_HIDDEN              3
};

enum CtrlBarAlignment 
{
  FlAlignTop        = FL_ALIGN_TOP       ,  // wxFL_ALIGN_TOP        0
  FlAlignBottom     = FL_ALIGN_BOTTOM    ,  // wxFL_ALIGN_BOTTOM     1
  FlAlignLeft       = FL_ALIGN_LEFT      ,  // wxFL_ALIGN_LEFT       2
  FlAlignRight      = FL_ALIGN_RIGHT       // wxFL_ALIGN_RIGHT      3
};

enum FrameLayoutPlugins 
{
  /**
    This class intercepts bar-decoration and sizing events, and draws 3D hints
    around fixed and flexible bars, similar to those in Microsoft DevStudio 6.x
    from fl (c) Aleksandras Gluchovas
  */
  BarHintsPlugin            = 0x0001,
  
  /**
    A plugin to draw animated hints when the user drags a pane.
    from fl (c) Aleksandras Gluchovas
  */
  HintAnimationPlugin       = 0x0002,
  /*
    This plugin adds row-dragging functionality to the pane.
    It handles mouse movement and pane background-erasing plugin events.
    The behaviour and appearance resembles drag and drop positioning
    of the toolbar rows in Netscape Communicator 4.xx.
    from fl (c) Aleksandras Gluchovas
*/
  RowDragPlugin             = 0x0004,
  /** 
    Implements double-buffering to reduce flicker.
    
    from fl (c) Aleksandras Gluchovas
    
    Bitmap and memory DC buffers are shared 'resources' among all instances of
    antiflicker plugins within the application.
    Locking for multithreaded applications is not yet implemented.
  */
  AntiflickerPlugin         = 0x0008,
  /**
    This class enables customization of a bar, popping up a
    menu and handling basic customization such as floating
    and horizontal/vertical alignment of the bar.
    from fl (c) Aleksandras Gluchovas
  */
  SimpleCustomizationPlugin = 0x0010,
  /**
    Plugin class implementing bar dragging.
    from fl (c) Aleksandras Gluchovas

    currently not supported
  */
  BarDragPlugin             = 0x0020
};


ACDK_DECL_CLASS(BarDimHandlerBase);


/**
  cbBarDimHandlerBase
*/
class ACDK_WX_IDE_PUBLIC BarDimHandlerBase
: extends WxObject
{
  ACDK_WITH_METAINFO(BarDimHandlerBase)
public:
  ACDK_WX_STD_MEMBERS(BarDimHandlerBase, WxObject)
};

ACDK_DECL_CLASS(DynToolBarDimHandler);

/**
  wrapper to cbDynToolBarDimHandler
  // later implement cbBarDimHandlerBase
*/
class ACDK_WX_IDE_PUBLIC DynToolBarDimHandler
: extends BarDimHandlerBase
{
  ACDK_WITH_METAINFO(DynToolBarDimHandler)
public:
  DynToolBarDimHandler() : BarDimHandlerBase(new cbDynToolBarDimHandler(), false) {}
};


ACDK_DECL_CLASS(DimInfo);

/**
  small wrapper for cbDimInfo
*/
class ACDK_WX_IDE_PUBLIC DimInfo
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DimInfo)
protected:
  cbDimInfo _dimInfo;
public:
  DimInfo(int dh_x, int dh_y, int dv_x, int dv_y, int f_x,  int f_y, bool isFixed  = true, 
          int horizGap = 6, int vertGap  = 6,
          IN(RBarDimHandlerBase) dimHandler = Nil)
    : _dimInfo(dh_x, dh_y, dv_x, dv_y, f_x, f_y, isFixed, horizGap, vertGap, CLS2WXPTR(dimHandler))
  {
  }
  foreign cbDimInfo& getDimInfo() { return _dimInfo; }
};


// for macro compat
typedef cbDockPane wxDockPane;
typedef cbRowInfo wxRowInfo;
typedef cbBarInfo wxBarInfo;


ACDK_DECL_CLASS(BarInfo);

class ACDK_WX_IDE_PUBLIC BarInfo
: extends WxObject
{
  ACDK_WITH_METAINFO(BarInfo)
public:
  ACDK_WX_STD_MEMBERS(BarInfo, WxObject)
  //bool IsFixed() const { return mDimInfo.mIsFixed; }
  inline bool isFixed() const { return getWx()->IsFixed(); }
  //bool IsExpanded() const { return this == mpRow->mpExpandedBar; }
  inline bool isExpanded() const { return getWx()->IsExpanded(); }
  inline RString getName() { return WXS2S(getWx()->mName); }
  inline void setName(IN(RString) name) { getWx()->mName = S2WXS(name); }
  inline RRect getBounds() { return WXVAL2CLS(Rect, getWx()->mBounds); }
  inline void setBounds(IN(RRect) rect) { getWx()->mBounds = CLS2WXREF(rect); }
  inline RRect getBoundsInParent() { return WXVAL2CLS(Rect, getWx()->mBoundsInParent); }
  inline void setBoundsInParent(IN(RRect) rect) { getWx()->mBoundsInParent = CLS2WXREF(rect); }
  inline int getState() { return getWx()->mState; }
  inline void setState(int state) { getWx()->mState = state; }
  
  /// the actual window object, NULL if no window
  /// is attached to the control bar (possible!)
  inline RWindow getBarWindow() { return WXPTR2CLS(Window, getWx()->mpBarWnd); }
  inline void setBarWindow(IN(RWindow) window) { getWx()->mpBarWnd = CLS2WXPTR(window); }
      
   // back-ref to the row, which contains this bar
  //cbRowInfo*    mpRow;
/*
    // are set up according to the types of the surrounding bars in the row
    bool          mHasLeftHandle;
    bool          mHasRightHandle;

    // determines if this bar can float. The layout's setting as priority. For 
    // example, if the layout's mFloatingOn is false, this setting is irrelevant
    // since nothing will float at all. If the layout's floating is on, use this
    // setting to prevent specific bars from floating. In other words, all bars 
    // float by default and floating can be turned off on individual bars.
    bool          mFloatingOn;    // default: ON (which is also the layout's mFloatingOn default setting)

    cbDimInfo     mDimInfo;       // preferred sizes for each, control bar state


    int           mAlignment;     // alignment of the pane to which this
                                  // bar is currently placed

    int           mRowNo;         // row, into which this bar would be placed,
                                  // when in the docking state

    double        mLenRatio;      // length ratio among not-fixed-size bars

    wxPoint       mPosIfFloated;  
    */
};


ACDK_DECL_CLASS(RowInfo);

class ACDK_WX_IDE_PUBLIC RowInfo
: extends WxObject
{
  ACDK_WITH_METAINFO(RowInfo)
public:
  ACDK_WX_STD_MEMBERS(RowInfo, WxObject)
  //cbBarInfo* GetFirstBar()
  inline RBarInfo getFirstBar() { RETURN_WXPTR2CLS(BarInfo, getWx()->GetFirstBar()); }
};


ACDK_DECL_CLASS(DockPane);

class ACDK_WX_IDE_PUBLIC DockPane
: extends WxObject
{
  ACDK_WITH_METAINFO(DockPane)
public:
  ACDK_WX_STD_MEMBERS(DockPane, WxObject)
  
    //cbRowInfo* GetRow( int row );
    inline RRowInfo getRow(int row) { RETURN_WXPTR2CLS(RowInfo, getWx()->GetRow(row)); }

        // Returns the row index for the given row info.  Internal function called by plugins.

    //int GetRowIndex( cbRowInfo* pRow );
  inline int getRowIndex(IN(RRowInfo) pRow) { return getWx()->GetRowIndex(CLS2WXPTR(pRow)); }

        // Returns the row at the given vertical position.
        // Returns -1 if the row is not present at given vertical position.
        // Internal function called by plugins.

    //int     GetRowAt( int paneY );
  inline int getRowAt(int paneY) { return getWx()->GetRowAt(paneY); }

        // Returns the row between the given vertical positions.
        // Returns -1 if the row is not present.
        // Internal function called by plugins.

    //int     GetRowAt( int upperY, int lowerY );
  inline int getRowAt(int upperY, int lowerY) { return getWx()->GetRowAt(upperY, lowerY); }

        // Sets up flags in the row information structure, so that
        // they match the changed state of row items correctly.
        // Internal function called by plugins.

    //void SyncRowFlags( cbRowInfo* pRow );
  inline void syncRowFlags(IN(RRowInfo) pRow) { getWx()->SyncRowFlags(CLS2WXPTR(pRow)); }

        // Returns TRUE if the bar's dimension information indicates a fixed size.
        // Internal function called by plugins.

    //bool IsFixedSize( cbBarInfo* pInfo );
  inline bool isFixedSize(IN(RBarInfo) pInfo) { return getWx()->IsFixedSize(CLS2WXPTR(pInfo)); }

        // Returns the number of bars whose size is not fixed.
        // Internal function called by plugins.

    //int  GetNotFixedBarsCount( cbRowInfo* pRow );
  inline int getNotFixedBarsCount(IN(RRowInfo) pRow) { return getWx()->GetNotFixedBarsCount(CLS2WXPTR(pRow)); }

        // Gets the vertical position at the given row.
        // Internal function called by plugins.

    //int GetRowY( cbRowInfo* pRow );
  inline int getRowY(IN(RRowInfo) pRow) { return getWx()->GetRowY(CLS2WXPTR(pRow)); }

        // Returns TRUE if there are any variable-sized rows above this one.
        // Internal function called by plugins.

    //bool HasNotFixedRowsAbove( cbRowInfo* pRow );
  inline bool hasNotFixedRowsAbove(IN(RRowInfo) pRow) { return getWx()->HasNotFixedRowsAbove(CLS2WXPTR(pRow)); }

        // Returns TRUE if there are any variable-sized rows below this one.
        // Internal function called by plugins.

    //bool HasNotFixedRowsBelow( cbRowInfo* pRow );
  inline bool hasNotFixedRowsBelow(IN(RRowInfo) pRow) { return getWx()->HasNotFixedRowsBelow(CLS2WXPTR(pRow)); }

        // Returns TRUE if there are any variable-sized rows to the left of this one.
        // Internal function called by plugins.

    //bool HasNotFixedBarsLeft ( cbBarInfo* pBar );
  inline bool hasNotFixedBarsLeft(IN(RBarInfo) pBar) { return getWx()->HasNotFixedBarsLeft(CLS2WXPTR(pBar)); }

        // Returns TRUE if there are any variable-sized rows to the right of this one.
        // Internal function called by plugins.

    //bool HasNotFixedBarsRight( cbBarInfo* pBar );
  inline bool hasNotFixedBarsRight(IN(RBarInfo) pBar) { return getWx()->HasNotFixedBarsRight(CLS2WXPTR(pBar)); }

        // Calculate lengths.
        // Internal function called by plugins.

    //virtual void CalcLengthRatios( cbRowInfo* pInRow );
  inline virtual void calcLengthRatios(IN(RRowInfo) pInRow) { getWx()->CalcLengthRatios(CLS2WXPTR(pInRow)); }

        // Generates a cbLayoutRowEvent event to recalculate row layouts.
        // Internal function called by plugins.

    //virtual void RecalcRowLayout( cbRowInfo* pRow );
  inline virtual void recalcRowLayout(IN(RRowInfo) pRow) { getWx()->RecalcRowLayout(CLS2WXPTR(pRow)); }

        // Expands the bar.
        // Internal function called by plugins.

    //virtual void ExpandBar( cbBarInfo* pBar );
  inline virtual void expandBar(IN(RBarInfo) pBar) { getWx()->ExpandBar(CLS2WXPTR(pBar)); }

        // Contracts the bar.
        // Internal function called by plugins.

    //virtual void ContractBar( cbBarInfo* pBar );
  inline virtual void contractBar(IN(RBarInfo) pBar) { getWx()->ContractBar(CLS2WXPTR(pBar)); }

        // Sets up links between bars.
        // Internal function called by plugins.

    //void InitLinksForRow( cbRowInfo* pRow );
  inline void initLinksForRow(IN(RRowInfo) pRow) { getWx()->InitLinksForRow(CLS2WXPTR(pRow)); }

        // Sets up links between bars.
        // Internal function called by plugins.

    //void InitLinksForRows();
  inline void initLinksForRows() { getWx()->InitLinksForRows(); }

        // Coordinate translation between parent's frame and this pane.
        // Internal function called by plugins.

    //void FrameToPane( int* x, int* y );
  inline void frameToPane(INOUT(int) x, INOUT(int) y) { getWx()->FrameToPane(&x, &y); }

        // Coordinate translation between parent's frame and this pane.
        // Internal function called by plugins.

    //void PaneToFrame( int* x, int* y );
  inline void paneToFrame(INOUT(int) x, INOUT(int) y) { getWx()->PaneToFrame(&x, &y); }

        // Coordinate translation between parent's frame and this pane.
        // Internal function called by plugins.

    //void FrameToPane( wxRect* pRect );
  inline void frameToPane(IN(RRect) pRect) { getWx()->FrameToPane(CLS2WXPTR(pRect)); }

        // Coordinate translation between parent's frame and this pane.
        // Internal function called by plugins.

    //void PaneToFrame( wxRect* pRect );
  inline void paneToFrame(IN(RRect) pRect) { getWx()->PaneToFrame(CLS2WXPTR(pRect)); }

        // Returns TRUE if pos is within the given rectangle.
        // Internal function called by plugins.

    //inline bool HasPoint( const wxPoint& pos, int x, int y, int width, int height );
  inline bool hasPoint(IN(RPoint) pos, int x, int y, int width, int height) { return getWx()->HasPoint(CLS2WXREF(pos), x, y, width, height); }

        // Returns the minimal row height for the given row.
        // Internal function called by plugins.

    //int GetMinimalRowHeight( cbRowInfo* pRow );
  inline int getMinimalRowHeight(IN(RRowInfo) pRow) { return getWx()->GetMinimalRowHeight(CLS2WXPTR(pRow)); }

        // Sets the row height for the given height. newHeight includes the height of row handles, if present.
        // Internal function called by plugins.

    //void SetRowHeight( cbRowInfo* pRow, int newHeight );
  inline void setRowHeight(IN(RRowInfo) pRow, int newHeight) { getWx()->SetRowHeight(CLS2WXPTR(pRow), newHeight); }

        // Inserts the bar at the given row number.
        // Internal function called by plugins.

    //void DoInsertBar( cbBarInfo* pBar, int rowNo );
  inline void doInsertBar(IN(RBarInfo) pBar, int rowNo) { getWx()->DoInsertBar(CLS2WXPTR(pBar), rowNo); }

};


ACDK_DECL_CLASS(FrameLayout);

/**
  see wxFrameLayout
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/06 13:12:12 $
*/
class ACDK_WX_IDE_PUBLIC FrameLayout
: extends acdk::wx::EvtHandler
{
  ACDK_WITH_METAINFO(FrameLayout)
public:
  /// wxFrameLayout
  ACDK_WX_STD_MEMBERS(FrameLayout, EvtHandler)
  //void EnableFloating( bool enable = TRUE );
  
  FrameLayout(IN(RWindow) parentFrame, IN(RWindow) frameClient = Nil, bool  activateNow  = true)
  : EvtHandler(new wxFrameLayout(CLS2WXPTR(parentFrame), CLS2WXPTR(frameClient), activateNow))
  {}
  /**
    @paramIds is combination of FrameLayoutPlugins 
  */
  void addPlugins(int plugIds)
  {
    if (plugIds & BarHintsPlugin)
      addPlugin("cbBarHintsPlugin"); 
    if (plugIds & RowDragPlugin)
      addPlugin("cbRowDragPlugin"); 
    if (plugIds & AntiflickerPlugin)
      addPlugin("cbAntiflickerPlugin"); 
    if (plugIds & SimpleCustomizationPlugin)
      addPlugin("cbSimpleCustomizationPlugin"); 
  }
  bool addPlugin(IN(RString) pluginWxClassName)
  {
    wxClassInfo* classInfo = wxClassInfo::FindClass(S2WXS(pluginWxClassName));
    if (classInfo == 0)
      return false;
    getWx()->AddPlugin(classInfo);
    return true;
  }
  inline void enableFloating(bool enable = true) { getWx()->EnableFloating(enable); }

        // Activate can be called after some other layout has been deactivated,
        // and this one must take over the current contents of the frame window.
        //
        // Effectively hooks itself to the frame window, re-displays all non-hidden
        // bar windows and repaints the decorations.

    //void Activate();
  inline void activate() { getWx()->Activate(); }

        // Deactivate unhooks itself from frame window, and hides all non-hidden windows.
        //
        // Note: two frame layouts should not be active at the same time in the
        // same frame window, since it would cause messy overlapping of bar windows
        // from both layouts.

    //void Deactivate();
  inline void deactivate() { getWx()->Deactivate(); }

        // Hides the bar windows, and also the client window if present.

    //void HideBarWindows();
  inline void hideBarWindows() { getWx()->HideBarWindows(); }

        // Destroys the bar windows.

    //void DestroyBarWindows();
  inline void destroyBarWindows() { getWx()->DestroyBarWindows(); }

        // Passes the client window (e.g. MDI client window) to be controlled by
        // frame layout, the size and position of which should be adjusted to be
        // surrounded by controlbar panes, whenever the frame is resized or the dimensions
        // of control panes change.

    //void SetFrameClient( wxWindow* pFrameClient );
  inline void setFrameClient(IN(RWindow) pFrameClient) { getWx()->SetFrameClient(CLS2WXPTR(pFrameClient)); }

        // Returns the frame client, or NULL if not present.

    //wxWindow* GetFrameClient();
  inline RWindow getFrameClient() { RETURN_WXPTR2CLS(Window, getWx()->GetFrameClient()); }

        // Returns the parent frame.

    //wxWindow& GetParentFrame() { return *mpFrame; }
  inline RWindow getParentFrame() { return WXVAL2CLS(Window, getWx()->GetParentFrame()); }

        // Returns an array of panes. Used by update managers.
  RDockPaneArray getPanesArray()
  {
    wxDockPane** wxpanes = getWx()->GetPanesArray();
    RDockPaneArray ret = new DockPaneArray(0);
    for (int i = 0; i < MAX_PANES; ++i)
      ret->append(WXPTR2CLS(DockPane, wxpanes[i]));
    return ret;
  }
  

       
  /** Returns a pane for the given alignment. See pane alignment types.
    cbDockPane* GetPane( int alignment ) { return mPanes[alignment]; }
  */
  inline RDockPane getPane(int alignment) { RETURN_WXPTR2CLS(DockPane, getWx()->GetPane(alignment)); }

  /**
    note: the order of argument is a little bit different
    // Adds bar information to the frame layout. The appearance of the layout is not refreshed
        // immediately; RefreshNow() can be called if necessary.
        //
        // Notes: the argument pBarWnd can by NULL, resulting in bar decorations to be drawn
        // around the empty rectangle (filled with default background colour).
        // Argument dimInfo can be reused for adding any number of bars, since
        // it is not used directly - instead its members are copied. If the dimensions
        // handler is present, its instance is shared (reference counted). The dimension
        // handler should always be allocated on the heap.

        // pBarWnd is the window to be managed.

        // dimInfo contains dimension information.

        // alignment is a value such as FL_ALIGN_TOP.

        // rowNo is the vertical position or row in the pane (if in docked state).

        // columnPos is the horizontal position within the row in pixels (if in docked state).

        // name is a name by which the bar can be referred in layout customization dialogs.

        // If spyEvents is TRUE, input events for the bar should be "spyed" in order
        // to forward unhandled mouse clicks to the frame layout, for example to enable
        // easy draggablity of toolbars just by clicking on their interior regions.
        // For widgets like text/tree control this value should be FALSE,
        // since there's no certain way to detect  whether the event was actually handled.

        // state is the initial state, such as wxCBAR_DOCKED_HORIZONTALLY,
        // wxCBAR_FLOATING, wxCBAR_HIDDEN.
  */
  inline void addBar(IN(RWindow) pBarWnd, IN(RDimInfo) dimInfo, int alignment = FlAlignTop, int state = CbarDockedHorizontally, int rowNo = 0, int columnPos = 0, 
                      IN(RString)  name = "bar", bool spyEvents = false) 
    { getWx()->AddBar(CLS2WXPTR(pBarWnd), dimInfo->getDimInfo(), alignment, rowNo, columnPos, S2WXS(name), spyEvents, state); }

        // ReddockBar can be used for repositioning existing bars. The given bar is first removed
        // from the pane it currently belongs to, and inserted into the pane, which "matches"
        // the given rectangular area. If pToPane is not NULL, the bar is docked to this given pane.
        // To dock a bar which is floating, use the wxFrameLayout::DockBar method.

    //bool RedockBar( cbBarInfo* pBar, const wxRect& shapeInParent, cbDockPane* pToPane = NULL, bool updateNow = TRUE );
  inline bool redockBar(IN(RBarInfo) pBar, IN(RRect) shapeInParent, IN(RDockPane) pToPane = Nil, bool updateNow = true) { return getWx()->RedockBar(CLS2WXPTR(pBar), CLS2WXREF(shapeInParent), CLS2WXPTR(pToPane), updateNow); }

        // Finds the bar in the framelayout, by name.

    //cbBarInfo* FindBarByName( const wxString& name );
  inline RBarInfo findBarByName(IN(RString)  name) { RETURN_WXPTR2CLS(BarInfo, getWx()->FindBarByName(S2WXS(name))); }

        // Finds the bar in the framelayout, by window.

    //cbBarInfo* FindBarByWindow( const wxWindow* pWnd );
  inline RBarInfo findBarByWindow(IN(RWindow) pWnd) { RETURN_WXPTR2CLS(BarInfo, getWx()->FindBarByWindow(CLS2WXPTR(pWnd))); }

        // Gets an array of bars.

    // ### TODO BarArrayT& GetBars();

        // Changes the bar's docking state (see possible control bar states).

    //void SetBarState( cbBarInfo* pBar, int newStatem, bool updateNow );
  inline void setBarState(IN(RBarInfo) pBar, int newStatem, bool updateNow) { getWx()->SetBarState(CLS2WXPTR(pBar), newStatem, updateNow); }

        // Toggles the bar between visible and hidden.

    //void InverseVisibility( cbBarInfo* pBar );
  inline void inverseVisibility(IN(RBarInfo) pBar) { getWx()->InverseVisibility(CLS2WXPTR(pBar)); }

        // Reflects changes in bar information structure visually.
        // For example, moves the bar, changes its dimension information,
        // or changes the pane to which it is docked.

    //void ApplyBarProperties( cbBarInfo* pBar );
  inline void applyBarProperties(IN(RBarInfo) pBar) { getWx()->ApplyBarProperties(CLS2WXPTR(pBar)); }

        // Removes the bar from the layout permanently, and hides its corresponding window if present.

    //void RemoveBar( cbBarInfo* pBar );
  inline void removeBar(IN(RBarInfo) pBar) { getWx()->RemoveBar(CLS2WXPTR(pBar)); }

        // Recalculates the layout of panes, and all bars/rows in each pane.

    //void RecalcLayout( bool repositionBarsNow = FALSE );
  inline void recalcLayout(bool repositionBarsNow = false) { getWx()->RecalcLayout(repositionBarsNow); }

        // Returns the client height.

    //int GetClientHeight();
  inline int getClientHeight() { return getWx()->GetClientHeight(); }

        // Returns the client width.

    //int GetClientWidth();
  inline int getClientWidth() { return getWx()->GetClientWidth(); }

        // Returns the client's rectangle.

    //wxRect& GetClientRect()        { return mClntWndBounds;     }
  inline RRect getClientRect() { return WXVAL2CLS(Rect, getWx()->GetClientRect()); }

        // Returns a reference to the updates manager.
        // Note: in future, the updates manager will become a normal plugin.

    // ### TODO cbUpdatesManagerBase& GetUpdatesManager();

        // Destroys the previous manager if any, and sets the new one.

    // ### TODO void SetUpdatesManager( cbUpdatesManagerBase* pUMgr );

        // Gets the pane properties for the given alignment.

    // ### TODO void GetPaneProperties( cbCommonPaneProperties& props, int alignment = FL_ALIGN_TOP );

        // Sets the pane properties for the given alignment.
        // Note: changing properties of panes does not result immediate on-screen update.

    // ### TODO void SetPaneProperties( const cbCommonPaneProperties& props, int paneMask = wxALL_PANES );

        // Sets the margins for the given panes.
        // The margins should go into cbCommonPaneProperties in the future.
        //
        // Note: this method should be called before any custom plugins are attached.

    //void SetMargins( int top, int bottom, int left, int right, int paneMask = wxALL_PANES );
  inline void setMargins(int top, int bottom, int left, int right, int paneMask = wxALL_PANES) { getWx()->SetMargins(top, bottom, left, right, paneMask); }

        // Sets the pane background colour.

    //void SetPaneBackground( const wxColour& colour );
  inline void setPaneBackground(IN(RColour) colour) { getWx()->SetPaneBackground(CLS2WXREF(colour)); }

        // Recalculates layout and performs on-screen update of all panes.

    //void RefreshNow( bool recalcLayout = TRUE );
  inline void refreshNow(bool recalcLayout = true) { getWx()->RefreshNow(recalcLayout); }

  /* ignore, probably
        // Event handler for a size event.

    void OnSize       ( wxSizeEvent&  event );

        // Event handler for a left down button event.

    void OnLButtonDown( wxMouseEvent& event );

        // Event handler for a left doubleclick button event.

    void OnLDblClick  ( wxMouseEvent& event );

        // Event handler for a left button up event.

    void OnLButtonUp  ( wxMouseEvent& event );

        // Event handler for a right button down event.

    void OnRButtonDown( wxMouseEvent& event );

        // Event handler for a right button up event.

    void OnRButtonUp  ( wxMouseEvent& event );

        // Event handler for a mouse move event.

    void OnMouseMove  ( wxMouseEvent& event );

        // This function should be used instead of passing the event to the ProcessEvent method
        // of the top-level plugin directly. This method checks if events are currently
        // captured and ensures that plugin-event is routed correctly.
    
    void FirePluginEvent( cbPluginEvent& event );

        // Captures user input events for the given plugin.
        // Input events are: mouse movement, mouse clicks, keyboard input.

    void CaptureEventsForPlugin ( cbPluginBase* pPlugin );

        // Releases user input events for the given plugin.
        // Input events are: mouse movement, mouse clicks, keyboard input

    void ReleaseEventsFromPlugin( cbPluginBase* pPlugin );

        // Called by plugins; also captures the mouse in the parent frame.

    void CaptureEventsForPane( cbDockPane* toPane );

        // Called by plugins; also releases mouse in the parent frame.

    void ReleaseEventsFromPane( cbDockPane* fromPane );

        // Returns the current top-level plugin (the one that receives events first,
        // except if input events are currently captured by some other plugin).
    */
    // ### TODO cbPluginBase& GetTopPlugin();

        // Hooking custom plugins to frame layout.
        //
        // Note: when hooking one plugin on top of the other,
        // use SetNextHandler or similar methods
        // of wxEvtHandler class to compose the chain of plugins,
        // than pass the left-most handler in this chain to
        // the above methods (assuming that events are delegated
        // from left-most towards right-most handler).
        //
        // This secenario is very inconvenient and "low-level",
        // so use the Add/Push/PopPlugin methods instead.

    // ### TODO void SetTopPlugin( cbPluginBase* pPlugin );

        // Similar to wxWindow's "push/pop-event-handler" methods, execept
        // that the plugin is deleted upon "popping".

    // ### TODO void PushPlugin( cbPluginBase* pPugin );

        // Similar to wxWindow's "push/pop-event-handler" methods, execept
        // that the plugin is deleted upon "popping".

    //void PopPlugin();
  inline void popPlugin() { getWx()->PopPlugin(); }

        // Pop all plugins.
    //void PopAllPlugins();
  inline void popAllPlugins() { getWx()->PopAllPlugins(); }

        // Adds the default plugins. These are cbPaneDrawPlugin, cbRowLayoutPlugin, cbBarDragPlugin,
        // cbAntiflickerPlugin, cbSimpleCustomizePlugin.
        //
        // This method is automatically invoked if no plugins were found upon
        // firing of the first plugin-event, i.e. when wxFrameLayout configures itself.

    //void PushDefaultPlugins();
  inline void pushDefaultPlugins() { getWx()->PushDefaultPlugins(); }

        // An advanced methods for plugin configuration    using their
        // dynamic class information, for example CLASSINFO(pluginClass).

        // First checks if the plugin of the given class is already "hooked up".
        // If not, adds it to the top of the plugins chain.
     // sample: CLASSINFO ( cbAntiflickerPlugin )
    // ### TODO void AddPlugin( wxClassInfo* pPlInfo, int paneMask = wxALL_PANES );

        // First checks if the plugin of the given class is already hooked.
        // If so, removes it, and then inserts it into the chain
        // before the plugin of the class given by pNextPlInfo.
        //
        // Note: this method is handy in some cases where the order
        // of the plugin-chain could be important, for example when one plugin overrides
        // some functionality of another already-hooked plugin,
        // so that the former plugin should be hooked before the one
        // whose functionality is being overridden.

    // ### TODO void AddPluginBefore( wxClassInfo* pNextPlInfo, wxClassInfo* pPlInfo, int paneMask = wxALL_PANES );

        // Checks if the plugin of the given class is hooked, and removes
        // it if found.

    // ### TODO void RemovePlugin( wxClassInfo* pPlInfo );

        // Finds a plugin with the given class, or returns NULL if a plugin of the given
        // class is not hooked.

    // ### TODO cbPluginBase* FindPlugin( wxClassInfo* pPlInfo );

        // Returns true if there is a top plugin.

    //bool HasTopPlugin();
  inline bool hasTopPlugin() { return getWx()->HasTopPlugin(); }
};

ACDK_DECL_CLASS(DynamicToolBar);

/**
  see wxFrameLayout
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/06 13:12:12 $
*/
class ACDK_WX_IDE_PUBLIC DynamicToolBar
: extends acdk::wx::ToolBarBase
{
  ACDK_WITH_METAINFO(DynamicToolBar)
public:
  /// wxDynamicToolBar
  // wxToolBarBase
  ACDK_WX_STD_MEMBERS(DynamicToolBar, ToolBarBase)
  
  DynamicToolBar(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(),
                 int style = BorderNone, int orientation = OrientVertical, int rowsOrColumns = 1)
  : ToolBarBase(new wxDynamicToolBar(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, orientation, rowsOrColumns))
  {}
  // other methods already defined in ToolBarToolBase
};


} // namespace ide
} //namespace wx
} // namespace acdk

#endif //acdk_wx_ide_FrameLayout_h
