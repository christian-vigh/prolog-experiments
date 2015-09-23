unit AmziXLibrary_TLB;

{ Ce fichier contient des déclarations Pascal importées depuis une
  bibliothèque de types. Ce fichier sera modifié à chaque importation
  ou rafraichissement de l'éditeur de bibliothèque de types. Les
  modifications apportées à ce fichier seront perdues au cours du 
  processus de rafraichissement. }

{ AmziXLibrary (bibliothèque) }
{ Version 1.0 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_AmziXLibrary: TGUID = '{B8828B00-8E95-11D2-B4F1-00805F5D29BA}';

const

{ TxDragMode }

  dmManual = 0;
  dmAutomatic = 1;

{ TxMouseButton }

  mbLeft = 0;
  mbRight = 1;
  mbMiddle = 2;

const

{ GUID de classe de composants }
  Class_AmziX: TGUID = '{B8828B03-8E95-11D2-B4F1-00805F5D29BA}';

type

{ Déclarations Forward : Interfaces }
  IAmziX = interface;
  IAmziXDisp = dispinterface;
  IAmziXEvents = dispinterface;

{ Déclarations Forward : CoClasses }
  AmziX = IAmziX;

{ Déclarations Forward : Enums }
  TxDragMode = TOleEnum;
  TxMouseButton = TOleEnum;

{ Interface dispatch pour AmziX Contrôle }

  IAmziX = interface(IDispatch)
    ['{B8828B01-8E95-11D2-B4F1-00805F5D29BA}']
    procedure Click; safecall;
    function Get_Cancel: WordBool; safecall;
    procedure Set_Cancel(Value: WordBool); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Default: WordBool; safecall;
    procedure Set_Default(Value: WordBool); safecall;
    function Get_DragCursor: Smallint; safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    function Get_DragMode: TxDragMode; safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Font: Font; safecall;
    procedure Set_Font(const Value: Font); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure AboutBox; safecall;
    procedure XInitLS(const xplname: WideString); safecall;
    procedure XAddLSX(const lsxname: WideString); safecall;
    procedure XReset; safecall;
    procedure XCloseLS; safecall;
    procedure XLoadXPL(const xplname: WideString); safecall;
    function XGetStrArg(n: Integer): WideString; safecall;
    function XExecStr(const s: WideString): WordBool; safecall;
    procedure XClearCall; safecall;
    function XRedo: WordBool; safecall;
    procedure XAssertaStr(const s: WideString); safecall;
    procedure XAssertzStr(const s: WideString); safecall;
    procedure XRetractStr(const s: WideString); safecall;
    function XCallStr(const s: WideString): WordBool; safecall;
    property Cancel: WordBool read Get_Cancel write Set_Cancel;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Default: WordBool read Get_Default write Set_Default;
    property DragCursor: Smallint read Get_DragCursor write Set_DragCursor;
    property DragMode: TxDragMode read Get_DragMode write Set_DragMode;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Font: Font read Get_Font write Set_Font;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
  end;

{ Déclaration DispInterface  pour interface Dual IAmziX }

  IAmziXDisp = dispinterface
    ['{B8828B01-8E95-11D2-B4F1-00805F5D29BA}']
    procedure Click; dispid 1;
    property Cancel: WordBool dispid 2;
    property Caption: WideString dispid 3;
    property Default: WordBool dispid 4;
    property DragCursor: Smallint dispid 5;
    property DragMode: TxDragMode dispid 6;
    property Enabled: WordBool dispid 7;
    property Font: Font dispid 8;
    property Visible: WordBool dispid 9;
    property Cursor: Smallint dispid 10;
    procedure AboutBox; dispid -552;
    procedure XInitLS(const xplname: WideString); dispid 11;
    procedure XAddLSX(const lsxname: WideString); dispid 13;
    procedure XReset; dispid 15;
    procedure XCloseLS; dispid 16;
    procedure XLoadXPL(const xplname: WideString); dispid 17;
    function XGetStrArg(n: Integer): WideString; dispid 19;
    function XExecStr(const s: WideString): WordBool; dispid 20;
    procedure XClearCall; dispid 22;
    function XRedo: WordBool; dispid 23;
    procedure XAssertaStr(const s: WideString); dispid 26;
    procedure XAssertzStr(const s: WideString); dispid 27;
    procedure XRetractStr(const s: WideString); dispid 28;
    function XCallStr(const s: WideString): WordBool; dispid 12;
  end;

{ Interface d'événements pour AmziX.Contrôle }

  IAmziXEvents = dispinterface
    ['{B8828B02-8E95-11D2-B4F1-00805F5D29BA}']
    procedure OnClick; dispid 1;
    procedure OnKeyPress(var Key: Smallint); dispid 2;
  end;

{ AmziXContrôle }

  TAmziXOnKeyPress = procedure(Sender: TObject; var Key: Smallint) of object;

  TAmziX = class(TOleControl)
  private
    FOnClick: TNotifyEvent;
    FOnKeyPress: TAmziXOnKeyPress;
    FIntf: IAmziX;
  protected
    procedure InitControlData; override;
    procedure InitControlInterface(const Obj: IUnknown); override;
  public
    procedure Click;
    procedure AboutBox;
    procedure XInitLS(const xplname: WideString);
    procedure XAddLSX(const lsxname: WideString);
    procedure XReset;
    procedure XCloseLS;
    procedure XLoadXPL(const xplname: WideString);
    function XGetStrArg(n: Integer): WideString;
    function XExecStr(const s: WideString): WordBool;
    procedure XClearCall;
    function XRedo: WordBool;
    procedure XAssertaStr(const s: WideString);
    procedure XAssertzStr(const s: WideString);
    procedure XRetractStr(const s: WideString);
    function XCallStr(const s: WideString): WordBool;
    property ControlInterface: IAmziX read FIntf;
  published
    property TabStop;
    property Align;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property Cancel: WordBool index 2 read GetWordBoolProp write SetWordBoolProp stored False;
    property Caption: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property Default: WordBool index 4 read GetWordBoolProp write SetWordBoolProp stored False;
    property DragCursor: Smallint index 5 read GetSmallintProp write SetSmallintProp stored False;
    property DragMode: TxDragMode index 6 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index 7 read GetWordBoolProp write SetWordBoolProp stored False;
    property Font: TFont index 8 read GetTFontProp write SetTFontProp stored False;
    property Visible: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 10 read GetSmallintProp write SetSmallintProp stored False;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnKeyPress: TAmziXOnKeyPress read FOnKeyPress write FOnKeyPress;
  end;

procedure Register;

implementation

uses ComObj;

procedure TAmziX.InitControlData;
const
  CEventDispIDs: array[0..1] of Integer = (
    $00000001, $00000002);
  CTFontIDs: array [0..0] of Integer = (
    $00000008);
  CControlData: TControlData = (
    ClassID: '{B8828B03-8E95-11D2-B4F1-00805F5D29BA}';
    EventIID: '{B8828B02-8E95-11D2-B4F1-00805F5D29BA}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
end;

procedure TAmziX.InitControlInterface(const Obj: IUnknown);
begin
  FIntf := Obj as IAmziX;
end;

procedure TAmziX.Click;
begin
  ControlInterface.Click;
end;

procedure TAmziX.AboutBox;
begin
  ControlInterface.AboutBox;
end;

procedure TAmziX.XInitLS(const xplname: WideString);
begin
  ControlInterface.XInitLS(xplname);
end;

procedure TAmziX.XAddLSX(const lsxname: WideString);
begin
  ControlInterface.XAddLSX(lsxname);
end;

procedure TAmziX.XReset;
begin
  ControlInterface.XReset;
end;

procedure TAmziX.XCloseLS;
begin
  ControlInterface.XCloseLS;
end;

procedure TAmziX.XLoadXPL(const xplname: WideString);
begin
  ControlInterface.XLoadXPL(xplname);
end;

function TAmziX.XGetStrArg(n: Integer): WideString;
begin
  Result := ControlInterface.XGetStrArg(n);
end;

function TAmziX.XExecStr(const s: WideString): WordBool;
begin
  Result := ControlInterface.XExecStr(s);
end;

procedure TAmziX.XClearCall;
begin
  ControlInterface.XClearCall;
end;

function TAmziX.XRedo: WordBool;
begin
  Result := ControlInterface.XRedo;
end;

procedure TAmziX.XAssertaStr(const s: WideString);
begin
  ControlInterface.XAssertaStr(s);
end;

procedure TAmziX.XAssertzStr(const s: WideString);
begin
  ControlInterface.XAssertzStr(s);
end;

procedure TAmziX.XRetractStr(const s: WideString);
begin
  ControlInterface.XRetractStr(s);
end;

function TAmziX.XCallStr(const s: WideString): WordBool;
begin
  Result := ControlInterface.XCallStr(s);
end;


procedure Register;
begin
  RegisterComponents('ActiveX', [TAmziX]);
end;

end.
