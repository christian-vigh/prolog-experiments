unit AmziXImpl;

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, AmziXLibrary_TLB, Amzi4, Dialogs, SysUtils;

type
  TAmziX = class(TActiveXControl, IAmziX)
  private
    { Déclarations privées }
    FDelphiControl: TButton;
    FEvents: IAmziXEvents;
    LSEngine : TLSEngine;
    MyTp : TTerm;
    procedure ClickEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
  protected
    { Déclarations protégées }
    procedure InitializeControl; override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    function Get_Cancel: WordBool; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_Default: WordBool; safecall;
    function Get_DragCursor: Smallint; safecall;
    function Get_DragMode: TxDragMode; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: Font; safecall;
    function Get_Visible: WordBool; safecall;
    procedure AboutBox; safecall;
    procedure Click; safecall;
    procedure Set_Cancel(Value: WordBool); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_Default(Value: WordBool); safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: Font); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure XInitLS(const xplname: WideString); safecall;
    procedure XAddLSX(const lsxname: WideString); safecall;
    procedure XReset; safecall;
    procedure XCloseLS; safecall;
    procedure XLoadXPL(const xplname: WideString); safecall;
    function XGetStrArg(n: Integer): WideString; safecall;
    function XExecStr(const s: WideString): WordBool; safecall;
    function XCallStr(const s: WideString): WordBool; safecall;
    procedure XClearCall; safecall;
    function XRedo: WordBool; safecall;
    procedure XAssertaStr(const s: WideString); safecall;
    procedure XAssertzStr(const s: WideString); safecall;
    procedure XRetractStr(const s: WideString); safecall;
  end;

implementation

uses About1;

{ TAmziX }

procedure TAmziX.InitializeControl;
begin
  FDelphiControl := Control as TButton;
  FDelphiControl.OnClick := ClickEvent;
  FDelphiControl.OnKeyPress := KeyPressEvent;
  LSEngine := TLSEngine.Create(nil);
end;

procedure TAmziX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IAmziXEvents;
end;

procedure TAmziX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Définissez les pages de propriété ici. Celle-ci sont définies en appelant
    DefinePropertyPage avec l'id de classe de la page.  Par exemple,
      DefinePropertyPage(Class_AmziXPage); }
end;

function TAmziX.Get_Cancel: WordBool;
begin
  Result := FDelphiControl.Cancel;
end;

function TAmziX.Get_Caption: WideString;
begin
  Result := WideString(FDelphiControl.Caption);
end;

function TAmziX.Get_Cursor: Smallint;
begin
  Result := Smallint(FDelphiControl.Cursor);
end;

function TAmziX.Get_Default: WordBool;
begin
  Result := FDelphiControl.Default;
end;

function TAmziX.Get_DragCursor: Smallint;
begin
  Result := Smallint(FDelphiControl.DragCursor);
end;

function TAmziX.Get_DragMode: TxDragMode;
begin
  Result := Ord(FDelphiControl.DragMode);
end;

function TAmziX.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TAmziX.Get_Font: Font;
begin
  GetOleFont(FDelphiControl.Font, Result);
end;

function TAmziX.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

procedure TAmziX.AboutBox;
begin
  ShowAmziXAbout;
end;

procedure TAmziX.Click;
begin
  FDelphiControl.Click;
end;

procedure TAmziX.Set_Cancel(Value: WordBool);
begin
  FDelphiControl.Cancel := Value;
end;

procedure TAmziX.Set_Caption(const Value: WideString);
begin
  FDelphiControl.Caption := TCaption(Value);
end;

procedure TAmziX.Set_Cursor(Value: Smallint);
begin
  FDelphiControl.Cursor := TCursor(Value);
end;

procedure TAmziX.Set_Default(Value: WordBool);
begin
  FDelphiControl.Default := Value;
end;

procedure TAmziX.Set_DragCursor(Value: Smallint);
begin
  FDelphiControl.DragCursor := TCursor(Value);
end;

procedure TAmziX.Set_DragMode(Value: TxDragMode);
begin
  FDelphiControl.DragMode := TDragMode(Value);
end;

procedure TAmziX.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TAmziX.Set_Font(const Value: Font);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TAmziX.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TAmziX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TAmziX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TAmziX.XInitLS(const xplname: WideString);
begin
 LSEngine.InitLS(xplname);
end;

procedure TAmziX.XAddLSX(const lsxname: WideString);
begin
 LSEngine.AddLSX(lsxname);
end;

procedure TAmziX.XReset;
begin
 LSEngine.Reset;
end;

procedure TAmziX.XCloseLS;
begin
 LSEngine.CloseLS;
end;

procedure TAmziX.XLoadXPL(const xplname: WideString);
begin
 LSEngine.LoadXPL(xplname);
end;

function TAmziX.XGetStrArg(n: Integer): WideString;
begin
 Result:=LSEngine.GetPStrArg(MyTp,n);
end;

function TAmziX.XExecStr(const s: WideString): WordBool;
begin
 Result:=LSEngine.ExecPStr(MyTp,s);
end;

function TAmziX.XCallStr(const s: WideString): WordBool;
begin
 Result:=LSEngine.CallPStr(MyTp,s);
end;

procedure TAmziX.XClearCall;
begin
 LSEngine.ClearCall;
end;

function TAmziX.XRedo: WordBool;
begin
 Result:=LSEngine.Redo;
end;

procedure TAmziX.XAssertaStr(const s: WideString);
begin
 LSEngine.AssertaPStr(s);
end;

procedure TAmziX.XAssertzStr(const s: WideString);
begin
 LSEngine.AssertzPStr(s);
end;

procedure TAmziX.XRetractStr(const s: WideString);
begin
 LSEngine.RetractPStr(s);
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TAmziX,
    TButton,
    Class_AmziX,
    1,
    '',
    0);
end.
