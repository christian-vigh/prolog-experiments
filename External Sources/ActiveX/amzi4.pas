unit Amzi4;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

const
  lsfalse: integer = 0;
  lstrue:  integer = 1;

type
  ELogicServer = class(Exception);

  { Various types used by the Logic Server API calls }
  TTerm = pointer;  { The basic Prolog term }
  { Enumerated Prolog types and enumerated Delphi types, used for mapping
    Prolog types to Delphi types }
  TPType = (pATOM, pINT, pSTR, pFLOAT, pSTRUCT, pLIST, pTERM, pADDR, pVAR);
  TDType = (dATOM, dSTR, dINT, dLONG, dSHORT, dFLOAT, dDOUBLE, dADDR, dTERM);
  TTypeInt = integer; { Generic type for casting types in DLL calls }
  { Enumerated stream identifier, used when redirecting Prolog I/O }
  TPStream = (CUR_IN, CUR_OUT, CUR_ERR, USER_IN, USER_OUT, USER_ERR);
  TPStreamInt = integer; { Generic type for stream identifiers in DLL calls}
  TTFi = integer;  { Prolog T/F or error code return code }
  TRC = integer;  { Integer return code }
  TArity = Word;  { The arity of a functor }
  TEngID = longint;  { ID for Engine, only one allowed now }
  TExtPred = function(EngID: TEngID): TTFi; stdcall; { An extended predicate function }
  TPutC = procedure(c: Integer);
  TPutS = procedure(s: PChar);
  TGetC = function: Integer;
  TUngetC = procedure;
  TPredInit = record
    Pname: PChar;
    Parity: TArity;
    Pfunc: TExtPred;
  end;
  TPredInitPtr = ^TPredInit;

  { The Logic Server component, a class that encapsulates all of the API
    calls as methods }
  TLSEngine = class(TComponent)
  private
    eng: TEngID;
    rc: TRC;
    initializedB, createdB: BOOL;
    buf: array[0..255] of char;
    procedure LSError(apiname: String; rc: integer);
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Main entry points to set up Prolog environment }
    procedure Init(xplname: String);
    procedure InitLS(xplname: String);
    procedure InitLSXP(p: pointer);
    procedure InitLSX;
    procedure AddLSX(lsxname: String);
    procedure AddPred(pname: String; parity: TArity; pfunc: TExtPred);
    procedure InitPreds(PIptr: TPredInitPtr);
    procedure Load(xplname: String);
    procedure LoadXPL(xplname: String);
    function Main: Boolean;
    procedure Reset;
    procedure Close;
    procedure CloseLS;
    { Function and predicate parameters }
    procedure GetParm(n: integer; dt: TDType; p: pointer);
    function GetPStrParm(n: integer): string;
    function GetIntParm(n: integer): integer;
    function GetLongParm(n: integer): longint;
    function GetShortParm(n: integer): longint;
    function GetFloatParm(n: integer): double;
    function GetParmType(n: integer): TPType;
    function StrParmLen(n: integer): integer;
    function UnifyParm(n: integer; dt: TDType; p: pointer): Boolean;
    function UnifyPStrParm(n: integer; s: string): Boolean;
    function UnifyAtomParm(n: integer; s: string): Boolean;
    function UnifyIntParm(n: integer; i: integer): Boolean;
    function UnifyLongParm(n: integer; i: longint): Boolean;
    function UnifyShortParm(n: integer; i: longint): Boolean;
    function UnifyFloatParm(n: integer; f: double): Boolean;
    { Calling Prolog from Delphi }
    function Exec(var tp: TTerm): Boolean;
    function ExecStr(var tp: TTerm; s: PChar): Boolean;
    function ExecPStr(var tp: TTerm; s: string): Boolean;
    function Call(var tp: TTerm): Boolean;
    function CallStr(var tp: TTerm; s: PChar): Boolean;
    function CallPStr(var tp: TTerm; s: string): Boolean;
    function Redo: Boolean;
    procedure ClearCall;
    { Asserting and retracting }
    procedure Asserta(t: TTerm);
    procedure Assertz(t: TTerm);
    procedure Retract(t: TTerm);
    procedure AssertaStr(s: PChar);
    procedure AssertzStr(s: PChar);
    procedure RetractStr(s: PChar);
    procedure AssertaPStr(s: string);
    procedure AssertzPStr(s: string);
    procedure RetractPStr(s: string);
    { String/term conversion functions }
    procedure TermToStr(t: TTerm; s: PChar; n: integer);
    procedure TermToStrQ(t: TTerm; s: PChar; n: integer);
    procedure StrToTerm(var tp: TTerm; s: PChar);
    function TermToPStr(t: TTerm): string;
    function TermToPStrQ(t: TTerm): string;
    procedure PStrToTerm(var tp: TTerm; s: string);
    function StrTermLen(t: TTerm): integer;
     { Making Prolog types }
    procedure MakeAtom(var tp: TTerm; s: string);
    procedure MakeStr(var tp: TTerm; s: PChar);
    procedure MakePStr(var tp: TTerm; s: string);
    procedure MakeInt(var tp: TTerm; i: longint);
    procedure MakeFloat(var tp: TTerm; f: double);
    procedure MakeAddr(var tp: TTerm; p: pointer);
    { Getting C values from Prolog terms }
    function GetTermType(t: TTerm): TPType;
    procedure GetTerm(t: TTerm; dt: TDType; p: pointer);
    function GetPStrTerm(t: TTerm): string;
    function GetIntTerm(t: TTerm): integer;
    function GetLongTerm(t: TTerm): longint;
    function GetShortTerm(t: TTerm): longint;
    function GetFloatTerm(t: TTerm): double;
    { Structure hacking functions }
    procedure GetFA(t: TTerm; var s: string; var ap: TArity);
    function GetFunctor(t: TTerm): string;
    function GetArity(t: TTerm): integer;
    procedure MakeFA(var tp: TTerm; s: string; a: TArity);
    function UnifyArg(var tp: TTerm; n: integer; dt: TDType; p: pointer): Boolean;
    function UnifyPStrArg(var tp: TTerm; n: integer; s: string): Boolean;
    function UnifyAtomArg(var tp: TTerm; n: integer; s: string): Boolean;
    function UnifyIntArg(var tp: TTerm; n: integer; i: integer): Boolean;
    function UnifyLongArg(var tp: TTerm; n: integer; i: longint): Boolean;
    function UnifyShortArg(var tp: TTerm; n: integer; i: longint): Boolean;
    function UnifyFloatArg(var tp: TTerm; n: integer; f: double): Boolean;
    procedure GetArg(t: TTerm; n: integer; dt: TDType; p: pointer);
    function GetPStrArg(t: TTerm; n: integer): string;
    function GetIntArg(t: TTerm; n: integer): integer;
    function GetLongArg(t: TTerm; n: integer): longint;
    function GetShortArg(t: TTerm; n: integer): longint;
    function GetFloatArg(t: TTerm; n: integer): double;
    function GetArgType(t: TTerm; n: integer): TPType;
    function StrArgLen(t: TTerm; i: integer): integer;
    function Unify(t1: TTerm; t2: TTerm): Boolean;
    { List hacking functions }
    procedure MakeList(var tp: TTerm);
    procedure PushList(var tp: TTerm; t: TTerm);
    function PopList(var tp: TTerm; dt: TDType; p: pointer): TRC;
    function PopPStrList(var tp: TTerm; var s: string): TRC;
    function PopIntList(var tp: TTerm; var i: integer): TRC;
    function PopLongList(var tp: TTerm; var i: longint): TRC;
    function PopShortList(var tp: TTerm; var i: longint): TRC;
    function PopFloatList(var tp: TTerm; var f: double): TRC;
    function GetHead(t: TTerm; dt: TDType; p: pointer): TRC;
    function GetPStrHead(t: TTerm; var s: string): TRC;
    function GetIntHead(t: TTerm; var i: integer): TRC;
    function GetLongHead(t: TTerm; var i: longint): TRC;
    function GetShortHead(t: TTerm; var i: longint): TRC;
    function GetFloatHead(t: TTerm; var f: double): TRC;
    function GetTail(t: TTerm): TTerm;
    { Stream I/O functions }
    procedure SetStream(st: TPStream; i: integer);
    function GetStream(st: TPStream): integer;
    procedure SetInput(pfunc1: TGetC; pfunc2: TUngetC);
    procedure SetOutput(pfunc1: TPutC; pfunc2: TPutS);
    { Miscellaneous functions }
    procedure GetVersion(var s: string);
    function GetPVersion: string;
    { Error handling functions }
    function GetExceptRC: TRC;
    procedure GetExceptMsg(s: PChar; l: integer);
    procedure GetExceptReadBuffer(s: PChar; l:integer);
    procedure GetExceptCallStack(s: PChar; l: integer);
  published
  end;

procedure Register;

implementation

{ Defines the actual DLL entry points for the Logic Server API.
  See the file AMZI.H for the complete C header file definition. }

{ Main entry points to set up Prolog environment }
function lsInitA(var eng: TEngID; xplname: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsInit2A(var eng: TEngID; xplname: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsInitLSX(eng: TEngID; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsAddLSXA(eng: TEngID; lsxname: PChar; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsAddPredA(eng: TEngID; pname: PChar; parity: TArity; pfunc: TExtPred; ptr: Pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsInitPredsA(eng: TEngID; PIptr: TPredInitPtr): TRC; stdcall; external 'AMZI4.DLL';
function lsLoadA(eng: TEngID; xplname: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsMain(eng: TEngID): TTFi; stdcall; external 'AMZI4.DLL';
function lsReset(eng: TEngID): TRC; stdcall; external 'AMZI4.DLL';
function lsClose(eng: TEngID): TRC; stdcall; external 'AMZI4.DLL';
{ Function and predicate parameters }
function lsGetParm(eng: TEngID; n: integer; dt: TTypeInt; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsGetParmType(eng: TEngID; n: integer): TTypeInt; stdcall; external 'AMZI4.DLL';
function lsStrParmLen(eng: TEngID; n: integer): integer; stdcall; external 'AMZI4.DLL';
function lsUnifyParm(eng: TEngID; n: integer; dt: TTypeInt; p: pointer): TTFi; stdcall; external 'AMZI4.DLL';
{ Calling Prolog from Delphi }
function lsExec(eng: TEngID; var tp: TTerm): TTFi; stdcall; external 'AMZI4.DLL';
function lsExecStrA(eng: TEngID; var tp: TTerm; s: PChar): TTFi; stdcall; external 'AMZI4.DLL';
function lsCall(eng: TEngID; var tp: TTerm): TTFi; stdcall; external 'AMZI4.DLL';
function lsCallStrA(eng: TEngID; var tp: TTerm; s: PChar): TTFi; stdcall; external 'AMZI4.DLL';
function lsRedo(eng: TEngID): TTFi; stdcall; external 'AMZI4.DLL';
function lsClearCall(eng: TEngID): TRC; stdcall; external 'AMZI4.DLL';
{ Asserting and retracting }
function lsAsserta(eng: TEngID; t: TTerm): TRC; stdcall; external 'AMZI4.DLL';
function lsAssertz(eng: TEngID; t: TTerm): TRC; stdcall; external 'AMZI4.DLL';
function lsRetract(eng: TEngID; t: TTerm): TRC; stdcall; external 'AMZI4.DLL';
function lsAssertaStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsAssertzStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsRetractStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
{ String/term conversion functions }
function lsTermToStrA(eng: TEngID; t: TTerm; s: PChar; n: integer): TRC; stdcall; external 'AMZI4.DLL';
function lsTermToStrQA(eng: TEngID; t: TTerm; s: PChar; n: integer): TRC; stdcall; external 'AMZI4.DLL';
function lsStrToTermA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
{ Making Prolog types }
function lsMakeAtomA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsMakeStrA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
function lsMakeInt(eng: TEngID; var tp: TTerm; i: longint): TRC; stdcall; external 'AMZI4.DLL';
function lsMakeFloat(eng: TEngID; var tp: TTerm; f: double): TRC; stdcall; external 'AMZI4.DLL';
function lsMakeAddr(eng: TEngID; var tp: TTerm; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
{ Getting C values from Prolog terms }
function lsGetTermType(eng: TEngID; t: TTerm): TTypeInt; stdcall; external 'AMZI4.DLL';
function lsGetTerm(eng: TEngID; t: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsStrTermLen(eng: TEngID; t: TTerm): integer; stdcall; external 'AMZI4.DLL';
{ Structure hacking functions }
function lsGetFAA(eng: TEngID; t: TTerm; s: PChar; var ap: TArity): TRC; stdcall; external 'AMZI4.DLL';
function lsMakeFAA(eng: TEngID; var tp: TTerm; s: PChar; a: TArity): TRC; stdcall; external 'AMZI4.DLL';
function lsUnifyArg(eng: TEngID; var tp: TTerm; n: integer; dt: TTypeInt; p: pointer): TTFi; stdcall; external 'AMZI4.DLL';
function lsGetArg(eng: TEngID; t: TTerm; n: integer; dt: TTypeInt; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsGetArgType(eng: TEngID; t: TTerm; n: integer): TTypeInt; stdcall; external 'AMZI4.DLL';
function lsStrArgLen(eng: TEngID; t: TTerm; i: integer): integer; stdcall; external 'AMZI4.DLL';
function lsUnify(eng: TEngID; t1: TTerm; t2: TTerm): TTFi; stdcall; external 'AMZI4.DLL';
{ List hacking functions }
function lsMakeList(eng: TEngID; var tp: TTerm): TRC; stdcall; external 'AMZI4.DLL';
function lsPushList(eng: TEngID; var tp: TTerm; t: TTerm): TRC; stdcall; external 'AMZI4.DLL';
function lsPopList(eng: TEngID; var tp: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsGetHead(eng: TEngID; t: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'AMZI4.DLL';
function lsGetTail(eng: TEngID; t: TTerm): TTerm; stdcall; external 'AMZI4.DLL';
{ Stream I/O functions }
function lsSetStream(eng: TEngID; st: TPStreamInt; i: integer): TRC; stdcall; external 'AMZI4.DLL';
function lsGetStream(eng: TEngID; st: TPStreamInt): integer; stdcall; external 'AMZI4.DLL';
function lsSetInput(eng: TEngID; pfunc1: TGetC; pfunc2: TUngetC): TRC; stdcall; external 'AMZI4.DLL';
function lsSetOutputA(eng: TEngID; pfunc1: TPutC; pfunc2: TPutS): TRC; stdcall; external 'AMZI4.DLL';
{ Miscellaneous functions }
function lsGetVersionA(eng: TEngID; s: PChar): TRC; stdcall; external 'AMZI4.DLL';
{ Error handling functions }
function lsGetExceptRC(eng: TEngID): TRC; stdcall; external 'AMZI4.DLL';
procedure lsGetExceptMsgA(eng: TEngID; s: PChar; l: integer) stdcall; external 'AMZI4.DLL';
procedure lsGetExceptReadBufferA(eng: TEngID; s: PChar; l: integer) stdcall; external 'AMZI4.DLL';
procedure lsGetExceptCallStackA(eng: TEngID; s: PChar; l: integer) stdcall; external 'AMZI4.DLL';

constructor TLSEngine.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  createdB := true;
  initializedB := false;
end;

destructor TLSEngine.Destroy;
begin
  if initializedB then lsClose(eng);
  inherited Destroy;
end;

{ The function definitions map Logic Server methods to the
  actual DLL entry points. }

{ Main entry points to set up Prolog environment }

procedure TLSEngine.Init(xplname: string);
begin
  InitLS(xplname);
end;

procedure TLSEngine.InitLS(xplname: String);
begin
  if not createdB then LSError('LS not created', 0);
  if initializedB then lsClose(eng);
  StrPCopy(buf, xplname);
  rc := lsInitA(eng, buf);
  if rc <> 0 then LSError('lsInit', rc);
  initializedB := true;
end;

procedure TLSEngine.InitLSX;
begin
  rc := lsInitLSX(eng, nil);
  if rc<> 0 then LSError('lsInitLSX', rc);
end;

procedure TLSEngine.InitLSXP(p: pointer);
begin
  rc := lsInitLSX(eng, p);
  if rc<> 0 then LSError('lsInitLSX', rc);
end;

procedure TLSEngine.AddLSX(lsxname: string);
begin
  StrPCopy(buf, lsxname);
  rc := lsAddLSXA(eng, buf, nil);
  if rc <> 0 then LSError('lsAddLSX', rc);
end;

procedure TLSEngine.AddPred(pname: string; parity: TArity; pfunc: TExtPred);
begin
  StrPCopy(buf, pname);
  rc := lsAddPredA(eng, buf, parity, pfunc, Pointer(eng));
  if rc <> 0 then LSError('lsAddPred', rc);
end;

procedure TLSEngine.InitPreds(PIptr: TPredInitPtr);
begin
  rc := lsInitPredsA(eng, PIptr);
  if rc <> 0 then LSError('lsInitPreds', rc);
end;

procedure TLSEngine.Load(xplname: string);
begin
  LoadXPL(xplname);
end;

procedure TLSEngine.LoadXPL(xplname: string);
begin
  StrPCopy(buf, xplname);
  rc := lsLoadA(eng, buf);
  if rc <> 0 then LSError('lsLoad', rc);
end;

function TLSEngine.Main: Boolean;
begin
  rc := lsMain(eng);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsMain', rc);
  end;
end;

procedure TLSEngine.Reset;
begin
  rc := lsReset(eng);
  if rc <> 0 then LSError('lsReset', rc);
end;

procedure TLSEngine.Close;
begin
  CloseLS;
end;

procedure TLSEngine.CloseLS;
begin
  rc := lsClose(eng);
  if rc <> 0 then LSError('lsClose', rc);
  initializedB := false;
end;

{ Function and predicate parameters }

procedure TLSEngine.GetParm(n: integer; dt: TDType; p: pointer);
begin
  rc := lsGetParm(eng, n, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetParm', rc);
end;

function TLSEngine.GetPStrParm(n: integer): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetParm(eng, n, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := StrPas(buf);
end;

function TLSEngine.GetIntParm(n: integer): integer;
var
  i: integer;
begin
  rc := lsGetParm(eng, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSEngine.GetLongParm(n: integer): longint;
var
  i: longint;
begin
  rc := lsGetParm(eng, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSEngine.GetShortParm(n: integer): longint;
var
  i: longint;
begin
  rc := lsGetParm(eng, n, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSEngine.GetFloatParm(n: integer): double;
var
  f: double;
begin
  rc := lsGetParm(eng, n, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := f;
end;

function TLSEngine.GetParmType(n: integer): TPType;
begin
  Result := TPType(lsGetParmType(eng, n));
end;

function TLSEngine.StrParmLen(n: integer): integer;
begin
  Result := lsStrParmLen(eng, n);
end;

function TLSEngine.UnifyParm(n: integer; dt: TDType; p: pointer): Boolean;
begin
  rc := lsUnifyParm(eng, n, TTypeInt(dt), p);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyPStrParm(n: integer; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsUnifyParm(eng, n, TTypeInt(dSTR), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyAtomParm(n: integer; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsUnifyParm(eng, n, TTypeInt(dATOM), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyIntParm(n: integer; i: integer): Boolean;
begin
  rc := lsUnifyParm(eng, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyLongParm(n: integer; i: longint): Boolean;
begin
  rc := lsUnifyParm(eng, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyShortParm(n: integer; i: longint): Boolean;
begin
  rc := lsUnifyParm(eng, n, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSEngine.UnifyFloatParm(n: integer; f: double): Boolean;
begin
  rc := lsUnifyParm(eng, n, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;


{ Calling Prolog from Delphi }

function TLSEngine.Exec(var tp: TTerm): Boolean;
begin
  rc := lsExec(eng, tp);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExec', rc);
  end;
end;

function TLSEngine.ExecStr(var tp: TTerm; s: PChar): Boolean;
begin
  rc := lsExecStrA(eng, tp, s);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', rc);
  end;
end;

function TLSEngine.ExecPStr(var tp: TTerm; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsExecStrA(eng, tp, buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', rc);
  end;
end;

function TLSEngine.Call(var tp: TTerm): Boolean;
begin
  rc := lsCall(eng, tp);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCall', rc);
  end;
end;

function TLSEngine.CallStr(var tp: TTerm; s: PChar): Boolean;
begin
  rc := lsCallStrA(eng, tp, s);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', rc);
  end;
end;

function TLSEngine.CallPStr(var tp: TTerm; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsCallStrA(eng, tp, buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', rc);
  end;
end;

function TLSEngine.Redo: Boolean;
begin
  rc := lsRedo(eng);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsRedo', rc);
  end;
end;

procedure TLSEngine.ClearCall;
begin
  rc := lsClearCall(eng);
  if rc <> 0 then LSError('lsClearCall', rc);
end;

{ Asserting and retracting }

procedure TLSEngine.Asserta(t: TTerm);
begin
  rc := lsAsserta(eng, t);
  if rc <> 0 then LSError('lsAsserta', rc);
end;

procedure TLSEngine.Assertz(t: TTerm);
begin
  rc := lsAssertz(eng, t);
  if rc <> 0 then LSError('lsAssertz', rc);
end;

procedure TLSEngine.Retract(t: TTerm);
begin
  rc := lsRetract(eng, t);
  if rc <> 0 then LSError('lsRetract', rc);
end;

procedure TLSEngine.AssertaStr(s: PChar);
begin
  rc := lsAssertaStrA(eng, s);
  if rc <> 0 then LSError('lsAssertaStr', rc);
end;

procedure TLSEngine.AssertzStr(s: PChar);
begin
  rc := lsAssertzStrA(eng, s);
  if rc <> 0 then LSError('lsAssertzStr', rc);
end;

procedure TLSEngine.RetractStr(s: PChar);
begin
  rc := lsRetractStrA(eng, s);
  if rc <> 0 then LSError('lsRetractStr', rc);
end;

procedure TLSEngine.AssertaPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsAssertaStrA(eng, buf);
  if rc <> 0 then LSError('lsAssertaStr', rc);
end;

procedure TLSEngine.AssertzPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsAssertzStrA(eng, buf);
  if rc <> 0 then LSError('lsAssertzStr', rc);
end;

procedure TLSEngine.RetractPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsRetractStrA(eng, buf);
  if rc <> 0 then LSError('lsRetractStr', rc);
end;

{ String/term conversion functions }

procedure TLSEngine.TermToStr(t: TTerm; s: PChar; n: integer);
begin
  rc := lsTermToStrA(eng, t, s, n);
  if rc <> 0 then LSError('lsTermToStr', rc);
end;

procedure TLSEngine.TermToStrQ(t: TTerm; s: PChar; n: integer);
begin
  rc := lsTermToStrQA(eng, t, s, n);
  if rc <> 0 then LSError('lsTermToStrQ', rc);
end;

procedure TLSEngine.StrToTerm(var tp: TTerm; s: PChar);
begin
  rc := lsStrToTermA(eng, tp, s);
  if rc <> 0 then LSError('lsStrToTerm', rc);
end;

function TLSEngine.TermToPStr(t: TTerm): string;
begin
  rc := lsTermToStrA(eng, t, buf, 255);
  if rc <> 0 then LSError('lsTermToStr', rc);
  Result := strpas(buf);
end;

function TLSEngine.TermToPStrQ(t: TTerm): string;
begin
  rc := lsTermToStrQA(eng, t, buf, 255);
  if rc <> 0 then LSError('lsTermToStrQ', rc);
  Result := strpas(buf);
end;

procedure TLSEngine.PStrToTerm(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsStrToTermA(eng, tp, buf);
  if rc <> 0 then LSError('lsStrToTerm', rc);
end;

function TLSEngine.StrTermLen(t: TTerm): integer;
begin
  Result := lsStrTermLen(eng, t);
end;

{ Making Prolog types }

procedure TLSEngine.MakeAtom(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsMakeAtomA(eng, tp, buf);
  if rc <> 0 then LSError('lsMakeAtom', rc);
end;

procedure TLSEngine.MakeStr(var tp: TTerm; s: PChar);
begin
  rc := lsMakeStrA(eng, tp, s);
  if rc <> 0 then LSError('lsMakeStr', rc);
end;

procedure TLSEngine.MakePStr(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsMakeStrA(eng, tp, buf);
  if rc <> 0 then LSError('lsMakeStr', rc);
end;

procedure TLSEngine.MakeInt(var tp: TTerm; i: longint);
begin
  rc := lsMakeInt(eng, tp, i);
  if rc <> 0 then LSError('lsMakeInt', rc);
end;

procedure TLSEngine.MakeFloat(var tp: TTerm; f: double);
begin
  rc := lsMakeFloat(eng, tp, f);
  if rc <> 0 then LSError('lsMakeFloat', rc);
end;

procedure TLSEngine.MakeAddr(var tp: TTerm; p: pointer);
begin
  rc := lsMakeAddr(eng, tp, p);
  if rc <> 0 then LSError('lsMakeAddr', rc);
end;

{ Getting C values from Prolog terms }

function TLSEngine.GetTermType(t: TTerm): TPType;
begin
  Result := TPType(lsGetTermType(eng, t));
end;

procedure TLSEngine.GetTerm(t: TTerm; dt: TDType; p: pointer);
begin
  rc := lsGetTerm(eng, t, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetTerm', rc);
end;

function TLSEngine.GetPStrTerm(t: TTerm): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetTerm(eng, t, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := StrPas(buf);
end;

function TLSEngine.GetIntTerm(t: TTerm): integer;
var
  i: integer;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSEngine.GetLongTerm(t: TTerm): longint;
var
  i: longint;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSEngine.GetShortTerm(t: TTerm): longint;
var
  i: longint;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSEngine.GetFloatTerm(t: TTerm): double;
var
  f: double;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := f;
end;

{ Structure hacking functions }

procedure TLSEngine.GetFA(t: TTerm; var s: string; var ap: TArity);
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetFA', rc);
  s := StrPas(buf);
end;

function TLSEngine.GetFunctor(t: TTerm): string;
var
  ap: TArity;
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetFunctor', rc);
  Result := StrPas(buf);
end;

function TLSEngine.GetArity(t: TTerm): integer;
var
  ap: TArity;
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetArity', rc);
  Result := ap;
end;

procedure TLSEngine.MakeFA(var tp: TTerm; s: string; a: TArity);
begin
  StrPCopy(buf, s);
  rc := lsMakeFAA(eng, tp, buf, a);
  if rc <> 0 then LSError('lsMakeFA', rc);
end;

function TLSEngine.UnifyArg(var tp: TTerm; n: integer; dt: TDType; p: pointer): Boolean;
begin
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dt), p);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyPStrArg(var tp: TTerm; n: integer; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dSTR), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyAtomArg(var tp: TTerm; n: integer; s: string): Boolean;
begin
  StrPCopy(buf, s);
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dATOM), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyIntArg(var tp: TTerm; n: integer; i: integer): Boolean;
begin
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyLongArg(var tp: TTerm; n: integer; i: longint): Boolean;
begin
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyShortArg(var tp: TTerm; n: integer; i: longint): Boolean;
begin
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSEngine.UnifyFloatArg(var tp: TTerm; n: integer; f: double): Boolean;
begin
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;


procedure TLSEngine.GetArg(t: TTerm; n: integer; dt: TDType; p: pointer);
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetArg', rc);
end;

function TLSEngine.GetPStrArg(t: TTerm; n: integer): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetArg(eng, t, n, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := StrPas(buf);
end;

function TLSEngine.GetIntArg(t: TTerm; n: integer): integer;
var
  i: integer;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSEngine.GetLongArg(t: TTerm; n: integer): longint;
var
  i: longint;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSEngine.GetShortArg(t: TTerm; n: integer): longint;
var
  i: longint;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSEngine.GetFloatArg(t: TTerm; n: integer): double;
var
  f: double;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := f;
end;

function TLSEngine.GetArgType(t: TTerm; n: integer): TPType;
begin
  Result := TPType(lsGetArgType(eng, t, n));
end;

function TLSEngine.StrArgLen(t: TTerm; i: integer): integer;
begin
  Result := lsStrArgLen(eng, t, i);
end;

function TLSEngine.Unify(t1: TTerm; t2: TTerm): Boolean;
begin
  rc := lsUnify(eng, t1, t2);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnify', rc);
  end;
end;

{ List hacking functions }

procedure TLSEngine.MakeList(var tp: TTerm);
begin
  rc := lsMakeList(eng, tp);
  if rc <> 0 then LSError('lsMakeList', rc);
end;

procedure TLSEngine.PushList(var tp: TTerm; t: TTerm);
begin
  rc := lsPushList(eng, tp, t);
  if rc <> 0 then LSError('lsPushList', rc);
end;

function TLSEngine.PopList(var tp: TTerm; dt: TDType; p: pointer): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dt), p);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.PopPStrList(var tp: TTerm; var s: string): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dSTR), @buf);
  s := StrPas(buf);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.PopIntList(var tp: TTerm; var i: integer): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dLONG), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.PopLongList(var tp: TTerm; var i: longint): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dLONG), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.PopShortList(var tp: TTerm; var i: longint): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.PopFloatList(var tp: TTerm; var f: double): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSEngine.GetHead(t: TTerm; dt: TDType; p: pointer): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dt), p);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetPStrHead(t: TTerm; var s: string): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dSTR), @buf);
  s := StrPas(buf);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetIntHead(t: TTerm; var i: integer): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dLONG), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetLongHead(t: TTerm; var i: longint): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dLONG), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetShortHead(t: TTerm; var i: longint): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetFloatHead(t: TTerm; var f: double): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSEngine.GetTail(t: TTerm): TTerm;
begin
  Result := lsGetTail(eng, t);
end;

{ Stream I/O functions }

procedure TLSEngine.SetStream(st: TPStream; i: integer);
begin
  rc := lsSetStream(eng, TPStreamInt(st), i);
  if rc <> 0 then LSError('lsSetStream', rc);
end;

function TLSEngine.GetStream(st: TPStream): integer;
begin
  rc := lsGetStream(eng, TPStreamInt(st));
end;

procedure TLSEngine.SetInput(pfunc1: TGetC; pfunc2: TUngetC);
begin
  rc := lsSetInput(eng, pfunc1, pfunc2);
  if rc <> 0 then LSError('lsSetInput', rc);
end;

procedure TLSEngine.SetOutput(pfunc1: TPutC; pfunc2: TPutS);
begin
  rc := lsSetOutputA(eng, pfunc1, pfunc2);
  if rc <> 0 then LSError('lsSetOutput', rc);
end;

{ Miscellaneous functions }

procedure TLSEngine.GetVersion(var s: string);
begin
  rc := lsGetVersionA(eng, buf);
  if rc <> 0 then LSError('lsGetVersion', rc);
  s := StrPas(buf);
end;

function TLSEngine.GetPVersion: string;
begin
  rc := lsGetVersionA(eng, buf);
  if rc <> 0 then LSError('lsGetVersion', rc);
  Result := StrPas(buf);
end;

{ Error handling functions }

procedure TLSEngine.GetExceptMsg(s: PChar; l:integer);
begin
  lsGetExceptMsgA(eng, s, l);
end;

function TLSEngine.GetExceptRC: TRC;
begin
  Result := lsGetExceptRC(eng);
end;

procedure TLSEngine.GetExceptReadBuffer(s: PChar; l: integer);
begin
  lsGetExceptReadBufferA(eng, s, l);
  if rc <> 0 then LSError('lsGetExceptReadBuffer', rc);
end;


procedure TLSEngine.GetExceptCallStack(s: PChar; l: integer);
begin
  lsGetExceptCallStackA(eng, s, l);
  if rc <> 0 then LSError('lsGetExceptCallStack', rc);
end;

{ Non-Logic Server functions }

{ Error handling for most logic server functions.
  An exception is raised and the logic server is closed.
  This is important as it frees up all the memory allocated
  by the logic server. }

procedure TLSEngine.LSError(apiname: String; rc: integer);
var
  s: array[0..256] of char;
  ps: PChar;
  ss: String;
begin
  ps := s;
  lsGetExceptMsgA(eng, ps, 255);
  ss := strpas(ps);
  raise ELogicServer.Create(apiname + ': ' + IntToStr(rc) + ' ' + ss);
end;

procedure Register;
begin
  RegisterComponents('Additional', [TLSEngine]);
end;

end.
