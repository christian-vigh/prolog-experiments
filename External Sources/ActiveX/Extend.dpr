library Extend;

{Extended predicates demo for AmziX Component
December 1998 by Thomas Steiner}
uses
  SysUtils,
  Dialogs;

const     lsFalse = 0;
          lsTrue  = 1;
          DLLName = 'AMZI4.DLL';

{Type declarations}

type      TEngID   = longInt;                    {ID for engine}
          TDType   = (dATOM,dSTR,dINT,dLONG,
                      dSHORT,dFLOAT,dDOUBLE,
                      dADDR,dTERM);              {casts for DLL calls}
          TPType =   (pATOM, pINT, pSTR, pFLOAT,
                      pSTRUCT, pLIST, pTERM, pADDR,
                      pVAR);

          TTypeInt = integer;                    {type casting in DLL calls}
          TRC      = integer;                    {return code}
          TTFi     = integer;                    {Prolog T/F or error code}
          TArity   = word;                       {The arity of a functor}
          TExtPred = function(PEngID : TEngID) : TTFi; stdCall;
                                                 {Extended predicate proto}


var       ABuffer : array[0..255] of Char;

{Import AMZI!Prolog Logic Server API functions from external DLL}

function lsGetParm(eng:TEngID;n:integer;dt:TTypeInt;p:pointer):TRC;
         stdCall; external DLLName;

function lsUnifyParm(eng:TEngID;n:integer;dt:TTypeInt;p:pointer):TTFi;
         stdCall; external DLLName;

function lsAddPredA(eng: TEngID; pname: PChar; parity: TArity;
         pfunc: TExtPred; Ptr : Pointer): TRC;
         stdcall; external DLLName;

{Declare exported functions (extended predicates)}
{Predicate to show a message in a MessageBox}
function p_showMsg(PEngID : TEngID) : TTFi; stdCall; export;
var PBuffer : PChar;
    ARC : TRC;
    AMessage : String;
begin PBuffer:=ABuffer;
      ARC := lsGetParm(PEngID,1,TTypeInt(dSTR),PBuffer);
      if ARC=0
      then begin
            AMessage := StrPas(ABuffer);
            ShowMessage(AMessage);
            Result := lsTrue;
           end
      else begin
            AMessage := 'Dialog Error: '+IntToStr(ARC);
            ShowMessage(AMessage);
            Result := lsFalse;
           end;
end;

{Predicate to initialize the extended predicates above}
function InitPreds(PEngID : TEngID;PPointer : Pointer) : TTFi; stdCall; export;
var AnError : Integer;
    ARC : TRC;
begin AnError := 0;
      ARC := lsAddPredA(PEngID,'showMsg',1,p_showMsg,Pointer(PEngID));
      AnError := AnError + ARC;
      If (AnError = 0)
      then ShowMessage('Extended predicates installed')
      else ShowMessage('Extended pred inst err!');
      Result := ARC;
end;

exports
 p_showMsg,InitPreds;
begin
end.
