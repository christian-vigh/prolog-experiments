library AmziXLibrary;

uses
  ComServ,
  AmziXLibrary_TLB in 'AmziXLibrary_TLB.pas',
  AmziXImpl in 'AmziXImpl.pas' {AmziX: CoClass},
  About1 in 'About1.pas' {AmziXAbout},
  Amzi4 in 'C:\amzi4\include\amzi4.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{$E ocx}

begin
end.
