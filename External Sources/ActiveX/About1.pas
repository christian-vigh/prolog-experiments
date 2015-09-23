unit About1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Web, WebBrow;

type
  TAmziXAbout = class(TForm)
    NameLbl: TLabel;
    OkBtn: TButton;
    CopyrightLbl: TLabel;
    DescLbl: TLabel;
    Label1: TLabel;
    WebBrowser1: TWebBrowser;
  end;

procedure ShowAmziXAbout;

implementation

{$R *.DFM}

procedure ShowAmziXAbout;
begin
  with TAmziXAbout.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;


end.
