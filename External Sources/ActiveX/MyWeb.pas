unit MyWeb;

interface

uses Amzi4;

type TMyWeb = class(TWebBrowser)
              protected
               procedure OnDocumentAvailable(Sender :TObject); override;
              end;

     TMyProxy = class(TWebProxy)
                end;

implementation

end.
