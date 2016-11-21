unit CommonFilters;
// Common filters which affect all views in the application

interface
uses Classes, Generics.Collections;

var
  ShowInstalledOnly: boolean;       //Hide assemblies which are not present in cache / not installed
  ShowDeploymentsOnly: boolean;     //Hide assemblies which are not deployments

procedure FilterChanged(Sender: TObject);

var
  OnFilterChanged: TList<TNotifyEvent>;

implementation
uses SysUtils;

procedure FilterChanged(Sender: TObject);
var event: TNotifyEvent;
begin
  for event in OnFilterChanged do
    event(Sender);
end;

initialization
  OnFilterChanged := TList<TNotifyEvent>.Create;

finalization
{$IFDEF DEBUG}
  FreeAndNil(OnFilterChanged);
{$ENDIF}

end.
