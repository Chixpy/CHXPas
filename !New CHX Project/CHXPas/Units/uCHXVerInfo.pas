unit uCHXVerInfo;

{< Version info utils.

  Highly based in uVersionSupport.pas by Mike Thompson.

  Mainly using LCLPlatformDisplayNames and converting to constants at
    building time.

  // {$I %HOME%} = User Home Directory
  // {$I %FILE%} = Current pas file
  // {$I %LINE%} = current line number
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

const
  GetCompilerInfo = 'FPC ' + {$I %FPCVERSION%};
  GetTargetInfo = {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
  GetOS = {$I %FPCTARGETOS%};
  GetLCLVersion = 'LCL ' + lcl_version;
  GetCompiledDate = {$I %DATE%} + ' at ' + {$I %TIME%};

function GetWidgetSet: string;

function GetFileVersion: string;
function GetProductVersion: string;
function GetResourceStrings(oStringList: TStrings): boolean;


implementation

uses Resource, VersionTypes, VersionResource, InterfaceBase, LCLPlatformDef;

type
  TVersionInfo = class
  private
    FBuildInfoAvailable: boolean;
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Instance: THandle);

    property BuildInfoAvailable: boolean read FBuildInfoAvailable;

    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
  end;

function GetWidgetSet: string;
begin
  Result := LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
end;

{ Routines to expose TVersionInfo data }

var
  FInfo: TVersionInfo;

procedure CreateInfo;
begin
  if not Assigned(FInfo) then
  begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  end;
end;

function GetResourceStrings(oStringList: TStrings): boolean;
var
  i, j: integer;
  oTable: TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  if FInfo.BuildInfoAvailable then
  begin
    Result := True;
    for i := 0 to FInfo.StringFileInfo.Count - 1 do
    begin
      oTable := FInfo.StringFileInfo.Items[i];

      for j := 0 to oTable.Count - 1 do
        if Trim(oTable.ValuesByIndex[j]) <> '' then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

function ProductVersionToString(PV: TFileProductVersion): string;
begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
end;

function GetProductVersion: string;
begin
  CreateInfo;

  if FInfo.BuildInfoAvailable then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  else
    Result := 'No build information available';
end;

function GetFileVersion: string;
begin
  CreateInfo;

  if FInfo.BuildInfoAvailable then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
  else
    Result := 'No build information available';
end;

{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  Result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  Result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  Result := FVersResource.VarFileInfo;
end;

constructor TVersionInfo.Create;
begin
  inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;

  inherited Destroy;
end;

procedure TVersionInfo.Load(Instance: THandle);
var
  Stream: TResourceStream;
  ResID: integer;
  Res: TFPResourceHandle;
begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
  if Res = 0 then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  finally
    Stream.Free;
  end;
end;

initialization
  FInfo := nil;

finalization
  if Assigned(FInfo) then
    FInfo.Free;
end.
