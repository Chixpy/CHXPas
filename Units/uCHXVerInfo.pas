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
  Classes, SysUtils, InterfaceBase;

const
  GetCompilerInfo = 'FPC ' + {$I %FPCVERSION%};
  GetTargetInfo = {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
  GetOS = {$I %FPCTARGETOS%};
  GetLCLVersion = 'LCL ' + lcl_version;
  GetCompiledDate = {$I %DATE%}+ ' at ' + {$I %TIME%};

function GetWidgetSet: string;

implementation

function GetWidgetSet: string;
begin
  Result := LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
end;

end.

