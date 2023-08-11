unit ufrMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil,
  LCLTranslator,
  // CHX units
  uCHXConst, uCHXStrUtils, uCHXVerInfo,
  // CHX frames
  // CHX forms
  ufrCHXForm;

type

  { TCHXForm }

  TCHXForm = class(TfrmCHXForm)
    procedure FormCreate(Sender: TObject);
  private
    FBaseFolder: string;
    FGUIConfig: string;
    FIconsFile: string;
    procedure SetBaseFolder(AValue: string);
    procedure SetGUIConfig(AValue: string);
    procedure SetIconsFile(AValue: string);

  public

    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< Base folder for relative paths of files. }
    property GUIConfig: string read FGUIConfig write SetGUIConfig;
    property IconsFile: string read FIconsFile write SetIconsFile;

  end;

var
  CHXForm: TCHXForm;

implementation

{$R *.lfm}

{ TCHXForm }

procedure TCHXForm.FormCreate(Sender: TObject);
begin
  // Tipical CHX program init...

  // Title of application, usually it's autodeleted in .lpr file...
  //   There is an option
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);
  Self.Caption:= Format(krsFmtWindowCaption, [Application.Title, Self.Caption]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Loading translation
  SetDefaultLang('', 'Locale');

  // File to load window states and config
  GUIConfig := 'GUI.ini';
  // IconsFile can be a fixed value or a value readed from GUIConfig
  IconsFile := 'Images\GUI\Icons.ini';



  // TODO: Load program config from previous file, a fixed value or
  //   a value from previous file with the filename of config file.



  // Actually loading icons and window configuration
  LoadGUIIcons(IconsFile);
  LoadGUIConfig(GUIConfig);
end;

procedure TCHXForm.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFile(AValue);
end;

procedure TCHXForm.SetGUIConfig(AValue: string);
begin
  FGUIConfig := SetAsFile(AValue);
end;

procedure TCHXForm.SetIconsFile(AValue: string);
begin
  FIconsFile := SetAsFile(AValue);
end;

end.

