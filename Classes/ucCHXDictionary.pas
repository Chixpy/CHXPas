unit ucCHXDictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8;

type

    cCHXDictionaryGenMap = specialize TFPGMap<string, string>;

    { cWJPublicationList }

  { cCHXDictionary }

  cCHXDictionary = class(cCHXDictionaryGenMap)
   public
     constructor Create;
     destructor destroy; override;
  end;


implementation

{ cCHXDictionary }

constructor cCHXDictionary.Create;
begin
  inherited Create;
end;

destructor cCHXDictionary.destroy;
begin
  inherited destroy;
end;

end.

