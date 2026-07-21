program TestCHXColor;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, uCHXColor;

procedure Assert(const Condition: Boolean; const Msg: string);
begin
  if Condition then
    Writeln('[ OK ] ', Msg)
  else
    Writeln('[FAIL] ', Msg);
end;

procedure TestRGB2HSL2RGB;
var
  H: Word;
  S, L, R, G, B: Byte;
begin
  Writeln('--- Testing RGB -> HSL -> RGB (Roundtrip) ---');
  
  // Probar con un Rojo Puro (255, 0, 0) -> Debería ser H=0, S=100, L=50
  RGB2HSL(255, 0, 0, H, S, L);
  Assert((H = 0) and (S = 100) and (L = 50), 
    Format('Red Pure -> HSL: H=%d (Exp:0), S=%d (Exp:100), L=%d (Exp:50)', [H, S, L]));
    
  // Volver a RGB
  HSL2RGB(H, S, L, R, G, B);
  Assert((R >= 254) and (G <= 1) and (B <= 1), 
    Format('HSL -> RGB Roundtrip: R=%d, G=%d, B=%d', [R, G, B]));
end;

procedure TestRGB2HSV2RGB;
var
  H: Word;
  S, V, R, G, B: Byte;
begin
  Writeln;
  Writeln('--- Testing RGB -> HSV -> RGB (Roundtrip) ---');
  
  // Probar con un Verde de intensidad media (0, 128, 0)
  RGB2HSV(0, 128, 0, H, S, V);
  // Esperado aprox: H=120, S=100, V=50
  Assert((H = 120) and (S = 100), 
    Format('Green Mid -> HSV: H=%d (Exp:120), S=%d (Exp:100), V=%d', [H, S, V]));
    
  // Volver a RGB
  HSV2RGB(H, S, V, R, G, B);
  Assert((R <= 1) and (G >= 127) and (B <= 1), 
    Format('HSV -> RGB Roundtrip: R=%d, G=%d, B=%d', [R, G, B]));
end;

procedure TestFastHue;
var
  R, G, B: Byte;
begin
  Writeln;
  Writeln('--- Testing CHXFastHue Critical Points ---');
  
  // Probar punto crítico inicial (Hue = 0 -> Rojo Puro)
  CHXFastHue(0, R, G, B);
  Assert((R = 255) and (G = 0) and (B = 0), 'FastHue(0) is Red Pure');
  
  // Probar punto crítico intermedio corregido en tus notas (Hue = 43)
  CHXFastHue(43, R, G, B);
  Assert((R = 255) and (G = 255) and (B = 0), 'FastHue(43) boundary control (Yellow)');
  
  // Probar punto crítico (Hue = 171)
  CHXFastHue(171, R, G, B);
  Assert((R = 0) and (G = 0) and (B = 255), 'FastHue(171) boundary control (Blue)');
end;

begin
  try
    TestRGB2HSL2RGB;
    TestRGB2HSV2RGB;
    TestFastHue;
  except
    on E: Exception do
      Writeln('Exception: ', E.Message);
  end;
  
  Writeln;
  Writeln('Press Enter to exit...');
  Readln;
end.
