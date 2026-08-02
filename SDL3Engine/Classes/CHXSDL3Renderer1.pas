------------------------------
MÉTODOS QUE USAN CHXSEGMENT

function cCHXSDL3Renderer.Segment(const Segm: TCHXSegment): Boolean;
begin
  { El truco del puntero de tu manuscrito es genial en Pascal: 
    Como TCHXSegment son dos TSDL_FPoint contiguos en memoria, 
    se le engaña a SDL para que trace una polilínea de 2 puntos (1 segmento). }
  Result := SDL_RenderLines(SDL3R, @Segm, 2);
end;

function cCHXSDL3Renderer.Segments(const SegArray: TCHXSegmentDynArray; 
const idxFirst: Integer = 0; Count: Integer = 0): Boolean;
var
  MaxCount, i: Integer;
begin
  if (idxFirst < 0) or (idxFirst > High(SegArray)) or (Count < 0) then 
    Exit(False);

  MaxCount := Length(SegArray) - idxFirst;
  Result := True;

  if Count = 0 then
    Count := MaxCount
  else if Count > MaxCount then
  begin
    Count := MaxCount; 
    Result := False; 
  end;

  // SDL no tiene un dibujado en lote nativo para múltiples segmentos no conectados, 
  // así que iteramos sobre el array casteando cada uno.
  for i := 0 to (Count - 1) do
  begin
    Result := SDL_RenderLines(SDL3R, @SegArray[idxFirst + i], 2) and Result;
  end;
end;

function cCHXSDL3Renderer.Segments(const PArr: TSDLFPointDynArr; 
const idxFirst: Integer = 0; SegCount: Integer = 0): Boolean;
var
  MaxCount, i: Integer;
begin
  { SegCount is the number of segments (pairs of points), whereas idxFirst is the starting point.
    It uses an absolute index, allowing it to start on an odd position.
    Unlike Lines, which draws segments continuously from the end of the previous one,
    this method draws them independently using point pairs. }
  if (idxFirst < 0) or (idxFirst >= High(PArr)) or (SegCount < 0) then Exit(False);

  MaxCount := (Length(PArr) - idxFirst) div 2; 
  Result := True;

  if SegCount = 0 then
    SegCount := MaxCount
  else if SegCount > MaxCount then
  begin
    SegCount := MaxCount; 
    Result := False; // Draw, but return an error
  end;

  for i := 0 to (SegCount - 1) do
    Result := SDL_RenderLines(SDL3R, @PArr[idxFirst + i * 2], 2) and Result;
end;

FIN DE LOS MÉTODOS CON SEGMENT
------------------------------
