unit ufTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, CheckLst, uCHXStrUtils,
  // 2023
  uc2DShape, uc2DShapeList, ucPoint;

type

  { TForm1 }

  TForm1 = class(TForm)
    bAddPoint: TButton;
    bAddShape: TButton;
    cbx2DShape: TComboBox;
    chkClosedShape: TCheckBox;
    clbShapes: TCheckListBox;
    fseX: TFloatSpinEdit;
    fseY: TFloatSpinEdit;
    gbxGrid: TGroupBox;
    gbxPoints: TGroupBox;
    gbxShape: TGroupBox;
    lblX: TLabel;
    lblY: TLabel;
    lbxPoints: TListBox;
    lGridSize: TLabel;
    lPSize: TLabel;
    pbGraph: TPaintBox;
    pData: TPanel;
    pPoints: TPanel;
    pShapes: TPanel;
    seGridSize: TSpinEdit;
    sePSize: TSpinEdit;
    Splitter1: TSplitter;
    procedure bAddPointClick(Sender: TObject);
    procedure bAddShapeClick(Sender: TObject);
    procedure chkClosedShapeChange(Sender: TObject);
    procedure clbShapesClick(Sender: TObject);
    procedure clbShapesClickCheck(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure lbxPointsClick(Sender: TObject);
    procedure pbGraphPaint(Sender: TObject);
    procedure seGridSizeEditingDone(Sender: TObject);
    procedure sePSizeEditingDone(Sender: TObject);
  private
    FCurrPoint: cPoint;
    FCurrShape: c2DShape;
    FShapeList: c2DShapeList;
    procedure SetCurrPoint(AValue: cPoint);
    procedure SetCurrShape(AValue: c2DShape);

  protected
    procedure UpdateGraph;
    procedure UpdatePoints;

  public
    property ShapeList: c2DShapeList read FShapeList;

    property CurrShape: c2DShape read FCurrShape write SetCurrShape;
    property CurrPoint: cPoint read FCurrPoint write SetCurrPoint;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Make sure to use same decimal symbol '.'
  StandardFormatSettings;

  FShapeList := c2DShapeList.Create(True);
end;

procedure TForm1.lbxPointsClick(Sender: TObject);
begin
  if clbShapes.ItemIndex >= 0 then
    CurrPoint := cPoint(lbxPoints.Items.Objects[lbxPoints.ItemIndex])
  else
    CurrPoint := nil;

  if Assigned(CurrPoint) then
  begin
    fseX.Value := CurrPoint.X;
    fseY.Value := CurrPoint.Y;
  end;
end;

procedure TForm1.pbGraphPaint(Sender: TObject);
begin
  UpdateGraph;
end;

procedure TForm1.seGridSizeEditingDone(Sender: TObject);
begin
  UpdateGraph;
end;

procedure TForm1.sePSizeEditingDone(Sender: TObject);
begin
  UpdateGraph;
end;

procedure TForm1.SetCurrPoint(AValue: cPoint);
begin
  if FCurrPoint = AValue then Exit;
  FCurrPoint := AValue;
end;

procedure TForm1.SetCurrShape(AValue: c2DShape);
begin
  if FCurrShape = AValue then Exit;
  FCurrShape := AValue;

  UpdatePoints;
end;

procedure TForm1.UpdateGraph;
var
  CenterX, CenterY: LongInt; // Ugly, so we don't need to

  procedure DrawPoint(aPoint: cPoint);
  var
    X1, Y1: LongInt;
  begin

    X1 := Round(aPoint.X * seGridSize.Value) + CenterX;
    // Y-axis in canvas is top-bottom
    Y1 := -Round(aPoint.Y * seGridSize.Value) + CenterY;

    pbGraph.Canvas.Ellipse(X1 - sePSize.Value, Y1 - sePSize.Value,
      X1 + sePSize.Value, Y1 + sePSize.Value);
    pbGraph.Canvas.Line(X1 - sePSize.Value, Y1 - sePSize.Value,
      X1 + sePSize.Value, Y1 + sePSize.Value);
    pbGraph.Canvas.Line(X1 - sePSize.Value, Y1 + sePSize.Value,
      X1 + sePSize.Value, Y1 - sePSize.Value);
  end;

  procedure DrawSegment(aPoint1, aPoint2: cPoint);
  var
    X1, X2, Y1, Y2: LongInt;
  begin
    X1 := Round(aPoint1.X * seGridSize.Value) + CenterX;
    // Y-axis in canvas is top-bottom
    Y1 := -Round(aPoint1.Y * seGridSize.Value) + CenterY;
    X2 := Round(aPoint2.X * seGridSize.Value) + CenterX;
    // Y-axis in canvas is top-bottom
    Y2 := -Round(aPoint2.Y * seGridSize.Value) + CenterY;

    pbGraph.Canvas.Line(X1, Y1, X2, Y2);
  end;

var
  i, j: LongInt;
  aShape: c2DShape;
begin
  CenterX := pbGraph.ClientWidth div 2;
  CenterY := pbGraph.ClientHeight div 2;

  // Canvas draw methods are for use with TPoint / TRect
  //   so here we must make many slow conversions.
  pbGraph.Canvas.Clear;

  // Draw Axes
  pbGraph.Canvas.Pen.Width := 5;
  pbGraph.Canvas.Line(0, CenterY, pbGraph.ClientWidth, CenterY);
  pbGraph.Canvas.Line(CenterX, 0, CenterX, pbGraph.ClientHeight);

  pbGraph.Canvas.Pen.Width := 3;
  i := CenterX - seGridSize.Value;
  while i > 0 do
  begin
    pbGraph.Canvas.Line(i, CenterY - 5, i, CenterY + 5);
    i -= seGridSize.Value;
  end;
  i := CenterX + seGridSize.Value;
  while i < pbGraph.ClientWidth do
  begin
    pbGraph.Canvas.Line(i, CenterY - 5, i, CenterY + 5);
    i += seGridSize.Value;
  end;
  i := CenterY - seGridSize.Value;
  while i > 0 do
  begin
    pbGraph.Canvas.Line(CenterX - 5, i, CenterX + 5, i);
    i -= seGridSize.Value;
  end;
  i := CenterY + seGridSize.Value;
  while i < pbGraph.ClientHeight do
  begin
    pbGraph.Canvas.Line(CenterX - 5, i, CenterX + 5, i);
    i += seGridSize.Value;
  end;

  // Draw shapes
  pbGraph.Canvas.Pen.Width := 1;

  i := 0;
  while i < clbShapes.Items.Count do
  begin
    if clbShapes.Checked[i] then
    begin
      aShape := c2DShape(clbShapes.Items.Objects[i]);
      case aShape.Points.Count of
        0: ;
        1: DrawPoint(aShape.Points[0]);
        2: DrawSegment(aShape.Points[0], aShape.Points[1]);
        else
        begin
          j := 0;
          while j < aShape.Points.Count - 1 do
          begin
            DrawSegment(aShape.Points[j], aShape.Points[j + 1]);
            Inc(j);
          end;

          // j = aShape.Points.Count - 1
          if aShape.IsClosed then
            DrawSegment(aShape.Points[0], aShape.Points[j]);

        end;
      end;
    end;

    Inc(i);
  end;
end;

procedure TForm1.UpdatePoints;
var
  i: LongInt;
  aPoint: cPoint;
begin
  lbxPoints.Clear;

  if not Assigned(CurrShape) then
    Exit;

  i := 0;
  while i < CurrShape.Points.Count do
  begin
    aPoint := CurrShape.Points[i];
    lbxPoints.AddItem(aPoint.WriteString, aPoint);
    Inc(i);
  end;
end;

procedure TForm1.bAddPointClick(Sender: TObject);
var
  aPoint: cPoint;
begin
  if not Assigned(CurrShape) then
    Exit;

  aPoint := CurrShape.Points.AddNewPoint;

  aPoint.X := fseX.Value;
  aPoint.Y := fseY.Value;

  UpdatePoints;

  UpdateGraph;

  CurrPoint := aPoint;
end;

procedure TForm1.bAddShapeClick(Sender: TObject);
var
  aShape: c2DShape;
begin
  case cbx2DShape.ItemIndex of
    0: aShape := ShapeList.AddNewShape(c2DShape);
    else
      aShape := ShapeList.AddNewShape(c2DShape);
  end;

  clbShapes.AddItem(aShape.ToString, aShape);

  CurrShape := aShape;
end;

procedure TForm1.chkClosedShapeChange(Sender: TObject);
begin
  if Assigned(CurrShape) then
    CurrShape.IsClosed := chkClosedShape.Checked;

  UpdateGraph;
end;

procedure TForm1.clbShapesClick(Sender: TObject);
begin
  if clbShapes.ItemIndex >= 0 then
    CurrShape := c2DShape(clbShapes.Items.Objects[clbShapes.ItemIndex])
  else
    CurrShape := nil;

  if Assigned(CurrShape) then
  begin
    chkClosedShape.Checked := CurrShape.IsClosed;
  end
  else
    chkClosedShape.Checked := False;
end;

procedure TForm1.clbShapesClickCheck(Sender: TObject);
begin
  UpdateGraph;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FShapeList.Free;

  CanClose := True;
end;

end.
