unit ucNOTCMover;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uCHXPoint3DF;

type

  { cNOTCMover }

  cNOTCMover = class
  private
    FAcceleration : TPoint3DF;
    FForce : TPoint3DF;
    FMass : Double;
    FPosition : TPoint3DF;
    FVelocity : TPoint3DF;
    procedure SetAcceleration(AValue : TPoint3DF);
    procedure SetForce(AValue : TPoint3DF);
    procedure SetMass(AValue : Double);
    procedure SetPosition(AValue : TPoint3DF);
    procedure SetVelocity(AValue : TPoint3DF);
  public
    property Mass : Double read FMass write SetMass;
    {< Mass of the entity. }

    property Position : TPoint3DF read FPosition write SetPosition;
    {< Position of the entity. }
    property Velocity : TPoint3DF read FVelocity write SetVelocity;
    {< Velocity of the entity. }
    property Acceleration : TPoint3DF read FAcceleration write SetAcceleration;
    {< Arbitrary acceleration without any force that will be applyed on
         Update call.
    }
    property Force : TPoint3DF read FForce write SetForce;
    {< Final force that will be applyed on Update call. }

    procedure AddForce(aForce : TPoint3DF);
    {< Adds a force to the entity. }

    procedure Update;
    {< Apply the force (adding the result to acceleration), updating velocity
         and position.

       Force and Acceleration will be reseted to zero after processed.
    }

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cNOTCMover }

procedure cNOTCMover.SetPosition(AValue : TPoint3DF);
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
end;

procedure cNOTCMover.SetAcceleration(AValue : TPoint3DF);
begin
  if FAcceleration = AValue then Exit;
  FAcceleration := AValue;
end;

procedure cNOTCMover.SetForce(AValue : TPoint3DF);
begin
  if FForce = AValue then Exit;
  FForce := AValue;
end;

procedure cNOTCMover.SetMass(AValue : Double);
begin
  if FMass = AValue then Exit;
  FMass := AValue;
end;

procedure cNOTCMover.SetVelocity(AValue : TPoint3DF);
begin
  if FVelocity = AValue then Exit;
  FVelocity := AValue;
end;

procedure cNOTCMover.AddForce(aForce : TPoint3DF);
begin
  Force.Add(aForce);
end;

procedure cNOTCMover.Update;
begin
  if Mass <> 0 then
    Acceleration.Add(Force / Mass);

  Velocity.Add(Acceleration);

  Position.Add(Velocity);

  Acceleration := TPoint3DF.Zero;
  Force := TPoint3DF.Zero;
end;

constructor cNOTCMover.Create;
begin
  inherited Create;

  Mass := 1;
end;

destructor cNOTCMover.Destroy;
begin
  inherited Destroy;
end;

end.
