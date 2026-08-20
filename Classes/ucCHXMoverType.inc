(*< Unit Template a Mover class with custom 3D data type, mainly for
  `TCHXVec3[x]`.

  Usage: Create a unit file, replace IDs:

  ```
    unit <UnitName>;

    {$macro ON}
    {$define UnitsUsed := , {Units} ; }
    {<
      Units needed for TCHXVec3Type.

      Begin with `,` and ends with `;`. If not units are needed must be `;`.
    }

    {$define cCHXMoverType := {cCHXMover} }
    //< ID for the main type.
    {$define MassType := {MassType} }
    //< Type for Mass property, better if its the same of used by TCHXVec3Type.
    {$define TCHXVec3Type := {TCHXVec3} }
    {< Type for 3D vector properties: Position, Velocity, Acceleration and
      Force.
    }


    {$I 'ucCHXMoverType.inc'}
  ```

  Four types of `cCHXMover[x]`` are implemented, for all basic `TCHXVec3[x]`
  types:

  - `TCHXVec3S` for `uTCHXVec3S` (Single).
  - `TCHXVec3D` for `uTCHXVec3D` (Double).
  - `TCHXVec3E` for `uTCHXVec3E` (Extended).
  - `TCHXVec3R` for `uTCHXVec3R` (Real).
*)
{$MODE ObjFPC}{$H+}{$INLINE ON}
interface

uses
  Classes, SysUtils UnitsUsed

type

  { cCHXMoverType }

  cCHXMoverType = class
  public
    Mass : MassType;
    {< Mass of the entity. }
    Position : TCHXVec3Type;
    {< Position of the entity. }
    Velocity : TCHXVec3Type;
    {< Velocity of the entity. }
    Acceleration : TCHXVec3Type;
    {< Arbitrary acceleration without any force that will be applyed on
         Update call.
    }
    Force : TCHXVec3Type;
    {< Final force that will be applyed on Update call. }

    constructor Create;

    procedure AddForce(aForce : TCHXVec3Type);
    {< Adds a force to the entity. }

    procedure Update;
    {< Apply the force (adding the result to acceleration), updating velocity
         and position.

       Force and Acceleration will be reseted to zero after processed.
    }

    destructor Destroy; override;
  end;

implementation

{ cCHXMoverType }

constructor cCHXMoverType.Create;
begin
  inherited Create;

  Mass := 1;
end;

procedure cCHXMoverType.AddForce(aForce : TCHXVec3Type);
begin
  Force.Add(aForce);
end;

procedure cCHXMoverType.Update;
begin
  if Mass <> 0 then
    Acceleration.Add(Force / Mass);

  Velocity.Add(Acceleration);

  Position.Add(Velocity);

  Acceleration.Init3D(0, 0, 0);
  Force.Init3D(0, 0, 0);
end;

destructor cCHXMoverType.Destroy;
begin
  inherited Destroy;
end;

end.
