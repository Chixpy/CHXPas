unit ucCHXMoverE;

{$macro ON}
{$define UnitsUsed := , utCHXVec3E ; }
{<
  Units needed for TCHXVec3Type.

  Begin with `,` and ends with `;`. If not units are needed must be `;`.
}

{$define cCHXMoverType := cCHXMoverE }
//< ID for the main type.
{$define MassType := Extended }
//< Type for Mass property, better if its the same of used by TCHXVec3Type.
{$define TCHXVec3Type := TCHXVec3E }
{< Type for 3D vector properties: Position, Velocity, Acceleration and
  Force.
}


{$I 'ucCHXMoverType.inc'}
