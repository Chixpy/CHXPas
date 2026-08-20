unit ucCHXMoverD;

{$macro ON}
{$define UnitsUsed := , utCHXVec3D ; }
{<
  Units needed for TCHXVec3Type.

  Begin with `,` and ends with `;`. If not units are needed must be `;`.
}

{$define cCHXMoverType := cCHXMoverD }
//< ID for the main type.
{$define MassType := Double }
//< Type for Mass property, better if its the same of used by TCHXVec3Type.
{$define TCHXVec3Type := TCHXVec3D }
{< Type for 3D vector properties: Position, Velocity, Acceleration and
  Force.
}


{$I 'ucCHXMoverType.inc'}
