program TestCHXMath;
{<
  Test program for unit uCHXMAth.pas.

  (C) 2026 Chixpy https://github.com/Chixpy
}

uses uCHXMath, Math;

var
  i, j: Integer;
begin
  WriteLn('== CONSTANTS ==');
  WriteLn('-- PI RELATED --');
  WriteLn('        Pi = ', pi:1:16);
  WriteLn('      k2Pi = ', k2Pi:1:16);
  WriteLn('   kHalfPi = ', kHalfPi:1:16);
  WriteLn('kQuarterPi = ', kQuarterPi:1:16);
  WriteLn;
  
  WriteLn('-- ROOTS AND INVERSES --');  
  WriteLn('kSqRt2 = ', kSqRt2:1:20);
  WriteLn('kSqRt3 = ', kSqRt3:1:20);
  WriteLn('kSqRt5 = ', kSqRt5:1:20);
  WriteLn('kCbRt2 = ', kCbRt2:1:20);
  WriteLn('kCbRt3 = ', kCbRt3:1:20);
  WriteLn(' k4Rt5 = ', k4Rt5:1:20);
  WriteLn('k12Rt2 = ', k12Rt2:1:20);
  WriteLn('kInvSqRt2 = ', kInvSqRt2:1:20);
  WriteLn('kInvSqRt3 = ', kInvSqRt3:1:20);
  WriteLn('kInvSqRt5 = ', kInvSqRt5:1:20);
  WriteLn('kInvCbRt2 = ', kInvCbRt2:1:20);
  WriteLn('kInvCbRt3 = ', kInvCbRt3:1:20);
  WriteLn(' kInv4Rt5 = ', kInv4Rt5:1:20);
  WriteLn('kInv12Rt2 = ', kInv12Rt2:1:20);
  WriteLn;
  
  WriteLn('-- GOLD/SILVER RATIOS, FRACTAL DIMENSION --');  
  WriteLn('     kGoldRatio = ', kGoldRatio:1:20);
  WriteLn('kSuperGoldRatio = ', kSuperGoldRatio:1:20);
  WriteLn('     kGoldAngle = ', kGoldAngle:1:20);
  WriteLn('     kGoldSpire = ', kGoldSpire:1:20);
  WriteLn('   kSilverRatio = ', kSilverRatio:1:20);
  WriteLn('    kDimCantor = ', kDimCantor:1:20);
  WriteLn(' kDimApollonio = ', kDimApollonio:1:20);
  WriteLn('    kDimDragon = ', kDimDragon:1:20);
  WriteLn('kDimSierpinski = ', kDimSierpinski:1:20);
  WriteLn;
  
  WriteLn('-- OTHER CONSTANTS  --');  
  WriteLn('kLn2 = ', kLn2:1:20);
  WriteLn('kHalfFact = ', kHalfFact:1:20);
  WriteLn('kHexAreaF = ', kHexAreaF:1:20);
  WriteLn('kEulerN = ', kEulerN:1:20);
  WriteLn('kEulerK = ', kEulerK:1:20);
  WriteLn('kGaussK = ', kGaussK:1:20);
  WriteLn;

  WriteLn('== FUNCTIONS ==');
  WriteLn('-- SUCESIONS --');
  WriteLn('Some random factorials:');
  for i := 1 to 5 do
  begin
    j := RandomRange(0, 21); // 21 is never chosen
    WriteLn('  Factorial[', j, '] = ', Factorial[j]);
  end;
  WriteLn('Some random Fibonacci:');
  for i := 1 to 5 do
  begin
    j := RandomRange(0, 47); // 47 is never chosen
    WriteLn('  Fibonacci[', j, '] = ', Fibonacci[j]);
  end;
  WriteLn;

  WriteLn('-- GCD AND LCM --');
  WriteLn('GCD(24, 50) = ', GCD(24, 50));
  WriteLn('GCD(-24, 50) = ', GCD(-24, 50));
  WriteLn('GCD(24, -50) = ', GCD(24, -50));
  WriteLn('GCD(0, 50) = ', GCD(0, 50));
  WriteLn('GCD(50, 0) = ', GCD(50, 0));
  WriteLn('LCM(24, 50) = ', LCM(24, 50));
  WriteLn('LCM(-24, 50) = ', LCM(-24, 50));
  WriteLn('LCM(24, -50) = ', LCM(24, -50));
  WriteLn('LCM(0, 50) = ', LCM(0, 50));
  WriteLn('LCM(50, 0) = ', LCM(50, 0));
  WriteLn;

  WriteLn('-- INTERPOLATIONS --');
  WriteLn('InterpolateLinear(0.6, 0, 1) = ', InterpolateLinear(0.6, 0, 1):2:4);
  WriteLn('InterpolateLinear(-1.5, 0, 1) = ', 
    InterpolateLinear(-1.5, 0, 1):2:4);
  WriteLn('InterpolateCos(0.6, 0, 1) = ', InterpolateCos(0.6, 0, 1):2:4);
  WriteLn('InterpolateCos(-1.5, 0, 1) = ', InterpolateCos(-1.5, 0, 1):2:4);
  WriteLn('InterpolateCubic(0.6, -1, 0, 1, 2) = ', 
    InterpolateCubic(0.6, -1, 0, 1, 2):2:4);
  WriteLn('InterpolateCubic(-1.5, -1, 0, 1, 2) = ', 
    InterpolateCubic(-1.5, -1, 0, 1, 2):2:4);
  WriteLn;

  WriteLn('-- CYCLE NORMALIZATIONS --');
  WriteLn('CycleNorm(-720.0, 0.0, 360.0) = ',
    CycleNorm(-720.0, 0.0, 360.0):0:4);
  WriteLn('CycleNorm(360.0, 0.0, -360.0) = ',
    CycleNorm(360.0, 0.0, -360.0):0:4);
  WriteLn('CycleNorm(361.0, 1.0, 361.0) = ',
    CycleNorm(361.0, 1.0, 361.0):0:4);
  WriteLn('CycleNorm(361.0, 1.0, -361.0) = ',
    CycleNorm(361.0, 1.0, -361.0):0:4);
  WriteLn('CycleNorm(24, 1, 12) = ', CycleNorm(24, 1, 12));
  WriteLn('CycleNorm(24, -1, -12) = ', CycleNorm(24, -1, -12));
  WriteLn('CycleNorm(-1, 1, 12) = ', CycleNorm(-1, 1, 12));
  WriteLn('CycleNorm(13, 1, 12) = ', CycleNorm(13, 1, 12));
  WriteLn('CycleNorm(0, 1, 12) = ', CycleNorm(0, 1, 12));
  WriteLn;

  WriteLn('-- REGULAR POLYGONS --');
  WriteLn('RPApothemSL(3, 10) = ', RPApothemSL(3, 10):3:4);
  WriteLn('RPApothemCC(4, 12) = ', RPApothemCC(4, 12):3:4);
  WriteLn('RPSagittaSL(5, 10) = ', RPSagittaSL(5, 10):3:4);
  WriteLn('RPSagittaCC(6, 12) = ', RPSagittaCC(6, 12):3:4);
  WriteLn('RPIntAngle(4) = ', RPIntAngle(4):3:4);
  WriteLn('RPIntAngleSum(6) = ', RPIntAngleSum(6):3:4);
  WriteLn('RPExtAngle(8) = ', RPExtAngle(8):3:4);
  WriteLn('RPExtAngleSum = ', RPExtAngleSum:3:4);
  WriteLn('RPAreaSL(4, 4) = ', RPAreaSL(4, 4):3:4);
  WriteLn('RPAreaCC(5, 12) = ', RPAreaCC(5, 12):3:4);
  WriteLn('RPNDiagonals(8) = ', RPNDiagonals(8));
  WriteLn('RPCirCircleSL(5, 10) = ', RPCirCircleSL(5, 10):3:4);
  WriteLn('RPSideLCC(5, 10) = ', RPSideLCC(5, 10):3:4);

  WriteLn;
  WriteLn('You may want to redirect output to a file.')
end.
{ 
  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
