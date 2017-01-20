--
--  Copyright (C) 2016 Christiane Kuhn, secunet AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Numerics.Float_Random;

package body Auxiliary.My_Math
is

   use Ada.Numerics.Float_Random;

   Seed : Generator;

   -------------------------------------------------------------------------

   function Create_Permutation
     (N : Integer)
      return Types.Integer_Vectors.Vector
   is
      Sorted : Types.Integer_Vectors.Vector;
      Result : Types.Integer_Vectors.Vector;
      Rndm   : Positive;
      J      : Integer;
   begin
      for I in 1 .. N loop
         Sorted.Append (I);
      end loop;

      for I in 0 .. N - 1 loop
         Rndm := Positive (Random (Seed) * Float (N - I) + 1.0);
         while Rndm = N + 1 - I loop
            Rndm := Positive (Random (Seed) * Float (N - I) + 1.0);
         end loop;
         J := Sorted.Element (Index => Rndm);
         Result.Append (J);
         Sorted.Delete (Rndm);
      end loop;

      return Result;
   end Create_Permutation;

   -------------------------------------------------------------------------

   function LCM (A, B : Float) return Float
   is
      I  : Float := 1.0;
      A2 : Float;
      B2 : Float;
   begin
      if A = 0.0 or B = 0.0 then
         return 0.0;
      end if;

      if A < B then
         A2 := B;
         B2 := A;
      else
         A2 := A;
         B2 := B;
      end if;

      while ((A2 * I) / B2) /= Float'Ceiling ((A2 * I) / B2) loop
         I := I + 1.0;
      end loop;

      return A2 * I;
   end LCM;

end Auxiliary.My_Math;
