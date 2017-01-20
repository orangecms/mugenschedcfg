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

package body Types.Chain.Chains_With_Score
is

   -------------------------------------------------------------------------

   function Find
     (Graph : Points_Type;
      X     : Float)
      return Point_Type
   is
   begin
      for P of Graph loop
         if P.X = X then
            return P;
         end if;
      end loop;

      return Infinity;
   end Find;

   -------------------------------------------------------------------------

   procedure Last_Two_Points
     (Graph :     Points_Type;
      P1    : out Point_Type;
      P2    : out Point_Type)
   is
      C : Set_Of_Points_Package.Cursor := Graph.Last;
   begin
      P2 := Set_Of_Points_Package.Element (Position => C);
      Set_Of_Points_Package.Previous (Position => C);
      P1 := Set_Of_Points_Package.Element (Position => C);
   end Last_Two_Points;

   -------------------------------------------------------------------------

   procedure Nearest_Left
     (X     :     Float;
      Graph :     Points_Type;
      P     : out Point_Type)
   is
   begin
      P := Infinity;

      for Point of Graph loop
         exit when Point.X > X;
         P := Point;
      end loop;
   end Nearest_Left;

   -------------------------------------------------------------------------

   procedure Nearest_Right
     (X     :     Float;
      Graph :     Points_Type;
      P     : out Point_Type)
   is
   begin
      P := Infinity;

      for Point of reverse Graph loop
         exit when Point.X < X;
         P := Point;
      end loop;
   end Nearest_Right;

   -------------------------------------------------------------------------

   function Score
     (C : Chains_With_Score_Type;
      X : Float)
      return Float
   is
      P1, P2 : Point_Type;
   begin

      --  Point smaller than defined function.

      if C.Datapoints.First_Element.X > X then
         return Float'First;
      end if;

      --  Point is defined.

      P1 := Find
        (Graph => C.Datapoints,
         X     => X);
      if P1 /= Infinity then
         return P1.Y;
      end if;

      --  Interpolate.

      if C.Datapoints.Last_Element.X < X then
         Last_Two_Points
           (Graph => C.Datapoints,
            P1    => P1,
            P2    => P2);
      else
         Nearest_Left
           (X     => X,
            Graph => C.Datapoints,
            P     => P1);
         Nearest_Right
           (X     => X,
            Graph => C.Datapoints,
            P     => P2);
      end if;

      return P1.Y * ((P2.X - X) / (P2.X - P1.X)) +
        P2.Y * ((X - P1.X) / (P2.X - P1.X));
   end Score;

end Types.Chain.Chains_With_Score;
