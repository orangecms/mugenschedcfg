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

with Types.Chain.Chains;

with Ada.Containers.Ordered_Sets;

package Types.Chain.Chains_With_Score
is

   type Point_Type is record
      X, Y : Float;
   end record;

   Infinity : constant Point_Type
     := (X => Float'Last, Y => Float'Last);

   function "<" (Left, Right : Point_Type) return Boolean
   is (Left.X < Right.X);

   package Set_Of_Points_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Point_Type,
      "<"          => "<");

   subtype Points_Type is Set_Of_Points_Package.Set;

   type Chains_With_Score_Type is tagged record
      Chain      : Types.Chain.Chains.List; -- List of chain links
      Datapoints : Points_Type;
   end record;

   function Score
     (C : Chains_With_Score_Type;
      X : Float)
      return Float;

private

   procedure Nearest_Left
     (X     :     Float;
      Graph :     Points_Type;
      P     : out Point_Type)
   with
      Pre => X > Graph.First_Element.X;

   procedure Nearest_Right
     (X     :     Float;
      Graph :     Points_Type;
      P     : out Point_Type)
   with
      Pre => X < Graph.Last_Element.X;

   use type Ada.Containers.Count_Type;

   procedure Last_Two_Points
     (Graph :     Points_Type;
      P1    : out Point_Type;
      P2    : out Point_Type)
   with
      Pre => Graph.Length > 1;

   --  Return exact point for given X if such a point exists. Returns infinity
   --  if not found.
   function Find
     (Graph : Points_Type;
      X     : Float)
      return Point_Type;

end Types.Chain.Chains_With_Score;
