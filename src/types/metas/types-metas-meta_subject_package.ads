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

with Ada.Strings.Unbounded;

with Types.Integer_Vectors;
with Types.Positive_Set;

package Types.Metas.Meta_Subject_Package
is

   type Meta_Subject_Type is tagged record
      Name               : Ada.Strings.Unbounded.Unbounded_String;
      Level              : Natural;
      Number             : Integer;
      CPU                : Natural := 0;
      Combined_Subjects  : Types.Integer_Vectors.Vector;
      Tick_Vector        : Types.Integer_Vectors.Vector;
      --  Saves the ticks every combined Subject gets.
      Plan_Vector        : Types.Integer_Vectors.Vector;
      Simultaneous_Set   : Types.Positive_Set.Set;
      Same_CPU_Set       : Types.Positive_Set.Set;
      Length             : Integer;
      Min_Length         : Integer;
      On_CPU_With_Number : Integer;
      --  Has to be = Number in input.
      Plan               : Positive := 1;
      Order_Number       : Integer  := 0;
   end record;

   --  Returns True if Left is shorter than Right.
   function "<" (Left, Right : Meta_Subject_Type) return Boolean;

   --  Returns True if Left has fewer restrictions or if the number of the
   --  first restriction is lower.
   function Less_Restrictions (Left, Right : Meta_Subject_Type) return Boolean;

   --  Returns True if Left has fewer restrictions or if the number of the
   --  first restriction is lower or, if equal, if Left is shorter than Right.
   function Less_Restrictions_and_Length
     (Left, Right : Meta_Subject_Type)
      return Boolean;

end Types.Metas.Meta_Subject_Package;
