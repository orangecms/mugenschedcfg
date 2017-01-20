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

with Ada.Containers;

package body Types.Metas.Meta_Subject_Package
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Meta_Subject_Type) return Boolean
   is (Left.Length < Right.Length);

   -------------------------------------------------------------------------

   function Less_Restrictions (Left, Right : Meta_Subject_Type) return Boolean
   is
      use type Ada.Containers.Count_Type;

      L_Len : constant Ada.Containers.Count_Type
        := Left.Simultaneous_Set.Length;
      R_Len : constant Ada.Containers.Count_Type
        := Right.Simultaneous_Set.Length;
   begin
      if L_Len /= R_Len then
         return L_Len < R_Len;
      else
         if L_Len = 0 then
            return True;
         else --  Maybe need to check for more than just the first...
            return Left.Simultaneous_Set.First_Element < Right.Simultaneous_Set
              .First_Element;
         end if;
      end if;
   end Less_Restrictions;

   -------------------------------------------------------------------------

   function Less_Restrictions_and_Length
     (Left, Right : Meta_Subject_Type)
      return Boolean
   is
      use type Ada.Containers.Count_Type;

      L_Len : constant Ada.Containers.Count_Type
        := Left.Simultaneous_Set.Length;
      R_Len : constant Ada.Containers.Count_Type
        := Right.Simultaneous_Set.Length;
   begin
      if L_Len /= R_Len then
         return L_Len < R_Len;
      else
         if Left.Simultaneous_Set.Is_Empty then
            return Left.Length <= Right.Length;
         end if;
         declare
            L_First : constant Positive := Left.Simultaneous_Set.First_Element;
            R_First : constant Positive
              := Right.Simultaneous_Set.First_Element;
         begin
            if L_First /= R_First then
               return L_First < R_First;
            else
               return Left.Length <= Right.Length;
            end if;
         end;
      end if;
   end Less_Restrictions_and_Length;

end Types.Metas.Meta_Subject_Package;
