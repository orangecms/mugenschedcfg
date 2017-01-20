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

package body Types.Data_Structures
is

   -------------------------------------------------------------------------

   function Create (Plan_Count, CPU_Count : Positive) return All_Plans_Type
   is
      P : All_Plans_Type;
      L : constant Level_Type (0 .. CPU_Count) := (others => <>);
   begin
      P.Append
        (New_Item => L,
         Count    => Ada.Containers.Count_Type (Plan_Count));
      return P;
   end Create;

end Types.Data_Structures;
