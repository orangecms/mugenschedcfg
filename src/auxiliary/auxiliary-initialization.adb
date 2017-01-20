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

package body Auxiliary.Initialization
is

   -------------------------------------------------------------------------

   procedure Build_Per_Level_Subjects
     (Subject_Array :     Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Array   : out Types.Data_Structures.Subject_Level_Array)
   is
   begin
      for L of Level_Array loop
         L.Clear;
      end loop;

      for S of Subject_Array loop
         Level_Array (S.Level).Append (S);
      end loop;
   end Build_Per_Level_Subjects;

end Auxiliary.Initialization;
