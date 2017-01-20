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

with Types.Data_Structures;
with Types.Metas.Meta_Subject_Vectors;

package Auxiliary.XML
is

   --  Return XML representation of given scheduling plan.
   function To_XML
     (Plan      : Types.Data_Structures.All_Plans_Type;
      Subjects  : Types.Metas.Meta_Subject_Vectors.Vector;
      Tick_Rate : Positive)
      return String;

   --  Fill gaps with idle subjects.
   function Add_Idle_Subjects
     (Plan  : Types.Data_Structures.All_Plans_Type;
      Sizes : Types.Data_Structures.Per_Plan_Level_Sizes_Type)
      return Types.Data_Structures.All_Plans_Type;

end Auxiliary.XML;
