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

with Types.Basics;
with Types.Data_Structures;
with Types.Integer_Vectors;
with Types.Positive_Vectors;
with Types.Metas.Meta_Subject_Package;
with Types.Metas.Meta_Subject_Vectors;

package Auxiliary.Print_Functions
is

   package TMS renames Types.Metas.Meta_Subject_Package;

   procedure Print (L : Types.Basics.Natural_Array);

   procedure Print_Level_no_subj (Level : Types.Data_Structures.Level_Type);

   procedure print_one_Subject (S : TMS.Meta_Subject_Type);

   procedure Print_Subject_Vector
     (V : Types.Metas.Meta_Subject_Vectors.Vector);

   procedure Print_Before_And_After
     (Before : Types.Data_Structures.All_Plans_Type;
      After  : Types.Data_Structures.All_Plans_Type);

   procedure Print_Final
     (Plan         : Types.Data_Structures.All_Plans_Type;
      All_Subjects : Types.Metas.Meta_Subject_Vectors.Vector);

   procedure Print_CPU_Table
     (Final_Plans          : Types.Data_Structures.All_Plans_Type;
      Level_Array          : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count : Types.Positive_Vectors.Vector;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      On_Which_CPU         : Types.Integer_Vectors.Vector);

end Auxiliary.Print_Functions;
