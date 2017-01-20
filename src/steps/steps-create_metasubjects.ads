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
with Types.CPU_Sets;
with Types.Metas.Meta_Subject_Vectors;

package Steps.Create_Metasubjects
is

   package TDS renames Types.Data_Structures;

   type Plans_Filling_Array is array (Positive range <>) of Integer;

   procedure Create_Metasubjects
     (Subjects          :        Types.Metas.Meta_Subject_Vectors.Vector;
      All_Subjects      :        Types.Metas.Meta_Subject_Vectors.Vector;
      All_Current_Metas :        Types.Metas.Meta_Subject_Vectors.Vector;
      Meta_Subjects_In  :        Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Size        :        Types.Basics.Natural_Array;
      Number_Of_Plan    :        Positive;
      Current_Level     :        Positive;
      Combine_Vector    :        Types.CPU_Sets.Vector;
      On_Same_CPU       : in out Types.CPU_Sets.Vector;
      Meta_Subjects_Out :    out Types.Metas.Meta_Subject_Vectors.Vector);

   --  Combine subjects so that the summed Length of them is less or equal a
   --  Treshold_Factor
   procedure Build_Combine_Sets
     (All_Subjects          :     Types.Metas.Meta_Subject_Vectors.Vector;
      Same_CPU_Allowed      :     Types.Basics.Execution_Restrictions_Array;
      Majorframes           :     Plans_Filling_Array;
      Plans_Subject_Lengths :     TDS.Per_Plan_Subject_Lengths_Type;
      Combine_Vector        : out Types.CPU_Sets.Vector);

end Steps.Create_Metasubjects;
