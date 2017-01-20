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

with Types.Combined_With;
with Types.Data_Structures;
with Types.Positive_Vectors;
with Types.Metas.Meta_Subject_Vectors;
with Types.Metas.Meta_Subject_Vector_Vectors;

package Steps.Plan_Building_Metasubjects
is

   package TDS renames Types.Data_Structures;

   procedure Build_Plans_From_Metasubjects
     (Lowest_Level_Plan   :     TDS.All_Plans_Type;
      All_Subjects        :     Types.Metas.Meta_Subject_Vectors.Vector;
      Meta_Subjects_Plans :     Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      All_Level_Sizes     :     TDS.Per_Plan_Level_Sizes_Type;
      Failed_In_Shorten   :     Types.Combined_With.Set;
      Per_Plan_Levels     :     Types.Positive_Vectors.Vector;
      Final_Plans         : out TDS.All_Plans_Type);

end Steps.Plan_Building_Metasubjects;
