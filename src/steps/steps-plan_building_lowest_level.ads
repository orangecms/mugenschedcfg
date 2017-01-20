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
with Types.CPU_Sets;
with Types.Data_Structures;
with Types.Integer_Vectors;
with Types.Positive_Vectors;
with Types.Metas.Meta_Subject_Vectors;

package Steps.Plan_Building_Lowest_Level
is

   package TDS renames Types.Data_Structures;

   procedure Puzzle_Metasubjects
     (On_Same_CPU              :     Types.CPU_Sets.Vector;
      Subjects_Level_Plans     :     TDS.All_Plans_Level_Type;
      Metasubjects_Level_Plans :     TDS.All_Plans_Level_Type;
      All_Subjects             :     Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed     :     Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed         :     Types.Basics.Execution_Restrictions_Array;
      Level_Sizes              :     TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count     :     Types.Positive_Vectors.Vector;
      Success                  : out Boolean;
      On_Which_CPU             : out Types.Integer_Vectors.Vector;
      Plans                    : out TDS.All_Plans_Type);

   procedure Order_Strategies
     (Strategy             :        Integer;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Level_Sizes          :        TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count :        Types.Positive_Vectors.Vector;
      Subjects_To_Plan     : in out Types.Metas.Meta_Subject_Vectors.Vector;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Success              :    out Boolean;
      Plan                 :    out TDS.Level_Type);

end Steps.Plan_Building_Lowest_Level;
