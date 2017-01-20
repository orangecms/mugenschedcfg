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

with Types.Basics; use Types.Basics;
with Types.Data_Structures; use Types.Data_Structures;
with Types.Integer_Vectors;
with Types.Metas.Meta_Subject_Vector_Vectors;
with Types.Positive_Set;
with Types.Positive_Vectors;

package Steps.Plan_Building_Blockers
is

   package TMM renames Types.Metas.Meta_Subject_Vector_Vectors;

   --  Returns True if no conflicting restrictions are present.
   function Restrictions_Are_Satisfiable
     (Subject_Set1 : Types.Positive_Set.Set;
      Subject_Set2 : Types.Positive_Set.Set;
      Restrictions : Execution_Restrictions_Array)
      return Boolean;

   procedure Build_Plan_With_Blockers
     (Plans                               :     All_Plans_Type;
      Current_Level                       :     Positive;
      All_Plans_Subjects_And_Metasubjects :     TMM.Vector;
      All_Level_Sizes                     :     Per_Plan_Level_Sizes_Type;
      Simultaneous_Allowed                :     Execution_Restrictions_Array;
      Per_Plan_Level_Count                :     Types.Positive_Vectors.Vector;
      Result                              : out All_Plans_Type;
      Success                             : out Boolean;
      Failed_Plans                        : out Types.Integer_Vectors.Vector);

end Steps.Plan_Building_Blockers;
