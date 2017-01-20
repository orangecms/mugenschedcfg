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
with Types.Metas.Meta_Subject_Vectors;
with Types.Metas.Meta_Subject_Vector_Vectors;
with Types.Chain.Chains_Vectors;
with Types.Data_Structures;
with Types.Chain.Per_Plan_Chain_Vectors;
with Types.Positive_Vectors;

package Objective
is

   type Ticks_Per_Subject_Type is array (Positive range <>) of Integer
     with Default_Component_Value => 0;

   function Calc_Objective_Score_Simple
     (Plans                  : Types.Data_Structures.All_Plans_Type;
      All_Subjects           : Types.Metas.Meta_Subject_Vectors.Vector;
      Per_Plan_Meta_Subjects : Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      Per_Plan_Level_Sizes   : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      Per_Plan_Chains        : Types.Chain.Per_Plan_Chain_Vectors.Vector;
      Per_Plan_Level_Count   : Types.Positive_Vectors.Vector)
      return Float;

   function Calc_Objective_Score_Split
     (Plans                  : Types.Data_Structures.All_Plans_Type;
      All_Subjects           : Types.Metas.Meta_Subject_Vectors.Vector;
      Per_Plan_Meta_Subjects : Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      Per_Plan_Chains        : Types.Chain.Per_Plan_Chain_Vectors.Vector)
      return Float;

   function Calc_Ticks_Per_Subject
     (Plan                       : Types.Data_Structures.Level_Type;
      Subjects_And_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Sizes                : Types.Basics.Natural_Array;
      Majorframe                 : Integer)
      return Ticks_Per_Subject_Type;

   function Calc_Score_Of_One_Plan
     (Ticks_Per_Subject : Ticks_Per_Subject_Type;
      Chains_Vector     : Types.Chain.Chains_Vectors.Vector)
      return Float;

end Objective;
