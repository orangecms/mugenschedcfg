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
with Types.Metas.Meta_Subject_Vector_Vectors;
with Types.Metas.Meta_Subject_Vectors;
with Types.Chain.Per_Plan_Chain_Vectors;
with Types.Integer_Vectors;

package Input.Subjects
is

   package TB renames Types.Basics;
   package TM renames Types.Metas;
   package TC renames Types.Chain;
   package TD renames Types.Data_Structures;

   procedure Get_Input
     (Per_Plan_Subjects          : out TM.Meta_Subject_Vector_Vectors.Vector;
      All_Subjects               : out TM.Meta_Subject_Vectors.Vector;
      Per_Plan_Chains            : out TC.Per_Plan_Chain_Vectors.Vector;
      Per_Plan_Subject_Lengths   : out TD.Per_Plan_Subject_Lengths_Type;
      Per_Plan_Subject_Min_Sizes : out TD.Per_Plan_Subject_Lengths_Type);

   procedure Get_On_Same_CPU (On_Same_CPU : out Types.CPU_Sets.Vector);

   --  Initialize per-plan meta-subject numbers.
   procedure Init_Meta_Numbers;

   --  Return next meta-subject number of given plan.
   function Get_Next_Meta_Number (Plan : Positive) return Positive;

private

   Per_Plan_Meta_Subject_Numbers : Types.Integer_Vectors.Vector;

end Input.Subjects;
