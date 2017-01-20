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
with Types.Chain.Chains_Vectors;
with Types.Data_Structures;
with Types.Metas.Meta_Subject_Vectors;
with Objective;

package Steps.Grow
is

   --  Grows plan at Level 1, ONLY chains with at least one subject on level 1
   --  get bigger
   --  Chosen: greedy
   procedure Grow_One_Level_Steady_Order
     (All_Chains           :        Types.Chain.Chains_Vectors.Vector;
      Level_Sizes          :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject    : in out Objective.Ticks_Per_Subject_Type);

end Steps.Grow;
