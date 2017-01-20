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
with Types.Metas.Meta_Subject_Vectors;

package Steps.Combine_Metasubjects
is

   --  Once for every plan
   --  Combine metasubjects if there is enough space in them and
   --  only_equal: they have the same simultaneous_Set
   --  not only equal: the one with more restrictions has enough space to put
   --  the other into it
   --  goal: using space in metasubjects that would have been empty otherwise
   procedure Combine_Metas_Without_Loss
     (Level_Sizes          :        Types.Basics.Natural_Array;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Only_Equal           :        Boolean;
      Change_Plan          :        Boolean;
      Metas_To_Combine     : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Plan                 : in out Types.Data_Structures.Level_Type);

   procedure Set_CPU_For_All
     (Meta_Number :        Positive;
      CPU         :        Positive;
      Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector);

end Steps.Combine_Metasubjects;
