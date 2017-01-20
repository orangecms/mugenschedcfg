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
with Types.Minfs.Minorframes;
with Objective;

package Steps.Short
is

   package TB  renames Types.Basics;
   package TCV renames Types.Chain.Chains_Vectors;
   package TMV renames Types.Metas.Meta_Subject_Vectors;

   type Iterator_Array_Type is array (Positive range <>) of
     Types.Minfs.Minorframes.Cursor;
   type time_Array_Type is array (Positive range <>) of Integer
     with Default_Component_Value => Integer'Last;
   type Next_CPU_Type is array (Positive range <>) of Integer
     with Default_Component_Value => -1;

   procedure Shorten_Plan
     (Majorframe                     :        Integer;
      Subjects_and_Meta_Subjects_in  :        TMV.Vector;
      Simultaneous_Allowed           :        TB.Execution_Restrictions_Array;
      Level_Sizes                    :        TB.Natural_Array;
      Chains_Vector                  :        TCV.Vector;
      Ticks_Per_Subject              : in out Objective.Ticks_Per_Subject_Type;
      Plan                           : in out Types.Data_Structures.Level_Type;
      Subjects_and_Meta_Subjects_out : out    TMV.Vector;
      Success                        : out    Boolean);

   procedure Move_Left (M : in out Types.Minfs.Minorframe_Type);

end Steps.Short;
