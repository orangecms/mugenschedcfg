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

with Ada.Containers.Indefinite_Vectors;

with Types.Basics;
with Types.Minfs.Minorframes;
with Types.Metas.Meta_Subject_Vectors;

package Types.Data_Structures
is

   type Level_Type is array (Natural range <>) of Types.Minfs.Minorframes.List;

   package All_Plans_Package is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Level_Type);

   type All_Plans_Type (<>) is new All_Plans_Package.Vector with private;

   --  Create all plans structure for given plan/cpu count.
   function Create (Plan_Count, CPU_Count : Positive) return All_Plans_Type;

   type Subject_Level_Array is array (Natural range <>) of
     Types.Metas.Meta_Subject_Vectors.Vector;

   package All_Plans_Level_Package is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Subject_Level_Array);

   type All_Plans_Level_Type is new
     All_Plans_Level_Package.Vector with private;

   use type Types.Basics.Natural_Array;

   package Vector_Of_Natural_Arrays_Package is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Types.Basics.Natural_Array);

   type Per_Plan_Level_Sizes_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with private;

   type Per_Plan_Subject_Levels_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with private;

   type Per_Plan_Subject_Lengths_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with private;

private

   type All_Plans_Type is new All_Plans_Package.Vector with null record;

   type All_Plans_Level_Type is new
     All_Plans_Level_Package.Vector with null record;

   type Per_Plan_Level_Sizes_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with null record;

   type Per_Plan_Subject_Levels_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with null record;

   type Per_Plan_Subject_Lengths_Type is new
     Vector_Of_Natural_Arrays_Package.Vector with null record;

end Types.Data_Structures;
