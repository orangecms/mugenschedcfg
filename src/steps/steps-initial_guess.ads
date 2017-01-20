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

with Types.Data_Structures;
with Types.Positive_Vectors;
with Types.Chain.Per_Plan_Chain_Vectors;

package Steps.Initial_Guess
is

   package TD renames Types.Data_Structures;

   --  Will return optimal lengths if score derivation is monotonically
   --  decreasing.
   procedure Guess_Length
     (Per_Plan_Chains      :     Types.Chain.Per_Plan_Chain_Vectors.Vector;
      Level_Size_Array     :     TD.Per_Plan_Level_Sizes_Type;
      Min_Sizes_List       :     TD.Per_Plan_Subject_Lengths_Type;
      All_Levels           :     TD.Per_Plan_Subject_Levels_Type;
      Per_Plan_Level_Count :     Types.Positive_Vectors.Vector;
      Plans_Subject_Length : out TD.Per_Plan_Subject_Lengths_Type);

end Steps.Initial_Guess;
