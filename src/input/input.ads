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

with Muxml;

with Types.Float_Vectors;
with Types.Positive_Vectors;
with Types.Basics;
with Types.Data_Structures;

package Input
is

   --  Process input data.
   procedure Process (File : String);

   --  Return plan count.
   function Plan_Count return Positive;

   --  Return CPU count.
   function CPU_Count return Positive;

   --  Return tick rate.
   function Tick_Rate return Positive;

   --  Return subject count.
   function Subject_Count return Positive;

   --  Return maximum level count.
   function Level_Count return Positive;

   --  Return plan weightings.
   function Plan_Weighting return Types.Float_Vectors.Vector;

   --  Return per-plan level sizes.
   function Per_Plan_Level_Sizes
     return Types.Data_Structures.Per_Plan_Level_Sizes_Type;

   --  Return per-plan level count.
   function Per_Plan_Level_Count return Types.Positive_Vectors.Vector;

   --  Return same CPU allowed configuration.
   function Same_CPU_Allowed return Types.Basics.Execution_Restrictions_Array;

   --  Return simultaneous allowed configuration.
   function Simultaneous_Allowed
     return Types.Basics.Execution_Restrictions_Array;

private

   XML_Data : Muxml.XML_Data_Type;

   Plans                : Natural := 0;
   CPUs                 : Natural := 0;
   Nr_Of_Subjects       : Natural := 0;
   Max_Levels           : Natural := 0;
   Simultaneous_Domains : Natural := 0;
   Same_CPU_Domains     : Natural := 0;
   Tickr                : Natural := 0;

   Plan_Weights : Types.Float_Vectors.Vector;

   Per_Plan_Level_Sizes_Data : Types.Data_Structures.Per_Plan_Level_Sizes_Type;

   Per_Plan_Levels : Types.Positive_Vectors.Vector;

   function CPU_Count return Positive is (CPUs);

   function Tick_Rate return Positive is (Tickr);

   function Level_Count return Positive is (Max_Levels);

   function Per_Plan_Level_Sizes
     return Types.Data_Structures.Per_Plan_Level_Sizes_Type
   is (Per_Plan_Level_Sizes_Data);

   function Plan_Count return Positive is (Plans);

   function Plan_Weighting return Types.Float_Vectors.Vector
   is (Plan_Weights);

   function Subject_Count return Positive is (Nr_Of_Subjects);

   function Per_Plan_Level_Count return Types.Positive_Vectors.Vector
   is (Per_Plan_Levels);

end Input;
