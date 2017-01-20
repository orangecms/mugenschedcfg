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

with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Input;
with Types.Minfs.Minorframes;

package body Auxiliary.Print_Functions
is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   J : Integer;

   My_All_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;

   procedure Print_Minorframes_b
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer);

   procedure Print_Minorframes
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer);

   procedure Print_Minorframes_no_subj
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer);

   procedure Print_Minorframe_b
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer);

   procedure Print_Minorframe
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer);

   procedure Print_Minorframe_No_Subj
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer);

   procedure Print_Level_No_Reset_B (Level : Types.Data_Structures.Level_Type);

   procedure Print_Level_No_Reset (Level : Types.Data_Structures.Level_Type);

   ------------------------------------------------------------------------

   procedure Print (L : Types.Basics.Natural_Array)
   is
   begin
      for I in L'Range loop
         Put_Line (I'Img & L (I)'Img);
      end loop;
   end Print;

   ------------------------------------------------------------------------

   procedure Print_Before_And_After
     (Before : Types.Data_Structures.All_Plans_Type;
      After  : Types.Data_Structures.All_Plans_Type)
   is
   begin
      for P in Positive range 1 .. Positive (Before.Length) loop
         Put_Line ("Plan" & P'Img & " before");
         Print_Level_No_Reset_B (Before (P));
         Put_Line ("Plan" & P'Img & " after");
         Print_Level_No_Reset_B (After (P));
      end loop;
   end Print_Before_And_After;

   ------------------------------------------------------------------------

   --  Prints a table with all CPU-Amounts
   procedure Print_CPU_Table
     (Final_Plans          : Types.Data_Structures.All_Plans_Type;
      Level_Array          : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count : Types.Positive_Vectors.Vector;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      On_Which_CPU         : Types.Integer_Vectors.Vector)
   is
      subtype Plan_Range    is Positive range 1 .. Input.Plan_Count;
      subtype CPU_Range     is Natural  range 0 .. Input.CPU_Count;
      subtype Subject_Range is Positive range 1 .. Input.Subject_Count;

      type CPU_Amount_Array is array (Subject_Range) of Float
        with Default_Component_Value => 0.0;

      type Ticks_Array is array (Subject_Range) of Integer
        with Default_Component_Value => 0;

      type All_CPU_Amount_Arrays is array (Plan_Range)
        of CPU_Amount_Array;

      type Sum_CPU_Array is array (CPU_Range) of Integer
        with Default_Component_Value => 0;

      type All_Sum_CPU_Arrays is array (Plan_Range) of
        Sum_CPU_Array;

      type Idle_CPU_Array is array (CPU_Range) of Float
        with Default_Component_Value => 0.0;

      type All_Idle_CPU_Arrays is array (Plan_Range) of
        Idle_CPU_Array;

      All_CPU_Amounts : All_CPU_Amount_Arrays;
      Majorframe      : Integer;
      Sum_Ticks       : All_Sum_CPU_Arrays;
      Ticks           : Ticks_Array;
      All_idle_CPU    : All_Idle_CPU_Arrays;
      CPU             : Integer;
   begin
      for P in Positive range 1 .. Input.Plan_Count loop
         Majorframe := Level_Array (P)(Per_Plan_Level_Count (P));
         for S in Subject_Range loop
            Ticks (S) := 0;
         end loop;

         for C in CPU_Range loop
            for M of Final_Plans (P)(C) loop
               Ticks (Subject_Range (M.Subject)) :=
                 Ticks (Subject_Range (M.Subject)) + M.Endtime
                   - M.Starttime + 1;
               Sum_Ticks (P)(C) := Sum_Ticks (P)(C)
                 + M.Endtime - M.Starttime + 1;
            end loop;
         end loop;

         for S in Subject_Range loop
            All_CPU_Amounts (P)(S) :=
              Float (Ticks (S)) / Float (Majorframe);
         end loop;
         for C in CPU_Range loop
            All_idle_CPU (P)(C) := Float (Majorframe - Sum_Ticks (P)(C))
              / Float (Majorframe);
         end loop;
      end loop;

      for S in Subject_Range loop
         CPU := On_Which_CPU (All_Subjects (S)
                           .On_CPU_With_Number);
         Put (To_String (All_Subjects (S).Name) & " "
              & CPU'Img & " ");
         for P of All_CPU_Amounts loop
            Ada.Float_Text_IO.Put (P (S), 1, 10, -0);
            Put (" ");
         end loop;
         New_Line;
      end loop;
      New_Line;
      Put_Line ("idle times on CPUs");

      for C in CPU_Range loop
         if C /= 0 then
            Put ("CPU" & C'Img & " ");
            for P of All_idle_CPU loop
               Ada.Float_Text_IO.Put (P (C), 1, 10, -0);
               Put (" ");
            end loop;
            New_Line;
         end if;
      end loop;
   end Print_CPU_Table;

   ------------------------------------------------------------------------

   procedure Print_Final
     (Plan         : Types.Data_Structures.All_Plans_Type;
      All_Subjects : Types.Metas.Meta_Subject_Vectors.Vector)
   is
   begin
      My_All_Subjects := All_Subjects;
      J := 0;
      for P in Positive range 1 .. Input.Plan_Count loop
         Put_Line ("------ [" & P'Img & "] ------");
         J := P - 1;
         Print_Level_No_Reset (Plan (P));
      end loop;
   end Print_Final;

   -------------------------------------------------------------------------

   procedure Print_Level_No_Reset (Level : Types.Data_Structures.Level_Type)
   is
      CPU_ID : Natural := 0;
   begin
      for M of Level loop

         --  TODO: 0 level is empty because it is not a valid CPU
         --  (see CPU_Range).

         Print_Minorframes (M, CPU_ID);
         CPU_ID := CPU_ID + 1;
      end loop;
   end Print_Level_No_Reset;

   -------------------------------------------------------------------------

   procedure Print_Level_No_Reset_B (Level : Types.Data_Structures.Level_Type)
   is
      CPU_ID : Natural := 0;
   begin
      for M of Level loop
         Print_Minorframes_b (M, CPU_ID);
         CPU_ID := CPU_ID + 1;
      end loop;
   end Print_Level_No_Reset_B;

   -------------------------------------------------------------------------

   procedure Print_Level_no_subj (Level : Types.Data_Structures.Level_Type)
   is
      I : Integer := -1;
   begin
      J := 0;
      for M of Level loop
         I := I + 1;

         if I > 0 then
            Print_Minorframes_no_subj (M, I);
         end if;
      end loop;
   end Print_Level_no_subj;

   -------------------------------------------------------------------------

   procedure Print_Minorframe
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer)
   is
   begin
      Put_Line
        (To_String (My_All_Subjects (M.Subject).Name) & " "
         & Integer'Image (M.Subject + J * Input.Subject_Count) & " "
         & CPU'Img & " " & M.Starttime'Img & " " & M.Endtime'Img);
   end Print_Minorframe;

   -------------------------------------------------------------------------

   procedure Print_Minorframe_b
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer)
   is
   begin
      J := J + 1;
      Put_Line (M.Subject'Img & " " & J'Img & " " & CPU'Img & " "
                & M.Starttime'Img & " " & M.Endtime'Img);
   end Print_Minorframe_b;

   -------------------------------------------------------------------------

   procedure Print_Minorframe_No_Subj
     (M   : Types.Minfs.Minorframe_Type;
      CPU : Integer)
   is
   begin
      Put (M.Subject'Img & " "
           & Integer'Image (M.Subject + J * Input.Subject_Count) & " "
           & CPU'Img & " "
           & M.Starttime'Img & " "
           & M.Endtime'Img);
      Put ("|");
      for S of M.Simultaneous_Set loop
         Put (S'Img);
      end loop;
      New_Line;
   end Print_Minorframe_No_Subj;

   -------------------------------------------------------------------------

   procedure Print_Minorframes
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer)
   is
   begin
      for M of Min loop
         Print_Minorframe (M, CPU);
      end loop;
   end Print_Minorframes;

   -------------------------------------------------------------------------

   procedure Print_Minorframes_b
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer)
   is
   begin
      for M of Min loop
         Print_Minorframe_b (M, CPU);
      end loop;
   end Print_Minorframes_b;

   -------------------------------------------------------------------------

   procedure Print_Minorframes_no_subj
     (Min : Types.Minfs.Minorframes.List;
      CPU : Integer)
   is
   begin
      for M of Min loop
         Print_Minorframe_No_Subj (M, CPU);
      end loop;
   end Print_Minorframes_no_subj;

   -------------------------------------------------------------------------

   procedure print_one_Subject (S : TMS.Meta_Subject_Type)
   is
   begin
      Put (S.Plan'Img & " " & S.Number'Img & " " & To_String (S.Name) & " "
           & S.Level'Img & " " & S.Length'Img & " ");
      for R of S.Simultaneous_Set loop
         Put (R'Img & ",");
      end loop;
      Put ("|");
      for R of S.Same_CPU_Set loop
         Put (R'Img & ",");
      end loop;
      Put ("|");
      for R of S.Combined_Subjects loop
         Put (R'Img & ",");
      end loop;
      Put ("|");
      for R of S.Tick_Vector loop
         Put (R'Img & ",");
      end loop;

      New_Line;
   end print_one_Subject;

   -------------------------------------------------------------------------

   procedure Print_Subject_Vector (V : Types.Metas.Meta_Subject_Vectors.Vector)
   is
   begin
      Put_Line ("Plan Nbr Name Lvl Le Sim | CPU | Combines|Ticks");
      for S of V loop
         print_one_Subject (S);
      end loop;
   end Print_Subject_Vector;

end Auxiliary.Print_Functions;
