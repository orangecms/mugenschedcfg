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

with Ada.Text_IO;
with Ada.Containers;
with Ada.Command_Line;
with Ada.Exceptions;

with Mulog;
with Muxml;

with Input.Subjects;
with Input.Cmd_Line;
with Auxiliary.XML;
with Auxiliary.Initialization;
with Auxiliary.Print_Functions;
with Types.Minfs.Minorframes;
with Types.CPU_Sets;
with Types.Metas.Meta_Subject_Vectors;
with Types.Metas.Meta_Subject_Vector_Vectors;
with Types.Combined_With;
with Types.Integer_Vectors;
with Types.Chain.Per_Plan_Chain_Vectors;
with Types.Data_Structures;
with Types.Basics;
with Types.Metas.Meta_Subject_Package;
with Types.Float_Vectors;
with Types.Positive_Vectors;
with Steps.Create_Metasubjects;
with Steps.Plan_Building_Lowest_Level;
with Steps.Plan_Building_Metasubjects;
with Steps.Grow;
with Steps.Short;
with Steps.Combine_Metasubjects;
with Steps.Plan_Building_Blockers;
with Objective;

procedure Mugenschedcfg
is

   use Ada.Text_IO;
   use Auxiliary.Print_Functions;
   use Input.Subjects;
   use Steps.Combine_Metasubjects;
   use Steps.Plan_Building_Metasubjects;
   use Types.Data_Structures;

   All_Level_Size_Array : Per_Plan_Level_Sizes_Type;
   Per_Plan_Chains      : Types.Chain.Per_Plan_Chain_Vectors.Vector;

   All_Subjects      : Types.Metas.Meta_Subject_Vectors.Vector;
   Per_Plan_Subjects : Types.Metas.Meta_Subject_Vector_Vectors.Vector;

   All_Plans_Current_Meta_Subjects :
   Types.Metas.Meta_Subject_Vector_Vectors.Vector;

   Current_Subject_List       : Types.Metas.Meta_Subject_Vectors.Vector;
   Current_Subject_List_Copy  : Types.Metas.Meta_Subject_Vectors.Vector;
   Current_Subject_List_Copy2 : Types.Metas.Meta_Subject_Vectors.Vector;

   Subjects_Level_Array_Plans     : All_Plans_Level_Type;
   Metasubjects_Level_Array_Plans : All_Plans_Level_Type;

   Current_Meta_Subjects     : Types.Metas.Meta_Subject_Vectors.Vector;
   All_Current_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;

   Success     : Boolean;
   On_Same_CPU : Types.CPU_Sets.Vector;
   I, J        : Integer;
   Meta        : Types.Metas.Meta_Subject_Package.Meta_Subject_Type;

   Already_Shortened : Boolean := False;

   Combine_Vector       : Types.CPU_Sets.Vector;
   Reorder_Subject_List : Types.Metas.Meta_Subject_Vectors.Vector;
   Metas_To_Combine     : Types.Metas.Meta_Subject_Vectors.Vector;

   All_Plans_Subjects_And_Metasubjects :
   Types.Metas.Meta_Subject_Vector_Vectors.Vector;

   --  Shorten
   Reorder_Tries         : constant Integer := 5;
   Too_Long              : Boolean          := False;
   Short_Success         : Boolean          := True;
   Failed_In_Shorten     : Types.Combined_With.Set;
   On_Which_CPU          : Types.Integer_Vectors.Vector;
   Plans_Subject_Lengths : Per_Plan_Subject_Lengths_Type;

   --  Levelwise planning and growing
   Best_Score              : Float;
   Current_Score           : Float;
   Failed_Plans            : Types.Integer_Vectors.Vector;
   Stop_Criteria_Reached   : Boolean                  := False;
   First_Round             : Boolean                  := True;
   Min_Sizes               : Per_Plan_Subject_Lengths_Type;
   Plan_Weighting          : Types.Float_Vectors.Vector;
   Original_Plan_Weighting : Types.Float_Vectors.Vector
     := Input.Plan_Weighting;

   Debug_Output : File_Type;

begin
   Input.Cmd_Line.Init (Description => "Muen scheduling plan generator");

   Input.Process (File => Input.Cmd_Line.XML_Config_File);
   Input.Subjects.Init_Meta_Numbers;

   declare

      Plan_Count : constant Positive := Input.Plan_Count;
      subtype Plan_Range is Positive range 1 .. Plan_Count;

      CPU_Count : constant Positive := Input.CPU_Count;
      subtype CPU_Range is Natural range 0 .. CPU_Count;
      --  "0" = CPU unknown

      Subject_Count : constant Positive := Input.Subject_Count;
      subtype Subject_Range is Positive range 1 .. Subject_Count;

      Level_Count : constant Positive := Input.Level_Count;
      subtype Level_Range is Natural range 0 .. Level_Count;
      --  "0" = don't change this subject anymore

      Current_Level : Level_Range := Level_Range'Last;

      Per_Plan_Levels : Types.Positive_Vectors.Vector
        := Input.Per_Plan_Level_Count;

      --  Creates all metasubjects for the given plan
      procedure Create_Metas_For_Plan (P : Plan_Range);

      ----------------------------------------------------------------------

      procedure Create_Metas_For_Plan (P : Plan_Range)
      is
      begin
         Put_Line ("Plan" & P'Img);
         Current_Level := Per_Plan_Levels (P); --  start on highest(MAX) level

         --  Clear
         Current_Meta_Subjects.Clear;
         All_Current_Meta_Subjects.Clear;
         for L in Level_Range loop
            Metasubjects_Level_Array_Plans (P)(L).Clear;
         end loop;

         --  Move downwards till Level 1
         while Current_Level > 1 loop
            Put_Line ("Current_Level" & Current_Level'Img);

            --  Update On_CPU_With_Number (subjects with equal
            --  On_CPU_With_Numbers have to be planned on the same CPU later;
            --  Subjects will get equal On_CPU_With_Numbers
            --  if they are combined in a metasubject).

            for S of Subjects_Level_Array_Plans.Reference (Index => P)
              (Current_Level)
            loop
               S.On_CPU_With_Number := All_Subjects
                 (S.Number).On_CPU_With_Number;
            end loop;

            --  Move one level
            Steps.Create_Metasubjects.Create_Metasubjects
              (Subjects          => Subjects_Level_Array_Plans (P)
               (Current_Level),
               Number_Of_Plan    => P,
               All_Subjects      => All_Subjects,
               Meta_Subjects_In  => Current_Meta_Subjects,
               Level_Size        => All_Level_Size_Array (P),
               Current_Level     => Current_Level,
               Meta_Subjects_Out => Current_Meta_Subjects,
               All_Current_Metas => All_Current_Meta_Subjects,
               On_Same_CPU       => On_Same_CPU,
               Combine_Vector    => Combine_Vector);

            --  Update the On_CPU_With_Number of the subjects:
            I := 1;
            for C of On_Same_CPU loop
               for S of C loop
                  All_Subjects (S).On_CPU_With_Number := I;
               end loop;
               I := I + 1;
            end loop;

            Current_Level := Current_Level - 1;

            All_Current_Meta_Subjects.Append (Current_Meta_Subjects);
            Metasubjects_Level_Array_Plans (P)(Current_Level).
              Append (Current_Meta_Subjects);
         end loop;

         if All_Plans_Current_Meta_Subjects.Last_Index < P then
            All_Plans_Current_Meta_Subjects.Append (All_Current_Meta_Subjects);
         else
            All_Plans_Current_Meta_Subjects (P)
              := All_Current_Meta_Subjects;
         end if;
      end Create_Metas_For_Plan;

      Majorframes : Steps.Create_Metasubjects.Plans_Filling_Array (Plan_Range);

      Current_Ticks_Per_Subject : Objective.Ticks_Per_Subject_Type
        (Subject_Range);

      Simultaneous_Allowed : constant Types.Basics.Execution_Restrictions_Array
        := Input.Simultaneous_Allowed;
      Same_CPU_Allowed     : constant Types.Basics.Execution_Restrictions_Array
        := Input.Same_CPU_Allowed;

      All_Plans      : All_Plans_Type := Create
        (Plan_Count => Plan_Count,
         CPU_Count  => CPU_Count);
      All_Plans_Copy : All_Plans_Type := All_Plans;
      Final_Plans    : All_Plans_Type := All_Plans;
      Best_Plans     : All_Plans_Type := All_Plans;
   begin
      Mulog.Log (Msg => "Generating plans ...");
      Mulog.Log (Msg => "Writing log to '"
                 & Input.Cmd_Line.Debug_Output_File & "'");

      --  HACK: Redirect output to debug file.

      Create
        (File => Debug_Output,
         Mode => Ada.Text_IO.Out_File,
         Name => Input.Cmd_Line.Debug_Output_File);
      Set_Output (File => Debug_Output);

      --  Get input data.
      --  Note: Length guessing is done there.

      Get_Input
        (Per_Plan_Subjects          => Per_Plan_Subjects,
         All_Subjects               => All_Subjects,
         Per_Plan_Chains            => Per_Plan_Chains,
         Per_Plan_Subject_Lengths   => Plans_Subject_Lengths,
         Per_Plan_Subject_Min_Sizes => Min_Sizes);

      All_Level_Size_Array := Input.Per_Plan_Level_Sizes;

      --  Build Meta_subject-Vectors per level.

      declare
         S : constant Subject_Level_Array (Level_Range) := (others => <>);
      begin
         Subjects_Level_Array_Plans.Append
           (New_Item => S,
            Count    => Ada.Containers.Count_Type (Plan_Count));
         Metasubjects_Level_Array_Plans.Append
           (New_Item => S,
            Count    => Ada.Containers.Count_Type (Plan_Count));
      end;

      for P in Plan_Range loop
         Auxiliary.Initialization.Build_Per_Level_Subjects
           (Subject_Array => Per_Plan_Subjects (P),
            Level_Array   => Subjects_Level_Array_Plans.Reference
              (Index => P));
      end loop;

      for L in All_Level_Size_Array.Iterate loop
         Put_Line ("Level sizes for plan"
                   & Vector_Of_Natural_Arrays_Package.To_Index (L)'Img);
         Print (All_Level_Size_Array (L));
         New_Line;
      end loop;

      for P in Plan_Range loop
         Print_Subject_Vector (Per_Plan_Subjects (P));
         New_Line;

         Majorframes (P) := All_Level_Size_Array (P)(Per_Plan_Levels (P));
      end loop;

      --  Will loop at most two times (if it needs to reset to Min_Sizes).

      while not Stop_Criteria_Reached loop

         --  Reset all used structures.

         Failed_In_Shorten.Clear;
         Combine_Vector.Clear;
         for P of All_Plans loop
            for C of P loop
               C.Clear;
            end loop;
         end loop;

         Get_On_Same_CPU (On_Same_CPU => On_Same_CPU);
         All_Plans_Current_Meta_Subjects.Clear;

         for P of Metasubjects_Level_Array_Plans loop
            for L of P loop
               L.Clear;
            end loop;
         end loop;

         for P in Plan_Range loop
            for S of Per_Plan_Subjects (P) loop
               Plans_Subject_Lengths (P)(S.Number)
                 := Plans_Subject_Lengths (P)(S.Number)
                 * All_Level_Size_Array (P)(Per_Plan_Levels (P))
                 / All_Level_Size_Array (P)(S.Level);
            end loop;
         end loop;

         --  Decide which subjects will be combined to a metasubject.
         --  Criteria: Summed length of combined subjects does not use more
         --  than 0.4% of a CPU.

         Steps.Create_Metasubjects.Build_Combine_Sets
           (All_Subjects          => All_Subjects,
            Same_CPU_Allowed      => Same_CPU_Allowed,
            Majorframes           => Majorframes,
            Plans_Subject_Lengths => Plans_Subject_Lengths,
            Combine_Vector        => Combine_Vector);

         Put_Line ("Subjects to be combined");
         for C of Combine_Vector loop
            for A of C loop
               Put (A'Img);
            end loop;
            New_Line;
         end loop;

         --  Create metasubjects for all plans.

         for P in Plan_Range loop
            Create_Metas_For_Plan (P);
         end loop;

         --  Update  On_CPU_With_Numbers
         --  (subjects that are combined in a metasubject need the same
         --  On_CPU_With_Number, so that they will be plannend on the same CPU)
         --
         --  got correct: All_Subjects; On_Same_CPU
         --  maybe incorrect:On_CPU_With_Numbers of Subjects_Level_Array_plans

         for Subject_Plan of Subjects_Level_Array_Plans loop
            for Subject_Level of Subject_Plan loop
               for M of Subject_Level loop
                  M.On_CPU_With_Number := All_Subjects
                    (M.Number).On_CPU_With_Number;
               end loop;
            end loop;
         end loop;

         --  Maybe incorrect: On_CPU_With_Numbers of Meta_subject...

         for P in Plan_Range loop
            for Subject_Level of Metasubjects_Level_Array_Plans (P) loop
               for M of Subject_Level loop
                  Meta := M;
                  while Meta.Combined_Subjects.First_Element > Subject_Count
                  loop
                     Meta := All_Plans_Current_Meta_Subjects (P)
                       (Meta.Combined_Subjects.First_Element - Subject_Count);
                  end loop;
                  M.On_CPU_With_Number :=
                    All_Subjects (Meta.Combined_Subjects.First_Element)
                    .On_CPU_With_Number;
               end loop;
            end loop;
         end loop;

         --  All_Plans_Current_Meta_Subjects
         for P in Plan_Range loop
            All_Plans_Current_Meta_Subjects (P).Clear;
         end loop;

         for P in Plan_Range loop
            for L in reverse Level_Range loop
               All_Plans_Current_Meta_Subjects (P).Append
                 (Metasubjects_Level_Array_Plans (P)(L));
            end loop;
         end loop;

         for I in Plan_Range loop
            Print_Subject_Vector (All_Plans_Current_Meta_Subjects (I));
            New_Line;
         end loop;

         for C of On_Same_CPU loop
            if not C.Is_Empty then
               Put ("On same CPU");
               for L of C loop
                  Put (L'Img);
               end loop;
               New_Line;
            end if;
         end loop;

         --  Lowest level puzzle -> find CPU alloc
         --  Now all plans are on lowest level: try to fit all in

         Put_Line ("Start puzzling subjects");

         Steps.Plan_Building_Lowest_Level.Puzzle_Metasubjects
           (On_Same_CPU              => On_Same_CPU,
            Subjects_Level_Plans     => Subjects_Level_Array_Plans,
            Metasubjects_Level_Plans => Metasubjects_Level_Array_Plans,
            All_Subjects             => All_Subjects,
            Simultaneous_Allowed     => Simultaneous_Allowed,
            Same_CPU_Allowed         => Same_CPU_Allowed,
            Success                  => Success,
            Plans                    => All_Plans,
            Level_Sizes              => All_Level_Size_Array,
            Per_Plan_Level_Count     => Per_Plan_Levels,
            On_Which_CPU             => On_Which_CPU);

         --  Result possibilities:
         --  o Same_CPU causes a fail: Success=False
         --  o CPUs ok, but too long: need to check majorframe and plan
         --  o All ok, but maybe better throughput/latency in chains possible

         Put_Line ("Puzzle status " & Success'Img);

         Short_Success := False;

         if Success then

            --  Score and output the score.

            Current_Score := Objective.Calc_Objective_Score_Simple
              (Plans                  => All_Plans,
               All_Subjects           => All_Subjects,
               Per_Plan_Meta_Subjects => All_Plans_Current_Meta_Subjects,
               Per_Plan_Level_Sizes   => All_Level_Size_Array,
               Per_Plan_Chains        => Per_Plan_Chains,
               Per_Plan_Level_Count   => Per_Plan_Levels);

            Put_Line ("Score (simple)" & Current_Score'Img);

            --  Combine metasubjects on same CPU.
            --
            --  Some subjects weren't combined to metasubjects because they
            --  would have blocked a big part of one CPU (needed some options
            --  to find good CPUs for every subject).
            --  Now the CPUs per subject are fixed, so they can be combined.

            All_Plans_Copy := All_Plans;

            for P in Plan_Range loop
               Metas_To_Combine := All_Subjects;
               Types.Metas.Meta_Subject_Vectors.Append
                 (Container => Metas_To_Combine,
                  New_Item  => All_Plans_Current_Meta_Subjects (P));

               Put_Line ("Combining" & P'Img);
               Combine_Metas_Without_Loss
                 (Metas_To_Combine     => Metas_To_Combine,
                  Plan                 => All_Plans.Reference (Index => P),
                  Level_Sizes          => All_Level_Size_Array (P),
                  Simultaneous_Allowed => Simultaneous_Allowed,
                  Only_Equal           => True,
                  Change_Plan          => True);

               for S in Subject_Range loop
                  Metas_To_Combine.Delete_First;
               end loop;
               All_Plans_Current_Meta_Subjects (P) := Metas_To_Combine;
            end loop;

            Put_Line ("COMBINING Result");
            Auxiliary.Print_Functions.
              Print_Before_And_After (All_Plans_Copy, All_Plans);

            --  Plan might be too long so: Short too long plans.

            Short_Success := True;

            for P in Plan_Range loop
               Too_Long := False;
               Already_Shortened := False;

               for CPU in CPU_Range loop
                  --  If plan on one CPU is longer than majorframe of this plan
                  if not All_Plans (P)(CPU).Is_Empty and then
                    All_Plans (P)(CPU).Last_Element.Endtime
                    >= All_Level_Size_Array (P)(1)
                  then
                     Too_Long := True;
                  end if;
               end loop;

               if Too_Long then
                  Put_Line ("Plan" & P'Img & " is too long");

                  Current_Subject_List := All_Subjects;
                  --  Properties of subjects may differ in different plans
                  for S of Per_Plan_Subjects (P) loop
                     Current_Subject_List (S.Number).Length := S.Length;
                     Current_Subject_List (S.Number).Level  := S.Level;
                     Current_Subject_List (S.Number).Simultaneous_Set
                       := S.Simultaneous_Set;
                  end loop;

                  Current_Subject_List.Append
                    (All_Plans_Current_Meta_Subjects (P));

                  Current_Ticks_Per_Subject := Objective.Calc_Ticks_Per_Subject
                    (Plan                       => All_Plans (P),
                     Subjects_And_Meta_Subjects => Current_Subject_List,
                     Level_Sizes                => All_Level_Size_Array (P),
                     Majorframe                 => All_Level_Size_Array (P)
                     (Per_Plan_Levels (P)));

                  Put_Line ("Short Plan" & P'Img);
                  Steps.Short.Shorten_Plan
                    (Plan                           =>
                       All_Plans.Reference (Index => P),
                     Majorframe                     =>
                       All_Level_Size_Array (P)(Per_Plan_Levels (P)),
                     Subjects_and_Meta_Subjects_in  => Current_Subject_List,
                     Subjects_and_Meta_Subjects_out =>
                       Current_Subject_List_Copy,
                     Simultaneous_Allowed           => Simultaneous_Allowed,
                     Ticks_Per_Subject              =>
                       Current_Ticks_Per_Subject,
                     Level_Sizes                    =>
                       All_Level_Size_Array (P),
                     Chains_Vector                  => Per_Plan_Chains (P),
                     Success                        => Success);

                  for S of Per_Plan_Subjects (P) loop
                     S.Length := Current_Subject_List_Copy (S.Number).Length;
                  end loop;

                  Current_Subject_List_Copy2 := Current_Subject_List_Copy;

                  for I in 1 .. Subject_Count loop
                     Current_Subject_List_Copy.Delete_First;
                  end loop;

                  All_Plans_Current_Meta_Subjects (P).Clear;
                  All_Plans_Current_Meta_Subjects (P).Append
                    (Current_Subject_List_Copy);

                  --  If shorting fails: Reorder and try to short again.

                  J := 1;
                  while not Success and J < Reorder_Tries loop
                     Put_Line ("Shorting of plan" & P'Img & " failed (try"
                               & J'Img & " of"
                               & Positive'Image (Reorder_Tries - 1) & ")");

                     Reorder_Subject_List := Subjects_Level_Array_Plans (P)(1);

                     for M of All_Plans_Current_Meta_Subjects (P) loop
                        if M.Level = 1 then
                           Reorder_Subject_List.Append
                             (All_Plans_Current_Meta_Subjects
                                (P)(M.Number - Subject_Count));
                        end if;
                     end loop;

                     Steps.Plan_Building_Lowest_Level.Order_Strategies
                       (On_Same_CPU          => On_Same_CPU,
                        On_Which_CPU         => On_Which_CPU,
                        Subjects_To_Plan     => Reorder_Subject_List,
                        All_Subjects         => All_Subjects,
                        Simultaneous_Allowed => Simultaneous_Allowed,
                        Same_CPU_Allowed     => Same_CPU_Allowed,
                        Success              => Success,
                        Plan                 => All_Plans.Reference
                          (Index => P),
                        Level_Sizes          => All_Level_Size_Array,
                        Per_Plan_Level_Count => Per_Plan_Levels,
                        Strategy             => J);

                     All_Plans_Copy (P) := All_Plans (P);

                     if Success then
                        Current_Subject_List_Copy :=
                          Current_Subject_List_Copy2;

                        Steps.Short.Shorten_Plan
                          (Plan                           =>
                             All_Plans.Reference (Index => P),
                           Majorframe                     =>
                             All_Level_Size_Array (P)(Per_Plan_Levels (P)),
                           Subjects_and_Meta_Subjects_in  =>
                             Current_Subject_List_Copy2,
                           Subjects_and_Meta_Subjects_out =>
                             Current_Subject_List_Copy,
                           Simultaneous_Allowed           =>
                             Simultaneous_Allowed,
                           Ticks_Per_Subject              =>
                             Current_Ticks_Per_Subject,
                           Level_Sizes                    =>
                             All_Level_Size_Array (P),
                           Chains_Vector                  =>
                             Per_Plan_Chains (P),
                           Success                        => Success);

                        for S of Per_Plan_Subjects (P) loop
                           S.Length := Current_Subject_List_Copy
                             (S.Number).Length;
                        end loop;

                        Current_Subject_List_Copy2 :=
                          Current_Subject_List_Copy;

                        for I in 1 .. Subject_Count loop
                           Current_Subject_List_Copy.Delete_First;
                        end loop;

                        All_Plans_Current_Meta_Subjects (P).Clear;
                        All_Plans_Current_Meta_Subjects (P).Append
                          (Current_Subject_List_Copy);
                     end if;

                     J := J + 1;
                     if not Success and J >= Reorder_Tries then

                        if Already_Shortened then
                           --  Give up
                           Put_Line ("ALL TRIES FAILED");
                           Short_Success := False;
                           Failed_In_Shorten.Insert (P);
                        else
                           --  Change metas and give it a last try:
                           --  Set Length=Min-Length for all subjects
                           Put_Line ("MIN SIZE_STRAT IS TRIED");
                           J := 1;
                           Already_Shortened := True;
                           for L of Subjects_Level_Array_Plans (P) loop
                              for S of L loop
                                 S.Length := Min_Sizes (P)(Subject_Range
                                                           (S.Number));
                              end loop;
                           end loop;

                           --  Reset all other data

                           Input.Subjects.Init_Meta_Numbers;
                           Create_Metas_For_Plan (P);
                           Metas_To_Combine := All_Subjects;
                           Types.Metas.Meta_Subject_Vectors.Append
                             (Container => Metas_To_Combine,
                              New_Item  =>
                                All_Plans_Current_Meta_Subjects (P));

                           Put_Line ("Combining" & P'Img);
                           Combine_Metas_Without_Loss
                             (Metas_To_Combine     => Metas_To_Combine,
                              Plan                 => All_Plans.Reference
                                (Index => P),
                              Level_Sizes          => All_Level_Size_Array (P),
                              Simultaneous_Allowed => Simultaneous_Allowed,
                              Only_Equal           => False,
                              Change_Plan          => True);

                           for S in Subject_Range loop
                              Metas_To_Combine.Delete_First;
                           end loop;
                           All_Plans_Current_Meta_Subjects (P)
                             := Metas_To_Combine;
                        end if;
                     end if;
                  end loop;

                  Auxiliary.Print_Functions.Print_Subject_Vector
                    (All_Plans_Current_Meta_Subjects (P));

               end if;
            end loop;
         end if;
         Put_Line ("shorten finished");
         Put ("Failed:");
         for I of Failed_In_Shorten loop
            Put (I'Img);
         end loop;
         New_Line;
         Put_Line ("SHORTING Result");
         Print_Before_And_After (All_Plans_Copy, All_Plans);

         if Short_Success then
            if not First_Round then
               Plan_Weighting := Original_Plan_Weighting;
            end if;

            --  Combining again: Hard
            --  (Combine even if simultaneous set is only a subset)

            All_Plans_Copy := All_Plans;

            for P in Plan_Range loop
               Metas_To_Combine := All_Subjects;
               Types.Metas.Meta_Subject_Vectors.Append
                 (Container => Metas_To_Combine,
                  New_Item  => All_Plans_Current_Meta_Subjects (P));

               Put_Line ("Combining" & P'Img);
               Combine_Metas_Without_Loss
                 (Metas_To_Combine     => Metas_To_Combine,
                  Plan                 => All_Plans.Reference (Index => P),
                  Level_Sizes          => All_Level_Size_Array (P),
                  Simultaneous_Allowed => Simultaneous_Allowed,
                  Only_Equal           => False,
                  Change_Plan          => True);

               for S in Subject_Range loop
                  Metas_To_Combine.Delete_First;
               end loop;
               All_Plans_Current_Meta_Subjects (P) := Metas_To_Combine;
            end loop;
            Put_Line ("COMBINING RESULT");
            Auxiliary.Print_Functions.
              Print_Before_And_After (All_Plans_Copy, All_Plans);
            --  END COMBINING AGAIN HARD

            All_Plans_Copy := All_Plans;

            --  Update All_Plans_Subjects_And_Metasubjects
            for P in Plan_Range loop

               Metas_To_Combine := All_Subjects;
               for M of Metas_To_Combine loop
                  M.Level := 0;
               end loop;
               for S of Per_Plan_Subjects (P) loop
                  Metas_To_Combine (S.Number).Length := S.Length;
                  Metas_To_Combine (S.Number).Level := S.Level;
                  Metas_To_Combine (S.Number).Plan := P;
               end loop;

               Types.Metas.Meta_Subject_Vectors.Append
                 (Container => Metas_To_Combine,
                  New_Item  => All_Plans_Current_Meta_Subjects (P));

               for C in CPU_Range loop
                  for M of All_Plans (P)(C) loop
                     Set_CPU_For_All
                       (Meta_Number => M.Subject,
                        Subjects    => Metas_To_Combine,
                        CPU         => C);
                  end loop;
               end loop;

               All_Plans_Subjects_And_Metasubjects.Append (Metas_To_Combine);

            end loop;

            --  Build final plans out of lowest level plans and metas.

            for P in Plan_Range loop
               Metas_To_Combine := All_Plans_Subjects_And_Metasubjects (P);
               Current_Ticks_Per_Subject :=
                 Objective.Calc_Ticks_Per_Subject
                   (Plan                       => All_Plans (P),
                    Subjects_And_Meta_Subjects => Metas_To_Combine,
                    Level_Sizes                => All_Level_Size_Array (P),
                    Majorframe                 => All_Level_Size_Array (P)
                    (Per_Plan_Levels (P)));
               --  Grow level 1
               Steps.Grow.Grow_One_Level_Steady_Order
                 (All_Chains          => Per_Plan_Chains (P),
                  Plan                => All_Plans.Reference (Index => P),
                  Meta_and_Subjects   => Metas_To_Combine,
                  Ticks_Per_Subject   => Current_Ticks_Per_Subject,
                  Level_Sizes         => All_Level_Size_Array (P),
                  Majorframe          => All_Level_Size_Array (P)
                  (Per_Plan_Levels (P)),
                  Simultaneous_Allowed => Simultaneous_Allowed);
            end loop;

            --  Output more regular if Combined_Sujects reversed
            for A of All_Plans_Current_Meta_Subjects loop
               for M of A loop
                  M.Combined_Subjects.Reverse_Elements;
                  M.Tick_Vector.Reverse_Elements;
               end loop;
            end loop;

            Current_Level := 2;

            Build_Plans_From_Metasubjects
              (Lowest_Level_Plan   => All_Plans,
               All_Subjects        => All_Subjects,
               Meta_Subjects_Plans => All_Plans_Current_Meta_Subjects,
               All_Level_Sizes     => All_Level_Size_Array,
               Final_Plans         => Final_Plans,
               Failed_In_Shorten   => Failed_In_Shorten,
               Per_Plan_Levels     => Per_Plan_Levels);

            Current_Score := Objective.Calc_Objective_Score_Split
              (Plans                  => Final_Plans,
               All_Subjects           => All_Subjects,
               Per_Plan_Meta_Subjects => All_Plans_Current_Meta_Subjects,
               Per_Plan_Chains        => Per_Plan_Chains);
            Best_Score := Current_Score;
            Best_Plans := Final_Plans;
            Success := True;
            Print_Final (Final_Plans, All_Subjects);
            Put_Line ("Score without puzzle every level" & Current_Score'Img);

            --  Try to make final plans levelwise.
            --
            --  Subjects are planned at next higher level with subjects of
            --  lower levels blocking. After planning, the chains on this level
            --  are grown.

            I := Level_Count;
            while Success and I > 1 loop
               --  Plan and Grow a Level
               I := I - 1;
               --  Plan A Level
               Steps.Plan_Building_Blockers.Build_Plan_With_Blockers
                 (Plans                               => All_Plans,
                  Result                              => All_Plans_Copy,
                  Current_Level                       => Current_Level,
                  All_Plans_Subjects_And_Metasubjects =>
                    All_Plans_Subjects_And_Metasubjects,
                  All_Level_Sizes                     => All_Level_Size_Array,
                  Simultaneous_Allowed                => Simultaneous_Allowed,
                  Success                             => Success,
                  Failed_Plans                        => Failed_Plans,
                  Per_Plan_Level_Count                => Per_Plan_Levels);

               if Success then

                  --  Subjects aren't changed in Build_Plan_With_Blockers
                  --  only has to adjust the All_Level_Size_Array
                  for P in Plan_Range loop
                     if Current_Level <= Per_Plan_Levels (P) and then not
                       Types.Integer_Vectors.Contains (Failed_Plans, P)
                     then
                        Per_Plan_Levels (P) := Per_Plan_Levels (P) - 1;
                        for L in Level_Range (1) .. Level_Range
                          (Level_Count - 1)
                        loop
                           All_Level_Size_Array (P)(L) :=
                             All_Level_Size_Array (P)(L + 1);
                        end loop;
                        All_Level_Size_Array (P)
                          (Level_Range (Level_Count)) := 0;

                        for S of All_Plans_Subjects_And_Metasubjects (P)
                        loop
                           if S.Level > 0 then
                              S.Level := S.Level - 1;
                           end if;
                        end loop;

                        for S of All_Plans_Current_Meta_Subjects (P) loop
                           if S.Level > 0 then
                              S.Level := S.Level - 1;
                           end if;
                        end loop;
                     end if;

                     --  Grow a level.

                     Metas_To_Combine :=
                       All_Plans_Subjects_And_Metasubjects (P);
                     Current_Ticks_Per_Subject :=
                       Objective.Calc_Ticks_Per_Subject
                         (Plan                       => All_Plans_Copy (P),
                          Subjects_And_Meta_Subjects => Metas_To_Combine,
                          Level_Sizes                =>
                            All_Level_Size_Array (P),
                          Majorframe                 =>
                            All_Level_Size_Array (P)
                          (Per_Plan_Levels (P)));

                     --  Grow level 1
                     Steps.Grow.Grow_One_Level_Steady_Order
                       (All_Chains           => Per_Plan_Chains (P),
                        Plan                 => All_Plans_Copy.Reference
                          (Index => P),
                        Meta_and_Subjects    => Metas_To_Combine,
                        Ticks_Per_Subject    => Current_Ticks_Per_Subject,
                        Level_Sizes          => All_Level_Size_Array (P),
                        Majorframe           => All_Level_Size_Array (P)
                        (Per_Plan_Levels (P)),
                        Simultaneous_Allowed => Simultaneous_Allowed);
                  end loop;

                  All_Plans := All_Plans_Copy;

                  --  Build the other levels out of metasubjects
                  for P in Plan_Range loop
                     for S of All_Plans_Current_Meta_Subjects (P) loop
                        if S.Level = 0 then
                           S.Level := 1;
                        end if;
                     end loop;

                  end loop;
                  Build_Plans_From_Metasubjects
                    (Lowest_Level_Plan   => All_Plans_Copy,
                     All_Subjects        => All_Subjects,
                     Meta_Subjects_Plans => All_Plans_Current_Meta_Subjects,
                     All_Level_Sizes     => All_Level_Size_Array,
                     Final_Plans         => Final_Plans,
                     Failed_In_Shorten   => Failed_In_Shorten,
                     Per_Plan_Levels     => Per_Plan_Levels);

                  Current_Score := Objective.Calc_Objective_Score_Split
                    (Plans                  => Final_Plans,
                     All_Subjects           => All_Subjects,
                     Per_Plan_Meta_Subjects => All_Plans_Current_Meta_Subjects,
                     Per_Plan_Chains        => Per_Plan_Chains);

                  Put_Line ("------------Plan ON A Level--------------------");
                  Print_Final (Final_Plans, All_Subjects);
                  Put_Line ("Score on a Level" & Current_Score'Img);

                  if Current_Score >= Best_Score then
                     Put_Line ("BEST WAS UPDATED");
                     Best_Score := Current_Score;
                     Best_Plans := Final_Plans;
                  end if;
               end if;

            end loop;
            if not Success then
               Put_Line ("FAILED PUZZLE");
            end if;

            --  HACK: OUTPUT BEST PLAN to file

            Put_Line ("Writing final plan to '"
                      & Input.Cmd_Line.Plan_Raw_File & "'");

            declare
               Final_Plan : File_Type;
            begin
               Create
                 (File => Final_Plan,
                  Mode => Ada.Text_IO.Out_File,
                  Name => Input.Cmd_Line.Plan_Raw_File);
               Set_Output (File => Final_Plan);

               Print_Final (Best_Plans, All_Subjects);

               Close (File => Final_Plan);
               Set_Output (File => Debug_Output);
            end;

            declare
               Filename   : constant String
                 := Input.Cmd_Line.Plan_Raw_File & ".xml";
               Final_Plan : File_Type;
            begin
               Put_Line ("Writing XML scheduling plan to '" & Filename & "'");

               Create
                 (File => Final_Plan,
                  Mode => Ada.Text_IO.Out_File,
                  Name => Filename);

               Put (File => Final_Plan,
                    Item => Auxiliary.XML.To_XML
                      (Plan      => Auxiliary.XML.Add_Idle_Subjects
                         (Plan  => Best_Plans,
                          Sizes => All_Level_Size_Array),
                       Subjects  => All_Subjects,
                       Tick_Rate => Input.Tick_Rate));
               Close (File => Final_Plan);
            end;

            --  Output score and idle times.

            Put_Line (Best_Score'Img);
            Print_CPU_Table
              (Final_Plans          => Final_Plans,
               Level_Array          => All_Level_Size_Array,
               Per_Plan_Level_Count => Per_Plan_Levels,
               All_Subjects         => All_Subjects,
               On_Which_CPU         => On_Which_CPU);
         end if;

         if not Short_Success and First_Round then

            --  Try heuristic with min lengths instead of guessed lengths.

            First_Round := False;
            --  Cut lengths to min!
            Plans_Subject_Lengths := Min_Sizes;
            --  All subjects per plan..
            for P in Plan_Range loop
               for S of Per_Plan_Subjects (P) loop
                  S.CPU := 0;
                  S.Length := Min_Sizes (P)(Subject_Range (S.Number));
                  S.On_CPU_With_Number := S.Number;
                  S.Order_Number := 0;
               end loop;
               Print_Subject_Vector (Per_Plan_Subjects (P));
            end loop;
            Input.Subjects.Init_Meta_Numbers;

            for P in Plan_Range loop
               Auxiliary.Initialization.Build_Per_Level_Subjects
                 (Subject_Array => Per_Plan_Subjects (P),
                  Level_Array   => Subjects_Level_Array_Plans.Reference
                    (Index => P));
            end loop;

            Original_Plan_Weighting := Plan_Weighting;
            for P of Plan_Weighting loop
               P := 1.0;
            end loop;
         else
            Stop_Criteria_Reached := True;
            if not Short_Success then
               Put_Line ("HEURISTIC FAILED");
            end if;
         end if;
      end loop;
   end;

   Set_Output (Standard_Output);
   Close (File => Debug_Output);

exception
   when Input.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Muxml.Validation_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Processing failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Set_Output (Standard_Output);
      if Is_Open (File => Debug_Output) then
         Close (File => Debug_Output);
      end if;
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mugenschedcfg;
