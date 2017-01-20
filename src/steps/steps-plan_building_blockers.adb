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

with Ada.Text_IO; use Ada.Text_IO;

with Input;
with Types.Metas.Meta_Subject_Vectors;
with Types.Minfs.Minorframes;
with Types.Metas.Meta_Subject_Restr_Length_Sorter;
with Types.Minfs.Minorframes_Iterator_Vectors;
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Auxiliary.Print_Functions; use Auxiliary.Print_Functions;

package body Steps.Plan_Building_Blockers
is

   type Per_CPU_Gap_Array is array (Natural range <>) of
     Types.Minfs.Minorframes_Iterator_Vectors.Vector;

   procedure Use_Gaps_As_They_Come
     (CPU                  :        Natural;
      Simultaneous_Allowed :        Execution_Restrictions_Array;
      Subject              :        Meta_Subject_Type;
      Length               :        Integer;
      Plan_Length          :        Integer;
      Plan                 : in out Level_Type;
      All_Gaps_Vectors     : in out Per_CPU_Gap_Array;
      Success              :    out Boolean);

   procedure Plan_Subject_At_Given_Place
     (CPU              :        Natural;
      Gap_Nbr          :        Integer;
      Starttime        :        Integer;
      Subject          :        Meta_Subject_Type;
      Length           :        Integer;
      Plan             : in out Level_Type;
      All_Gaps_Vectors : in out Per_CPU_Gap_Array);

   procedure Plan_Subject_On_Given_CPU
     (CPU                  :        Natural;
      Simultaneous_Allowed :        Execution_Restrictions_Array;
      Subject              :        Meta_Subject_Type;
      Length               :        Integer;
      Plan_Length          :        Integer;
      Plan                 : in out Level_Type;
      All_Gaps_Vectors     : in out Per_CPU_Gap_Array;
      Success              :    out Boolean);

   -------------------------------------------------------------------------

   procedure Build_Plan_With_Blockers
     (Plans                               :     All_Plans_Type;
      Current_Level                       :     Positive;
      All_Plans_Subjects_And_Metasubjects :     TMM.Vector;
      All_Level_Sizes                     :     Per_Plan_Level_Sizes_Type;
      Simultaneous_Allowed                :     Execution_Restrictions_Array;
      Per_Plan_Level_Count                :     Types.Positive_Vectors.Vector;
      Result                              : out All_Plans_Type;
      Success                             : out Boolean;
      Failed_Plans                        : out Types.Integer_Vectors.Vector)
   is
      use Types.Minfs;
      use type Types.Positive_Set.Set;
      use type Types.Minfs.Minorframes.Cursor;

      subtype Plan_Range is Positive range 1 .. Input.Plan_Count;

      CPU_Count : constant Positive := Input.CPU_Count;
      subtype CPU_Range       is Natural  range 0 .. CPU_Count;
      subtype Valid_CPU_Range is Positive range 1 .. CPU_Count;

      subtype All_CPU_Gap_Array is Per_CPU_Gap_Array (CPU_Range);

      type Gaps_All_Plans_Arrays is array (Plan_Range) of
        All_CPU_Gap_Array;

      type Iterators_Array is array (Valid_CPU_Range) of
        Minorframes.Cursor;
      type Time_Array is array (Valid_CPU_Range) of Integer
        with Default_Component_Value => -1;

      type Starttime_Important_Array is array (Valid_CPU_Range) of Boolean
        with Default_Component_Value => True;

      type Sim_Set_Per_Cpu_Array is array (Valid_CPU_Range)
        of Types.Positive_Set.Set;

      Gaps_All_Plans      : Gaps_All_Plans_Arrays;
      Plans_Copy          : All_Plans_Type := Plans;
      Need_To_Plan        : Types.Metas.Meta_Subject_Vectors.Vector;
      Current_Sim         : Types.Positive_Set.Set;
      Starttime_Important : Starttime_Important_Array;
      Sim_Set_Per_Cpu     : Sim_Set_Per_Cpu_Array;
      Endtimes            : Time_Array;

      Iterators, All_Iterators : Iterators_Array;

      Time_Till : Integer := -1;

      M, N : Minorframes.Cursor;

      Minf, Gap : Minorframe_Type;

      Done, Noone_Failed, Single_Success : Boolean;
   begin
      Success := False;
      Failed_Plans.Clear;

      for P in Plan_Range loop
         Put_Line ("original Plan");
         Auxiliary.Print_Functions.Print_Level_no_subj (Plans_Copy (P));
         for C in CPU_Range loop
            M := Plans_Copy (P)(C).First;
            while M /= Minorframes.No_Element
            loop
               N := Minorframes.Next (M);
               if Minorframes.Element (M).Subject >
                 Input.Subject_Count
               then
                  Plans_Copy (P)(C).Delete (M);
               end if;
               M := N;
            end loop;
         end loop;
      end loop;

      --  Create a minorframe for every gap with the restrictions there
      --  Save cursors of the "gap-minfs"(identification via subject=-1)
      Gap.Subject := -1;
      for P in Plan_Range loop
         if not (Per_Plan_Level_Count (P) < Current_Level) then
            for C in 1 .. CPU_Count loop
               Endtimes (C) := -1;
               Sim_Set_Per_Cpu (C).Clear;
            end loop;
            Done := False;

            for C in 1 .. CPU_Count loop
               Iterators (C) := Plans_Copy (P)(C).First;
               All_Iterators (C) := Plans_Copy (P)(C).First;
               while All_Iterators (C) /= Minorframes.No_Element
                 and then Minorframes.Element
                   (All_Iterators (C)).Starttime = Endtimes (C) + 1
               loop
                  Endtimes (C) := Minorframes.Element
                    (All_Iterators (C)).Endtime;
                  Minorframes.Next (All_Iterators (C));
               end loop;

               while Iterators (C) /= Minorframes.No_Element
                 and then Minorframes.Element
                   (Iterators (C)).Simultaneous_Set.Is_Empty
               loop
                  Minorframes.Next (Iterators (C));
               end loop;

               if Iterators (C) /= Minorframes.No_Element
                 and then Minorframes.Element (Iterators (C)).Starttime = 0
               then
                  Starttime_Important (C) := False;
               end if;

            end loop;

            --  Now first with restrictions is in Iterators
            --  Gaps before this time have no restrictions
            Time_Till := All_Level_Sizes (P)(Current_Level - 1) - 1;
            for C in 1 .. CPU_Count loop
               if Iterators (C) /= Minorframes.No_Element
               then
                  if Starttime_Important (C) then
                     if Minorframes.Element
                       (Iterators (C)).Starttime - 1 < Time_Till
                     then
                        Time_Till := Minorframes.Element
                          (Iterators (C)).Starttime - 1;
                     end if;
                  else
                     if Minorframes.Element
                       (Iterators (C)).Endtime < Time_Till
                     then
                        Time_Till := Minorframes.Element
                          (Iterators (C)).Endtime;
                     end if;
                  end if;
               end if;
            end loop;

            --  Calc starting Sim_Set
            for C in 1 .. CPU_Count loop
               --  Put in domains when endtime was chosen
               if Iterators (C) /= Minorframes.No_Element
                 and then not Starttime_Important (C)
               then
                  Sim_Set_Per_Cpu (C).Union
                    (Source => Minorframes.Element
                       (Iterators (C)).Simultaneous_Set);
               end if;

            end loop;

            Current_Sim := Types.Positive_Set.Empty_Set;
            for C in 1 .. CPU_Count loop
               Current_Sim.Union (Sim_Set_Per_Cpu (C));
            end loop;

            while not Done loop
               --  Build gaps till time till with Current_Sim
               for C in 1 .. CPU_Count loop

                  while All_Iterators (C) /= Minorframes.No_Element
                    and then Minorframes.Element
                      (All_Iterators (C)).Starttime - 1 <= Time_Till
                  loop
                     Gap.Starttime := Endtimes (C) + 1;
                     Gap.Endtime := Minorframes.Element
                       (All_Iterators (C)).Starttime - 1;
                     Gap.Simultaneous_Set := Current_Sim;
                     Plans_Copy (P)(C).Insert
                       (Before   => All_Iterators (C),
                        New_Item => Gap,
                        Count    => 1);

                     Endtimes (C) := Minorframes.Element
                       (All_Iterators (C)).Endtime;
                     Minorframes.Next (All_Iterators (C));
                     while All_Iterators (C) /= Minorframes.No_Element
                       and then Endtimes (C) + 1 = Minorframes.Element
                       (All_Iterators (C)).Starttime
                     loop
                        Endtimes (C) := Minorframes.Element
                          (All_Iterators (C)).Endtime;
                        Minorframes.Next (All_Iterators (C));
                     end loop;
                  end loop;

                  if Endtimes (C) + 1 <= Time_Till then
                     Gap.Starttime := Endtimes (C) + 1;
                     Gap.Endtime := Time_Till;
                     Gap.Simultaneous_Set := Current_Sim;
                     if All_Iterators (C) /= Minorframes.No_Element then
                        Plans_Copy (P)(C).Insert
                          (Before   => All_Iterators (C),
                           New_Item => Gap,
                           Count    => 1);
                     else
                        Plans_Copy (P)(C).Append (Gap);
                     end if;
                     Endtimes (C) := Time_Till;
                  end if;
               end loop;

               --  Update Sim_Set
               for C in 1 .. CPU_Count loop
                  --  Kick out domains when endtime was chosen
                  if Iterators (C) /= Minorframes.No_Element
                    and then not Starttime_Important (C)
                    and then Time_Till = Minorframes.Element
                      (Iterators (C)).Endtime
                  then
                     Sim_Set_Per_Cpu (C).Difference
                       (Source => Minorframes.Element
                          (Iterators (C)).Simultaneous_Set);
                     Minorframes.Next (Iterators (C));
                     while Iterators (C) /= Minorframes.No_Element
                       and then Minorframes.Element
                         (Iterators (C)).Simultaneous_Set.Is_Empty
                     loop
                        Minorframes.Next (Iterators (C));
                     end loop;
                     Starttime_Important (C) := True;
                  end if;

                  --  Add domains if starttime is the Time_Till
                  if Iterators (C) /= Minorframes.No_Element
                    and then Starttime_Important (C)
                    and then Time_Till = Minorframes.Element
                      (Iterators (C)).Starttime - 1
                  then
                     Sim_Set_Per_Cpu (C).Union
                       (Minorframes.Element (Iterators (C)).Simultaneous_Set);
                     while Minorframes.Next (Iterators (C))
                       /= Minorframes.No_Element
                       and then Minorframes.Element
                         (Minorframes.Next (Iterators (C))).Simultaneous_Set
                       = Minorframes.Element (Iterators (C)).Simultaneous_Set
                       and then Minorframes.Element
                         (Minorframes.Next (Iterators (C))).Starttime
                       = Minorframes.Element (Iterators (C)).Endtime + 1
                     loop
                        Minorframes.Next (Iterators (C));
                     end loop;
                     Starttime_Important (C) := False;
                  end if;

               end loop;

               Current_Sim := Types.Positive_Set.Empty_Set;
               for C in 1 .. CPU_Count loop
                  Current_Sim.Union (Sim_Set_Per_Cpu (C));
               end loop;

               --  Find new time till the sim_set has to change
               Time_Till := All_Level_Sizes (P)(Current_Level - 1) - 1;
               for C in 1 .. CPU_Count loop
                  if Iterators (C) /= Minorframes.No_Element then
                     if Starttime_Important (C) then
                        if Minorframes.Element (Iterators (C)).Starttime - 1
                          < Time_Till
                        then
                           Time_Till := Minorframes.Element
                             (Iterators (C)).Starttime - 1;
                        end if;
                     else
                        if Minorframes.Element (Iterators (C)).Endtime
                          < Time_Till
                        then
                           Time_Till := Types.Minfs
                             .Minorframes.Element (Iterators (C))
                               .Endtime;
                        end if;
                     end if;
                  end if;
               end loop;

               Done := True;
               for C in 1 .. CPU_Count loop
                  if Endtimes (C) /= All_Level_Sizes (P)
                    (Current_Level - 1) - 1
                  then
                     Done := False;
                  end if;
               end loop;

            end loop;

            declare
               Plans_Copy2 : constant All_Plans_Type := Plans_Copy;
            begin

               --  Repeat the created plan factor times (so it has length
               --  of higher level)
               for K in 1 .. (All_Level_Sizes (P)(Current_Level)
                              / All_Level_Sizes (P) (Current_Level - 1)) - 1
               loop
                  for C in CPU_Range loop
                     M := Plans_Copy2 (P)(C).First;
                     while M /= Minorframes.No_Element loop
                        Minf := Minorframes.Element (M);
                        Minf.Starttime := Minf.Starttime +
                          (All_Level_Sizes (P)
                           (Current_Level - 1) * K);
                        Minf.Endtime := Minf.Endtime +
                          (All_Level_Sizes (P) (Current_Level - 1) * K);
                        Plans_Copy (P)(C).Append (Minf);
                        Minorframes.Next (M);
                     end loop;
                  end loop;
               end loop;
            end;

            Put_Line ("Repeated" & P'Img);
            Auxiliary.Print_Functions.Print_Level_no_subj (Plans_Copy (P));

            for C in CPU_Range loop
               M := Plans_Copy (P)(C).First;
               while M /= Minorframes.No_Element loop
                  if Minorframes.Element (M).Subject = -1 then
                     Gaps_All_Plans (P)(C).Append (M);
                  end if;
                  Minorframes.Next (M);
               end loop;
            end loop;

         end if;
      end loop;

      --  Plan all subjects in a good order
      --  because CPUs are given this can be done per Plan
      --  Possible improve: scoring done per plan and just use the
      --  ones that get better
      for P in Plan_Range loop
         if  not (Per_Plan_Level_Count (P) < Current_Level) then
            Need_To_Plan.Clear;
            --  fins out who needs to be planned
            for S of All_Plans_Subjects_And_Metasubjects (P) loop
               if S.Level = Current_Level then
                  Need_To_Plan.Append (S);
               end if;
            end loop;
            --  Find a good order...
            --  Currently used:2.sorting criteria: length;
            --  1.sorting criteria:restr
            Types.Metas.Meta_Subject_Restr_Length_Sorter.Sort (Need_To_Plan);
            Need_To_Plan.Reverse_Elements;
            Noone_Failed := True;
            --  Plan subjects in this order
            for S of Need_To_Plan loop
               Plan_Subject_On_Given_CPU
                 (CPU                 => S.CPU,
                  Plan                => Plans_Copy.Reference (Index => P),
                  Simultaneous_Allowed => Simultaneous_Allowed,
                  Subject             => S,
                  Length              => S.Length,
                  All_Gaps_Vectors    => Gaps_All_Plans (P),
                  Success             => Single_Success,
                  Plan_Length         => All_Level_Sizes (P) (Current_Level));
               if not Single_Success then
                  Noone_Failed := False;
               end if;

            end loop;

            Auxiliary.Print_Functions.Print_Level_no_subj (Plans_Copy (P));

            --  EFFICIENCY: keep a version without deleted gaps and don't
            --  calculate gaps new next level
            --  delete left "gap-minfs"
            for C in CPU_Range loop
               M := Plans_Copy (P)(C).First;
               while M /= Minorframes.No_Element loop
                  N := Minorframes.Next (M);
                  if Minorframes.Element (M).Subject = -1 then
                     Minorframes.Delete
                       (Container => Plans_Copy (P)(C),
                        Position  => M,
                        Count     => 1);
                  end if;
                  M := N;
               end loop;
            end loop;

            if Noone_Failed then
               Result (P) := Plans_Copy (P);
               Success := True;
            else
               Result (P) := Plans (P);
               Failed_Plans.Append (P);
            end if;
         end if;
      end loop;

      Put_Line ("Finished level" & Current_Level'Img);
      for P in Plan_Range loop
         Put_Line ("Plan" & P'Img);
         Auxiliary.Print_Functions.Print_Level_no_subj (Plans_Copy (P));
      end loop;

   end Build_Plan_With_Blockers;

   -------------------------------------------------------------------------

   --  Theoretical: CPUs of subjects that aren't already planned in lower
   --  levels can be changed
   --  Practical: allowing to change the CPUs is expected to result in no
   --  useable plan (not all metas with their given length fit into the plan)
   procedure Plan_Subject_At_Given_Place
     (CPU              :        Natural;
      Gap_Nbr          :        Integer;
      Starttime        :        Integer;
      Subject          :        Meta_Subject_Type;
      Length           :        Integer;
      Plan             : in out Level_Type;
      All_Gaps_Vectors : in out Per_CPU_Gap_Array)
   is
      use Types.Minfs;
      use type Types.Minfs.Minorframes.Cursor;

      I, Last_Endtime, Extra_Before : Integer;

      Sim_Set : Types.Positive_Set.Set;

      Minf, New_Minf, New_Minf2 : Minorframe_Type;

      No_Tamper  : Minorframes.Cursor;
      Gap_Vector : Minorframes_Iterator_Vectors.Vector
        := All_Gaps_Vectors (CPU);
      All_Gaps_Vectors2 : constant Per_CPU_Gap_Array := All_Gaps_Vectors;
   begin
      --  Plan it there
      New_Minf.Subject := Subject.Number;
      New_Minf.Starttime := Starttime;
      New_Minf.Endtime := Starttime + Length - 1;
      New_Minf.Simultaneous_Set := Subject.Simultaneous_Set;

      --  Update gaps and gap_vector
      I := Gap_Nbr;
      while Minorframes.Element (Gap_Vector (I)).Endtime < New_Minf.Endtime
      loop
         Minorframes.Delete
           (Container => Plan (CPU),
            Position  => Gap_Vector (I),
            Count     => 1);

         Gap_Vector.Delete (I);
      end loop;

      Last_Endtime := Minorframes.Element (Gap_Vector (I)).Endtime;
      Sim_Set := Minorframes.Element
        (Gap_Vector (I)).Simultaneous_Set;
      Minorframes.Replace_Element
        (Container => Plan (CPU),
         Position  => Gap_Vector (I),
         New_Item  => New_Minf);

      if Last_Endtime /= New_Minf.Endtime then
         New_Minf2.Subject := -1;
         New_Minf2.Starttime := New_Minf.Endtime + 1;
         New_Minf2.Endtime := Last_Endtime;
         New_Minf2.Simultaneous_Set := Sim_Set;
         if Gap_Vector (I) = Minorframes.No_Element then
            Minorframes.Append (Container => Plan (CPU),
                                New_Item  => New_Minf2,
                                Count     => 1);
         else
            Minorframes.Insert
              (Container => Plan (CPU),
               Before    => Minorframes.Next (Gap_Vector (I)),
               New_Item  => New_Minf2,
               Count     => 1);
         end if;
         Gap_Vector (I) := Minorframes.Next (Gap_Vector (I));
      else
         Gap_Vector.Delete (I);
      end if;

      --  Update gaps on other CPUs...
      if not Subject.Simultaneous_Set.Is_Empty then
         for C in 0 .. Input.CPU_Count loop
            if C /= CPU and C /= 0 then
               Extra_Before := 0;
               I := 1;
               while I <= All_Gaps_Vectors2 (C).Last_Index loop
                  Minf := Minorframes.Element
                    (All_Gaps_Vectors (C)
                     (I + Extra_Before));
                  if not Types.Positive_Set.Is_Subset
                    (Subset => New_Minf.Simultaneous_Set,
                     Of_Set => Minf.Simultaneous_Set)
                  then
                     if New_Minf.Starttime <= Minf.Starttime and then
                       New_Minf.Endtime >= Minf.Endtime
                     then
                        --  Gap lies into new_Minf
                        New_Minf2.Starttime := Minf.Starttime;
                        New_Minf2.Endtime := Minf.Endtime;
                        New_Minf2.Subject := -1;
                        New_Minf2.Simultaneous_Set :=
                          Types.Positive_Set.Union
                            (Left  => New_Minf.Simultaneous_Set,
                             Right => Minf.Simultaneous_Set);
                        Minorframes.Replace_Element
                          (Container => Plan (C),
                           Position  => All_Gaps_Vectors (C)(I + Extra_Before),
                           New_Item  => New_Minf2);

                     elsif New_Minf.Starttime <= Minf.Starttime and then
                       New_Minf.Endtime >= Minf.Starttime
                     then
                        --  They are planned simultaneous
                        New_Minf2.Starttime := New_Minf.Endtime + 1;
                        New_Minf2.Endtime := Minf.Endtime;
                        New_Minf2.Subject := -1;
                        New_Minf2.Simultaneous_Set := Minf.Simultaneous_Set;
                        Minorframes.Replace_Element
                          (Container => Plan (C),
                           Position  => All_Gaps_Vectors (C)(I + Extra_Before),
                           New_Item  => New_Minf2);

                        New_Minf2.Starttime := Minf.Starttime;
                        New_Minf2.Endtime := New_Minf.Endtime;
                        New_Minf2.Subject := -1;
                        New_Minf2.Simultaneous_Set :=
                          Types.Positive_Set.Union
                            (Left  => New_Minf.Simultaneous_Set,
                             Right => Minf.Simultaneous_Set);
                        Minorframes.Insert
                          (Container => Plan (C),
                           Before    => All_Gaps_Vectors (C)
                           (I + Extra_Before),
                           New_Item  => New_Minf2,
                           Count     => 1);
                        No_Tamper := Minorframes
                          .Previous (All_Gaps_Vectors (C)
                                     (I + Extra_Before));

                        All_Gaps_Vectors (C).Insert
                          (Before   => I + Extra_Before,
                           New_Item => No_Tamper);
                        Extra_Before := Extra_Before + 1;

                     elsif  New_Minf.Starttime >= Minf.Starttime
                       and then New_Minf.Starttime <= Minf.Endtime
                     then
                        --  They are planned simultaneous
                        New_Minf2.Starttime := New_Minf.Starttime;
                        New_Minf2.Endtime := Minf.Endtime;
                        New_Minf2.Subject := -1;
                        New_Minf2.Simultaneous_Set :=
                          Types.Positive_Set.Union
                            (Left  => New_Minf.Simultaneous_Set,
                             Right => Minf.Simultaneous_Set);

                        Minorframes.Replace_Element
                          (Container => Plan (C),
                           Position  => All_Gaps_Vectors (C)(I + Extra_Before),
                           New_Item  => New_Minf2);

                        New_Minf2.Starttime := Minf.Starttime;
                        New_Minf2.Endtime := New_Minf.Starttime - 1;
                        New_Minf2.Subject := -1;
                        New_Minf2.Simultaneous_Set := Minf.Simultaneous_Set;
                        Minorframes.Insert
                          (Container => Plan (C),
                           Before    => All_Gaps_Vectors (C)(I + Extra_Before),
                           New_Item  => New_Minf2,
                           Count     => 1);
                        No_Tamper := Minorframes.Previous
                          (All_Gaps_Vectors (C)(I + Extra_Before));

                        All_Gaps_Vectors (C).Insert
                          (Before   => I + Extra_Before,
                           New_Item => No_Tamper);
                        Extra_Before := Extra_Before + 1;

                     end if;

                  end if;
                  I := I + 1;
               end loop;
            end if;
         end loop;
      end if;

      All_Gaps_Vectors (CPU) := Gap_Vector;
   end Plan_Subject_At_Given_Place;

   -------------------------------------------------------------------------

   --  Try first: get it into without splitting
   --  (in gap with most restrictions(!) that is as short as possible)
   --  Need to split (no gap is long enough) => calc how many splits are
   --  needed try to distribute them equally (or at least nearly)
   --  If all this fails: try to get them in somehow
   procedure Plan_Subject_On_Given_CPU
     (CPU                  :        Natural;
      Simultaneous_Allowed :        Execution_Restrictions_Array;
      Subject              :        Meta_Subject_Type;
      Length               :        Integer;
      Plan_Length          :        Integer;
      Plan                 : in out Level_Type;
      All_Gaps_Vectors     : in out Per_CPU_Gap_Array;
      Success              :    out Boolean)
   is
      Starttime : Integer;
      Gap, Gap2 : Types.Minfs.Minorframe_Type;

      G : Types.Minfs.Minorframes.Cursor;

      Gap_Nbr, N, Summed_Length, Last_Endtime : Integer;

      Some_Restricting, Fill_Success : Boolean;

      I               : Integer := 1;
      Max_Restriction : Integer := -1;
      Found_Place     : Boolean := False;

      Gap_Vector : constant Types.Minfs.Minorframes_Iterator_Vectors.Vector
        := All_Gaps_Vectors (CPU);
   begin

      --  Try to plan without splitting
      while I <= Gap_Vector.Last_Index and not Found_Place loop
         G := Gap_Vector (I);
         Gap := Types.Minfs.Minorframes.Element (G);
         if Restrictions_Are_Satisfiable
           (Subject_Set1 => Gap.Simultaneous_Set,
            Subject_Set2 => Subject.Simultaneous_Set,
            Restrictions => Simultaneous_Allowed)
         then
            Summed_Length    := Gap.Endtime - Gap.Starttime + 1;
            Some_Restricting := False;
            Last_Endtime     := Gap.Endtime;
            N := I + 1;
            Max_Restriction := Integer (Gap.Simultaneous_Set.Length);

            --  Possible to use more than one gap, if they lay next to
            --  eachother  and have different restrictions,
            --  that are both okay for the subject...
            while Summed_Length < Length and then
              (N <= Gap_Vector.Last_Index and not Some_Restricting)
            loop
               Gap2 := Types.Minfs.Minorframes.Element (Gap_Vector (N));
               if Restrictions_Are_Satisfiable
                 (Subject_Set1 => Gap2.Simultaneous_Set,
                  Subject_Set2 => Subject.Simultaneous_Set,
                  Restrictions => Simultaneous_Allowed)
                 and then Gap2.Starttime = Last_Endtime + 1
               then
                  Summed_Length := Summed_Length + Gap2.Endtime -
                    Gap2.Starttime + 1;
                  N := N + 1;
                  Last_Endtime := Gap2.Endtime;
                  if Integer (Gap2.Simultaneous_Set.Length) > Max_Restriction
                  then
                     Max_Restriction := Integer (Gap2.Simultaneous_Set.Length);
                  end if;
               else
                  Some_Restricting := True;
               end if;
            end loop;

            if Summed_Length >= Length then
               Found_Place := True;
               Gap_Nbr := I;
               Starttime := Gap.Starttime;
            else
               I := N;
            end if;
         else
            I := I + 1;
         end if;
      end loop;

      if Found_Place then
         Plan_Subject_At_Given_Place
           (CPU              => CPU,
            Plan             => Plan,
            Gap_Nbr          => Gap_Nbr,
            Subject          => Subject,
            Length           => Length,
            Starttime        => Starttime,
            All_Gaps_Vectors => All_Gaps_Vectors);
      else
         --  Try to split as less and equally as possible
         Use_Gaps_As_They_Come
           (CPU                  => CPU,
            Plan                 => Plan,
            Simultaneous_Allowed => Simultaneous_Allowed,
            Subject              => Subject,
            Length               => Length,
            All_Gaps_Vectors     => All_Gaps_Vectors,
            Success              => Fill_Success,
            Plan_Length          => Plan_Length);

         Found_Place := Fill_Success;
      end if;

      Success := Found_Place;
   end Plan_Subject_On_Given_CPU;

   --------------------------------------------------------------------------

   function Restrictions_Are_Satisfiable
     (Subject_Set1 : Types.Positive_Set.Set;
      Subject_Set2 : Types.Positive_Set.Set;
      Restrictions : Execution_Restrictions_Array)
      return Boolean
   is
   begin
      for S1 of Subject_Set1 loop
         for S2 of Subject_Set2 loop
            if not Restrictions (S1, S2) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Restrictions_Are_Satisfiable;

   -------------------------------------------------------------------------

   --  Try to use gaps to plan a subject regular that needs to be splitted
   --  (equal distances and every minorframe nearly same length)
   --  If this isn't working: Try to plan them from the left (dont care how
   --  many splits are needed)
   procedure Use_Gaps_As_They_Come
     (CPU                  :        Natural;
      Simultaneous_Allowed :        Execution_Restrictions_Array;
      Subject              :        Meta_Subject_Type;
      Length               :        Integer;
      Plan_Length          :        Integer;
      Plan                 : in out Level_Type;
      All_Gaps_Vectors     : in out Per_CPU_Gap_Array;
      Success              :    out Boolean)
   is
      Gap, Gap2   : Types.Minfs.Minorframe_Type;
      G           : Types.Minfs.Minorframes.Cursor;
      Chosen_Gaps : Types.Integer_Vectors.Vector;

      Last_Starttime, Summed_Length, Last_Endtime, Gap_Nbr,
      Length_Between_Used_Gaps, Used_Length : Integer;

      Length_Left : Integer := Length;

      I : Integer := 1;
      J : Integer := 1;
      H : Integer := 1;

      Gap_Vector : constant Types.Minfs.Minorframes_Iterator_Vectors.Vector
        := All_Gaps_Vectors (CPU);
   begin
      --  Try to do it regularly
      while J < Integer'Min (Integer (Gap_Vector.Length), Length)
        and Length_Left > 0
      loop
         J := J + 1;
         Length_Left := Length;
         Used_Length := Length_Left / J;
         Length_Between_Used_Gaps := Plan_Length / J;
         Chosen_Gaps.Clear;
         I := 1;
         while I <= All_Gaps_Vectors (CPU).Last_Index and Length_Left > 0 loop
            Gap := Types.Minfs.Minorframes.Element (Gap_Vector (I));
            Length_Left := Length;
            Summed_Length := 0;
            Chosen_Gaps.Clear;
            H := I;
            Gap2 := Types.Minfs.Minorframes.Element (Gap_Vector (H));
            Last_Endtime := Gap2.Starttime - 1;
            while H <= All_Gaps_Vectors (CPU).Last_Index and then
              Restrictions_Are_Satisfiable
                (Subject_Set1 => Gap2.Simultaneous_Set,
                 Subject_Set2 => Subject.Simultaneous_Set,
                 Restrictions => Simultaneous_Allowed) and then
              Gap2.Starttime = Last_Endtime + 1
            loop
               Summed_Length := Summed_Length + Gap2.Endtime
                 - Gap2.Starttime + 1;
               Last_Endtime := Gap2.Endtime;
               H := H + 1;
               if H <= All_Gaps_Vectors (CPU).Last_Index then
                  Gap2 := Types.Minfs.Minorframes.Element (Gap_Vector (H));
               end if;
            end loop;

            if Gap.Starttime <= Length_Between_Used_Gaps
              and then Summed_Length >= Used_Length
            then
               --  This gap could be a starter
               if Restrictions_Are_Satisfiable
                 (Subject_Set1 => Gap.Simultaneous_Set,
                  Subject_Set2 => Subject.Simultaneous_Set,
                  Restrictions => Simultaneous_Allowed)
               then
                  if Length_Left mod Used_Length /= 0
                    and then Summed_Length > Used_Length
                  then
                     --  Plan one tick more if possible
                     Length_Left := Length_Left - Used_Length + 1;
                     Chosen_Gaps.Append (I);
                     Last_Starttime := Gap.Starttime;
                  else
                     Length_Left := Length_Left - Used_Length;
                     Chosen_Gaps.Append (I);
                     Last_Starttime := Gap.Starttime;
                  end if;
                  for K in I .. All_Gaps_Vectors (CPU).Last_Index loop
                     Gap := Types.Minfs.Minorframes.Element (Gap_Vector (K));
                     if Gap.Starttime = Last_Starttime
                       + Length_Between_Used_Gaps
                       and then
                         Restrictions_Are_Satisfiable
                           (Subject_Set1 => Gap.Simultaneous_Set,
                            Subject_Set2 => Subject.Simultaneous_Set,
                            Restrictions => Simultaneous_Allowed)
                     then
                        Summed_Length := 0;
                        H := K;
                        Gap2 := Types.Minfs.Minorframes.Element
                          (Gap_Vector (H));
                        Last_Endtime := Gap2.Starttime - 1;
                        while H <= All_Gaps_Vectors (CPU).Last_Index and then
                          Restrictions_Are_Satisfiable
                            (Subject_Set1 => Gap2.Simultaneous_Set,
                             Subject_Set2 => Subject.Simultaneous_Set,
                             Restrictions => Simultaneous_Allowed)
                          and then Gap2.Starttime = Last_Endtime + 1 loop
                           Summed_Length := Summed_Length + Gap2.Endtime -
                             Gap2.Starttime + 1;
                           Last_Endtime := Gap2.Endtime;
                           H := H + 1;
                           if H <= All_Gaps_Vectors (CPU).Last_Index then
                              Gap2 := Types.Minfs.Minorframes
                                .Element (Gap_Vector (H));
                           end if;
                        end loop;
                        --  This gap will be part
                        if Length_Left mod Used_Length /= 0
                          and then Summed_Length > Used_Length
                        then
                           --  Plan one tick more if possible
                           Length_Left := Length_Left - Used_Length + 1;
                           Chosen_Gaps.Append (K);
                           Last_Starttime := Gap.Starttime;

                        elsif Summed_Length >= Used_Length then
                           Length_Left := Length_Left - Used_Length;
                           Chosen_Gaps.Append (K);
                           Last_Starttime := Gap.Starttime;

                        end if;
                     end if;
                  end loop;
               end if;
            end if;
            I := I + 1;
         end loop;
      end loop;

      if Length_Left = 0 then
         --  It can be planned regular
         Length_Left := Length;
         Types.Integer_Vectors.Reverse_Elements (Chosen_Gaps);

         --  Plan it at every chosen gap
         for J of Chosen_Gaps loop
            Gap := Types.Minfs.Minorframes.Element (Gap_Vector (J));

            Summed_Length := 0;
            H := J;
            Gap2 := Types.Minfs.Minorframes.Element (Gap_Vector (H));
            Last_Endtime := Gap2.Starttime - 1;
            while H <= All_Gaps_Vectors (CPU).Last_Index
              and then Restrictions_Are_Satisfiable
                (Subject_Set1 => Gap2.Simultaneous_Set,
                 Subject_Set2 => Subject.Simultaneous_Set,
                 Restrictions => Simultaneous_Allowed)
              and then Gap2.Starttime = Last_Endtime + 1
            loop
               Summed_Length := Summed_Length + Gap2.Endtime
                 - Gap2.Starttime + 1;
               Last_Endtime := Gap2.Endtime;
               H := H + 1;

               --  FIXME: Do not iterate over gaps which might be deleted by
               --         Plan_Subject_At_Given_Place.

               begin
                  Gap2 := Types.Minfs.Minorframes.Element (Gap_Vector (H));

               exception
                  when others => null;
               end;
            end loop;

            if Length_Left mod Used_Length /= 0
              and then Summed_Length > Used_Length
            then
               --  Plan one tick more if possible
               Length_Left := Length_Left - Used_Length + 1;
               Plan_Subject_At_Given_Place
                 (CPU              => CPU,
                  Plan             => Plan,
                  Gap_Nbr          => J,
                  Subject          => Subject,
                  Length           => Used_Length + 1,
                  Starttime        => Gap.Starttime,
                  All_Gaps_Vectors => All_Gaps_Vectors);
            else
               Length_Left := Length_Left - Used_Length;
               Plan_Subject_At_Given_Place
                 (CPU              => CPU,
                  Plan             => Plan,
                  Gap_Nbr          => J,
                  Subject          => Subject,
                  Length           => Used_Length,
                  Starttime        => Gap.Starttime,
                  All_Gaps_Vectors => All_Gaps_Vectors);
            end if;

         end loop;
      else
         Length_Left := Length;
      end if;

      I := 1;
      --  If failed to plan regular: Try to get it in...somehow!
      --  Try gaps starting from left, use a gap if it is possible to use it
      while I <= All_Gaps_Vectors (CPU).Last_Index and Length_Left > 0 loop
         G := All_Gaps_Vectors (CPU)(I);
         Gap := Types.Minfs.Minorframes.Element (G);
         if Restrictions_Are_Satisfiable
           (Subject_Set1 => Gap.Simultaneous_Set,
            Subject_Set2 => Subject.Simultaneous_Set,
            Restrictions => Simultaneous_Allowed)
         then
            Used_Length := Integer'Min
              (Length_Left, Gap.Endtime - Gap.Starttime + 1);
            Length_Left := Length_Left - Used_Length;
            Gap_Nbr     := I;

            Plan_Subject_At_Given_Place
              (CPU              => CPU,
               Plan             => Plan,
               Gap_Nbr          => Gap_Nbr,
               Subject          => Subject,
               Length           => Used_Length,
               Starttime        => Gap.Starttime,
               All_Gaps_Vectors => All_Gaps_Vectors);
         else
            I := I + 1;
         end if;
      end loop;

      Success := Length_Left <= 0;
   end Use_Gaps_As_They_Come;

end Steps.Plan_Building_Blockers;
