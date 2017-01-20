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

with Types.Float_Vectors;
with Types.Minfs.Minorframes;
with Types.Positive_Set;
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Types.Metas.Meta_subject_sorter;
with Types.Metas.Meta_subject_restr_sorter;
with Auxiliary.My_Math; use Auxiliary.My_Math;
with Steps.Plan_Building_Blockers; use Steps.Plan_Building_Blockers;
with Input;

package body Steps.Plan_Building_Lowest_Level
is

   package TM renames Types.Metas;

   type Same_CPU_Length_Array is array (Positive range <>) of
     Types.Integer_Vectors.Vector;

   type Same_CPU_Domains_Array is array (Natural range <>) of
     Types.Positive_Set.Set;

   type Treshold_Array is array (Positive range <>) of Integer;

   procedure Calc_Starting_Time
     (CPU                  :     Natural;
      Subject              :     Meta_Subject_Type;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Plan                 :     TDS.Level_Type;
      Time                 : out Integer;
      Place                : out Types.Minfs.Minorframes.Cursor);

   procedure Collision_Happens
     (Starttime            :     Integer;
      Endtime              :     Integer;
      Simultaneous_Set     :     Types.Positive_Set.Set;
      Plan                 :     TDS.Level_Type;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Collision_Happend    : out Boolean;
      Collision_Ends       : out Integer);

   procedure Collision_With_ML_Happens
     (Starttime            :     Integer;
      Endtime              :     Integer;
      Simultaneous_Set     :     Types.Positive_Set.Set;
      ML                   :     Types.Minfs.Minorframes.List;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Collision            : out Boolean;
      Collision_Ends       : out Integer);

   procedure Find_Initial_Order
     (Subjects_and_Meta_Subjects : in out TM.Meta_Subject_Vectors.Vector);

   procedure Plan_A_Subject
     (Subject              :        Meta_Subject_Type;
      All_Subjects         :        TM.Meta_Subject_Vectors.Vector;
      Same_CPU_Lengths     :        Same_CPU_Length_Array;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Treshold             :        Treshold_Array;
      Level_Size_Array     :        TDS.Per_Plan_Level_Sizes_Type;
      Same_CPU_Domains     : in out Same_CPU_Domains_Array;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Plans                : in out TDS.All_Plans_Type;
      Treshold_Reached     :    out Boolean;
      Success              :    out Boolean);

   procedure Puzzle_All_Plans
     (Left_Subjects        :        TM.Meta_Subject_Vectors.Vector;
      Same_CPU_Domains     :        Same_CPU_Domains_Array;
      All_Subjects         :        TM.Meta_Subject_Vectors.Vector;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      Same_CPU_Lengths     :        Same_CPU_Length_Array;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Treshold             :        Treshold_Array;
      Level_Size_Array     :        TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count :        Types.Positive_Vectors.Vector;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Plans                : in out TDS.All_Plans_Type;
      Success              : in out Boolean;
      Blind_End            : in out Boolean);

   --  New order with CPUs given.
   function Random_Order
     (Subjects : TM.Meta_Subject_Vectors.Vector)
      return TM.Meta_Subject_Vectors.Vector;

   -------------------------------------------------------------------------

   procedure Calc_Starting_Time
     (CPU                  :     Natural;
      Subject              :     Meta_Subject_Type;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Plan                 :     TDS.Level_Type;
      Time                 : out Integer;
      Place                : out Types.Minfs.Minorframes.Cursor)
   is
      use type Types.Minfs.Minorframes.Cursor;

      Time_Found : Boolean                        := False;
      Iterator   : Types.Minfs.Minorframes.Cursor := Plan (CPU).First;
      Position   : Types.Minfs.Minorframes.Cursor
        := Types.Minfs.Minorframes.No_Element;

      Collision_Happend : Boolean := True;
      Collision_Ends    : Integer := 0;
      Previous_End      : Integer := -1;
   begin
      if not Plan (CPU).Is_Empty then

         --  Check if it is possible to plan subject BEFORE a minorframe
         while not Time_Found and Iterator /= Plan (CPU).Last loop
            Time := Previous_End + 1;

            while Types.Minfs.Minorframes.Element (Iterator).Starttime >
              Time + Subject.Length - 1
              and then not Time_Found
            loop
               Collision_Happens
                 (Starttime            => Time,
                  Endtime              => Time + Subject.Length - 1,
                  Simultaneous_Set     => Subject.Simultaneous_Set,
                  Plan                 => Plan,
                  Simultaneous_Allowed => Simultaneous_Allowed,
                  Collision_Happend    => Collision_Happend,
                  Collision_Ends       => Collision_Ends);

               if Collision_Happend then
                  Time := Collision_Ends;
               else
                  Time_Found := True;
                  Position   := Iterator;
               end if;

            end loop;

            Previous_End := Types.Minfs.Minorframes.Element (Iterator).Endtime;
            Types.Minfs.Minorframes.Next (Iterator);
         end loop;

         if not Time_Found then
            Time := Previous_End + 1;
            --  Loop above will skip gap before last
            while Types.Minfs.Minorframes.Element (Iterator).Starttime >
              Time + Subject.Length - 1
              and then not Time_Found
            loop
               Collision_Happens
                 (Starttime            => Time,
                  Endtime              => Time + Subject.Length - 1,
                  Simultaneous_Set     => Subject.Simultaneous_Set,
                  Plan                 => Plan,
                  Simultaneous_Allowed => Simultaneous_Allowed,
                  Collision_Happend    => Collision_Happend,
                  Collision_Ends       => Collision_Ends);
               if Collision_Happend then
                  Time := Collision_Ends;
               else
                  Time_Found := True;
                  Position   := Iterator;
               end if;
            end loop;
         end if;
      end if;

      --  If time not found: it has to be planned AFTER the last one
      Iterator := Plan (CPU).Last;
      if not Time_Found then
         if not Plan (CPU).Is_Empty then
            Time := Types.Minfs.Minorframes.Element
              (Types.Minfs.Minorframes.Last (Plan (CPU))).Endtime + 1;
         end if;

         Collision_Happens
           (Starttime            => Time,
            Endtime              => Time + Subject.Length - 1,
            Simultaneous_Set     => Subject.Simultaneous_Set,
            Plan                 => Plan,
            Simultaneous_Allowed => Simultaneous_Allowed,
            Collision_Happend    => Collision_Happend,
            Collision_Ends       => Collision_Ends);

         while Collision_Happend loop
            Time := Collision_Ends;

            Collision_Happens
              (Starttime            => Time,
               Endtime              => Time + Subject.Length - 1,
               Simultaneous_Set     => Subject.Simultaneous_Set,
               Plan                 => Plan,
               Simultaneous_Allowed => Simultaneous_Allowed,
               Collision_Happend    => Collision_Happend,
               Collision_Ends       => Collision_Ends);
         end loop;
      end if;
      Place := Position;
   end Calc_Starting_Time;

   -------------------------------------------------------------------------

   --  Checks collisions with minfs on cpus
   procedure Collision_Happens
     (Starttime            :     Integer;
      Endtime              :     Integer;
      Simultaneous_Set     :     Types.Positive_Set.Set;
      Plan                 :     TDS.Level_Type;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Collision_Happend    : out Boolean;
      Collision_Ends       : out Integer)
   is
      B         : Boolean := False;
      Latest    : Integer := -1;
      Collision : Boolean;
      Current   : Integer;
   begin
      for CPU in 0 .. Input.CPU_Count loop
         Collision_With_ML_Happens (Starttime, Endtime, Simultaneous_Set,
                                    Plan (CPU), Simultaneous_Allowed,
                                    Collision, Current);
         if Collision then
            B := True;
            if Current > Latest then
               Latest := Current;
            end if;
         end if;
      end loop;

      Collision_Happend := B;
      Collision_Ends    := Latest;
   end Collision_Happens;

   -------------------------------------------------------------------------

   --  Checks for collisions with the plan of one CPU(Minorframes.List=ML)
   --  Collision_ends is next time where it is possible that no collision
   --  happens
   procedure Collision_With_ML_Happens
     (Starttime            :     Integer;
      Endtime              :     Integer;
      Simultaneous_Set     :     Types.Positive_Set.Set;
      ML                   :     Types.Minfs.Minorframes.List;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      Collision            : out Boolean;
      Collision_Ends       : out Integer)
   is
      B       : Boolean                        := False;
      C       : Types.Minfs.Minorframes.Cursor := ML.First;
      Current : Types.Minfs.Minorframe_Type;
   begin
      Collision_Ends := -1;
      if not ML.Is_Empty then
         while not Types.Minfs.Minorframes."="(C, ML.Last) loop
            Current := Types.Minfs.Minorframes.Element (C);
            if Current.Starttime <= Endtime and then
              Current.Endtime >= Starttime --  They are planned simultaneous
              and then not
                Restrictions_Are_Satisfiable
                  (Subject_Set1 => Simultaneous_Set,
                   Subject_Set2 => Current.Simultaneous_Set,
                   Restrictions => Simultaneous_Allowed)
            then
               B := True;
               Collision_Ends := Current.Endtime + 1;
            end if;
            C := Types.Minfs.Minorframes.Next (C);
         end loop;

         Current := Types.Minfs.Minorframes.Element (C);

         if Current.Starttime <= Endtime and then
           Current.Endtime >= Starttime -- They are planned simultaneous
           and then not
             Restrictions_Are_Satisfiable
               (Subject_Set1 => Simultaneous_Set,
                Subject_Set2 => Current.Simultaneous_Set,
                Restrictions => Simultaneous_Allowed)
         then
            B := True;
            Collision_Ends := Current.Endtime + 1;
         end if;
      end if;
      Collision := B;
   end Collision_With_ML_Happens;

   -------------------------------------------------------------------------

   procedure Find_Initial_Order
     (Subjects_and_Meta_Subjects : in out TM.Meta_Subject_Vectors.Vector)
   is
      I : Integer := 1;
   begin
      --  Sort them by Length increasing
      TM.Meta_subject_sorter.Sort (Subjects_and_Meta_Subjects);
      --  Decreasing
      Subjects_and_Meta_Subjects.Reverse_Elements;

      for S of Subjects_and_Meta_Subjects loop
         S.Order_Number := I;
         I := I + 1;
      end loop;
   end Find_Initial_Order;

   -------------------------------------------------------------------------

   --  Strategies:
   --  1-Sort by restr and plan them
   --  2- Sort by length and Plan
   --  else try random order
   --  currently: Strategies work with given CPUs
   --  Theoretically: CPUs might be changed at this time -> more complex
   --  strategies would be needed
   procedure Order_Strategies
     (Strategy             :        Integer;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Level_Sizes          :        TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count :        Types.Positive_Vectors.Vector;
      Subjects_To_Plan     : in out Types.Metas.Meta_Subject_Vectors.Vector;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Success              :    out Boolean;
      Plan                 :    out TDS.Level_Type)
   is
      subtype Plan_Range is Positive range 1 .. Input.Plan_Count;

      Plans : TDS.All_Plans_Type := TDS.Create
        (Plan_Count => Input.Plan_Count,
         CPU_Count  => Input.CPU_Count);
      I     : Integer := 1;

      Same_CPU_Domains : Same_CPU_Domains_Array (0 .. Input.CPU_Count);
      --  Only needed if CPUs not fixed

      Same_CPU_Lengths : Same_CPU_Length_Array (Plan_Range);
      Treshold         : Treshold_Array (Plan_Range);
      Blind_End        : Boolean := False;
      This_Plan        : constant Plan_Range
        := Subjects_To_Plan.First_Element.Plan;
   begin
      Success := False;

      for P in Plan_Range loop
         Treshold (P) := 10 * Level_Sizes (P)(1);
      end loop;

      if Strategy = 1 then
         --  Sort them by restriction
         TM.Meta_subject_restr_sorter.Sort (Subjects_To_Plan);
      elsif Strategy = 2 then
         --  Sort them by length
         TM.Meta_subject_sorter.Sort (Subjects_To_Plan);
      else
         Subjects_To_Plan := Random_Order (Subjects => Subjects_To_Plan);
      end if;

      --  To get the sorting decreasing:
      Subjects_To_Plan.Reverse_Elements;

      for S of Subjects_To_Plan loop
         S.Order_Number := I;
         I := I + 1;
      end loop;

      Puzzle_All_Plans
        (Left_Subjects        => Subjects_To_Plan,
         Same_CPU_Domains     => Same_CPU_Domains,
         All_Subjects         => All_Subjects,
         On_Same_CPU          => On_Same_CPU,
         On_Which_CPU         => On_Which_CPU,
         Same_CPU_Lengths     => Same_CPU_Lengths,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Same_CPU_Allowed     => Same_CPU_Allowed,
         Treshold             => Treshold,
         Level_Size_Array     => Level_Sizes,
         Per_Plan_Level_Count => Per_Plan_Level_Count,
         Plans                => Plans,
         Success              => Success,
         Blind_End            => Blind_End);

      if Success then
         Plan := Plans (This_Plan);
      end if;
   end Order_Strategies;

   -------------------------------------------------------------------------

   procedure Plan_A_Subject
     (Subject              :        Meta_Subject_Type;
      All_Subjects         :        TM.Meta_Subject_Vectors.Vector;
      Same_CPU_Lengths     :        Same_CPU_Length_Array;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Treshold             :        Treshold_Array;
      Level_Size_Array     :        TDS.Per_Plan_Level_Sizes_Type;
      Same_CPU_Domains     : in out Same_CPU_Domains_Array;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Plans                : in out TDS.All_Plans_Type;
      Treshold_Reached     :    out Boolean;
      Success              :    out Boolean)
   is
      use type Types.Minfs.Minorframes.Cursor;

      Chosen_CPU     : Natural := 0;
      Best_Score     : Float   := Float'Last;
      Score          : Float   := 0.0;
      Possible       : Boolean := True;
      Starting_Time  : Integer;
      Position       : Types.Minfs.Minorframes.Cursor;
      New_Minorframe : Types.Minfs.Minorframe_Type;
      I              : Integer;
      Starting_At    : Integer;
      Dont_Care      : Types.Minfs.Minorframes.Cursor;
      Extra_Time     : Integer;
      Plan_Weighting : constant Types.Float_Vectors.Vector
        := Input.Plan_Weighting;
   begin

      --  Choose CPU
      if On_Which_CPU (Subject.On_CPU_With_Number) = 0 then

         --  Choose CPU by approximate best-case scenario for each CPU_SET
         --  Take care of global(all plans) saved "is already
         --  on cpu"-cpu-domains use score function
         for C in 0 .. Input.CPU_Count loop
            if C /= 0 then

               Possible := True;
               for S of On_Same_CPU (Subject.On_CPU_With_Number) loop
                  if not Restrictions_Are_Satisfiable
                    (Subject_Set1 => All_Subjects (S).Same_CPU_Set,
                     Subject_Set2 => Same_CPU_Domains (C),
                     Restrictions => Same_CPU_Allowed)
                  then
                     Possible := False;
                  end if;
               end loop;

               if Possible then
                  Score := 0.0;
                  for Plan in Plans.Iterate loop
                     declare
                        P : constant Positive := TDS.All_Plans_Package.To_Index
                          (Position => Plan);
                     begin
                        if P = Subject.Plan then
                           if Plans (P)(C).Is_Empty then
                              Calc_Starting_Time
                                (CPU                  => C,
                                 Subject              => Subject,
                                 Simultaneous_Allowed => Simultaneous_Allowed,
                                 Plan                 => Plans (P),
                                 Time                 => Starting_At,
                                 Place                => Dont_Care);
                              Score := Score + Plan_Weighting (P)
                                * Float
                                (Same_CPU_Lengths (P)
                                 (Subject.On_CPU_With_Number)
                                 + Starting_At - Level_Size_Array (P)(1));
                           else
                              Calc_Starting_Time
                                (CPU                  => C,
                                 Subject              => Subject,
                                 Simultaneous_Allowed => Simultaneous_Allowed,
                                 Plan                 => Plans (P),
                                 Time                 => Starting_At,
                                 Place                => Dont_Care);
                              if Starting_At + Subject.Length - 1 >
                                Plans (P)(C).Last_Element.Endtime
                              then
                                 Extra_Time := Starting_At + Subject.Length - 1
                                   - Plans (P)(C).Last_Element.Endtime;
                              else
                                 Extra_Time := 0;
                              end if;

                              Score := Score + Plan_Weighting (P)
                                * Float
                                (Plans (P)(C).Last_Element.Endtime
                                 + Same_CPU_Lengths (P)
                                 (Subject.On_CPU_With_Number)
                                 - Subject.Length + Extra_Time
                                 - Level_Size_Array (P)(1));
                           end if;
                        else
                           if Plans (P)(C).Is_Empty then
                              Score := Score + Plan_Weighting (P)
                                * Float
                                (Same_CPU_Lengths (P)
                                 (Subject.On_CPU_With_Number)
                                 - Level_Size_Array (P)(1));
                           else
                              Score := Score + Plan_Weighting (P)
                                * Float
                                (Plans (P)(C).Last_Element.Endtime
                                 + Same_CPU_Lengths (P)
                                 (Subject.On_CPU_With_Number)
                                 - Level_Size_Array (P)(1));
                           end if;
                        end if;
                     end;
                  end loop;

                  if Score < Best_Score then
                     Chosen_CPU := C;
                     Best_Score := Score;
                  end if;
               end if;
            end if;
         end loop;

         On_Which_CPU (Subject.On_CPU_With_Number) := Integer (Chosen_CPU);

      else

         --  CPU is already Chosen
         I := On_Which_CPU (Subject.On_CPU_With_Number);
         Chosen_CPU := I;
      end if;

      Possible := False;
      if Chosen_CPU /= 0 then
         --  Choose next possible time
         Calc_Starting_Time (CPU                 => Chosen_CPU,
                            Subject              => Subject,
                            Simultaneous_Allowed => Simultaneous_Allowed,
                            Plan                 => Plans (Subject.Plan),
                            Time                 => Starting_Time,
                            Place                => Position);

         --  Create minorframe
         New_Minorframe.Subject          := Subject.Number;
         New_Minorframe.Starttime        := Starting_Time;
         New_Minorframe.Endtime          := Starting_Time + Subject.Length - 1;
         New_Minorframe.Simultaneous_Set := Subject.Simultaneous_Set;

         --  Put minorframe into the plan
         if Position = Types.Minfs.Minorframes.No_Element then
            Plans (Subject.Plan)(Chosen_CPU).Append (New_Minorframe);
         else
            Plans (Subject.Plan)(Chosen_CPU).Insert
              (Before   => Position,
               New_Item => New_Minorframe);
         end if;

         On_Which_CPU (Subject.On_CPU_With_Number) := Integer (Chosen_CPU);
         for C of On_Same_CPU (Subject.On_CPU_With_Number) loop
            Same_CPU_Domains (Chosen_CPU).Union
              (All_Subjects (C).Same_CPU_Set);
         end loop;

         Possible := True;
      end if;

      Treshold_Reached := Starting_Time + Subject.Length - 1 >=
        Treshold (Subject.Plan);
      Success := Possible;
   end Plan_A_Subject;

   -------------------------------------------------------------------------

   --  First call needs plans empty!
   --  Success needs to be False in first call
   --  Try to Plan all subjects in the order given with left_Subjects
   --  if  resulting majorframelength is higher than a given treshold: change
   --  order (backtracking) and try again
   --  Currently: Treshold is set so high that no bracktracking
   --  (=change of order) happens.
   --  If want to use backtracking: Adjust treshold if Puzzle_All_Plans failed
   --  (not implemented currently)
   procedure Puzzle_All_Plans
     (Left_Subjects        :        TM.Meta_Subject_Vectors.Vector;
      Same_CPU_Domains     :        Same_CPU_Domains_Array;
      All_Subjects         :        TM.Meta_Subject_Vectors.Vector;
      On_Same_CPU          :        Types.CPU_Sets.Vector;
      Same_CPU_Lengths     :        Same_CPU_Length_Array;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed     :        Types.Basics.Execution_Restrictions_Array;
      Treshold             :        Treshold_Array;
      Level_Size_Array     :        TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count :        Types.Positive_Vectors.Vector;
      On_Which_CPU         : in out Types.Integer_Vectors.Vector;
      Plans                : in out TDS.All_Plans_Type;
      Success              : in out Boolean;
      Blind_End            : in out Boolean)
   is
      Left_Subjects_Copy    : TM.Meta_Subject_Vectors.Vector := Left_Subjects;
      Treshold_Reached      : Boolean;
      Index                 : Integer;
      Plans_Copy            : TDS.All_Plans_Type := Plans;
      Blind_End_Copy        : Boolean            := False;
      Subject_Success       : Boolean;
      Same_CPU_Domains_Copy : Same_CPU_Domains_Array       := Same_CPU_Domains;
      On_Which_CPU_Copy     : Types.Integer_Vectors.Vector := On_Which_CPU;
   begin

      if Left_Subjects.Is_Empty then
         Success := True;
      else

         --  Plan first not already planed:
         Plan_A_Subject
           (All_Subjects         => All_Subjects,
            Level_Size_Array     => Level_Size_Array,
            Subject              => Left_Subjects.First_Element,
            Same_CPU_Domains     => Same_CPU_Domains_Copy,
            Same_CPU_Lengths     => Same_CPU_Lengths,
            On_Same_CPU          => On_Same_CPU,
            On_Which_CPU         => On_Which_CPU_Copy,
            Simultaneous_Allowed => Simultaneous_Allowed,
            Same_CPU_Allowed     => Same_CPU_Allowed,
            Treshold             => Treshold,
            Plans                => Plans_Copy,
            Treshold_Reached     => Treshold_Reached,
            Success              => Subject_Success);

         if Treshold_Reached or not Subject_Success then
            --  This order won't bring a solution
            Blind_End := True;
         else
            --  Try to plan next subject
            Left_Subjects_Copy.Delete_First;
            Puzzle_All_Plans
              (Left_Subjects        => Left_Subjects_Copy,
               Same_CPU_Domains     => Same_CPU_Domains_Copy,
               All_Subjects         => All_Subjects,
               On_Same_CPU          => On_Same_CPU,
               On_Which_CPU         => On_Which_CPU_Copy,
               Same_CPU_Lengths     => Same_CPU_Lengths,
               Simultaneous_Allowed => Simultaneous_Allowed,
               Same_CPU_Allowed     => Same_CPU_Allowed,
               Treshold             => Treshold,
               Level_Size_Array     => Level_Size_Array,
               Per_Plan_Level_Count => Per_Plan_Level_Count,
               Plans                => Plans_Copy,
               Success              => Success,
               Blind_End            => Blind_End_Copy);

            if Blind_End_Copy and not Success then
               --  Solution in this order failed
               Left_Subjects_Copy := Left_Subjects;
               Plans_Copy := Plans;
               --  Change order
               Index := Left_Subjects.First_Index;
               if Left_Subjects.Element (Index + 1).Order_Number >
                 Left_Subjects.Element (Index).Order_Number
               then
                  Left_Subjects_Copy (Index) :=
                    Left_Subjects.Element (Index + 1);
                  Left_Subjects_Copy (Index + 1) :=
                    Left_Subjects.Element (Index);

                  Puzzle_All_Plans
                    (Left_Subjects        => Left_Subjects_Copy,
                     Same_CPU_Domains     => Same_CPU_Domains,
                     All_Subjects         => All_Subjects,
                     On_Same_CPU          => On_Same_CPU,
                     On_Which_CPU         => On_Which_CPU,
                     Same_CPU_Lengths     => Same_CPU_Lengths,
                     Simultaneous_Allowed => Simultaneous_Allowed,
                     Same_CPU_Allowed     => Same_CPU_Allowed,
                     Treshold             => Treshold,
                     Level_Size_Array     => Level_Size_Array,
                     Per_Plan_Level_Count => Per_Plan_Level_Count,
                     Plans                => Plans_Copy,
                     Success              => Success,
                     Blind_End            => Blind_End_Copy);

                  if Blind_End_Copy and not Success then
                     Blind_End := True;
                  end if;

               else
                  --  The changed order was tried already => Failed
                  Blind_End := True;
               end if;
            end if;
         end if;
      end if;
      if Success then
         Plans := Plans_Copy;
         On_Which_CPU := On_Which_CPU_Copy;
      end if;
   end Puzzle_All_Plans;

   -------------------------------------------------------------------------

   procedure Puzzle_Metasubjects
     (On_Same_CPU              :     Types.CPU_Sets.Vector;
      Subjects_Level_Plans     :     TDS.All_Plans_Level_Type;
      Metasubjects_Level_Plans :     TDS.All_Plans_Level_Type;
      All_Subjects             :     Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed     :     Types.Basics.Execution_Restrictions_Array;
      Same_CPU_Allowed         :     Types.Basics.Execution_Restrictions_Array;
      Level_Sizes              :     TDS.Per_Plan_Level_Sizes_Type;
      Per_Plan_Level_Count     :     Types.Positive_Vectors.Vector;
      Success                  : out Boolean;
      On_Which_CPU             : out Types.Integer_Vectors.Vector;
      Plans                    : out TDS.All_Plans_Type)
   is
      subtype Plan_Range is Positive range 1 .. Input.Plan_Count;

      I                : Positive := 1;
      Same_CPU_Lengths : Same_CPU_Length_Array (Plan_Range);
      Subjects_To_Plan : TM.Meta_Subject_Vectors.Vector;
      Treshold         : Treshold_Array (Plan_Range);
      Blind_End        : Boolean := False;
      Same_CPU_Domains : Same_CPU_Domains_Array (0 .. Input.CPU_Count);
      On_Cpu_Number    : Integer;
   begin
      Success := False;

      --  Wanted first:Treshold= 2 times MajorframeLength => no solution in
      --  big scenarios; didn't found a good factor for all scenarios
      --  So: Treshold needs to be adjusted while runtime, if want to use
      --  backtracking in puzzle
      --  Currently: Treshold set very high:no backtracking used in puzzle
      for P in Plan_Range loop
         Treshold (P) := Integer'Last;
      end loop;

      for S in 1 .. Input.Subject_Count loop
         On_Which_CPU.Append (0);
      end loop;

      --  Calculate length of CPU_Sets on every plan
      --  CPU_SET: all subjects that HAVE TO BE planned at the same CPU
      for P in Plan_Range loop
         I := On_Same_CPU.First_Index;

         while not On_Same_CPU.Is_Empty and then I <= On_Same_CPU.Last_Index
         loop
            Same_CPU_Lengths (P).Append (0);
            I := I + 1;
         end loop;
      end loop;

      for P in Plan_Range loop
         for S of Subjects_Level_Plans (P)(1) loop
            On_Cpu_Number := S.On_CPU_With_Number;
            if On_Cpu_Number /= 0 then
               Same_CPU_Lengths (P)(On_Cpu_Number)
                 := Same_CPU_Lengths (P)(On_Cpu_Number) + S.Length;
            end if;
         end loop;

         for S of Metasubjects_Level_Plans (P)(1) loop
            On_Cpu_Number := S.On_CPU_With_Number;
            if On_Cpu_Number /= 0 then
               Same_CPU_Lengths (P)(On_Cpu_Number)
                 := Same_CPU_Lengths (P)(On_Cpu_Number) + S.Length;
            end if;
         end loop;

      end loop;

      --  Those subjects need to be planned:
      for P in Plan_Range loop
         Subjects_To_Plan.Append (Subjects_Level_Plans (P)(1));
         Subjects_To_Plan.Append (Metasubjects_Level_Plans (P)(1));
      end loop;

      --  Sort Meta_subjects per length, decreasing
      Find_Initial_Order (Subjects_To_Plan);

      --  Try to plan them in the chosen order, if treshold is exceeded:
      --  use backtracking
      Puzzle_All_Plans
        (Left_Subjects        => Subjects_To_Plan,
         Same_CPU_Domains     => Same_CPU_Domains,
         All_Subjects         => All_Subjects,
         On_Same_CPU          => On_Same_CPU,
         On_Which_CPU         => On_Which_CPU,
         Same_CPU_Lengths     => Same_CPU_Lengths,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Same_CPU_Allowed     => Same_CPU_Allowed,
         Treshold             => Treshold,
         Level_Size_Array     => Level_Sizes,
         Per_Plan_Level_Count => Per_Plan_Level_Count,
         Plans                => Plans,
         Success              => Success,
         Blind_End            => Blind_End);
   end Puzzle_Metasubjects;

   -------------------------------------------------------------------------

   --  This should be the only time when something random is tried
   function Random_Order
     (Subjects : TM.Meta_Subject_Vectors.Vector)
      return TM.Meta_Subject_Vectors.Vector
   is
      Result      : TM.Meta_Subject_Vectors.Vector;
      Permutation : Types.Integer_Vectors.Vector;
      Len         : constant Positive := Positive (Subjects.Length);
   begin
      Permutation := Create_Permutation (N => Len);

      for I in 1 .. Len loop
         Result.Append (New_Item => Subjects (Permutation (I)));
      end loop;

      return Result;
   end Random_Order;

end Steps.Plan_Building_Lowest_Level;
