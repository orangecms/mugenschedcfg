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
with Ada.Containers.Vectors;

with Input;
with Types.Integer_Vectors;
with Steps.Plan_Building_Blockers; use Steps.Plan_Building_Blockers;

package body Steps.Short
is

   type Shorten_Entry is
      record
         costs : Float;
         short_no_others : Boolean;
         Short_Others_Numbers : Types.Integer_Vectors.Vector;
         Short_Others_CPUs : Types.Integer_Vectors.Vector;
         Position : Types.Minfs.Minorframes.Cursor;
      end record;

   package shorten_vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Shorten_Entry);

   type shorten_vectors_array is array (Positive range <>)
     of shorten_vectors.Vector;

   --  Calc how  much score is lost, if subject is either shortened by one
   --  or subject is moved left by one and save how much and which costs less
   --  UNFAIR! If Movement_Costs = Shorten_Itself_Cost then it will be
   --  shortened itself -> Later scheduled meta-subjects disadvantaged
   function Calc_Shorten_Entry
     (Minor                      : Types.Minfs.Minorframes.Cursor;
      CPU                        : Natural;
      Simultaneous_Allowed       : Types.Basics.Execution_Restrictions_Array;
      Ticks_Per_Subject          : Objective.Ticks_Per_Subject_Type;
      Level_Sizes                : Types.Basics.Natural_Array;
      Subjects_And_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Majorframe                 : Integer;
      Chains_Vector              : Types.Chain.Chains_Vectors.Vector;
      All_Shorten_Vectors        : shorten_vectors_array)
      return Shorten_Entry;

   procedure Costs_Move_One_Left
     (Minor                :     Types.Minfs.Minorframes.Cursor;
      CPU                  :     Natural;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      All_Shorten_Vectors  :     shorten_vectors_array;
      Short_Others_Numbers : out Types.Integer_Vectors.Vector;
      Short_Others_CPUs    : out Types.Integer_Vectors.Vector;
      Costs                : out Float);

   procedure Costs_Shorten_A_Subject
     (Current_Subject            :        Integer;
      Current_Level              :        Positive;
      Level_Sizes                :        TB.Natural_Array;
      Chain_Vector               :        Types.Chain.Chains_Vectors.Vector;
      Majorframe                 :        Integer;
      Ticks_Per_Subject          : in out Objective.Ticks_Per_Subject_Type;
      Subjects_and_Meta_Subjects : in out TMV.Vector;
      Cost                       : out    Float);

   procedure minus_one (I : in out Integer);

   procedure Reduce_Endtime (M : in out Types.Minfs.Minorframe_Type);

   procedure Short_By_One
     (Short_CPU             :        Integer;
      Short_Number          :        Integer;
      Waiting_For_This      :        Types.Minfs.Minorframes.Cursor;
      All_Shorten_Vectors   :        shorten_vectors_array;
      Level_Sizes           :        Types.Basics.Natural_Array;
      Chain_Vector          :        Types.Chain.Chains_Vectors.Vector;
      Majorframe            :        Integer;
      Plan                  : in out Types.Data_Structures.Level_Type;
      Subjects              : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject     : in out Objective.Ticks_Per_Subject_Type;
      Earliest_Time_Changed : in out Integer);

   ------------------------------------------------------------------------

   function Calc_Shorten_Entry
     (Minor                      : Types.Minfs.Minorframes.Cursor;
      CPU                        : Natural;
      Simultaneous_Allowed       : Types.Basics.Execution_Restrictions_Array;
      Ticks_Per_Subject          : Objective.Ticks_Per_Subject_Type;
      Level_Sizes                : Types.Basics.Natural_Array;
      Subjects_And_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Majorframe                 : Integer;
      Chains_Vector              : Types.Chain.Chains_Vectors.Vector;
      All_Shorten_Vectors        : shorten_vectors_array)
      return Shorten_Entry
   is
      My_Shorten_Entry : Shorten_Entry;
      Current_Costs : Float := 0.0;
      Movement_Costs : Float;
      My_Ticks_Per_Subject : Objective.Ticks_Per_Subject_Type
        := Ticks_Per_Subject;
      My_Subjects_and_Meta_Subjects : TMV.Vector := Subjects_And_Meta_Subjects;
   begin

      My_Shorten_Entry.Position := Minor;

      if Types.Minfs.Minorframes.Element (Minor).Starttime = 0 then
         My_Shorten_Entry.short_no_others := True;

         Costs_Shorten_A_Subject
           (Ticks_Per_Subject          => My_Ticks_Per_Subject,
            Current_Subject            =>
              Types.Minfs.Minorframes.Element (Minor).Subject,
            Current_Level              => 1,
            Level_Sizes                => Level_Sizes,
            Subjects_and_Meta_Subjects => My_Subjects_and_Meta_Subjects,
            Chain_Vector               => Chains_Vector,
            Majorframe                 => Majorframe,
            Cost                       => My_Shorten_Entry.costs);

         --  Other fields of shorten_entry not needed
      else
         Costs_Shorten_A_Subject
           (Ticks_Per_Subject          => My_Ticks_Per_Subject,
            Current_Subject            =>
              Types.Minfs.Minorframes.Element (Minor).Subject,
            Current_Level              => 1,
            Level_Sizes                => Level_Sizes,
            Subjects_and_Meta_Subjects => My_Subjects_and_Meta_Subjects,
            Chain_Vector               => Chains_Vector,
            Majorframe                 => Majorframe,
            Cost                       => Current_Costs);

         --  Calc cost if blocking metasubjects are shortened
         Costs_Move_One_Left
           (Minor                => Minor,
            CPU                  => CPU,
            Simultaneous_Allowed => Simultaneous_Allowed,
            All_Shorten_Vectors  => All_Shorten_Vectors,
            Short_Others_Numbers => My_Shorten_Entry.Short_Others_Numbers,
            Short_Others_CPUs    => My_Shorten_Entry.Short_Others_CPUs,
            Costs                => Movement_Costs);

         if Movement_Costs < Current_Costs then
            My_Shorten_Entry.short_no_others := False;
            My_Shorten_Entry.costs := Movement_Costs;
         else
            My_Shorten_Entry.short_no_others := True;
            My_Shorten_Entry.costs := Current_Costs;
         end if;
      end if;

      return My_Shorten_Entry;
   end Calc_Shorten_Entry;

   ------------------------------------------------------------------------

   --  How expensive is it to short all that block this subject from beeing
   --  moved one left
   procedure Costs_Move_One_Left
     (Minor                :     Types.Minfs.Minorframes.Cursor;
      CPU                  :     Natural;
      Simultaneous_Allowed :     Types.Basics.Execution_Restrictions_Array;
      All_Shorten_Vectors  :     shorten_vectors_array;
      Short_Others_Numbers : out Types.Integer_Vectors.Vector;
      Short_Others_CPUs    : out Types.Integer_Vectors.Vector;
      Costs                : out Float)
   is
      I : Integer;
   begin
      Costs := 0.0;

      --  Need to check all CPUs for blocking subjects
      --  Shorten vectors of subjects that are planned to end BEFORE the
      --  current Subject are already calculated
      --  -> look up how expensive it is to
      --  shorten them there (maybe they are moved left and others before them
      --  are shortened, but we don't need to know this here)
      for C in 1 .. Input.CPU_Count loop
         if C = CPU then
            if not All_Shorten_Vectors (C).Is_Empty and then
              Types.Minfs.Minorframes.Element
                (All_Shorten_Vectors (C).Last_Element.Position)
                  .Endtime
                    = Types.Minfs.Minorframes.Element (Minor).Starttime - 1
              and then Costs /= Float'Last
            then
               Costs := Costs + All_Shorten_Vectors (C).Last_Element.costs;
               Short_Others_CPUs.Append (C);
               Short_Others_Numbers.Append (All_Shorten_Vectors (C)
                                            .Last_Index);
            end if;

         else
            I := All_Shorten_Vectors (C).Last_Index;

            while I > 0 and then Types.Minfs.Minorframes.Element
              (All_Shorten_Vectors (C)(I).Position).Endtime >
                Types.Minfs.Minorframes.Element (Minor).Starttime - 1 loop
               --  They are not planned at the same time
               I := I - 1;
            end loop;

            if I > 0 and then Types.Minfs.Minorframes.Element
              (All_Shorten_Vectors (C)(I).Position).Endtime =
                Types.Minfs.Minorframes.Element (Minor).Starttime - 1
            --  They would be simutaneous after moving left
              and then not
                Restrictions_Are_Satisfiable
                  (Subject_Set1 => Types.Minfs.Minorframes.Element
                     (All_Shorten_Vectors (C)(I).Position).Simultaneous_Set,
                   Subject_Set2 => Types.Minfs.Minorframes.Element
                     (Minor).Simultaneous_Set,
                   Restrictions => Simultaneous_Allowed)
              and then Costs /= Float'Last
            then
               Costs := Costs + All_Shorten_Vectors (C)(I).costs;
               Short_Others_CPUs.Append (C);
               Short_Others_Numbers.Append (I);
            end if;
         end if;
      end loop;
   end Costs_Move_One_Left;

   ------------------------------------------------------------------------

   --  EFFICIENCY could be improved by using a list that saves all chains to a
   --  given subject -> only these chains need to be checked
   --  ATTENTION Costs of shorten a metasubject does only check total ticks
   --  NO GARANTEE that the ticks of every subject are schedulable in the meta
   --  subject (Problems might be: Minimal Length of a minorframe of a subject;
   --                             No splitting of this subject allowed
   --                             Maybe more?)
   procedure Costs_Shorten_A_Subject
     (Current_Subject            :        Integer;
      Current_Level              :        Positive;
      Level_Sizes                :        TB.Natural_Array;
      Chain_Vector               :        Types.Chain.Chains_Vectors.Vector;
      Majorframe                 :        Integer;
      Ticks_Per_Subject          : in out Objective.Ticks_Per_Subject_Type;
      Subjects_and_Meta_Subjects : in out TMV.Vector;
      Cost                       : out    Float)
   is
      My_Ticks_per_Subject : Objective.Ticks_Per_Subject_Type
        := Ticks_Per_Subject;
      My_Subjects_and_Meta_Subjects : TMV.Vector := Subjects_and_Meta_Subjects;
      Min_Cost : Float := Float'Last;
      Current_Cost : Float;
      Chosen : Integer;
      Factor : Integer;
      Sum : Integer;
      J : Integer;
      Place : Integer;
   begin

      if  Subjects_and_Meta_Subjects (Current_Subject).Length = 1 then
         Cost := Float'Last; --  This subject cannot be shortened!
      else
         if Current_Subject <= Input.Subject_Count then
            --  Subject to shorten is a normal subject
            Cost := Objective.Calc_Score_Of_One_Plan
              (Ticks_Per_Subject => Ticks_Per_Subject,
               Chains_Vector     => Chain_Vector);
            --  Short it by one on its level
            My_Ticks_per_Subject (Current_Subject) :=
              My_Ticks_per_Subject (Current_Subject)
              - Majorframe / Level_Sizes (Current_Level);

            Cost := Cost - Objective.Calc_Score_Of_One_Plan
              (Ticks_Per_Subject => My_Ticks_per_Subject,
               Chains_Vector     => Chain_Vector);

            Ticks_Per_Subject := My_Ticks_per_Subject;

            Subjects_and_Meta_Subjects (Current_Subject).Length :=
              Subjects_and_Meta_Subjects (Current_Subject).Length - 1;

         else
            --  Subject to shorten is a metasubject

            Factor := Level_Sizes (Current_Level + 1) /
              Level_Sizes (Current_Level);

            --  So many ticks must be shortened

            Sum := 0;
            for T of Subjects_and_Meta_Subjects (Current_Subject).Tick_Vector
            loop
               Sum := Sum + T;
            end loop;

            if Subjects_and_Meta_Subjects (Current_Subject).Length * Factor
              > Sum
            then
               --  Enough idle time in metasubject
               Cost := 0.0;
               Factor := Factor -
                 (Subjects_and_Meta_Subjects (Current_Subject).Length * Factor
                  - Sum);
            end if;

            for I in 1 .. Factor loop
               J := 0;
               Min_Cost := Float'Last;
               --  Calc min cost of shorten one subject
               --  that is part of metasubject
               for S of Subjects_and_Meta_Subjects (Current_Subject)
                 .Combined_Subjects
               loop

                  My_Ticks_per_Subject := Ticks_Per_Subject;
                  My_Subjects_and_Meta_Subjects := Subjects_and_Meta_Subjects;

                  J := J + 1;
                  Costs_Shorten_A_Subject
                    (Ticks_Per_Subject          => My_Ticks_per_Subject,
                     Current_Subject            => S,
                     Current_Level              => Current_Level + 1,
                     Level_Sizes                => Level_Sizes,
                     Subjects_and_Meta_Subjects =>
                       My_Subjects_and_Meta_Subjects,
                     Chain_Vector               => Chain_Vector,
                     Cost                       => Current_Cost,
                     Majorframe                 => Majorframe);

                  if Min_Cost > Current_Cost then
                     Chosen := S;
                     Min_Cost := Current_Cost;
                     Place := J;
                  end if;

               end loop;

               if Min_Cost /= Float'Last then
                  --  Subject is allowed to be shortened
                  --  Short it
                  Costs_Shorten_A_Subject
                    (Ticks_Per_Subject          => Ticks_Per_Subject,
                     Current_Subject            => Chosen,
                     Current_Level              => Current_Level + 1,
                     Level_Sizes                => Level_Sizes,
                     Subjects_and_Meta_Subjects =>
                       Subjects_and_Meta_Subjects,
                     Chain_Vector               => Chain_Vector,
                     Cost                       => Current_Cost,
                     Majorframe                 => Majorframe);

                  Subjects_and_Meta_Subjects (Current_Subject)
                    .Tick_Vector.Update_Element
                      (Place, minus_one'Access);

                  if Subjects_and_Meta_Subjects (Current_Subject)
                    .Tick_Vector (Place) = 0
                  then
                     Subjects_and_Meta_Subjects (Current_Subject)
                       .Tick_Vector.Delete (Place);
                     Subjects_and_Meta_Subjects (Current_Subject)
                       .Combined_Subjects.Delete (Place);
                  end if;

                  if Cost /= Float'Last then
                     Cost := Cost + Current_Cost;
                  end if;
               else
                  Cost := Float'Last;
               end if;

            end loop;

            if Cost /= Float'Last then
               Subjects_and_Meta_Subjects (Current_Subject).Length :=
                 Subjects_and_Meta_Subjects (Current_Subject).Length - 1;
            end if;
         end if;
      end if;
   end Costs_Shorten_A_Subject;

   ------------------------------------------------------------------------

   procedure minus_one (I : in out Integer) is
   begin
      I := I - 1;
   end minus_one;

   ------------------------------------------------------------------------

   procedure Move_Left (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Starttime := M.Starttime - 1;
      M.Endtime   := M.Endtime - 1;
   end Move_Left;

   ------------------------------------------------------------------------

   procedure Reduce_Endtime (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Endtime := M.Endtime - 1;
   end Reduce_Endtime;

   ------------------------------------------------------------------------

   --  Short a plan by one after shorting entries are calculated.
   procedure Short_By_One
     (Short_CPU             :        Integer;
      Short_Number          :        Integer;
      Waiting_For_This      :        Types.Minfs.Minorframes.Cursor;
      All_Shorten_Vectors   :        shorten_vectors_array;
      Level_Sizes           :        Types.Basics.Natural_Array;
      Chain_Vector          :        Types.Chain.Chains_Vectors.Vector;
      Majorframe            :        Integer;
      Plan                  : in out Types.Data_Structures.Level_Type;
      Subjects              : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject     : in out Objective.Ticks_Per_Subject_Type;
      Earliest_Time_Changed : in out Integer)
   is
      Short_This : constant Shorten_Entry := All_Shorten_Vectors
        (Short_CPU)(Short_Number);
      I : Integer;
      Dont_Care : Float;
   begin

      --  To ensure one is not shortened double because of double dependencies
      if not Types.Minfs.Minorframes."="
        (Waiting_For_This, Types.Minfs.Minorframes.No_Element) and then
        Types.Minfs.Minorframes.Element (Waiting_For_This).Starttime - 1 >
        Types.Minfs.Minorframes.Element (Short_This.Position).Endtime
      then
         null;
      else

         if All_Shorten_Vectors (Short_CPU)(Short_Number).short_no_others then
            --  Short this subject
            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (Short_CPU),
               Position  => Short_This.Position,
               Process   => Reduce_Endtime'Access);

            if Types.Minfs.Minorframes.Element (Short_This.Position).Endtime
              < Earliest_Time_Changed
            then
               Earliest_Time_Changed :=
                 Types.Minfs.Minorframes.Element (Short_This.Position).Endtime;
            end if;

            Costs_Shorten_A_Subject
              (Ticks_Per_Subject          => Ticks_Per_Subject,
               Current_Subject            => Types.Minfs.Minorframes.Element
                 (Short_This.Position).Subject,
               Current_Level              => 1,
               Level_Sizes                => Level_Sizes,
               Subjects_and_Meta_Subjects => Subjects,
               Chain_Vector               => Chain_Vector,
               Majorframe                 => Majorframe,
               Cost                       => Dont_Care);

         else
            --  Short others and move left afterwards
            I := Short_This.Short_Others_Numbers.First_Index;
            while I <= Short_This.Short_Others_Numbers.Last_Index loop
               Short_By_One
                 (Plan                  => Plan,
                  Short_CPU             => Short_This.Short_Others_CPUs (I),
                  Short_Number          => Short_This.Short_Others_Numbers (I),
                  Waiting_For_This      => Short_This.Position,
                  All_Shorten_Vectors   => All_Shorten_Vectors,
                  Subjects              => Subjects,
                  Ticks_Per_Subject     => Ticks_Per_Subject,
                  Level_Sizes           => Level_Sizes,
                  Chain_Vector          => Chain_Vector,
                  Majorframe            => Majorframe,
                  Earliest_Time_Changed => Earliest_Time_Changed);
               I := I + 1;
            end loop;

            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (Short_CPU),
               Position  => Short_This.Position,
               Process   => Move_Left'Access);

            if Types.Minfs.Minorframes.Element (Short_This.Position).Starttime
              < Earliest_Time_Changed
            then
               Earliest_Time_Changed :=
                 Types.Minfs.Minorframes
                   .Element (Short_This.Position).Starttime;
            end if;
         end if;

      end if;

   end Short_By_One;

   ------------------------------------------------------------------------

   --  If one cannot move left because of two others and they cannot move
   --  because of one subject then there will be a mistake and the costs
   --  of this one subject will be added two instead of one time.
   --  This could be fixed by either using more memory O(2^#CPU)
   --  or allowing longer computing time O(n²)

   --  Efficiency: Dont have to calc all entries new every round

   --  Calculate how much rating is lost by shorten one subject by one or
   --  shorten those before it by one and move it
   --  SHOULD NOT BE CALLED IF PLAN IS SHORT ENOUGH (it will short it once in
   --  every case it is called)
   procedure Shorten_Plan
     (Majorframe                     :        Integer;
      Subjects_and_Meta_Subjects_in  :        TMV.Vector;
      Simultaneous_Allowed           :        TB.Execution_Restrictions_Array;
      Level_Sizes                    :        TB.Natural_Array;
      Chains_Vector                  :        TCV.Vector;
      Ticks_Per_Subject              : in out Objective.Ticks_Per_Subject_Type;
      Plan                           : in out Types.Data_Structures.Level_Type;
      Subjects_and_Meta_Subjects_out : out    TMV.Vector;
      Success                        : out    Boolean)
   is
      CPU_Count : constant Positive := Input.CPU_Count;
      subtype CPU_Range is Natural range 0 .. CPU_Count;
      subtype Positive_CPU_Range is Positive range 1 .. CPU_Range'Last;

      Iterator_Array : Iterator_Array_Type (Positive_CPU_Range);
      Time_done_till : Integer := -1;
      End_time_Array : time_Array_Type (Positive_CPU_Range);
      Numbers : time_Array_Type (Positive_CPU_Range);
      Next_CPU : Next_CPU_Type (Positive_CPU_Range);
      This_CPU : Integer;
      I : Integer;
      J : Integer;
      shorten_vector : shorten_vectors_array (Positive_CPU_Range);
      New_endtime : Integer := Integer'Last;
      New_shorten_Entry : Shorten_Entry;
      Last_Endtime : Integer := Integer'Last;
      Not_Possible : Boolean := False;
   begin
      Subjects_and_Meta_Subjects_out := Subjects_and_Meta_Subjects_in;

      Last_Endtime := -1;
      --  Init Last_endtimes to find out plans on which CPUs are too long
      for C in CPU_Range loop
         if not Plan (C).Is_Empty
           and then Plan (C).Last_Element.Endtime > Last_Endtime
         then
            Last_Endtime := Plan (C).Last_Element.Endtime;
         end if;
      end loop;

      while Last_Endtime >= Level_Sizes (1) and not Not_Possible loop

         --  Find order
         for S of shorten_vector loop
            shorten_vectors.Clear (S);
         end loop;

         for N of Numbers loop
            N := 1;
         end loop;

         for E of End_time_Array loop
            E := Integer'Last;
         end loop;

         for C of Next_CPU loop
            C := -1;
         end loop;

         for C in Positive_CPU_Range loop
            if not Types.Minfs.Minorframes."="
              (Plan (C), Types.Minfs.Minorframes.Empty_List)
            then
               Iterator_Array (C) := Plan (C).First;

               --  Sort CPUs after endtimes
               I := 1;
               while Plan (C).First_Element.Endtime
                 >= End_time_Array (I) loop
                  I := I + 1;
               end loop;
               J := I;

               while End_time_Array (J) /= Integer'Last loop
                  J := J + 1;
               end loop;

               while J > I loop
                  End_time_Array (J) := End_time_Array (J - 1);
                  Next_CPU (J) := Next_CPU (J - 1);
                  J := J - 1;
               end loop;

               End_time_Array (J) := Plan (C).First_Element.Endtime;
               Next_CPU (J) := C;
            end if;
         end loop;

         --  Iterate over all CPUs: CALC SHORTEN ENTRIES
         --  To every minorframe store if shorten self or shorten others and
         --  at which cost

         while Next_CPU (1) /= -1  loop

            --  Calculate Short_Entry for Iterator_Array(Next_CPU(1))
            New_shorten_Entry := Calc_Shorten_Entry
              (Minor                      => Iterator_Array (Next_CPU (1)),
               CPU                        => CPU_Range (Next_CPU (1)),
               Simultaneous_Allowed       => Simultaneous_Allowed,
               Ticks_Per_Subject          => Ticks_Per_Subject,
               Level_Sizes                => Level_Sizes,
               Subjects_And_Meta_Subjects => Subjects_and_Meta_Subjects_out,
               Majorframe                 => Majorframe,
               Chains_Vector              => Chains_Vector,
               All_Shorten_Vectors        => shorten_vector);

            --  Update Iteratorarray, Next_CPU and End_time_array
            shorten_vector (Next_CPU (1)).Append (New_shorten_Entry);
            This_CPU := Next_CPU (1);

            if not Types.Minfs.Minorframes."="(Iterator_Array (Next_CPU (1))
                                   , Plan (CPU_Range (Next_CPU (1))).Last)
            then
               Types.Minfs.Minorframes.Next (Iterator_Array (Next_CPU (1)));
               New_endtime := Types.Minfs.Minorframes
                 .Element (Iterator_Array (Next_CPU (1)))
                 .Endtime;
               End_time_Array (1) := New_endtime;

               I := 1;
               while I < CPU_Count and then
                 New_endtime > End_time_Array (I + 1) loop
                  End_time_Array (I) := End_time_Array (I + 1);
                  Next_CPU (I) := Next_CPU (I + 1);
                  I := I + 1;
               end loop;
               End_time_Array (I) := New_endtime;
               Next_CPU (I) := This_CPU;
            else
               I := 1;
               while  I < CPU_Count and then Next_CPU (I + 1) /= -1 loop
                  Next_CPU (I) := Next_CPU (I + 1);
                  End_time_Array (I) := End_time_Array (I + 1);
                  I := I + 1;
               end loop;
               Next_CPU (I) := -1;
               End_time_Array (I) := Integer'Last;

            end if;
            --  We are done and found the one with the next earlist endtime
            --  calc this one next
         end loop;

         if not shorten_vector (This_CPU).Is_Empty and then
           shorten_vector (This_CPU).Last_Element.costs = Float'Last
         then
            Not_Possible := True;
            for S of shorten_vector (This_CPU) loop
               Put (Types.Minfs.Minorframes.Element (S.Position).Subject'Img);
               Put (S.costs'Img);
               Put (S.short_no_others'Img);
               for I of S.Short_Others_Numbers loop
                  Put (I'Img);
               end loop;
               Put ("|");
               for I of S.Short_Others_CPUs loop
                  Put (I'Img);
               end loop;
               New_Line;
            end loop;
         else
            --  Latest Endtime is on This_CPU
            --  shorten this like shorten entries say
            Time_done_till := Integer'Last;

            Short_By_One
              (Plan                  => Plan,
               Short_CPU             => This_CPU,
               Short_Number          => shorten_vector (This_CPU).Last_Index,
               Waiting_For_This      => Types.Minfs.Minorframes.No_Element,
               All_Shorten_Vectors   => shorten_vector,
               Subjects              => Subjects_and_Meta_Subjects_out,
               Ticks_Per_Subject     => Ticks_Per_Subject,
               Level_Sizes           => Level_Sizes,
               Chain_Vector          => Chains_Vector,
               Majorframe            => Majorframe,
               Earliest_Time_Changed => Time_done_till);

            Last_Endtime := -1;

            for C in CPU_Range loop
               if not Plan (C).Is_Empty
                 and then Plan (C).Last_Element.Endtime > Last_Endtime
               then
                  Last_Endtime := Plan (C).Last_Element.Endtime;
               end if;
            end loop;

         end if;

         --  If not majorframelength reaches -> need to shorten again
      end loop;

      Success := not Not_Possible;
      Put_Line ("Last_Endtime" & Last_Endtime'Img);
      Put_Line (Level_Sizes (1)'Img);
      Put_Line (Success'Img);
   end Shorten_Plan;

end Steps.Short;
