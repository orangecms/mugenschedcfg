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
with Types.Minfs.Minorframes;
with Types.Chain.Chains;
with Types.Chain.Chains_Cursor_Vectors;
with Types.Chain.Chain_Link; use Types.Chain.Chain_Link;
with Types.Chain.Chains_With_Score; use Types.Chain.Chains_With_Score;
with Types.Float_Vectors;
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Types.Integer_Vectors;
with Types.Minfs.Minorframe_Iterator;
with Steps.Plan_Building_Blockers; use Steps.Plan_Building_Blockers;

package body Steps.Grow
is

   function Calc_Blocking_Subjects_Left
     (Position             : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array;
      Plan                 : Types.Data_Structures.Level_Type)
      return Types.Minfs.Minorframe_Iterator.List;

   function Calc_Blocking_Subjects_Right
     (Position             : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      Length_L1            : Integer;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array;
      Plan                 : Types.Data_Structures.Level_Type)
      return Types.Minfs.Minorframe_Iterator.List;

   function Create_Meta_List
     (Wanted            : Integer;
      Search_In         : Integer;
      Found             : Boolean;
      Meta_and_subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Current_List      : Types.Integer_Vectors.Vector)
      return Types.Integer_Vectors.Vector;

   procedure Expand_Left
     (M                    :        Types.Minfs.Minorframes.Cursor;
      CPU                  :        Natural;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean);

   --  Expand subject with "Number" by one, moving of other minorframes is
   --  allowed.
   procedure Expand_Level_1_By_1
     (Number               :        Integer;
      CPU                  :        Natural;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Length_L1            :        Integer;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Global_Success       :    out Boolean);

   procedure Expand_One_Left  (M : in out Types.Minfs.Minorframe_Type);
   procedure Expand_One_Right (M : in out Types.Minfs.Minorframe_Type);

   procedure Expand_Right
     (M                    :        Types.Minfs.Minorframes.Cursor;
      CPU                  :        Natural;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Length_L1            :        Integer;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean);

   procedure Grow_Chain
     (Chosen               :        Types.Chain.Chains.List;
      Change_Plan          :        Boolean;
      My_Levels            :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject    : in out Objective.Ticks_Per_Subject_Type;
      Troughput            : in out Float;
      Ticks                :    out Integer;
      Extra_Troughput      :    out Float);

   procedure Grow_Subject_By_One
     (Chosen               :        Integer;
      My_Levels            :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_needed         :    out Integer;
      Ticks_gained         :    out Integer;
      Global_Success       :    out Boolean);

   procedure Move_Left
     (M                    :        Types.Minfs.Minorframes.Cursor;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean);

   procedure Move_One_Left (M : in out Types.Minfs.Minorframe_Type);

   procedure Move_One_Right (M : in out Types.Minfs.Minorframe_Type);

   procedure Move_Right
     (M                    :        Types.Minfs.Minorframes.Cursor;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Length_L1            :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean);

   -------------------------------------------------------------------------

   function Calc_Blocking_Subjects_Left
     (Position             : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array;
      Plan                 : Types.Data_Structures.Level_Type)
      return Types.Minfs.Minorframe_Iterator.List
   is
      Result   : Types.Minfs.Minorframe_Iterator.List;
      Iterator : Types.Minfs.Minorframes.Cursor;
      Minor    : constant Types.Minfs.Minorframe_Type
        := Types.Minfs.Minorframes.Element (Position);
   begin
      if Minor.Starttime /= 0 then
         for C in 0 .. Input.CPU_Count loop
            if not Plan (C).Is_Empty then
               if C /= CPU then
                  Iterator := Types.Minfs.Minorframes.First (Plan (C));
                  while
                    Types.Minfs.Minorframes.Element (Iterator).Endtime
                    < Minor.Starttime - 1
                    and not Types.Minfs.Minorframes."="
                      (Iterator, Plan (C).Last) loop
                     Types.Minfs.Minorframes.Next (Iterator);
                  end loop;

                  if Types.Minfs.Minorframes.Element (Iterator).Endtime =
                    Minor.Starttime - 1
                  then
                     if not Restrictions_Are_Satisfiable
                       (Subject_Set1 => All_Subjects (Types.Minfs.Minorframes
                        .Element (Iterator)
                        .Subject).Simultaneous_Set,
                        Subject_Set2 => All_Subjects (Minor.Subject)
                        .Simultaneous_Set,
                        Restrictions => Simultaneous_Allowed)
                     then
                        Result.Append (Iterator);
                     end if;
                  end if;
               else
                  if not Types.Minfs.Minorframes."="
                    (Position, Plan (CPU).First)
                  then
                     Iterator := Types.Minfs.Minorframes.Previous (Position);
                     if Types.Minfs.Minorframes.Element (Iterator).Endtime =
                       Minor.Starttime - 1
                     then
                        Result.Append (Iterator);
                     end if;
                  end if;
               end if;
            end if;

         end loop;
      end if;

      return Result;
   end Calc_Blocking_Subjects_Left;

   -------------------------------------------------------------------------

   function Calc_Blocking_Subjects_Right
     (Position             : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      Length_L1            : Integer;
      All_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array;
      Plan                 : Types.Data_Structures.Level_Type)
      return Types.Minfs.Minorframe_Iterator.List
   is
      Result   : Types.Minfs.Minorframe_Iterator.List;
      Iterator : Types.Minfs.Minorframes.Cursor;
      Minor    : constant Types.Minfs.Minorframe_Type
        := Types.Minfs.Minorframes.Element (Position);
   begin
      if Minor.Endtime /= Length_L1 - 1 then
         for C in 0 .. Input.CPU_Count loop
            if not Plan (C).Is_Empty then
               if C /= CPU then
                  Iterator := Types.Minfs.Minorframes.Last (Plan (C));
                  while Types.Minfs.Minorframes.Element (Iterator).Starttime
                    > Minor.Endtime + 1
                    and not Types.Minfs.Minorframes."="(Iterator, Plan (C)
                                                        .First)
                  loop
                     Types.Minfs.Minorframes.Previous (Iterator);
                  end loop;

                  if Types.Minfs.Minorframes.Element (Iterator).Starttime
                    = Minor.Endtime + 1
                  then
                     if not Restrictions_Are_Satisfiable
                       (Subject_Set1 => All_Subjects (Types.Minfs.Minorframes
                        .Element (Iterator)
                        .Subject).Simultaneous_Set,
                        Subject_Set2 => All_Subjects (Minor.Subject)
                        .Simultaneous_Set,
                        Restrictions => Simultaneous_Allowed)
                     then
                        Result.Append (Iterator);
                     end if;
                  end if;
               else

                  if Types.Minfs.Minorframes.Has_Element
                    (Types.Minfs.Minorframes.Next (Position))
                  then
                     Iterator := Types.Minfs.Minorframes.Next (Position);
                     if Types.Minfs.Minorframes.Element (Iterator).Starttime =
                       Minor.Endtime + 1
                     then
                        Result.Append (Iterator);
                     end if;
                  end if;
               end if;
            end if;

         end loop;
      end if;

      return Result;
   end Calc_Blocking_Subjects_Right;

   -------------------------------------------------------------------------

   --  Results of this could be cached!
   --  Found needs to be False, search_in -1 in first call
   --  returns vector of metasubjects that include the wanted Subject
   function Create_Meta_List
     (Wanted            : Integer;
      Search_In         : Integer;
      Found             : Boolean;
      Meta_and_subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Current_List      : Types.Integer_Vectors.Vector)
      return Types.Integer_Vectors.Vector
   is
      use type Types.Integer_Vectors.Vector;

      List     : Types.Integer_Vectors.Vector := Current_List;
      Result   : Types.Integer_Vectors.Vector;
      Found_It : Boolean := Found;
      Ok       : Boolean := False;
   begin
      if Meta_and_subjects (Wanted).Level = 1 then
         List.Append (Wanted);
         return List;
      end if;

      if Search_In = -1 then
         for I in Input.Subject_Count + 1 .. Meta_and_subjects.Last_Index loop
            if Meta_and_subjects (I).Level = 1 then
               Result := Create_Meta_List
                 (Wanted            => Wanted,
                  Search_In         => I,
                  Found             => False,
                  Meta_and_subjects => Meta_and_subjects,
                  Current_List      => Types.Integer_Vectors.Empty_Vector);
               if Types.Integer_Vectors.Empty_Vector /= Result then
                  List := Result;
                  Ok   := True;
               end if;
            end if;
         end loop;
      else
         if Meta_and_subjects (Search_In).Combined_Subjects.Contains (Wanted)
         then
            Found_It := True;
            List.Append (Search_In);
            List.Append (Wanted);
            return List;
         end if;

         if Meta_and_subjects (Search_In).Level
           > Meta_and_subjects (Wanted).Level + 1
         then
            return Types.Integer_Vectors.Empty_Vector;
         else
            List.Append (Search_In);
            for I of Meta_and_subjects (Search_In).Combined_Subjects loop
               if I > Input.Subject_Count then
                  Result := Create_Meta_List
                    (Wanted            => Wanted,
                     Search_In         => I,
                     Found             => Found_It,
                     Meta_and_subjects => Meta_and_subjects,
                     Current_List      => List);
                  if not Types.Integer_Vectors."="
                    (Types.Integer_Vectors.Empty_Vector, Result)
                  then
                     List := Result;
                     Ok   := True;
                  end if;
               end if;
            end loop;
         end if;
      end if;

      return (if Ok then List else Types.Integer_Vectors.Empty_Vector);
   end Create_Meta_List;

   -------------------------------------------------------------------------

   procedure Expand_Left
     (M                    :        Types.Minfs.Minorframes.Cursor;
      CPU                  :        Natural;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean)
   is
      Blocking : Types.Minfs.Minorframe_Iterator.List;
   begin
      if Types.Minfs.Minorframes.Element (M).Starttime <= 0 then
         Success := False;
         return;
      end if;

      Success := True;

      --  Try left expand
      Blocking := Calc_Blocking_Subjects_Left
        (Position             => M,
         CPU                  => CPU,
         All_Subjects         => All_Subjects,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Plan                 => Plan);
      if Blocking.Is_Empty then
         Types.Minfs.Minorframes.Update_Element
           (Container => Plan (CPU),
            Position  => M,
            Process   => Expand_One_Left'Access);
      else
         --  Try move the others left
         while Success and not Blocking.Is_Empty loop
            Move_Left (M                    => Blocking.First_Element,
                       Plan                 => Plan,
                       All_Subjects         => All_Subjects,
                       Simultaneous_Allowed => Simultaneous_Allowed,
                       Success              => Success);
            if Success then
               Blocking.Delete_First;
            end if;
         end loop;
         if Success then
            --  Expand M now by one
            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (CPU),
               Position  => M,
               Process   => Expand_One_Left'Access);

         end if;
      end if;
   end Expand_Left;

   -------------------------------------------------------------------------

   procedure Expand_Level_1_By_1
     (Number               :        Integer;
      CPU                  :        Natural;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Length_L1            :        Integer;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Global_Success       :    out Boolean)
   is
      use type Types.Minfs.Minorframes.Cursor;

      Plan_Copy : constant Types.Data_Structures.Level_Type := Plan;

      Success   : Boolean                        := False;
      Ptr       : Types.Minfs.Minorframes.Cursor := Plan (CPU).First;
      M         : Types.Minfs.Minorframes.Cursor;
      List_Of_M : Types.Minfs.Minorframe_Iterator.List;
   begin
      if Plan (CPU).Is_Empty then
         Put_Line ("ERROR: This Plan shouldn't be empty" & CPU'Img);
      end if;

      while Ptr /= Plan (CPU).Last loop
         if Types.Minfs.Minorframes.Element (Ptr).Subject = Number then
            M := Ptr;
            List_Of_M.Append (Ptr);
         end if;
         Types.Minfs.Minorframes.Next (Ptr);
      end loop;

      if Types.Minfs.Minorframes.Element (Ptr).Subject = Number then
         M := Ptr;
         List_Of_M.Append (Ptr);
      end if;

      if M = Types.Minfs.Minorframes.No_Element then
         Put_Line ("ERROR:M is no Elem...");
      end if;

      while not Success and not List_Of_M.Is_Empty loop
         M := List_Of_M.First_Element;
         List_Of_M.Delete_First;

         --  BETTER: use a random decision..
         --  but it has to be repeatable right here
         if Number mod 2 = 0 then
            Expand_Left (M                    => M,
                         CPU                  => CPU,
                         All_Subjects         => All_Subjects,
                         Simultaneous_Allowed => Simultaneous_Allowed,
                         Plan                 => Plan,
                         Success              => Success);

            if not Success then
               Expand_Right (M                    => M,
                             CPU                  => CPU,
                             All_Subjects         => All_Subjects,
                             Simultaneous_Allowed => Simultaneous_Allowed,
                             Plan                 => Plan,
                             Length_L1            => Length_L1,
                             Success              => Success);
            end if;
         else
            Expand_Right (M                    => M,
                          CPU                  => CPU,
                          All_Subjects         => All_Subjects,
                          Simultaneous_Allowed => Simultaneous_Allowed,
                          Plan                 => Plan,
                          Length_L1            => Length_L1,
                          Success              => Success);
            if not Success then
               Expand_Left (M                    => M,
                            CPU                  => CPU,
                            All_Subjects         => All_Subjects,
                            Simultaneous_Allowed => Simultaneous_Allowed,
                            Plan                 => Plan,
                            Success              => Success);
            end if;
         end if;
      end loop;
      Global_Success := Success;

      if not Global_Success then
         Plan := Plan_Copy;
      end if;
   end Expand_Level_1_By_1;

   -------------------------------------------------------------------------

   procedure Expand_One_Left (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Starttime := M.Starttime - 1;
   end Expand_One_Left;

   -------------------------------------------------------------------------

   procedure Expand_One_Right (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Endtime := M.Endtime + 1;
   end Expand_One_Right;

   -------------------------------------------------------------------------

   procedure Expand_Right
     (M                    :        Types.Minfs.Minorframes.Cursor;
      CPU                  :        Natural;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Length_L1            :        Integer;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean)
   is
      Blocking : Types.Minfs.Minorframe_Iterator.List;
   begin
      if Types.Minfs.Minorframes.Element (M).Endtime >= Length_L1 - 1 then
         Success := False;
         return;
      end if;

      Success := True;
      --  Try right expand
      Blocking := Calc_Blocking_Subjects_Right
        (Position             => M,
         CPU                  => CPU,
         Length_L1            => Length_L1,
         All_Subjects         => All_Subjects,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Plan                 => Plan);

      if Blocking.Is_Empty then
         Types.Minfs.Minorframes.Update_Element
           (Container => Plan (CPU),
            Position  => M,
            Process   => Expand_One_Right'Access);
      else
         --  Try move the others right
         while Success and not Blocking.Is_Empty loop
            Move_Right (M                    => Blocking.First_Element,
                        Plan                 => Plan,
                        All_Subjects         => All_Subjects,
                        Length_L1            => Length_L1,
                        Simultaneous_Allowed => Simultaneous_Allowed,
                        Success              => Success);
            if Success then
               Blocking.Delete_First;
            end if;
         end loop;

         if Success then
            --  Expand right
            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (CPU),
               Position  => M,
               Process   => Expand_One_Right'Access);
         end if;
      end if;
   end Expand_Right;

   -------------------------------------------------------------------------

   procedure Grow_Chain
     (Chosen               :        Types.Chain.Chains.List;
      Change_Plan          :        Boolean;
      My_Levels            :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject    : in out Objective.Ticks_Per_Subject_Type;
      Troughput            : in out Float;
      Ticks                :    out Integer;
      Extra_Troughput      :    out Float)
   is
      Ticks_Per_Subject_Copy  : Objective.Ticks_Per_Subject_Type
        := Ticks_Per_Subject;
      Ticks_Per_Subject_Copy2 : Objective.Ticks_Per_Subject_Type
        := Ticks_Per_Subject;
      C                       : Chain_Link_Type;
      No_Failure              : Boolean := True;
      Meta_And_Subjects_Copy  : Types.Metas.Meta_Subject_Vectors.Vector
        := Meta_and_Subjects;
      Meta_And_Subjects_Copy2 : Types.Metas.Meta_Subject_Vectors.Vector
        := Meta_and_Subjects;
      Ticks_Copy              : Integer := 0;
      Plan_Copy               : Types.Data_Structures.Level_Type := Plan;
      Plan_Copy2              : Types.Data_Structures.Level_Type := Plan;
      Bottleneck_List         : Types.Chain.Chains_Cursor_Vectors.Vector;
      Current_Throughput      : Float;
      Single_Success          : Boolean;
      Current_Ticks           : Integer;
      Ticks_Gained            : Integer;
      I                       : Types.Chain.Chains.Cursor;
      J                       : Integer;
   begin
      Ticks := 0;

      --  Find out bottlenecks in this chain

      I := Chosen.First;
      while not Types.Chain.Chains."="(I, Types.Chain.Chains.No_Element) loop
         C := Types.Chain.Chains.Element (I);
         if not (Float (Ticks_Per_Subject_Copy (C.Subject))
                 * C.Processing_Speed > Troughput)
         then
            Bottleneck_List.Append (I);
         end if;
         Types.Chain.Chains.Next (I);
      end loop;

      if Bottleneck_List.Is_Empty then
         I := Chosen.First;
         while not Types.Chain.Chains."="(I, Chosen.Last) loop
            Bottleneck_List.Append (I);
            Types.Chain.Chains.Next (I);
         end loop;
         Bottleneck_List.Append (I);
      end if;

      No_Failure := True;

      --  Try to expand every bottleneck Subject
      while not Bottleneck_List.Is_Empty and No_Failure loop

         J := Bottleneck_List.First_Index;
         --  Increase every Subject of Bottleneck_List once
         while J <= Bottleneck_List.Last_Index loop
            I := Bottleneck_List (J);
            C := Chosen (I);
            --  Subjects can be in Types.Chain.Chains more than 1 time...
            if Float (Ticks_Per_Subject_Copy (C.Subject)) * C.Processing_Speed
              > Troughput
            then
               Bottleneck_List.Delete (J);
            else
               Grow_Subject_By_One
                 (Chosen               => C.Subject,
                  Plan                 => Plan_Copy,
                  Meta_and_Subjects    => Meta_And_Subjects_Copy,
                  Ticks_needed         => Current_Ticks,
                  Ticks_gained         => Ticks_Gained,
                  My_Levels            => My_Levels,
                  Majorframe           => Majorframe,
                  Global_Success       => Single_Success,
                  Simultaneous_Allowed => Simultaneous_Allowed);

               if Single_Success = False or else Ticks_Gained = 0 then
                  No_Failure := False;
               else
                  Ticks_Copy := Ticks_Copy + Current_Ticks;
                  Ticks_Per_Subject_Copy (C.Subject) :=
                    Ticks_Per_Subject_Copy (C.Subject) + Ticks_Gained;
               end if;

               J := J + 1;
            end if;
         end loop;

         if No_Failure then
            Ticks := Ticks_Copy;
            Meta_And_Subjects_Copy2 := Meta_And_Subjects_Copy;
            Plan_Copy2 := Plan_Copy;
            Ticks_Per_Subject_Copy2 := Ticks_Per_Subject_Copy;
         end if;
      end loop;

      Current_Throughput := Float'Last;

      I := Chosen.First;
      while not Types.Chain.Chains."="(I, Types.Chain.Chains.No_Element) loop
         C := Chosen (I);
         if Float (Ticks_Per_Subject_Copy2 (C.Subject)) * C.Processing_Speed
           < Current_Throughput
         then
            Current_Throughput := Float (Ticks_Per_Subject_Copy2 (C.Subject))
              * C.Processing_Speed;
         end if;
         Types.Chain.Chains.Next (I);
      end loop;

      Extra_Troughput := Current_Throughput - Troughput;

      if Change_Plan then
         Plan := Plan_Copy2;
         Meta_and_Subjects := Meta_And_Subjects_Copy2;
         Ticks_Per_Subject := Ticks_Per_Subject_Copy2;
         Troughput := Current_Throughput;
      end if;
   end Grow_Chain;

   -------------------------------------------------------------------------

   --  Meta_and_subjects need to have the right CPU!
   --  Position might be changed, order steady
   --  Chain on this Level with most improvement will be used in every round
   procedure Grow_One_Level_Steady_Order
     (All_Chains           :        Types.Chain.Chains_Vectors.Vector;
      Level_Sizes          :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject    : in out Objective.Ticks_Per_Subject_Type)
   is
      Chosen_Chain           : Integer := 0;
      Candidates             : Types.Chain.Chains_Vectors.Vector;
      Has_Some_On_This_Level : Boolean;
      Max_Improve            : Float;
      Current_Improve        : Float;
      Current_Throughput     : Float;
      Current_Ticks          : Integer;
      C                      : Chains_With_Score_Type;
      Troughputs             : Types.Float_Vectors.Vector;
      Min                    : Float;
      I                      : Integer;

      --  Rounding mistakes... without this some equals arent equal...

      Epsilon                : constant Float := 0.0000001;
   begin
      Put_Line ("STARTING GrowING");
      I := 1;
      for S of Meta_and_Subjects loop
         Put_Line (I'Img & S.CPU'Img);
         I := I + 1;
      end loop;

      I := 1;
      while I <= All_Chains.Last_Index loop
         C := All_Chains (I);

         Has_Some_On_This_Level := False;
         --  Calc throughput = min of all Subject throughputs
         for Link of C.Chain loop
            if Meta_and_Subjects (Link.Subject).Level = 1 then
               Has_Some_On_This_Level := True;
            end if;
         end loop;

         if Has_Some_On_This_Level then
            Candidates.Append (C);
         end if;
         I := I + 1;
      end loop;

      for C of Candidates loop
         Min := Float'Last;
         for Link of C.Chain loop
            if Float (Ticks_Per_Subject (Link.Subject)) * Link.Processing_Speed
              < Min
            then
               Min := Float (Ticks_Per_Subject (Link.Subject))
                 * Link.Processing_Speed;
            end if;
         end loop;
         Troughputs.Append (Min);
      end loop;

      while not Candidates.Is_Empty and Chosen_Chain /= -1 loop
         Chosen_Chain := -1;
         Max_Improve  := 0.0;
         Put_Line ("Candidates:");
         for Candy of Candidates loop
            Put_Line (Candy.Chain.First_Element.Subject'Img);
         end loop;
         I := 1;
         --  For every candidate chain calc the improvement and
         --  the needed ticks
         while I <= Candidates.Last_Index loop
            C := Candidates (I);
            Put_Line ("Grow Chain Nbr" & I'Img);
            Grow_Chain (Chosen              => C.Chain,
                        Plan                => Plan,
                        Meta_and_Subjects   => Meta_and_Subjects,
                        Change_Plan         => False,
                        Ticks               => Current_Ticks,
                        Ticks_Per_Subject   => Ticks_Per_Subject,
                        Troughput           => Troughputs.Reference (I),
                        My_Levels           => Level_Sizes,
                        Majorframe          => Majorframe,
                        Simultaneous_Allowed => Simultaneous_Allowed,
                        Extra_Troughput     => Current_Throughput);
            Put ("Ticks" & Current_Ticks'Img);

            if Current_Ticks /= 0 then
               Current_Improve
                 := (C.Score (Troughputs (I) + Current_Throughput) - C.Score
                     (Troughputs (I))) / Float (Current_Ticks);
            else
               Current_Improve := 0.0;
               Candidates.Delete (I);
               Troughputs.Delete (I);
               I := I - 1;
            end if;

            Put_Line ("finished Grow" & Current_Improve'Img);
            if Current_Improve > 0.0
              and then Current_Improve >= Max_Improve - Epsilon
            then
               if Current_Improve > Max_Improve then
                  Max_Improve  := Current_Improve;
                  Chosen_Chain := I;
               else
                  if Troughputs (I) < Troughputs (Chosen_Chain) then
                     Chosen_Chain := I;
                  end if;
               end if;
            end if;
            I := I + 1;
         end loop;

         if Chosen_Chain /= -1 then
            Put_Line ("REALLY expanding Chain" & Chosen_Chain'Img);
            --  Expand the one with the most improvement per tick
            Grow_Chain
              (Chosen               => Candidates (Chosen_Chain).Chain,
               Plan                 => Plan,
               Meta_and_Subjects    => Meta_and_Subjects,
               Change_Plan          => True,
               Ticks                => Current_Ticks,
               Ticks_Per_Subject    => Ticks_Per_Subject,
               Troughput            => Troughputs.Reference (Chosen_Chain),
               My_Levels            => Level_Sizes,
               Majorframe           => Majorframe,
               Simultaneous_Allowed => Simultaneous_Allowed,
               Extra_Troughput      => Current_Throughput);
         end if;
      end loop;
   end Grow_One_Level_Steady_Order;

   -------------------------------------------------------------------------

   --  Grow subject by one: this might be either by using left ticks in a meta
   --  or by expanding a (meta)subject on the lowest Level
   procedure Grow_Subject_By_One
     (Chosen               :        Integer;
      My_Levels            :        Types.Basics.Natural_Array;
      Majorframe           :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Meta_and_Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_needed         :    out Integer;
      Ticks_gained         :    out Integer;
      Global_Success       :    out Boolean)
   is
      Meta                       : Meta_Subject_Type;
      Metas_Containing_List      : Types.Integer_Vectors.Vector;
      Metas_Containing_List_Copy : Types.Integer_Vectors.Vector;
      Success                    : Boolean;
      Sum                        : Integer;
      Next                       : Integer;
      T_Next                     : Integer;
      No_Tamper                  : Integer;
   begin
      if Meta_and_Subjects (Chosen).Level /= 1 then
         Metas_Containing_List := Create_Meta_List
           (Wanted            => Chosen,
            Search_In         => -1,
            Found             => False,
            Meta_and_subjects => Meta_and_Subjects,
            Current_List      => Types.Integer_Vectors.Empty_Vector);

         Metas_Containing_List_Copy := Metas_Containing_List;
         Metas_Containing_List.Reverse_Elements;
         --  Find out where to expand it by one...

         Metas_Containing_List.Delete_First; --  This is the subject itself;

         Ticks_needed := 0;
         Ticks_gained := 0;
         Meta := Meta_and_Subjects (Metas_Containing_List.First_Element);
         Sum := 0;
         --  Find out if some meta has enough space left
         for S of Meta.Tick_Vector loop
            Sum := Sum + S;
         end loop;

         while Sum = Meta.Length * (My_Levels (Meta.Level + 1)
                                    / My_Levels (Meta.Level))
           and Meta.Level /= 1
         loop
            Metas_Containing_List.Delete_First;
            Meta := Meta_and_Subjects (Metas_Containing_List.First_Element);
            Sum  := 0;
            for S of Meta.Tick_Vector loop
               Sum := Sum + S;
            end loop;
         end loop;

         if Sum < Meta.Length * (My_Levels (Meta.Level + 1) /
                                   My_Levels (Meta.Level))
         then
            --  This meta has enough space left, no need to expand on level 1
            Meta_and_Subjects (Meta.Number).Length :=
              Meta_and_Subjects (Meta.Number).Length - 1;
            --  It will be +1 in the second loop, so it stays equal...
            Ticks_gained := Majorframe
              / My_Levels (Meta_and_Subjects (Chosen).Level);
            Ticks_needed := Majorframe / (My_Levels (Meta.Level));
            --  Delete who has not to be changed
            while Metas_Containing_List_Copy.First_Element /= Meta.Number loop
               Metas_Containing_List_Copy.Delete_First;
            end loop;
            --  Change the others

            while not Metas_Containing_List_Copy.Is_Empty loop
               if Metas_Containing_List_Copy.First_Element >
                 Input.Subject_Count
               then
                  Meta_and_Subjects (Meta.Number).Length :=
                    Meta_and_Subjects (Meta.Number).Length + 1;
                  Metas_Containing_List_Copy.Delete_First;
                  Next   := Metas_Containing_List_Copy.First_Element;
                  T_Next := 1;

                  while Meta.Combined_Subjects (T_Next) /= Next loop
                     T_Next := T_Next + 1;
                  end loop;

                  No_Tamper := Meta_and_Subjects (Meta.Number).
                    Tick_Vector (T_Next) + 1;
                  Types.Integer_Vectors.Replace_Element
                    (Container => Meta_and_Subjects (Meta.Number).Tick_Vector,
                     Index     => T_Next,
                     New_Item  => No_Tamper);
                  Meta := Meta_and_Subjects (Next);
               else
                  Meta_and_Subjects (Meta.Number).Length :=
                    Meta_and_Subjects (Meta.Number).Length + 1;
                  Metas_Containing_List_Copy.Delete_First;
               end if;
            end loop;

            Global_Success := True;
         elsif Meta.Level = 1 then
            --  Need to expand a meta on level 1 to grow the subject
            Expand_Level_1_By_1
              (Number               => Meta.Number,
               CPU                  => Meta.CPU,
               Plan                 => Plan,
               Simultaneous_Allowed => Simultaneous_Allowed,
               Global_Success       => Success,
               Length_L1            => My_Levels (1),
               All_Subjects         => Meta_and_Subjects);

            if Success then
               Ticks_gained := Majorframe
                 / My_Levels (Meta_and_Subjects (Chosen).Level);
               Ticks_needed := Majorframe / My_Levels (1);
               Global_Success := True;

               while not Metas_Containing_List_Copy.Is_Empty loop
                  if Metas_Containing_List_Copy.First_Element
                    > Input.Subject_Count
                  then
                     Meta_and_Subjects (Meta.Number).Length :=
                       Meta_and_Subjects (Meta.Number).Length + 1;
                     Metas_Containing_List_Copy.Delete_First;
                     Next := Metas_Containing_List_Copy.First_Element;
                     T_Next := 1;
                     while Meta.Combined_Subjects (T_Next) /= Next loop
                        T_Next := T_Next + 1;
                     end loop;
                     No_Tamper := Meta_and_Subjects (Meta.Number).
                       Tick_Vector (T_Next) + 1;
                     Types.Integer_Vectors.Replace_Element
                       (Container => Meta_and_Subjects (Meta.Number)
                        .Tick_Vector,
                        Index     => T_Next,
                        New_Item  => No_Tamper);

                     Meta := Meta_and_Subjects (Next);
                  else
                     Meta_and_Subjects (Meta.Number).Length :=
                       Meta_and_Subjects (Meta.Number).Length + 1;
                     Metas_Containing_List_Copy.Delete_First;
                  end if;
               end loop;
            else
               Ticks_gained   := 0;
               Ticks_needed   := 0;
               Global_Success := False;
            end if;
         else
            Put_Line ("THIS SHOULD NEVER HAPPEN");
         end if;
      else
         --  the subject to grow is a level 1 subject
         Expand_Level_1_By_1
           (Number               => Chosen,
            CPU                  => Meta_and_Subjects (Chosen).CPU,
            Plan                 => Plan,
            Simultaneous_Allowed => Simultaneous_Allowed,
            Global_Success       => Success,
            Length_L1            => My_Levels (1),
            All_Subjects         => Meta_and_Subjects);

         if Success then
            Meta_and_Subjects (Chosen).Length :=
              Meta_and_Subjects (Chosen).Length + 1;
            Ticks_gained := Majorframe / My_Levels (1);
            Ticks_needed := Majorframe / My_Levels (1);
            Global_Success := True;
         else
            Ticks_gained   := 0;
            Ticks_needed   := 0;
            Global_Success := False;
         end if;
      end if;
   end Grow_Subject_By_One;

   -------------------------------------------------------------------------

   procedure Move_Left
     (M                    :        Types.Minfs.Minorframes.Cursor;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean)
   is
      Blocking : Types.Minfs.Minorframe_Iterator.List;
      CPU      : constant Natural
        := All_Subjects (Types.Minfs.Minorframes.Element (M).Subject).CPU;
   begin
      if Types.Minfs.Minorframes.Element (M).Starttime = 0 or else
        All_Subjects (Types.Minfs.Minorframes.Element (M).Subject).Level = 0
      then
         Success := False;
         return;
      end if;

      Blocking := Calc_Blocking_Subjects_Left
        (Position             => M,
         CPU                  => CPU,
         All_Subjects         => All_Subjects,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Plan                 => Plan);
      Success := True;

      if Blocking.Is_Empty then
         Types.Minfs.Minorframes.Update_Element
           (Container => Plan (CPU),
            Position  => M,
            Process   => Move_One_Left'Access);
      else
         while Success and not Blocking.Is_Empty loop
            Move_Left (M                    => Blocking.First_Element,
                       Plan                 => Plan,
                       All_Subjects         => All_Subjects,
                       Simultaneous_Allowed => Simultaneous_Allowed,
                       Success              => Success);
            if Success then
               Blocking.Delete_First;
            end if;
         end loop;
         if Success then
            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (CPU),
               Position  => M,
               Process   => Move_One_Left'Access);
         end if;
      end if;
   end Move_Left;

   -------------------------------------------------------------------------

   procedure Move_One_Left (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Starttime := M.Starttime - 1;
      M.Endtime   := M.Endtime - 1;
   end Move_One_Left;

   -------------------------------------------------------------------------

   procedure Move_One_Right (M : in out Types.Minfs.Minorframe_Type)
   is
   begin
      M.Starttime := M.Starttime + 1;
      M.Endtime   := M.Endtime + 1;
   end Move_One_Right;

   -------------------------------------------------------------------------

   procedure Move_Right
     (M                    :        Types.Minfs.Minorframes.Cursor;
      All_Subjects         :        Types.Metas.Meta_Subject_Vectors.Vector;
      Length_L1            :        Integer;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Plan                 : in out Types.Data_Structures.Level_Type;
      Success              :    out Boolean)
   is
      Blocking : Types.Minfs.Minorframe_Iterator.List;
      CPU      : constant Natural
        := All_Subjects (Types.Minfs.Minorframes.Element (M).Subject).CPU;
   begin
      if Types.Minfs.Minorframes.Element (M).Endtime >= Length_L1 - 1 or else
        All_Subjects (Types.Minfs.Minorframes.Element (M).Subject).Level = 0
      then
         Success := False;
         return;
      end if;

      Blocking := Calc_Blocking_Subjects_Right
        (Position             => M,
         CPU                  => CPU,
         All_Subjects         => All_Subjects,
         Length_L1            => Length_L1,
         Simultaneous_Allowed => Simultaneous_Allowed,
         Plan                 => Plan);
      Success := True;

      if Blocking.Is_Empty then
         Types.Minfs.Minorframes.Update_Element
           (Container => Plan (CPU),
            Position  => M,
            Process   => Move_One_Right'Access);
      else
         while Success and not Blocking.Is_Empty loop
            Move_Right (M                    => Blocking.First_Element,
                        Plan                 => Plan,
                        All_Subjects         => All_Subjects,
                        Simultaneous_Allowed => Simultaneous_Allowed,
                        Length_L1            => Length_L1,
                        Success              => Success);
            if Success then
               Blocking.Delete_First;
            end if;
         end loop;

         if Success then
            Types.Minfs.Minorframes.Update_Element
              (Container => Plan (CPU),
               Position  => M,
               Process   => Move_One_Right'Access);
         end if;
      end if;
   end Move_Right;

end Steps.Grow;
