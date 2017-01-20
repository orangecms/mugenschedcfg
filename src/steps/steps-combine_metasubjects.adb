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
with Types.Integer_Vectors;
with Types.Minfs.Minorframes;
with Types.Positive_Set;
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Steps.Short; use Steps.Short;
with Steps.Plan_Building_Blockers; use Steps.Plan_Building_Blockers;

package body Steps.Combine_Metasubjects
is

   His_new_Number : Integer := -1;

   function Combinable_Without_Loss
     (All_Metas   :        Types.Metas.Meta_Subject_Vectors.Vector;
      I, J        :        Integer;
      Level_Sizes :        Types.Basics.Natural_Array;
      Only_Equal  :        Boolean;
      New_Numbers : in out Types.Integer_Vectors.Vector)
      return Boolean;

   procedure New_Subject_Number (Elem : in out Types.Minfs.Minorframe_Type);

   function Moveable_Left
     (Plan                 : Types.Data_Structures.Level_Type;
      Minor                : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array)
      return Boolean;

   procedure Move_Left_Plan
     (Plan                 : in out Types.Data_Structures.Level_Type;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array);

   -------------------------------------------------------------------------

   --  Find out if Metas Number J could be put into meta I without
   --  increasing size.
   function Combinable_Without_Loss
     (All_Metas   :        Types.Metas.Meta_Subject_Vectors.Vector;
      I, J        :        Integer;
      Level_Sizes :        Types.Basics.Natural_Array;
      Only_Equal  :        Boolean;
      New_Numbers : in out Types.Integer_Vectors.Vector)
      return Boolean
   is
      use type Types.Positive_Set.Set;

      SumI, SumJ : Integer := 0;
      Meta : Meta_Subject_Type;
   begin
      --  Check if on same cpu
      if All_Metas (I).CPU /= All_Metas (J).CPU then
         return False;
      end if;
      --  Check simultaneous set
      if Only_Equal then
         if All_Metas (J).Simultaneous_Set /= All_Metas (I).Simultaneous_Set
         then
            return False;
         end if;
      else
         if not Types.Positive_Set.Is_Subset
           (Subset => All_Metas (J).Simultaneous_Set,
            Of_Set => All_Metas (I).Simultaneous_Set)
         then
            return False;
         end if;
      end if;

      --  Calc how much ticks are used in which meta
      for T in 1 ..  All_Metas (I).Combined_Subjects.Last_Index loop
         if New_Numbers (Types.Integer_Vectors.Element
                        (All_Metas (I).Combined_Subjects, T)) /= -1
         then
            SumI := SumI + All_Metas (I).Tick_Vector (T);
         end if;
      end loop;

      for T in 1 ..  All_Metas (J).Combined_Subjects.Last_Index loop
         Meta := All_Metas (J);
         if New_Numbers
           (Types.Integer_Vectors.Element (Meta.Combined_Subjects, T)) /= -1
         then
            SumJ := SumJ + All_Metas (J).Tick_Vector (T);
         end if;
      end loop;

      if SumJ = 0 then
         if New_Numbers (J) /= -1 then
            New_Numbers (J) := -1;
            for K in J + 1 .. New_Numbers.Last_Index loop
               if New_Numbers (K) /= -1 then
                  New_Numbers (K) := New_Numbers (K) - 1;
               end if;
            end loop;
         end if;

      end if;

      if SumJ /= 0 and then
        All_Metas (I).Length *
        (Level_Sizes (All_Metas (I).Level + 1) /
           Level_Sizes (All_Metas (I).Level)) - SumI  > SumJ
      then
         return True;
      else
         return False;
      end if;
   end Combinable_Without_Loss;

   -------------------------------------------------------------------------

   procedure Combine_Metas_Without_Loss
     (Level_Sizes          :        Types.Basics.Natural_Array;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array;
      Only_Equal           :        Boolean;
      Change_Plan          :        Boolean;
      Metas_To_Combine     : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Plan                 : in out Types.Data_Structures.Level_Type)
   is
      Curs        : Types.Minfs.Minorframes.Cursor;
      Curs_Next   : Types.Minfs.Minorframes.Cursor;
      New_Numbers : Types.Integer_Vectors.Vector;
      M           : Types.Minfs.Minorframe_Type;
      I           : Integer;
      J           : Integer;
      Sum         : Integer;
   begin
      --  Give all the right CPU
      for C in 0 .. Input.CPU_Count loop
         for M of Plan (C) loop
            Set_CPU_For_All
              (Meta_Number => M.Subject,
               Subjects    => Metas_To_Combine,
               CPU         => C);
         end loop;
      end loop;

      --  Save who gets which new number..currently everyone has its own number
      --  -1 means it is already put into another meta
      New_Numbers.Clear;
      for I in 1 .. Metas_To_Combine.Last_Index loop
         New_Numbers.Append (I);
      end loop;

      --  From highest level going down: Check if its possible to combine
      --  them without a loss
      I := Input.Subject_Count + 1;
      while I <= Metas_To_Combine.Last_Index loop
         Sum := 0;
         for S in 1 .. Metas_To_Combine (I).Combined_Subjects.Last_Index loop
            if New_Numbers (Types.Integer_Vectors.Element
                            (Metas_To_Combine (I).Combined_Subjects, S)) /= -1
            then
               Sum := Sum + Metas_To_Combine (I).Tick_Vector (S);
            end if;
         end loop;

         if Sum = 0 then
            if New_Numbers (I) /= -1 then
               New_Numbers (I) := -1;
               for K in I + 1 .. New_Numbers.Last_Index loop
                  if New_Numbers (K) /= -1 then
                     New_Numbers (K) := New_Numbers (K) - 1;
                  end if;
               end loop;
            end if;

            J := Metas_To_Combine.Last_Index + 1; --  skip loop
         else
            J := Input.Subject_Count + 1;
         end if;
         if New_Numbers (I) = -1 then
            J := Metas_To_Combine.Last_Index + 1;
         end if;

         while J <= Metas_To_Combine.Last_Index loop
            if Metas_To_Combine (I).Level /= Metas_To_Combine (J).Level then
               J := J + 1;
            elsif New_Numbers (J) /= -1
              and then I /= J
              and then Combinable_Without_Loss
                (All_Metas   => Metas_To_Combine,
                 I           => I,
                 J           => J,
                 Level_Sizes => Level_Sizes,
                 Only_Equal  => Only_Equal,
                 New_Numbers => New_Numbers)
            then
               Put_Line ("Combined" & I'Img & J'Img);
               --  Combine them!
               Metas_To_Combine (I).Combined_Subjects.Append
                 (Metas_To_Combine (J).Combined_Subjects);
               Metas_To_Combine (I).Tick_Vector.Append
                 (Metas_To_Combine (J).Tick_Vector);

               New_Numbers (J) := -1;
               for K in J + 1 .. New_Numbers.Last_Index loop
                  if New_Numbers (K) /= -1 then
                     New_Numbers (K) := New_Numbers (K) - 1;
                  end if;
               end loop;
               J := J + 1;
            else
               J := J + 1;
            end if;

         end loop;
         I := I + 1;
      end loop;

      if Change_Plan then
         --  Delete and correct numbers
         for C of Plan loop
            if not C.Is_Empty then
               Curs := C.First;
               while not Types.Minfs.Minorframes."="(Curs, C.Last) loop
                  Curs_Next := Types.Minfs.Minorframes.Next (Curs);
                  M         := Types.Minfs.Minorframes.Element (Curs);
                  if New_Numbers (M.Subject) = -1 then
                     C.Delete (Curs);
                  else
                     His_new_Number := New_Numbers (M.Subject);
                     Types.Minfs.Minorframes.Update_Element
                       (Container => C,
                        Position  => Curs,
                        Process   => New_Subject_Number'Access);
                  end if;
                  Curs := Curs_Next;
               end loop;
               M := Types.Minfs.Minorframes.Element (Curs);
               if New_Numbers (M.Subject) = -1 then
                  C.Delete (Curs);
               else
                  His_new_Number := New_Numbers (M.Subject);
                  Types.Minfs.Minorframes.Update_Element
                    (Container => C,
                     Position  => Curs,
                     Process   => New_Subject_Number'Access);
               end if;
            end if;
         end loop;

         --  Delete and correct numbers in meta list
         for K in reverse 1 .. Metas_To_Combine.Last_Index loop
            if New_Numbers (K) = -1 then
               Types.Metas.Meta_Subject_Vectors.Delete
                 (Container => Metas_To_Combine,
                  Index     => K,
                  Count     => 1);
            else
               Metas_To_Combine (K).Number := New_Numbers (K);
               I := Metas_To_Combine (K).Combined_Subjects.First_Index;
               while I <= Metas_To_Combine (K).Combined_Subjects.Last_Index
               loop
                  if New_Numbers
                    (Types.Integer_Vectors.Element
                       (Metas_To_Combine (K).Combined_Subjects, I)) = -1
                  then
                     Types.Integer_Vectors.Delete
                       (Container => Metas_To_Combine (K).Combined_Subjects,
                        Index     => I,
                        Count     => 1);
                     Types.Integer_Vectors.Delete
                       (Container => Metas_To_Combine (K).Tick_Vector,
                        Index     => I,
                        Count     => 1);
                  else
                     Types.Integer_Vectors.Replace_Element
                       (Container => Metas_To_Combine (K).Combined_Subjects,
                        Index     => I,
                        New_Item  => New_Numbers (Types.Integer_Vectors.Element
                          (Metas_To_Combine (K).Combined_Subjects, I)));
                     I := I + 1;
                  end if;
               end loop;
            end if;
         end loop;

         --  Move plan left
         Move_Left_Plan (Plan                => Plan,
                        Simultaneous_Allowed => Simultaneous_Allowed);
      end if;

   end Combine_Metas_Without_Loss;

   -------------------------------------------------------------------------

   --  Moves all subjects left until they are blocking, doesnt change the order
   procedure Move_Left_Plan
     (Plan                 : in out Types.Data_Structures.Level_Type;
      Simultaneous_Allowed :        Types.Basics.Execution_Restrictions_Array)
   is
      use type Types.Minfs.Minorframes.Cursor;

      CPU_Count : constant Positive := Input.CPU_Count;
      subtype CPU_Range is Positive range 1 .. CPU_Count;

      Iterator_Array : Iterator_Array_Type (CPU_Range);
      End_Time_Array : time_Array_Type (CPU_Range);
      Next_CPU       : Next_CPU_Type (CPU_Range);
      This_CPU       : Integer;
      I              : Integer;
      J              : Integer;
      New_Endtime    : Integer := Integer'Last;
   begin
      for C in 1 .. CPU_Count loop
         if not Types.Minfs.Minorframes."="(Plan (C),
                                            Types.Minfs.Minorframes.Empty_List)
         then
            Iterator_Array (C) := Plan (C).First;

            --  Sort CPUs after endtimes
            I := 1;
            while Plan (C).First_Element.Endtime >= End_Time_Array (I) loop
               I := I + 1;
            end loop;
            J := I;

            while End_Time_Array (J) /= Integer'Last loop
               J := J + 1;
            end loop;

            while J > I loop
               End_Time_Array (J) := End_Time_Array (J - 1);
               Next_CPU (J)       := Next_CPU (J - 1);
               J := J - 1;
            end loop;

            End_Time_Array (J) := Plan (C).First_Element.Endtime;
            Next_CPU (J)       := C;
         end if;
      end loop;
      --  Iterate over all CPUs at the same time
      while Next_CPU (1) /= -1 loop
         if Moveable_Left
           (Plan                 => Plan,
            Minor                => Iterator_Array (Next_CPU (1)),
            CPU                  => Next_CPU (1),
            Simultaneous_Allowed => Simultaneous_Allowed)
         then
            --  Move left!
            Plan (Next_CPU (1)).Update_Element
              (Position => Iterator_Array (Next_CPU (1)),
               Process  => Move_Left'Access);
         else
            --  Walk on
            This_CPU := Next_CPU (1);

            if Iterator_Array (Next_CPU (1)) /= Plan (Next_CPU (1)).Last then
               Types.Minfs.Minorframes.Next (Iterator_Array (Next_CPU (1)));
               New_Endtime := Types.Minfs.Minorframes.Element
                 (Iterator_Array (Next_CPU (1))).Endtime;
               End_Time_Array (1) := New_Endtime;

               I := 1;
               while I < CPU_Count and then
                 New_Endtime > End_Time_Array (I + 1)
               loop
                  End_Time_Array (I) := End_Time_Array (I + 1);
                  Next_CPU (I) := Next_CPU (I + 1);
                  I := I + 1;
               end loop;
               End_Time_Array (I) := New_Endtime;
               Next_CPU (I) := This_CPU;
            else
               I := 1;
               while  I < CPU_Count and then Next_CPU (I + 1) /= -1 loop
                  Next_CPU (I) := Next_CPU (I + 1);
                  End_Time_Array (I) := End_Time_Array (I + 1);
                  I := I + 1;
               end loop;
               Next_CPU (I) := -1;
               End_Time_Array (I) := Integer'Last;
            end if;
         end if;
      end loop;
   end Move_Left_Plan;

   -------------------------------------------------------------------------

   --  A minorframe is moveable left if there is noone blocking
   function Moveable_Left
     (Plan                 : Types.Data_Structures.Level_Type;
      Minor                : Types.Minfs.Minorframes.Cursor;
      CPU                  : Natural;
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array)
      return Boolean
   is
      I       : Types.Minfs.Minorframes.Cursor;
      Movable : Boolean := True;
   begin
      if Types.Minfs.Minorframes.Element (Minor).Starttime = 0 then
         return False;
      end if;

      --  Check for all CPUs if someone is blocking
      for C in 1 .. Input.CPU_Count loop
         if C = CPU then
            if not Types.Minfs.Minorframes."="(Minor, Plan (C).First)
              and then Types.Minfs.Minorframes.Element
                (Types.Minfs.Minorframes.Previous (Minor)).Endtime =
                  Types.Minfs.Minorframes.Element (Minor).Starttime - 1
            then
               Movable := False;
            end if;
         else
            I := Plan (C).Last;

            while not Types.Minfs.Minorframes."=" (I, Plan (C).First)
              and then Types.Minfs.Minorframes.Element
                (I).Endtime >
              Types.Minfs.Minorframes.Element (Minor).Starttime - 1
            loop
               Types.Minfs.Minorframes.Previous (I);
            end loop;

            if not Plan (C).Is_Empty and then
              Types.Minfs.Minorframes.Element (I).Endtime =
              Types.Minfs.Minorframes.Element (Minor).Starttime - 1
              and then not
                Restrictions_Are_Satisfiable
                  (Subject_Set1 => Types.Minfs.Minorframes.Element
                     (I).Simultaneous_Set,
                   Subject_Set2 => Types.Minfs.Minorframes.Element
                     (Minor).Simultaneous_Set,
                   Restrictions => Simultaneous_Allowed)
            then
               Movable := False;
            end if;
         end if;
      end loop;

      return Movable;
   end Moveable_Left;

   -------------------------------------------------------------------------

   procedure New_Subject_Number (Elem : in out Types.Minfs.Minorframe_Type)
   is
   begin
      Elem.Subject := His_new_Number;
   end New_Subject_Number;

   -------------------------------------------------------------------------

   procedure Set_CPU_For_All
     (Meta_Number :        Positive;
      CPU         :        Positive;
      Subjects    : in out Types.Metas.Meta_Subject_Vectors.Vector)
   is
   begin
      Subjects (Meta_Number).CPU := CPU;
      if Meta_Number > Input.Subject_Count then
         for S of Subjects (Meta_Number).Combined_Subjects loop
            Set_CPU_For_All
              (Meta_Number => S,
               Subjects    => Subjects,
               CPU         => CPU);
         end loop;
      end if;
   end Set_CPU_For_All;

end Steps.Combine_Metasubjects;
