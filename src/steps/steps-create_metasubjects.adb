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

with Ada.Strings.Unbounded;

with Input.Subjects;
with Steps.Plan_Building_Blockers;
with Types.Metas.Meta_Subject_Package;
with Types.Combined_With;
with Types.Positive_Set;
with Types.Integer_Vectors;

package body Steps.Create_Metasubjects
is

   use Input.Subjects;
   use Steps.Plan_Building_Blockers;
   use Types.Metas.Meta_Subject_Package;

   -------------------------------------------------------------------------

   --  Combine subjects so that the summed Length of them is less or equal a
   --  Treshold_Factor
   procedure Build_Combine_Sets
     (All_Subjects          :     Types.Metas.Meta_Subject_Vectors.Vector;
      Same_CPU_Allowed      :     Types.Basics.Execution_Restrictions_Array;
      Majorframes           :     Plans_Filling_Array;
      Plans_Subject_Lengths :     TDS.Per_Plan_Subject_Lengths_Type;
      Combine_Vector        : out Types.CPU_Sets.Vector)
   is
      use type Types.Positive_Set.Set;

      subtype Plan_Range is Positive range 1 .. Input.Plan_Count;

      Treshold_Factor  : constant Float := 0.4;

      Plans_Filling    : Plans_Filling_Array (Plan_Range)
        := (others => 0);
      Subjects_Copy    : Types.Metas.Meta_Subject_Vectors.Vector
        := All_Subjects;
      Combine          : Types.CPU_Sets.Vector;
      A_Set            : Types.Combined_With.Set;
      Leave_Unfilled   : Boolean;
      All_Plans_Ok     : Boolean;
      S                : Integer;
      CPU              : Natural;
      Same_CPU_Set     : Types.Positive_Set.Set;
      Simultaneous_Set : Types.Positive_Set.Set;
   begin
      while not Subjects_Copy.Is_Empty loop
         Leave_Unfilled := False;
         Same_CPU_Set.Clear;
         Simultaneous_Set.Clear;
         A_Set.Clear;

         A_Set.Insert (Subjects_Copy.First_Element.Number);

         Types.Positive_Set.Union
           (Same_CPU_Set,
            Subjects_Copy.First_Element.Same_CPU_Set);
         Types.Positive_Set.Union
           (Simultaneous_Set,
            Subjects_Copy.First_Element.Simultaneous_Set);
         CPU := Subjects_Copy.First_Element.CPU;

         for P in Plan_Range loop
            Plans_Filling (P) := Plans_Subject_Lengths (P)
              (Subjects_Copy.First_Element.Number);

            if Plans_Filling (P) > Integer (Float (Majorframes (P))
                                            * Treshold_Factor)
            then
               Leave_Unfilled := True;
            end if;
         end loop;
         Subjects_Copy.Delete_First;

         while not Leave_Unfilled loop
            Leave_Unfilled := True;
            S := Subjects_Copy.First_Index;
            while Leave_Unfilled and S <= Subjects_Copy.Last_Index loop
               if Subjects_Copy (S).Simultaneous_Set = Simultaneous_Set
                 and then Subjects_Copy (S).CPU = CPU
                 and then Restrictions_Are_Satisfiable
                   (Subject_Set1 => Subjects_Copy (S).Same_CPU_Set,
                    Subject_Set2 => Same_CPU_Set,
                    Restrictions => Same_CPU_Allowed)
               then
                  All_Plans_Ok := True;
                  for P in Plan_Range loop
                     if Plans_Filling (P) + Plans_Subject_Lengths (P)
                       (Subjects_Copy (S).Number) >
                       Integer (Float (Majorframes (P)) * Treshold_Factor)
                     then
                        All_Plans_Ok := False;
                     end if;
                  end loop;

                  if All_Plans_Ok then
                     Leave_Unfilled := False;
                     A_Set.Insert (Subjects_Copy (S).Number);

                     for P in Plan_Range loop
                        Plans_Filling (P) := Plans_Filling (P)
                          + Plans_Subject_Lengths (P)
                          (Subjects_Copy (S).Number);
                     end loop;

                     Types.Positive_Set.Union
                       (Simultaneous_Set,
                        Subjects_Copy (S).Simultaneous_Set);
                     Types.Positive_Set.Union
                       (Same_CPU_Set,
                        Subjects_Copy (S).Same_CPU_Set);
                     Subjects_Copy.Delete (S);
                  end if;
               end if;
               S := S + 1;
            end loop;
         end loop;

         Combine.Append (A_Set);
      end loop;

      Combine_Vector := Combine;
   end Build_Combine_Sets;

   -------------------------------------------------------------------------

   procedure Create_Metasubjects
     (Subjects          :        Types.Metas.Meta_Subject_Vectors.Vector;
      All_Subjects      :        Types.Metas.Meta_Subject_Vectors.Vector;
      All_Current_Metas :        Types.Metas.Meta_Subject_Vectors.Vector;
      Meta_Subjects_In  :        Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Size        :        Types.Basics.Natural_Array;
      Number_Of_Plan    :        Positive;
      Current_Level     :        Positive;
      Combine_Vector    :        Types.CPU_Sets.Vector;
      On_Same_CPU       : in out Types.CPU_Sets.Vector;
      Meta_Subjects_Out :    out Types.Metas.Meta_Subject_Vectors.Vector)
   is
      My_Copy                   : Types.Metas.Meta_Subject_Vectors.Vector
        := Meta_Subjects_In;
      New_Meta_Subjects         : Types.Metas.Meta_Subject_Vectors.Vector;
      Current_Meta_Subject      : Meta_Subject_Type;
      A_Meta_Subject            : Meta_Subject_Type;
      A_Meta_Subject2           : Meta_Subject_Type;
      Next_Part_of_Meta_Subject : Positive;
      Current_Ticks             : Integer;
   begin
      Current_Meta_Subject.Name  := Ada.Strings.Unbounded.To_Unbounded_String
        ("Meta");
      Current_Meta_Subject.Level := Current_Level - 1;
      Current_Meta_Subject.Plan  := Number_Of_Plan;

      My_Copy.Append (Subjects);

      while not My_Copy.Is_Empty loop
         Current_Meta_Subject.Number
           := Get_Next_Meta_Number (Plan => Number_Of_Plan);

         Current_Meta_Subject.Combined_Subjects.Clear;
         Current_Meta_Subject.Tick_Vector.Clear;
         Current_Meta_Subject.Simultaneous_Set.Clear;
         Current_Meta_Subject.Same_CPU_Set.Clear;

         --  Put next subject into Meta_subject
         Current_Meta_Subject.Length :=
           Integer (Float'Ceiling (
                   Float (My_Copy.First_Element.Length) /
                     Float (Level_Size (Current_Level)
                       / Level_Size (Current_Level - 1))));

         Current_Meta_Subject.Combined_Subjects.Append
           (My_Copy.First_Element.Number);
         Current_Meta_Subject.Tick_Vector.Append
           (My_Copy.First_Element.Length);

         --  Find out which Subject is in this meta, because
         --  only subjects have the right On_CPU-with_Number
         --  and only subjects are listed in Combined_With(Combine_Vector)
         A_Meta_Subject := My_Copy.First_Element;

         while A_Meta_Subject.Combined_Subjects.First_Element
           > Input.Subject_Count
         loop
            A_Meta_Subject
              := All_Current_Metas
                (A_Meta_Subject.Combined_Subjects.First_Element
                 - Input.Subject_Count);
         end loop;

         Current_Meta_Subject.On_CPU_With_Number
           := All_Subjects (A_Meta_Subject.Combined_Subjects.First_Element)
           .On_CPU_With_Number;

         Types.Positive_Set.Union
           (Current_Meta_Subject.Simultaneous_Set,
            My_Copy.First_Element.Simultaneous_Set);

         Types.Positive_Set.Union
           (Current_Meta_Subject.Same_CPU_Set,
            My_Copy.First_Element.Same_CPU_Set);

         Current_Ticks := My_Copy.First_Element.Length;
         My_Copy.Delete_First;
         --  Find out someone to Combine it with
         for C of Combine_Vector loop
            if Types.Combined_With.Contains
              (C, A_Meta_Subject.Combined_Subjects.First_Element)
            then
               Next_Part_of_Meta_Subject := My_Copy.First_Index;

               while Next_Part_of_Meta_Subject <= My_Copy.Last_Index loop

                  A_Meta_Subject2 := My_Copy (Next_Part_of_Meta_Subject);

                  while A_Meta_Subject2.Combined_Subjects.First_Element
                    > Input.Subject_Count
                  loop
                     A_Meta_Subject2 := All_Current_Metas
                       (A_Meta_Subject2.Combined_Subjects.First_Element
                        - Input.Subject_Count);
                  end loop;

                  if Types.Combined_With.Contains
                    (C, A_Meta_Subject2.Combined_Subjects.First_Element)
                  then
                     --  Add S in this metasubj
                     Current_Meta_Subject.Combined_Subjects.Append
                       (My_Copy.Element (Next_Part_of_Meta_Subject).Number);
                     Current_Meta_Subject.Tick_Vector.Append
                       (My_Copy.Element (Next_Part_of_Meta_Subject).Length);

                     --  Update On_Same_CPU
                     if Current_Meta_Subject.On_CPU_With_Number /=
                       My_Copy.Element (Next_Part_of_Meta_Subject)
                       .On_CPU_With_Number
                     then
                        On_Same_CPU (Current_Meta_Subject.On_CPU_With_Number)
                          .Union
                            (On_Same_CPU
                               (My_Copy.Element (Next_Part_of_Meta_Subject)
                                .On_CPU_With_Number));
                        On_Same_CPU (My_Copy.Element
                                     (Next_Part_of_Meta_Subject)
                                     .On_CPU_With_Number).Clear;
                     end if;

                     Types.Positive_Set.Union
                       (Current_Meta_Subject.Simultaneous_Set,
                        My_Copy.Element (Next_Part_of_Meta_Subject)
                        .Simultaneous_Set);
                     Types.Positive_Set.Union
                       (Current_Meta_Subject.Same_CPU_Set,
                        My_Copy.Element (Next_Part_of_Meta_Subject)
                        .Same_CPU_Set);

                     Current_Ticks := Current_Ticks + My_Copy.
                       Element (Next_Part_of_Meta_Subject).Length;

                     Current_Meta_Subject.Length :=
                       Integer (Float'Ceiling (
                               Float (Current_Ticks) /
                                 Float (Level_Size (Current_Level)
                                   / Level_Size (Current_Level - 1)
                                  )));

                     My_Copy.Delete (Next_Part_of_Meta_Subject, 1);
                  else
                     Next_Part_of_Meta_Subject
                       := Next_Part_of_Meta_Subject + 1;
                  end if;
               end loop;
            end if;
         end loop;

         --  One Metasubject finished
         New_Meta_Subjects.Append (Current_Meta_Subject);
      end loop;

      Meta_Subjects_Out := New_Meta_Subjects;
   end Create_Metasubjects;

end Steps.Create_Metasubjects;
