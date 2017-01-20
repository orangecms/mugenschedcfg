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
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Types.Integer_Vectors;

package body Steps.Plan_Building_Metasubjects
is

   New_Endtime : Integer := -100;

   procedure Update_Endtime (Elem : in out Types.Minfs.Minorframe_Type);

   procedure Reset_Metasubject
     (Vector   : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Old_Copy :        Types.Metas.Meta_Subject_Vectors.Vector;
      Level    :        Positive);

   procedure Join_Minorframes (Plans : in out TDS.All_Plans_Type);

   -------------------------------------------------------------------------

   --  No care of Min_Length of the minorframe of subjects right now
   --  (min_Length in (meta)subjects has a different meaning)
   procedure Build_Plans_From_Metasubjects
     (Lowest_Level_Plan   :     TDS.All_Plans_Type;
      All_Subjects        :     Types.Metas.Meta_Subject_Vectors.Vector;
      Meta_Subjects_Plans :     Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      All_Level_Sizes     :     TDS.Per_Plan_Level_Sizes_Type;
      Failed_In_Shorten   :     Types.Combined_With.Set;
      Per_Plan_Levels     :     Types.Positive_Vectors.Vector;
      Final_Plans         : out TDS.All_Plans_Type)
   is
      J, Period, Repetition, Saved_Starttime, Last_Meta_Number,
      Low_Level_Meta_Nbr, Plan_Time, Min_Plan_Vector : Integer;

      Iterator : Types.Minfs.Minorframes.Cursor;

      Current, New_Minorframe : Types.Minfs.Minorframe_Type;

      Meta, A_Metas_Before : Meta_Subject_Type;

      Subjects_and_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector
        := All_Subjects;

      Subjects_and_Meta_Subjects_Unchanged :
      Types.Metas.Meta_Subject_Vectors.Vector;

      I          : Integer := 1;
      Left_Ticks : Integer := 0;

      Level : Positive;

      Null_Plan, Already_Deleted : Boolean;
   begin
      Final_Plans := TDS.Create (Plan_Count => Input.Plan_Count,
                                 CPU_Count  => Input.CPU_Count);

      for P in Positive range 1 .. Input.Plan_Count loop

         if not Failed_In_Shorten.Contains (P) then

            Subjects_and_Meta_Subjects := All_Subjects;
            Subjects_and_Meta_Subjects.Append (Meta_Subjects_Plans (P));

            --  Plan_Vector saves how many ticks are left to plan for the
            --  combined at this place
            for S of Subjects_and_Meta_Subjects loop
               for T of S.Tick_Vector loop
                  S.Plan_Vector.Append (T);
               end loop;
            end loop;
            Subjects_and_Meta_Subjects_Unchanged := Subjects_and_Meta_Subjects;

            Period := All_Level_Sizes (P)(1);
            for C in 0 .. Input.CPU_Count loop

               if not Lowest_Level_Plan (P)(C).Is_Empty then

                  Repetition := All_Level_Sizes (P)(Per_Plan_Levels (P))
                    / Period;
                  Iterator := Lowest_Level_Plan (P)(C).First;
                  I := 0;
                  J := -1;
                  while I < Repetition loop
                     --  We're in the I-th repetition
                     --  reset metas if needed
                     if Per_Plan_Levels (P) /= 1 and then I /= J then
                        Level := Per_Plan_Levels (P) - 1;
                        while I mod (All_Level_Sizes (P)(Level) /
                                       Period) /= 0
                        loop
                           Level := Level - 1;
                        end loop;
                        if Level > 1 then
                           Level := Level - 1;

                           Reset_Metasubject
                             (Subjects_and_Meta_Subjects,
                              Subjects_and_Meta_Subjects_Unchanged,
                              Level);
                        end if;
                     end if;

                     Current := Types.Minfs.Minorframes.Element (Iterator);

                     if Current.Subject <= Input.Subject_Count then
                        --  Its a normal subject
                        --  Plan it
                        New_Minorframe.Subject   := Current.Subject;
                        New_Minorframe.Starttime := Current.Starttime + (I)
                          * Period;
                        New_Minorframe.Endtime := Current.Endtime + (I)
                          * Period;
                        Final_Plans (P)(C).Append (New_Minorframe);
                     else
                        --  It is a meta, find out which subjects need to be
                        --  planned and for how many ticks
                        Meta := Subjects_and_Meta_Subjects (Current.Subject);

                        Left_Ticks := Current.Endtime - Current.Starttime + 1;

                        Saved_Starttime := Current.Starttime + (I) * Period;
                        Low_Level_Meta_Nbr := Meta.Number;
                        Last_Meta_Number := -1;

                        while Left_Ticks > 0 loop
                           Already_Deleted          := False;
                           New_Minorframe.Starttime := Saved_Starttime;
                           Null_Plan                := False;
                           Last_Meta_Number         := -1;

                           Meta := Subjects_and_Meta_Subjects
                             (Low_Level_Meta_Nbr);

                           --  Find next subject to plan
                           while not Meta.Combined_Subjects.Is_Empty and then
                             Meta.Combined_Subjects.First_Element
                               > Input.Subject_Count loop

                              Last_Meta_Number := Meta.Number;
                              Meta := Subjects_and_Meta_Subjects
                                (Meta.Combined_Subjects.First_Element);
                           end loop;

                           if Last_Meta_Number /= -1 and then
                             Subjects_and_Meta_Subjects (Last_Meta_Number)
                             .Plan_Vector.First_Element = 0
                           then
                              Put_Line ("Vector 0 should not happen");
                              Subjects_and_Meta_Subjects (Last_Meta_Number)
                                .Combined_Subjects.Delete_First;
                              Subjects_and_Meta_Subjects (Last_Meta_Number).
                                Tick_Vector.Delete_First;
                              Subjects_and_Meta_Subjects (Last_Meta_Number).
                                Plan_Vector.Delete_First;
                              Null_Plan := True;
                           end if;

                           if Meta.Combined_Subjects.Is_Empty or Null_Plan
                           then
                              if Null_Plan then
                                 null;
                              elsif Meta.Level = 1 then
                                 Left_Ticks := 0;
                              else
                                 Subjects_and_Meta_Subjects (Last_Meta_Number)
                                   .Combined_Subjects.Delete_First;
                                 Subjects_and_Meta_Subjects (Last_Meta_Number).
                                   Tick_Vector.Delete_First;
                                 Subjects_and_Meta_Subjects (Last_Meta_Number).
                                   Plan_Vector.Delete_First;
                              end if;

                           else
                              --  first of combined subject is now a "normal"
                              --  subject
                              --  Plan this! But not for more ticks than
                              --  allowed!
                              New_Minorframe.Subject := Meta.Combined_Subjects
                                .First_Element;
                              New_Minorframe.Starttime := Saved_Starttime;

                              if Last_Meta_Number /= -1 then
                                 Min_Plan_Vector := Subjects_and_Meta_Subjects
                                   (Last_Meta_Number)
                                   .Plan_Vector.First_Element;
                                 A_Metas_Before := Subjects_and_Meta_Subjects
                                   (Low_Level_Meta_Nbr);

                                 while A_Metas_Before.Number /= Meta.Number
                                 loop
                                    if Subjects_and_Meta_Subjects
                                      (A_Metas_Before.Number)
                                      .Plan_Vector.First_Element
                                        < Min_Plan_Vector
                                    then
                                       Min_Plan_Vector
                                         := Subjects_and_Meta_Subjects
                                           (A_Metas_Before.Number)
                                         .Plan_Vector.First_Element;
                                    end if;
                                    A_Metas_Before
                                      := Subjects_and_Meta_Subjects
                                        (A_Metas_Before.Combined_Subjects
                                         .First_Element);
                                 end loop;
                              end if;

                              if Last_Meta_Number /= -1 and then
                                Min_Plan_Vector < Left_Ticks and then
                                Min_Plan_Vector <
                                  Meta.Tick_Vector.First_Element
                              then
                                 Plan_Time := Min_Plan_Vector;
                              elsif
                                Left_Ticks < Meta.Tick_Vector.First_Element
                              then
                                 Plan_Time := Left_Ticks;
                              else
                                 Plan_Time := Meta.Tick_Vector.First_Element;
                              end if;

                              New_Minorframe.Endtime := Saved_Starttime
                                + Plan_Time - 1;

                              --  Update Tick_Vector
                              Subjects_and_Meta_Subjects (Meta.Number)
                                .Tick_Vector.Replace_Element
                                  (Subjects_and_Meta_Subjects (Meta.Number)
                                   .Tick_Vector.First,
                                   Meta.Tick_Vector.First_Element - Plan_Time);

                              if Subjects_and_Meta_Subjects (Meta.Number)
                                .Tick_Vector.First_Element = 0
                              then
                                 Subjects_and_Meta_Subjects (Meta.Number).
                                   Combined_Subjects.Delete_First;
                                 Subjects_and_Meta_Subjects (Meta.Number).
                                   Tick_Vector.Delete_First;
                                 Subjects_and_Meta_Subjects (Meta.Number).
                                   Plan_Vector.Delete_First;
                                 Already_Deleted := True;
                              end if;

                              --  Update Plan_Vector
                              if not Already_Deleted then
                                 Subjects_and_Meta_Subjects (Meta.Number)
                                   .Plan_Vector.Replace_Element
                                     (Subjects_and_Meta_Subjects (Meta.Number)
                                      .Plan_Vector.First,
                                      Subjects_and_Meta_Subjects (Meta.Number).
                                            Plan_Vector.First_Element
                                      - Plan_Time);

                                 if Subjects_and_Meta_Subjects (Meta.Number)
                                   .Plan_Vector.First_Element = 0
                                 then
                                    Subjects_and_Meta_Subjects (Meta.Number)
                                      .Combined_Subjects.Delete_First;
                                    Subjects_and_Meta_Subjects (Meta.Number).
                                      Tick_Vector.Delete_First;
                                    Subjects_and_Meta_Subjects (Meta.Number).
                                      Plan_Vector.Delete_First;
                                 end if;
                              end if;

                              Left_Ticks := Left_Ticks - Plan_Time;

                              --  Update Plan_Vector
                              if Last_Meta_Number /= -1 then
                                 A_Metas_Before := Subjects_and_Meta_Subjects
                                   (Low_Level_Meta_Nbr);
                                 while A_Metas_Before.Number
                                   /= Last_Meta_Number
                                 loop
                                    --  Update Plan_Vector
                                    Subjects_and_Meta_Subjects
                                      (A_Metas_Before.Number)
                                      .Plan_Vector.Replace_Element
                                        (Subjects_and_Meta_Subjects
                                           (A_Metas_Before.Number)
                                         .Plan_Vector.First,
                                         Subjects_and_Meta_Subjects
                                           (A_Metas_Before.Number).
                                               Plan_Vector.First_Element
                                         - Plan_Time);

                                    if Subjects_and_Meta_Subjects
                                      (A_Metas_Before.Number)
                                      .Plan_Vector.First_Element = 0
                                    then
                                       Subjects_and_Meta_Subjects
                                         (A_Metas_Before.Number)
                                         .Combined_Subjects.Delete_First;
                                       Subjects_and_Meta_Subjects
                                         (A_Metas_Before.Number).
                                         Tick_Vector.Delete_First;
                                       Subjects_and_Meta_Subjects
                                         (A_Metas_Before.Number).
                                         Plan_Vector.Delete_First;
                                       Meta := Subjects_and_Meta_Subjects
                                         (Low_Level_Meta_Nbr);
                                    end if;

                                    A_Metas_Before
                                      := Subjects_and_Meta_Subjects
                                        (A_Metas_Before.Combined_Subjects
                                         .First_Element);
                                 end loop;
                                 --  And once for Last_Meta_Number

                                 Subjects_and_Meta_Subjects (Last_Meta_Number)
                                   .Plan_Vector.Replace_Element
                                     (Subjects_and_Meta_Subjects
                                        (Last_Meta_Number)
                                      .Plan_Vector.First,
                                      Subjects_and_Meta_Subjects
                                        (Last_Meta_Number).
                                            Plan_Vector.First_Element
                                      - Plan_Time);

                                 if Subjects_and_Meta_Subjects
                                   (Last_Meta_Number)
                                   .Plan_Vector.First_Element = 0
                                 then
                                    Subjects_and_Meta_Subjects
                                      (Last_Meta_Number)
                                      .Combined_Subjects.Delete_First;
                                    Subjects_and_Meta_Subjects
                                      (Last_Meta_Number)
                                      .Tick_Vector.Delete_First;
                                    Subjects_and_Meta_Subjects
                                      (Last_Meta_Number)
                                      .Plan_Vector.Delete_First;

                                    Meta := Subjects_and_Meta_Subjects
                                      (Low_Level_Meta_Nbr);
                                 end if;

                              end if;

                              Saved_Starttime := Saved_Starttime +
                                Plan_Time;

                              Final_Plans (P)(C).Append (New_Minorframe);
                           end if;
                        end loop;

                     end if;

                     J := I;
                     if Types.Minfs.Minorframes."="
                       (Lowest_Level_Plan (P)(C).Last, Iterator)
                     then
                        Iterator := Lowest_Level_Plan (P)(C).First;
                        I := I + 1;
                     else
                        Types.Minfs.Minorframes.Next (Iterator);
                     end if;

                  end loop;
               end if;

            end loop;
         end if;
      end loop;

      Join_Minorframes (Final_Plans);
   end Build_Plans_From_Metasubjects;

   -------------------------------------------------------------------------

   --  If two minorframes planned directly after eachother have the same
   --  subject they will be combined to one (bigger) minorframe
   procedure Join_Minorframes (Plans : in out TDS.All_Plans_Type)
   is
      use type Types.Minfs.Minorframes.Cursor;

      I  : Types.Minfs.Minorframes.Cursor;
      I2 : Types.Minfs.Minorframes.Cursor;
   begin
      for P in Positive range 1 .. Input.Plan_Count loop
         for C in 0 .. Input.CPU_Count loop
            I := Plans (P)(C).First;
            while I /= Plans (P)(C).Last loop
               I2 := Types.Minfs.Minorframes.Next (I);
               if Types.Minfs.Minorframes.Element (I).Subject =
                 Types.Minfs.Minorframes.Element (I2).Subject and then
                 Types.Minfs.Minorframes.Element (I).Endtime =
                 Types.Minfs.Minorframes.Element (I2).Starttime - 1
               then
                  New_Endtime := Types.Minfs.Minorframes.Element (I2).Endtime;
                  Types.Minfs.Minorframes.Update_Element (Plans (P)(C), I
                                             , Update_Endtime'Access);
                  Types.Minfs.Minorframes.Delete (Plans (P)(C), I2);
               else
                  I := I2;
               end if;
            end loop;
         end loop;
      end loop;
   end Join_Minorframes;

   -------------------------------------------------------------------------

   procedure Reset_Metasubject
     (Vector   : in out Types.Metas.Meta_Subject_Vectors.Vector;
      Old_Copy :        Types.Metas.Meta_Subject_Vectors.Vector;
      Level    :        Positive)
   is
   begin
      for V of Vector loop
         if V.Level <= Level then
            V := Old_Copy (V.Number);
         end if;
      end loop;
   end Reset_Metasubject;

   -------------------------------------------------------------------------

   procedure Update_Endtime (Elem : in out Types.Minfs.Minorframe_Type)
   is
   begin
      Elem.Endtime := New_Endtime;
   end Update_Endtime;

end Steps.Plan_Building_Metasubjects;
