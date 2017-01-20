--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Steps.Create_Metasubjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Steps.Create_Metasubjects.Test_Data.Tests is


--  begin read only
   procedure Test_Create_Metasubjects (Gnattest_T : in out Test);
   procedure Test_Create_Metasubjects_995db5 (Gnattest_T : in out Test) renames Test_Create_Metasubjects;
--  id:2.2/995db5719f956580/Create_Metasubjects/1/0/
   procedure Test_Create_Metasubjects (Gnattest_T : in out Test) is
   --  steps-create_metasubjects.ads:30:4:Create_Metasubjects
--  end read only

      pragma Unreferenced (Gnattest_T);

      Subject_Count : constant := 47;

      A_Meta:Meta_Subject_Type;
      Subjects: Types.Metas.Meta_Subject_Vectors.Vector;
      All_Subjects: Types.Metas.Meta_Subject_Vectors.Vector;
      Number_of_Plan : Positive := 1;
      Meta_Subjects_in: Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Size : Natural_Array (1 .. 4);
      Current_Level : Positive := 4;
      All_Current_Metas: Types.Metas.Meta_Subject_Vectors.Vector;
      Meta_Subjects_out: Types.Metas.Meta_Subject_Vectors.Vector;
      On_Same_CPU_out: Types.CPU_Sets.Vector;
      Combine_Vector: Types.CPU_Sets.Vector;
      A_Set:Types.Combined_With.Set;
      Meta_Subjects_correct: Types.Metas.Meta_Subject_Vectors.Vector;
      On_Same_CPU_correct: Types.CPU_Sets.Vector;
      OK:Boolean:=False;
      A_Vector:Types.Integer_Vectors.Vector;
   begin
      Input.Process (File => "plans/complex.xml");
      Input.Subjects.Init_Meta_Numbers;

      A_Meta.Name := Ada.Strings.Unbounded.To_Unbounded_String ("Meta");
      A_Meta.Level:=Current_Level-1;
      A_Meta.Plan:=1;
      A_Meta.CPU:=0;
   --   A_Meta.Min_Length:=1;
      for L in 1 .. 4 loop
         if L = 4 then
            Level_Size (L) := 100;
            else
            Level_Size (L) := 50;
         end if;
      end loop;

      for I in 1..4 loop
         A_Meta.Number:=I;
         A_Meta.On_CPU_With_Number:=I;
         A_Meta.Combined_Subjects.Clear;
         A_Meta.Combined_Subjects.Append(I);
         A_Meta.Length:=1;
         Subjects.Append(A_Meta);
         All_Subjects.Append(A_Meta);
         A_Set.Clear;
         A_Set.Insert(I);
         On_Same_CPU_out.Append(A_Set);
      end loop;

      A_Set.Clear;
      A_Set.Insert(1);
      A_Set.Insert(2);
      A_Set.Insert(3);
      Combine_Vector.Append(A_Set);

      On_Same_CPU_correct.Append(A_Set);

      A_Set.Clear;
      On_Same_CPU_correct.Append(A_Set);
      On_Same_CPU_correct.Append(A_Set);

      A_Meta.Number:=Subject_Count+1;
      A_Meta.On_CPU_With_Number:=1;
      A_Vector.Append(1);
      A_Vector.Append(2);
      A_Vector.Append(3);
      A_Meta.Combined_Subjects:=A_Vector;
      A_Meta.Length:=2;
      A_Vector.Clear;
      A_Vector.Append(1);
      A_Vector.Append(1);
      A_Vector.Append(1);
      A_Meta.Tick_Vector:=A_Vector;

      A_Set.Clear;
      A_Set.Insert(4);
      Combine_Vector.Append(A_Set);

      On_Same_CPU_correct.Append(A_Set);

      Meta_Subjects_correct.Append(A_Meta);
      A_Meta.Number:=Subject_Count+2;
      A_Meta.Length:=1;
      A_Meta.On_CPU_With_Number:=1;
      A_Vector.Clear;
      A_Vector.Append(4);
      A_Meta.Combined_Subjects:=A_Vector;
      A_Vector.Clear;
      A_Vector.Append(1);
      A_Meta.Tick_Vector:=A_Vector;
      Meta_Subjects_correct.Append(A_Meta);

      Create_Metasubjects(Subjects          => Subjects,
                          All_Subjects      => All_Subjects,
                          Number_of_Plan    => Number_of_Plan,
                          Meta_Subjects_in  => Meta_Subjects_in,
                          Level_Size        => Level_Size,
                          Current_Level     => Current_Level,
                          All_Current_Metas => All_Current_Metas,
                          Meta_Subjects_out => Meta_Subjects_out,
                          On_Same_CPU       => On_Same_CPU_out,
                          Combine_Vector    => Combine_Vector);
       OK:=True;
      for I in 1..2 loop
         if not (Meta_Subjects_out(I).Number =Subject_Count+I)
           or not Types.Integer_Vectors."="
             (Meta_Subjects_out(I).Combined_Subjects,
              Meta_Subjects_correct(I).Combined_Subjects)
             or not Types.Integer_Vectors."="
               (Meta_Subjects_out(I).Tick_Vector,
                       Meta_Subjects_out(I).Tick_Vector)
           or Meta_Subjects_out(I).Level/=Current_Level-1
           or not(Meta_Subjects_out(I).On_CPU_With_Number =
                      Meta_Subjects_out(I).On_CPU_With_Number)
           or not(Meta_Subjects_out(I).Length =
                      Meta_Subjects_out(I).Length)
           or Meta_Subjects_out(I).Plan/=1
         then
            OK:=False;
         end if;
      end loop;

      if Types.CPU_Sets."="(On_Same_CPU_out,On_Same_CPU_correct) and
        Types.Metas.Meta_Subject_Vectors."="
          (Meta_Subjects_out,Meta_Subjects_correct)
      then
         OK:=True;
      end if;

      AUnit.Assertions.Assert
        (OK,
         "Steps.Combine_Metasubjects:Create_Metasubjects failed");

--  begin read only
   end Test_Create_Metasubjects;
--  end read only


--  begin read only
   procedure Test_Build_Combine_Sets (Gnattest_T : in out Test);
   procedure Test_Build_Combine_Sets_d02c35 (Gnattest_T : in out Test) renames Test_Build_Combine_Sets;
--  id:2.2/d02c35d43846d38d/Build_Combine_Sets/1/0/
   procedure Test_Build_Combine_Sets (Gnattest_T : in out Test) is
   --  steps-create_metasubjects.ads:44:4:Build_Combine_Sets
--  end read only

      pragma Unreferenced (Gnattest_T);

      Subject_Count : constant := 47;

      A_Meta:Meta_Subject_Type;
      All_Subjects: Types.Metas.Meta_Subject_Vectors.Vector;
      Same_CPU_Allowed : Execution_Restrictions_Array (1 .. 3, 1 .. 3);
      Majorframes : Plans_Filling_Array (1 .. 12);
      Plans_Subject_Length:Types.Data_Structures.Per_Plan_Subject_Lengths_Type;
      Result: Types.CPU_Sets.Vector;
      OK : Boolean := True;
      Sum:Integer;
      Seen_One:Boolean;
      Seen_Two:Boolean;
   begin
      for I in 1..Subject_Count loop
         A_Meta.Number:=I;
         All_Subjects.Append(A_Meta);
      end loop;

      --Set some Same_CPU_Sets
      Same_CPU_Allowed(1,2):=False;
      Same_CPU_Allowed(2,1):=False;
      for I in 1..Subject_Count loop
         if I mod 3 = 1 then
            All_Subjects(I).Same_CPU_Set.Insert(1);
         elsif I mod 3 = 2 then
            All_Subjects(I).Same_CPU_Set.Insert(2);
         end if;
      end loop;

      declare
         Subject_Lengths : constant Natural_Array (1 .. Subject_Count)
           := (others => 0);
      begin
         Plans_Subject_Length.Append
           (New_Item => Subject_Lengths,
            Count    => 12);
      end;

      --Set the lengths
      for P in 1 .. 12 loop
         Majorframes(P):=1000;
         Plans_Subject_Length(P)(1):=1000;
         for S in 2 .. Subject_Count loop
            if S mod 2 = 1 and S*30+Integer(P)*40<1000 then
               Plans_Subject_Length(P)(S):=S+Integer(P)*100;
            end if;
         end loop;
      end loop;

      Build_Combine_Sets(All_Subjects          => All_Subjects,
                         Same_CPU_Allowed      => Same_CPU_Allowed,
                         Majorframes           => Majorframes,
                         Plans_Subject_Lengths => Plans_Subject_Length,
                         Combine_Vector        => Result);

      -- This should be fulfilled for every input
      for P in 1 .. 12 loop
         for Set of Result loop
            Sum:=0;
            for Number of Set loop
               Sum:=Sum+Plans_Subject_Length(P)(Number);
            end loop;
            if Float(Sum)/Float(Majorframes(P))> 0.4
              and then Set.Length>1
            then
               OK:=False;
            end if;
         end loop;
      end loop;

      for Set of Result loop
         Seen_Two:=False;
         Seen_One:=False;
         for Number of Set loop
            if All_Subjects(Number).Same_CPU_Set.Contains(1) then
               Seen_One:=True;
            end if;
            if All_Subjects(Number).Same_CPU_Set.Contains(2) then
               Seen_Two:=True;
            end if;
         end loop;
         if Seen_Two and Seen_One then
            OK:=False;
         end if;
      end loop;

      AUnit.Assertions.Assert
        (OK,
         "Steps.Combine_Metasubjects:Build_Combine_Sets failed");
--  begin read only
   end Test_Build_Combine_Sets;
--  end read only

end Steps.Create_Metasubjects.Test_Data.Tests;
