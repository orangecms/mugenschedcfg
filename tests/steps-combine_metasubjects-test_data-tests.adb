--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Steps.Combine_Metasubjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Steps.Combine_Metasubjects.Test_Data.Tests is


--  begin read only
   procedure Test_Combine_Metas_Without_Loss (Gnattest_T : in out Test);
   procedure Test_Combine_Metas_Without_Loss_5a1502 (Gnattest_T : in out Test) renames Test_Combine_Metas_Without_Loss;
--  id:2.2/5a15027581788584/Combine_Metas_Without_Loss/1/0/
   procedure Test_Combine_Metas_Without_Loss (Gnattest_T : in out Test) is
   --  steps-combine_metasubjects.ads:31:4:Combine_Metas_Without_Loss
--  end read only

      pragma Unreferenced (Gnattest_T);

      A_Meta               : Meta_Subject_Type;
      Metas_To_Combine     : Types.Metas.Meta_Subject_Vectors.Vector;
      Plan                 : Types.Data_Structures.Level_Type (0 .. 4);
      Level_Sizes          : Types.Basics.Natural_Array (1 .. 4);
      Simultaneous_Allowed : Types.Basics.Execution_Restrictions_Array
        (1 .. 5, 1 .. 5);
      OK                   : Boolean := True;
      Vector               : Types.Integer_Vectors.Vector;

      Subject_Count : constant := 47;
   begin
      Input.Process (File => "plans/complex.xml");

      --  MetaSubject 1
      A_Meta.Number := Subject_Count + 1;
      A_Meta.Level  := 1;
      A_Meta.Length := 5;
      Vector.Append (1);
      A_Meta.Simultaneous_Set.Insert (1);
      A_Meta.Combined_Subjects := Vector;
      A_Meta.Tick_Vector := Vector;
      Metas_To_Combine.Append (A_Meta);

      --  MetaSubject 2
      A_Meta.Number := Subject_Count + 2;
      A_Meta.Level := 1;
      A_Meta.Length := 4;
      Vector.Clear;
      Vector.Append (2);
      A_Meta.Simultaneous_Set.Clear;
      A_Meta.Simultaneous_Set.Insert (1);
      A_Meta.Combined_Subjects := Vector;
      A_Meta.Tick_Vector := Vector;
      Metas_To_Combine.Append (A_Meta);

      --  MetaSubject 3
      A_Meta.Number := Subject_Count + 3;
      A_Meta.Level := 1;
      A_Meta.Length := 4;
      A_Meta.Simultaneous_Set.Clear;
      Vector.Clear;
      Vector.Append (3);
      A_Meta.Combined_Subjects := Vector;
      Vector.Clear;
      Vector.Append (12);
      A_Meta.Tick_Vector := Vector;
      Metas_To_Combine.Append (A_Meta);

      --Metasubject 4
      A_Meta.Number := Subject_Count + 4;
      A_Meta.Level := 1;
      A_Meta.Length := 3;
      Vector.Clear;
      Vector.Append (4);
      A_Meta.Combined_Subjects := Vector;
      A_Meta.Tick_Vector := Vector;
      Metas_To_Combine.Append (A_Meta);

      Level_Sizes :=
        (1      => 10,
         2      => 30,
         others => 0);

      Combine_Metas_Without_Loss
        (Metas_To_Combine     => Metas_To_Combine,
         Plan                 => Plan,
         Level_Sizes          => Level_Sizes,
         Simultaneous_Allowed => Simultaneous_Allowed,
         only_equal           => True,
         Change_Plan          => True);

      for M of Metas_To_Combine loop
         if (M.Combined_Subjects.Contains (1) and then
               not M.Combined_Subjects.Contains (2) and then
             M.Combined_Subjects.Length > 2) or else
           (M.Combined_Subjects.Contains (3) and then
            M.Combined_Subjects.Length > 1) or else
           (M.Combined_Subjects.Contains (4) and then
            M.Combined_Subjects.Length > 1)
         then
            OK := False;
         end if;
      end loop;

      Assert
        (Condition => OK,
         Message   => "Steps.Combine_Metasubjects:Combine_Metas_Without_Loss"
         & " failed");
--  begin read only
   end Test_Combine_Metas_Without_Loss;
--  end read only


--  begin read only
   procedure Test_Set_CPU_For_All (Gnattest_T : in out Test);
   procedure Test_Set_CPU_For_All_7ef59c (Gnattest_T : in out Test) renames Test_Set_CPU_For_All;
--  id:2.2/7ef59c5ba7658c42/Set_CPU_For_All/1/0/
   procedure Test_Set_CPU_For_All (Gnattest_T : in out Test) is
   --  steps-combine_metasubjects.ads:39:4:Set_CPU_For_All
--  end read only

      pragma Unreferenced (Gnattest_T);

      Subjects_and_Metasubjects: Types.Metas.Meta_Subject_Vectors.Vector;
      A_Meta:Meta_Subject_Type;
      New_CPU:Types.Integer_Vectors.Vector;
      OK:Boolean:=True;

      Subject_Count : constant := 47;
   begin
      A_Meta.CPU:=0;

      for I in 1..Subject_Count+20 loop
         Subjects_and_Metasubjects.Append(A_Meta);
      end loop;

      Subjects_and_Metasubjects(Subject_Count+1)
        .Combined_Subjects.Append(1);
      Subjects_and_Metasubjects(Subject_Count+1)
        .Combined_Subjects.Append(Subject_Count+4);
      Subjects_and_Metasubjects(Subject_Count+1)
        .Combined_Subjects.Append(Subject_Count+10);
      Subjects_and_Metasubjects(Subject_Count+4)
        .Combined_Subjects.Append(Subject_Count+16);
      Subjects_and_Metasubjects(Subject_Count+10)
        .Combined_Subjects.Append(5);
      Subjects_and_Metasubjects(Subject_Count+10)
        .Combined_Subjects.Append(3);
      Subjects_and_Metasubjects(Subject_Count+16)
        .Combined_Subjects.Append(6);

      New_CPU.Append(1);
      New_CPU.Append(3);
      New_CPU.Append(5);
      New_CPU.Append(6);
      New_CPU.Append(Subject_Count+1);
      New_CPU.Append(Subject_Count+4);
      New_CPU.Append(Subject_Count+10);
      New_CPU.Append(Subject_Count+16);

      Set_CPU_For_All
        (Meta_Number => Subject_Count+1,
         Subjects    => Subjects_and_Metasubjects,
         CPU         => 1);
      for I in 1 .. Subject_Count+20 loop
         if (Subjects_and_Metasubjects(I).CPU=1 and then
               not New_CPU.Contains(I)) or
           (Subjects_and_Metasubjects(I).CPU=0 and then
            New_CPU.Contains(I))
         then
            OK:=False;
         end if;
      end loop;

      AUnit.Assertions.Assert
        (OK,
         "Steps.Combine_Metasubjects:Set_CPU_For_All failed.");
--  begin read only
   end Test_Set_CPU_For_All;
--  end read only

end Steps.Combine_Metasubjects.Test_Data.Tests;
