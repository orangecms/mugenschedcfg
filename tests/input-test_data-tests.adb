--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Input.Test_Data.Tests is


--  begin read only
   procedure Test_Process (Gnattest_T : in out Test);
   procedure Test_Process_1bbd81 (Gnattest_T : in out Test) renames Test_Process;
--  id:2.2/1bbd8105b30ba096/Process/1/0/
   procedure Test_Process (Gnattest_T : in out Test) is
   --  input.ads:29:4:Process
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Plans = 0,
              Message   => "Plan count not 0");
      Assert (Condition => CPUs = 0,
              Message   => "CPU count not 0");
      Assert (Condition => Nr_Of_Subjects = 0,
              Message   => "Subject count not 0");
      Assert (Condition => Max_Levels = 0,
              Message   => "Max level count not 0");
      Assert (Condition => Same_CPU_Domains = 0,
              Message   => "Same CPU domains not 0");
      Assert (Condition => Simultaneous_Domains = 0,
              Message   => "Simultaneous domains not 0");

      Process (File => "plans/complex.xml");

      Assert (Condition => Plans = 12,
              Message   => "Plan count not 12");
      Assert (Condition => CPUs = 4,
              Message   => "CPU count not 4");
      Assert (Condition => Nr_Of_Subjects = 47,
              Message   => "Subject count not 47");
      Assert (Condition => Max_Levels = 4,
              Message   => "Max level count not 4");
      Assert (Condition => Same_CPU_Domains = 5,
              Message   => "Same CPU domains not 5");
      Assert (Condition => Simultaneous_Domains = 3,
              Message   => "Simultaneous domains not 3");

      Plans                := 0;
      CPUs                 := 0;
      Nr_Of_Subjects       := 0;
      Max_Levels           := 0;
      Same_CPU_Domains     := 0;
      Simultaneous_Domains := 0;

   exception
      when others =>
         Plans                := 0;
         CPUs                 := 0;
         Nr_Of_Subjects       := 0;
         Max_Levels           := 0;
         Same_CPU_Domains     := 0;
         Simultaneous_Domains := 0;
         raise;
--  begin read only
   end Test_Process;
--  end read only


--  begin read only
   procedure Test_Plan_Count (Gnattest_T : in out Test);
   procedure Test_Plan_Count_28b60c (Gnattest_T : in out Test) renames Test_Plan_Count;
--  id:2.2/28b60c04fe896aa6/Plan_Count/1/0/
   procedure Test_Plan_Count (Gnattest_T : in out Test) is
   --  input.ads:32:4:Plan_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Plans := 9;
      Assert (Condition => Plan_Count = 9,
              Message   => "Count mismatch");
      Plans := 0;

   exception
      when others =>
         Plans := 0;
         raise;
--  begin read only
   end Test_Plan_Count;
--  end read only


--  begin read only
   procedure Test_CPU_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Count_190426 (Gnattest_T : in out Test) renames Test_CPU_Count;
--  id:2.2/19042678bd4b7254/CPU_Count/1/0/
   procedure Test_CPU_Count (Gnattest_T : in out Test) is
   --  input.ads:35:4:CPU_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      CPUs := 8;
      Assert (Condition => CPUs = 8,
              Message   => "Count mismatch");
      CPUs := 0;

   exception
      when others =>
         CPUs := 0;
         raise;
--  begin read only
   end Test_CPU_Count;
--  end read only


--  begin read only
   procedure Test_Tick_Rate (Gnattest_T : in out Test);
   procedure Test_Tick_Rate_e6c92f (Gnattest_T : in out Test) renames Test_Tick_Rate;
--  id:2.2/e6c92fa85e23ccf4/Tick_Rate/1/0/
   procedure Test_Tick_Rate (Gnattest_T : in out Test) is
   --  input.ads:38:4:Tick_Rate
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Tickr := 12000;
      Assert (Condition => Tick_Rate = 12000,
              Message   => "Tick rate mismatch");
      Tickr := 0;

   exception
      when others =>
         Tickr := 0;
         raise;
--  begin read only
   end Test_Tick_Rate;
--  end read only


--  begin read only
   procedure Test_Subject_Count (Gnattest_T : in out Test);
   procedure Test_Subject_Count_bc3ae9 (Gnattest_T : in out Test) renames Test_Subject_Count;
--  id:2.2/bc3ae9086ba7636f/Subject_Count/1/0/
   procedure Test_Subject_Count (Gnattest_T : in out Test) is
   --  input.ads:41:4:Subject_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Nr_Of_Subjects := 13;
      Assert (Condition => Nr_Of_Subjects = 13,
              Message   => "Count mismatch");
      Nr_Of_Subjects := 0;

   exception
      when others =>
         Nr_Of_Subjects := 0;
         raise;
--  begin read only
   end Test_Subject_Count;
--  end read only


--  begin read only
   procedure Test_Level_Count (Gnattest_T : in out Test);
   procedure Test_Level_Count_1602d1 (Gnattest_T : in out Test) renames Test_Level_Count;
--  id:2.2/1602d1226f392c27/Level_Count/1/0/
   procedure Test_Level_Count (Gnattest_T : in out Test) is
   --  input.ads:44:4:Level_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Max_Levels := 5;
      Assert (Condition => Level_Count = 5,
              Message   => "Count mismatch");
      Max_Levels := 0;

   exception
      when others =>
         Max_Levels := 0;
         raise;
--  begin read only
   end Test_Level_Count;
--  end read only


--  begin read only
   procedure Test_Plan_Weighting (Gnattest_T : in out Test);
   procedure Test_Plan_Weighting_c527da (Gnattest_T : in out Test) renames Test_Plan_Weighting;
--  id:2.2/c527da584c645262/Plan_Weighting/1/0/
   procedure Test_Plan_Weighting (Gnattest_T : in out Test) is
   --  input.ads:47:4:Plan_Weighting
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Types.Float_Vectors.Vector;

      Ref : Types.Float_Vectors.Vector;
   begin
      Ref.Append (New_Item => 1.0);
      Ref.Append (New_Item => 4.0);
      Plan_Weights := Ref;

      Assert (Condition => Plan_Weighting = Plan_Weights,
              Message   => "Mismatch");
--  begin read only
   end Test_Plan_Weighting;
--  end read only


--  begin read only
   procedure Test_Per_Plan_Level_Sizes (Gnattest_T : in out Test);
   procedure Test_Per_Plan_Level_Sizes_3944dc (Gnattest_T : in out Test) renames Test_Per_Plan_Level_Sizes;
--  id:2.2/3944dc3ed1115e27/Per_Plan_Level_Sizes/1/0/
   procedure Test_Per_Plan_Level_Sizes (Gnattest_T : in out Test) is
   --  input.ads:50:4:Per_Plan_Level_Sizes
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Types.Data_Structures.Per_Plan_Level_Sizes_Type;

      Ref : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      L   : constant Types.Basics.Natural_Array
        := (1 => 40, 2 => 140, 3 => 200);
   begin
      Ref.Append (New_Item => L);
      Ref.Append (New_Item => L);
      Per_Plan_Level_Sizes_Data := Ref;

      Assert (Condition => Per_Plan_Level_Sizes = Ref,
              Message   => "Mismatch");
--  begin read only
   end Test_Per_Plan_Level_Sizes;
--  end read only


--  begin read only
   procedure Test_Per_Plan_Level_Count (Gnattest_T : in out Test);
   procedure Test_Per_Plan_Level_Count_31f260 (Gnattest_T : in out Test) renames Test_Per_Plan_Level_Count;
--  id:2.2/31f26052306009f5/Per_Plan_Level_Count/1/0/
   procedure Test_Per_Plan_Level_Count (Gnattest_T : in out Test) is
   --  input.ads:54:4:Per_Plan_Level_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Types.Positive_Vectors.Vector;

      Ref : Types.Positive_Vectors.Vector;
   begin
      Ref.Append (New_Item => 12);
      Ref.Append (New_Item => 222);
      Per_Plan_Levels := Ref;

      Assert (Condition => Per_Plan_Level_Count = Ref,
              Message   => "Mismatch");
--  begin read only
   end Test_Per_Plan_Level_Count;
--  end read only


--  begin read only
   procedure Test_Same_CPU_Allowed (Gnattest_T : in out Test);
   procedure Test_Same_CPU_Allowed_26b824 (Gnattest_T : in out Test) renames Test_Same_CPU_Allowed;
--  id:2.2/26b82408687cc327/Same_CPU_Allowed/1/0/
   procedure Test_Same_CPU_Allowed (Gnattest_T : in out Test) is
   --  input.ads:57:4:Same_CPU_Allowed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Same_CPU_Allowed;
--  end read only


--  begin read only
   procedure Test_Simultaneous_Allowed (Gnattest_T : in out Test);
   procedure Test_Simultaneous_Allowed_953f99 (Gnattest_T : in out Test) renames Test_Simultaneous_Allowed;
--  id:2.2/953f9910770da0a3/Simultaneous_Allowed/1/0/
   procedure Test_Simultaneous_Allowed (Gnattest_T : in out Test) is
   --  input.ads:60:4:Simultaneous_Allowed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Simultaneous_Allowed;
--  end read only

end Input.Test_Data.Tests;
