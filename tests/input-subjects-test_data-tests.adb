--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input.Subjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Input.Subjects.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Input (Gnattest_T : in out Test);
   procedure Test_Get_Input_a52626 (Gnattest_T : in out Test) renames Test_Get_Input;
--  id:2.2/a526264dd3d1f439/Get_Input/1/0/
   procedure Test_Get_Input (Gnattest_T : in out Test) is
   --  input-subjects.ads:34:4:Get_Input
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Input;
--  end read only


--  begin read only
   procedure Test_Get_On_Same_CPU (Gnattest_T : in out Test);
   procedure Test_Get_On_Same_CPU_e469f3 (Gnattest_T : in out Test) renames Test_Get_On_Same_CPU;
--  id:2.2/e469f3e9d60adf93/Get_On_Same_CPU/1/0/
   procedure Test_Get_On_Same_CPU (Gnattest_T : in out Test) is
   --  input-subjects.ads:41:4:Get_On_Same_CPU
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_On_Same_CPU;
--  end read only


--  begin read only
   procedure Test_Init_Meta_Numbers (Gnattest_T : in out Test);
   procedure Test_Init_Meta_Numbers_bbb8a1 (Gnattest_T : in out Test) renames Test_Init_Meta_Numbers;
--  id:2.2/bbb8a14f79b11e27/Init_Meta_Numbers/1/0/
   procedure Test_Init_Meta_Numbers (Gnattest_T : in out Test) is
   --  input-subjects.ads:44:4:Init_Meta_Numbers
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;
   begin
      Input.Plans          := 12;
      Input.Nr_Of_Subjects := 4;
      Init_Meta_Numbers;

      Assert (Condition => Per_Plan_Meta_Subject_Numbers.Length = 12,
              Message   => "Plan count mismatch");
      for N of Per_Plan_Meta_Subject_Numbers loop
         Assert (Condition => N = Input.Nr_Of_Subjects + 1,
                 Message   => "Number mismatch");
      end loop;

      Input.Plans          := 0;
      Input.Nr_Of_Subjects := 0;

   exception
      when others =>
         Input.Plans          := 0;
         Input.Nr_Of_Subjects := 0;
         raise;
--  begin read only
   end Test_Init_Meta_Numbers;
--  end read only


--  begin read only
   procedure Test_Get_Next_Meta_Number (Gnattest_T : in out Test);
   procedure Test_Get_Next_Meta_Number_0e32c4 (Gnattest_T : in out Test) renames Test_Get_Next_Meta_Number;
--  id:2.2/0e32c432d0b4a473/Get_Next_Meta_Number/1/0/
   procedure Test_Get_Next_Meta_Number (Gnattest_T : in out Test) is
   --  input-subjects.ads:47:4:Get_Next_Meta_Number
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Input.Plans          := 9;
      Input.Nr_Of_Subjects := 4;
      Init_Meta_Numbers;

      Per_Plan_Meta_Subject_Numbers (4) := 5;
      Assert (Condition => Get_Next_Meta_Number (4) = 5,
              Message   => "Number mismatch");
      Assert (Condition => Per_Plan_Meta_Subject_Numbers (4) = 6,
              Message   => "Next number mismatch");

      Input.Plans          := 0;
      Input.Nr_Of_Subjects := 0;

   exception
      when others =>
         Input.Plans := 0;
         Input.Nr_Of_Subjects := 0;
         raise;
--  begin read only
   end Test_Get_Next_Meta_Number;
--  end read only

end Input.Subjects.Test_Data.Tests;
