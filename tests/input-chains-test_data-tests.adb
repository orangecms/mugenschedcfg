--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input.Chains.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Input.Chains.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Input (Gnattest_T : in out Test);
   procedure Test_Get_Input_203400 (Gnattest_T : in out Test) renames Test_Get_Input;
--  id:2.2/203400b080dde559/Get_Input/1/0/
   procedure Test_Get_Input (Gnattest_T : in out Test) is
   --  input-chains.ads:22:4:Get_Input
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Input;
--  end read only

end Input.Chains.Test_Data.Tests;
