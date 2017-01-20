--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Steps.Short.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Steps.Short.Test_Data.Tests is


--  begin read only
   procedure Test_Shorten_Plan (Gnattest_T : in out Test);
   procedure Test_Shorten_Plan_a451aa (Gnattest_T : in out Test) renames Test_Shorten_Plan;
--  id:2.2/a451aaaf6d0dcc76/Shorten_Plan/1/0/
   procedure Test_Shorten_Plan (Gnattest_T : in out Test) is
   --  steps-short.ads:39:4:Shorten_Plan
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Shorten_Plan;
--  end read only


--  begin read only
   procedure Test_Move_Left (Gnattest_T : in out Test);
   procedure Test_Move_Left_a98d8e (Gnattest_T : in out Test) renames Test_Move_Left;
--  id:2.2/a98d8e4b927d6642/Move_Left/1/0/
   procedure Test_Move_Left (Gnattest_T : in out Test) is
   --  steps-short.ads:50:4:Move_Left
--  end read only

      pragma Unreferenced (Gnattest_T);

      M : Types.Minfs.Minorframe_Type
        := (Starttime => 12,
            Endtime   => 244,
            others    => <>);
   begin
      Move_Left (M => M);
      Assert (Condition => M.Starttime = 11,
              Message   => "Starttime mismatch");
      Assert (Condition => M.Endtime = 243,
              Message   => "Endtime mismatch");
--  begin read only
   end Test_Move_Left;
--  end read only

end Steps.Short.Test_Data.Tests;
