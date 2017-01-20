--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Steps.Initial_Guess.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Steps.Initial_Guess.Test_Data.Tests is


--  begin read only
   procedure Test_Guess_Length (Gnattest_T : in out Test);
   procedure Test_Guess_Length_1d92b3 (Gnattest_T : in out Test) renames Test_Guess_Length;
--  id:2.2/1d92b3bfd9729692/Guess_Length/1/0/
   procedure Test_Guess_Length (Gnattest_T : in out Test) is
   --  steps-initial_guess.ads:29:4:Guess_Length
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Guess_Length;
--  end read only

end Steps.Initial_Guess.Test_Data.Tests;
