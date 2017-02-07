--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input.Checks.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Input.Checks.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_3bfa1e (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/3bfa1e656c84148a/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  input-checks.ads:23:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Positive case tested in Process test");
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Fixed_CPU_In_Range (Gnattest_T : in out Test);
   procedure Test_Fixed_CPU_In_Range_0d98df (Gnattest_T : in out Test) renames Test_Fixed_CPU_In_Range;
--  id:2.2/0d98dfe95ae4f669/Fixed_CPU_In_Range/1/0/
   procedure Test_Fixed_CPU_In_Range (Gnattest_T : in out Test) is
   --  input-checks.ads:30:4:Fixed_CPU_In_Range
--  end read only

      pragma Unreferenced (Gnattest_T);

      CPU_Count : constant Natural := Input.CPUs;
      Data      : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Mugenschedcfg,
                   File => "plans/complex.xml");

      --  Positive test, must not raise an exception.

      Input.CPUs := 4;
      Fixed_CPU_In_Range (Config => Data);

      Input.CPUs := 1;

      begin
         Fixed_CPU_In_Range (Config => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Fixed CPU value 1 of subject 's01' not in allowed range"
                    & " 0 .. 0",
                    Message   => "Exception mismatch");
      end;

      Input.CPUs := CPU_Count;

   exception
      when others =>
         Input.CPUs := CPU_Count;
         raise;
--  begin read only
   end Test_Fixed_CPU_In_Range;
--  end read only

end Input.Checks.Test_Data.Tests;
