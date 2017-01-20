--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Types.Data_Structures.All_Plans_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Types.Data_Structures.All_Plans_Type_Test_Data.All_Plans_Type_Tests is


--  begin read only
   procedure Test_Create (Gnattest_T : in out Test_All_Plans_Type);
   procedure Test_Create_f8327d (Gnattest_T : in out Test_All_Plans_Type) renames Test_Create;
--  id:2.2/f8327d0562e6310f/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test_All_Plans_Type) is
   --  types-data_structures.ads:36:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      A : All_Plans_Type := Create
        (Plan_Count => 4,
         CPU_Count  => 8);
   begin
      Assert (Condition => A.Length = 4,
              Message   => "Plan count mismatch");

      for L of A loop
         Assert (Condition => L'First = 0,
                 Message   => "Array start mismatch");
         Assert (Condition => L'Last = 8,
                 Message   => "Array end mismatch");
      end loop;
--  begin read only
   end Test_Create;
--  end read only

end Types.Data_Structures.All_Plans_Type_Test_Data.All_Plans_Type_Tests;
