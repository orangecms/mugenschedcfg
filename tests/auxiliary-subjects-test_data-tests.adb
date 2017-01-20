--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Auxiliary.Subjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Auxiliary.Subjects.Test_Data.Tests is


--  begin read only
   procedure Test_Subject_By_Name (Gnattest_T : in out Test);
   procedure Test_Subject_By_Name_a10995 (Gnattest_T : in out Test) renames Test_Subject_By_Name;
--  id:2.2/a10995ddb5be89fd/Subject_By_Name/1/0/
   procedure Test_Subject_By_Name (Gnattest_T : in out Test) is
   --  auxiliary-subjects.ads:26:4:Subject_By_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Subjs : Types.Metas.Meta_Subject_Vectors.Vector;
   begin
      Subjs.Append
        (New_Item => Types.Metas.Meta_Subject_Package.Meta_Subject_Type'
           (Name   => To_Unbounded_String ("s1"),
            Level  => 5,
            others => <>));
      Subjs.Append
        (New_Item => Types.Metas.Meta_Subject_Package.Meta_Subject_Type'
           (Name   => To_Unbounded_String ("s2"),
            Level  => 9,
            others => <>));

      Assert (Condition => Subject_By_Name
              (Subjects => Subjs,
               Name     => "s2").Level = 9,
              Message   => "Subject mismatch (1)");
      Assert (Condition => Subject_By_Name
              (Subjects => Subjs,
               Name     => "s1").Level = 5,
              Message   => "Subject mismatch (2)");

      declare
         Dummy : Types.Metas.Meta_Subject_Package.Meta_Subject_Type;
      begin
         Dummy := Subject_By_Name (Subjects => Subjs,
                                   Name     => "nonexistent");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when No_Such_Subject => null;
      end;
--  begin read only
   end Test_Subject_By_Name;
--  end read only

end Auxiliary.Subjects.Test_Data.Tests;
