--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Types.Metas.Meta_Subject_Package.Meta_Subject_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Types.Metas.Meta_Subject_Package.Meta_Subject_Type_Test_Data.Meta_Subject_Type_Tests is


--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test_Meta_Subject_Type);
   procedure Test_Less_Than_4bd847 (Gnattest_T : in out Test_Meta_Subject_Type) renames Test_Less_Than;
--  id:2.2/4bd847acbf697269/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test_Meta_Subject_Type) is
   --  types-metas-meta_subject_package.ads:46:4:"<"
--  end read only

      pragma Unreferenced (Gnattest_T);

      M1, M2 : Meta_Subject_Type;
   begin
      M1.Length := 5;
      M2.Length := 10;

      Assert (Condition => M1 < M2,
              Message   => "M1 not less than M2");
      Assert (Condition => not (M2 < M1),
              Message   => "M2 less than M1");

      M1.Length := M2.Length;
      Assert (Condition => not (M1 < M2),
              Message   => "M1 less than M2");
--  begin read only
   end Test_Less_Than;
--  end read only


--  begin read only
   procedure Test_Less_Restrictions (Gnattest_T : in out Test_Meta_Subject_Type);
   procedure Test_Less_Restrictions_2296b7 (Gnattest_T : in out Test_Meta_Subject_Type) renames Test_Less_Restrictions;
--  id:2.2/2296b701c6c3f7f8/Less_Restrictions/1/0/
   procedure Test_Less_Restrictions (Gnattest_T : in out Test_Meta_Subject_Type) is
   --  types-metas-meta_subject_package.ads:50:4:Less_Restrictions
--  end read only

      pragma Unreferenced (Gnattest_T);

      M1, M2 : Meta_Subject_Type;
   begin
      M1.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Include (New_Item => 50);
      Assert (Condition => Less_Restrictions (Left  => M1,
                                              Right => M2),
              Message   => "Unexpected result (1)");

      M1.Simultaneous_Set.Include (New_Item => 4);
      M1.Simultaneous_Set.Include (New_Item => 25);
      Assert (Condition => not Less_Restrictions (Left  => M1,
                                                  Right => M2),
              Message   => "Unexpected result (2)");

      M1.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Clear;
      Assert (Condition => Less_Restrictions (Left  => M1,
                                              Right => M2),
              Message   => "Unexpected result (3)");


      M1.Simultaneous_Set.Include (New_Item => 4);
      M2.Simultaneous_Set.Include (New_Item => 25);
      Assert (Condition => Less_Restrictions (Left  => M1,
                                              Right => M2),
              Message   => "Unexpected result (4)");

      M2.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Include (New_Item => 2);
      Assert (Condition => not Less_Restrictions (Left  => M1,
                                                  Right => M2),
              Message   => "Unexpected result (5)");

      Assert (Condition => not Less_Restrictions (Left  => M1,
                                                  Right => M1),
              Message   => "Unexpected result (6)");
--  begin read only
   end Test_Less_Restrictions;
--  end read only


--  begin read only
   procedure Test_Less_Restrictions_and_Length (Gnattest_T : in out Test_Meta_Subject_Type);
   procedure Test_Less_Restrictions_and_Length_82e9de (Gnattest_T : in out Test_Meta_Subject_Type) renames Test_Less_Restrictions_and_Length;
--  id:2.2/82e9ded61502e48e/Less_Restrictions_and_Length/1/0/
   procedure Test_Less_Restrictions_and_Length (Gnattest_T : in out Test_Meta_Subject_Type) is
   --  types-metas-meta_subject_package.ads:54:4:Less_Restrictions_and_Length
--  end read only

      pragma Unreferenced (Gnattest_T);

      M1, M2 : Meta_Subject_Type;
   begin
      M1.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Include (New_Item => 50);
      Assert (Condition => Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M2),
              Message   => "Unexpected result (1)");

      M1.Simultaneous_Set.Include (New_Item => 4);
      M1.Simultaneous_Set.Include (New_Item => 25);
      Assert (Condition => not Less_Restrictions_and_Length (Left  => M1,
                                                             Right => M2),
              Message   => "Unexpected result (2)");

      M1.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Clear;
      M1.Length := 1;
      M2.Length := 2;
      Assert (Condition => Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M2),
              Message   => "Unexpected result (3)");

      M1.Simultaneous_Set.Include (New_Item => 4);
      M2.Simultaneous_Set.Include (New_Item => 25);
      Assert (Condition => Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M2),
              Message   => "Unexpected result (4)");

      M2.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Include (New_Item => 2);
      Assert (Condition => not Less_Restrictions_and_Length (Left  => M1,
                                                             Right => M2),
              Message   => "Unexpected result (5)");

      M2.Simultaneous_Set.Clear;
      M2.Simultaneous_Set.Include (New_Item => 4);
      Assert (Condition => Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M2),
              Message   => "Unexpected result (6)");

      M1.Length := 100;
      M2.Length := 1;
      Assert (Condition => not Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M2),
              Message   => "Unexpected result (7)");

      Assert (Condition => Less_Restrictions_and_Length (Left  => M1,
                                                         Right => M1),
              Message   => "Unexpected result (8)");
--  begin read only
   end Test_Less_Restrictions_and_Length;
--  end read only

end Types.Metas.Meta_Subject_Package.Meta_Subject_Type_Test_Data.Meta_Subject_Type_Tests;
