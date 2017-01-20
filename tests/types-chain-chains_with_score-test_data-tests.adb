--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Types.Chain.Chains_With_Score.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Types.Chain.Chains_With_Score.Test_Data.Tests is


--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test);
   procedure Test_Less_Than_f38ce5 (Gnattest_T : in out Test) renames Test_Less_Than;
--  id:2.2/f38ce5ceb64fce63/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test) is
   --  types-chain-chains_with_score.ads:32:4:"<"
--  end read only

      pragma Unreferenced (Gnattest_T);

      Res : Boolean;
   begin
      Res := (X => 2.0, Y => 4.0) < (X => 4.0, Y => 5.0);
      Assert (Condition => Res,
              Message   => "True expected");

      Res := (X => 2.0, Y => 4.0) < (X => 2.0, Y => 5.0);
      Assert (Condition => not Res,
              Message   => "False expected (1)");
      Res := (X => 5.0, Y => 4.0) < (X => 2.0, Y => 5.0);
      Assert (Condition => not Res,
              Message   => "False expected (2)");
--  begin read only
   end Test_Less_Than;
--  end read only


--  begin read only
   procedure Test_Nearest_Left (Gnattest_T : in out Test);
   procedure Test_Nearest_Left_4adf4d (Gnattest_T : in out Test) renames Test_Nearest_Left;
--  id:2.2/4adf4ddef301224d/Nearest_Left/1/0/
   procedure Test_Nearest_Left (Gnattest_T : in out Test) is
   --  types-chain-chains_with_score.ads:53:4:Nearest_Left
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Points_Type;
      Res   : Point_Type;

      P1 : constant Point_Type := (X => 4.0,  Y => 5.0);
      P2 : constant Point_Type := (X => 5.0,  Y => 6.0);
      P3 : constant Point_Type := (X => 9.0,  Y => 12.0);
      P4 : constant Point_Type := (X => 22.0, Y => 29.5);
   begin
      Graph.Insert (New_Item => P1);
      Graph.Insert (New_Item => P2);
      Graph.Insert (New_Item => P3);
      Graph.Insert (New_Item => P4);

      Nearest_Left (X     => 5.5,
                    Graph => Graph,
                    P     => Res);
      Assert (Condition => Res = P2,
              Message   => "P2 expected");
      Nearest_Left (X     => 10.5,
                    Graph => Graph,
                    P     => Res);
      Assert (Condition => Res = P3,
              Message   => "P3 expected");
      Nearest_Left (X     => 200.9,
                    Graph => Graph,
                    P     => Res);
      Assert (Condition => Res = P4,
              Message   => "P4 expected");
--  begin read only
   end Test_Nearest_Left;
--  end read only


--  begin read only
   procedure Test_Nearest_Right (Gnattest_T : in out Test);
   procedure Test_Nearest_Right_d054f2 (Gnattest_T : in out Test) renames Test_Nearest_Right;
--  id:2.2/d054f28d87870ea1/Nearest_Right/1/0/
   procedure Test_Nearest_Right (Gnattest_T : in out Test) is
   --  types-chain-chains_with_score.ads:60:4:Nearest_Right
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Points_Type;
      Res   : Point_Type;

      P1 : constant Point_Type := (X => 4.0,  Y => 5.0);
      P2 : constant Point_Type := (X => 5.0,  Y => 6.0);
      P3 : constant Point_Type := (X => 9.0,  Y => 12.0);
      P4 : constant Point_Type := (X => 22.0, Y => 29.5);
   begin
      Graph.Insert (New_Item => P1);
      Graph.Insert (New_Item => P2);
      Graph.Insert (New_Item => P3);
      Graph.Insert (New_Item => P4);

      Nearest_Right (X     => 1.5,
                     Graph => Graph,
                     P     => Res);
      Assert (Condition => Res = P1,
              Message   => "P1 expected");
      Nearest_Right (X     => 4.5,
                     Graph => Graph,
                     P     => Res);
      Assert (Condition => Res = P2,
              Message   => "P2 expected");
      Nearest_Right (X     => 21.9,
                     Graph => Graph,
                     P     => Res);
      Assert (Condition => Res = P4,
              Message   => "P4 expected");
--  begin read only
   end Test_Nearest_Right;
--  end read only


--  begin read only
   procedure Test_Last_Two_Points (Gnattest_T : in out Test);
   procedure Test_Last_Two_Points_3797d7 (Gnattest_T : in out Test) renames Test_Last_Two_Points;
--  id:2.2/3797d71289909c0f/Last_Two_Points/1/0/
   procedure Test_Last_Two_Points (Gnattest_T : in out Test) is
   --  types-chain-chains_with_score.ads:69:4:Last_Two_Points
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph      : Points_Type;
      Res1, Res2 : Point_Type;

      P1 : constant Point_Type := (X => 4.0,  Y => 5.0);
      P2 : constant Point_Type := (X => 5.0,  Y => 6.0);
      P3 : constant Point_Type := (X => 9.0,  Y => 12.0);
      P4 : constant Point_Type := (X => 22.0, Y => 29.5);
   begin
      Graph.Insert (New_Item => P1);
      Graph.Insert (New_Item => P2);
      Graph.Insert (New_Item => P3);
      Graph.Insert (New_Item => P4);

      Last_Two_Points (Graph => Graph,
                       P1    => Res1,
                       P2    => Res2);
      Assert (Condition => Res2 = P4,
              Message   => "Res2 not P4");
      Assert (Condition => Res1 = P3,
              Message   => "Res1 not P4");
--  begin read only
   end Test_Last_Two_Points;
--  end read only


--  begin read only
   procedure Test_Find (Gnattest_T : in out Test);
   procedure Test_Find_b915a1 (Gnattest_T : in out Test) renames Test_Find;
--  id:2.2/b915a128c4a3a35d/Find/1/0/
   procedure Test_Find (Gnattest_T : in out Test) is
   --  types-chain-chains_with_score.ads:78:4:Find
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Points_Type;

      P1 : constant Point_Type := (X => 4.0,  Y => 5.0);
      P2 : constant Point_Type := (X => 5.0,  Y => 6.0);
   begin
      Graph.Insert (New_Item => P1);
      Graph.Insert (New_Item => P2);

      Assert (Condition => Find
              (Graph => Graph,
               X     => 5.0) = P2,
              Message   => "P2 not found");
      Assert (Condition => Find
              (Graph => Graph,
               X     => 12.2) = Infinity,
              Message   => "Infinity expected");
--  begin read only
   end Test_Find;
--  end read only

end Types.Chain.Chains_With_Score.Test_Data.Tests;
