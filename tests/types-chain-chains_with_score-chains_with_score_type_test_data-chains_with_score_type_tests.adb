--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Types.Chain.Chains_With_Score.Chains_With_Score_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Types.Chain.Chains_With_Score.Chains_With_Score_Type_Test_Data.Chains_With_Score_Type_Tests is


--  begin read only
   procedure Test_Score (Gnattest_T : in out Test_Chains_With_Score_Type);
   procedure Test_Score_51ea93 (Gnattest_T : in out Test_Chains_With_Score_Type) renames Test_Score;
--  id:2.2/51ea93ebe6a1fb6b/Score/1/0/
   procedure Test_Score (Gnattest_T : in out Test_Chains_With_Score_Type) is
   --  types-chain-chains_with_score.ads:46:4:Score
--  end read only

      pragma Unreferenced (Gnattest_T);

      Chain : Chains_With_Score_Type;

      P1 : constant Point_Type := (X => 4.0,  Y => 5.0);
      P2 : constant Point_Type := (X => 5.0,  Y => 6.0);
      P3 : constant Point_Type := (X => 9.0,  Y => 1.0);
      P4 : constant Point_Type := (X => 15.0, Y => 13.0);
   begin
      Chain.Datapoints.Insert (New_Item => P1);
      Chain.Datapoints.Insert (New_Item => P2);
      Chain.Datapoints.Insert (New_Item => P3);
      Chain.Datapoints.Insert (New_Item => P4);

      Assert (Condition => Score (C => Chain, X => 4.5) = 5.5,
              Message   => "Score mismatch (1)");
      Assert (Condition => Score (C => Chain, X => 10.0) = 3.0,
              Message   => "Score mismatch (2)");
      Assert (Condition => Score (C => Chain, X => 9.0) = 1.0,
              Message   => "Score mismatch (3)");
--  begin read only
   end Test_Score;
--  end read only

end Types.Chain.Chains_With_Score.Chains_With_Score_Type_Test_Data.Chains_With_Score_Type_Tests;
