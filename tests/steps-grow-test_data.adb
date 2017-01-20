--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Steps.Grow.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   function Double_Score return Points_Type
   is
   begin
      return R : Points_Type do
         R.Insert (New_Item => (X => 0.0, Y => 0.0));
         R.Insert (New_Item => (X => 1.0, Y => 2.0));
      end return;
   end Double_Score;

   -------------------------------------------------------------------------

   function Half_Score return Points_Type
   is
   begin
      return R : Points_Type do
         R.Insert (New_Item => (X => 0.0, Y => 0.0));
         R.Insert (New_Item => (X => 1.0, Y => 0.5));
      end return;
   end Half_Score;

   -------------------------------------------------------------------------

   function Identity_Score return Points_Type
   is
   begin
      return R : Points_Type do
         R.Insert (New_Item => (X => 0.0, Y => 0.0));
         R.Insert (New_Item => (X => 1.0, Y => 1.0));
      end return;
   end Identity_Score;

end Steps.Grow.Test_Data;
