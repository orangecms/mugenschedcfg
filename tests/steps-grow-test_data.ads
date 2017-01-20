--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Types.Chain.Chains;
with Types.Metas.Meta_Subject_Vectors;
with Types.Chain.Chains_Vectors;
with Types.Chain.Chain_Link; use Types.Chain.Chain_Link;
with Types.Chain.Chains_With_Score; use Types.Chain.Chains_With_Score;
with Types.Metas.Meta_Subject_Package; use Types.Metas.Meta_Subject_Package;
with Types.Integer_Vectors;
with Types.Minfs.Minorframes;

package Steps.Grow.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   function Identity_Score return Points_Type;

   function Double_Score return Points_Type;

   function Half_Score return Points_Type;

end Steps.Grow.Test_Data;
