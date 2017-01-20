--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Types.Metas.Meta_Subject_Package.Meta_Subject_Type_Test_Data is

   Local_Meta_Subject_Type : aliased GNATtest_Generated.GNATtest_Standard.Types.Metas.Meta_Subject_Package.Meta_Subject_Type;
   procedure Set_Up (Gnattest_T : in out Test_Meta_Subject_Type) is
   begin
      Gnattest_T.Fixture := Local_Meta_Subject_Type'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Meta_Subject_Type) is
   begin
      null;
   end Tear_Down;

end Types.Metas.Meta_Subject_Package.Meta_Subject_Type_Test_Data;
