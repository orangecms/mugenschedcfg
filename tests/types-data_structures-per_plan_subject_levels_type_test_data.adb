--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Types.Data_Structures.Per_Plan_Subject_Levels_Type_Test_Data is

   Local_Per_Plan_Subject_Levels_Type : aliased GNATtest_Generated.GNATtest_Standard.Types.Data_Structures.Per_Plan_Subject_Levels_Type;
   procedure Set_Up (Gnattest_T : in out Test_Per_Plan_Subject_Levels_Type) is
   begin
      Gnattest_T.Fixture := Local_Per_Plan_Subject_Levels_Type'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Per_Plan_Subject_Levels_Type) is
   begin
      null;
   end Tear_Down;

end Types.Data_Structures.Per_Plan_Subject_Levels_Type_Test_Data;