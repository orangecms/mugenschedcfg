--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Types.Data_Structures.Per_Plan_Subject_Levels_Type_Test_Data is

   type Per_Plan_Subject_Levels_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Types.Data_Structures.Per_Plan_Subject_Levels_Type'Class;

--  begin read only
   type Test_Per_Plan_Subject_Levels_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Per_Plan_Subject_Levels_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Per_Plan_Subject_Levels_Type);
   procedure Tear_Down (Gnattest_T : in out Test_Per_Plan_Subject_Levels_Type);

end Types.Data_Structures.Per_Plan_Subject_Levels_Type_Test_Data;