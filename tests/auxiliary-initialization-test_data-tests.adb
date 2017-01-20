--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Auxiliary.Initialization.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Auxiliary.Initialization.Test_Data.Tests is


--  begin read only
   procedure Test_Build_Per_Level_Subjects (Gnattest_T : in out Test);
   procedure Test_Build_Per_Level_Subjects_522dd0 (Gnattest_T : in out Test) renames Test_Build_Per_Level_Subjects;
--  id:2.2/522dd07968cc7365/Build_Per_Level_Subjects/1/0/
   procedure Test_Build_Per_Level_Subjects (Gnattest_T : in out Test) is
   --  auxiliary-initialization.ads:24:4:Build_Per_Level_Subjects
--  end read only

      pragma Unreferenced (Gnattest_T);

      A_Meta       : Meta_Subject_Type;
      All_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Result       : Types.Data_Structures.Subject_Level_Array (0 .. 4);
      OK           : Boolean  := True;
      Level_Count  : constant := 4;
   begin
      A_Meta.Number := 1;
      A_Meta.Level  := 1;

      A_Meta.Number := 2;
      A_Meta.Level  := Level_Count;

      A_Meta.Number := 3;
      A_Meta.Level  := Level_Count;

      A_Meta.Number := 2;
      A_Meta.Level  := 1;

      A_Meta.Number := 2;
      A_Meta.Level  := Level_Count;

      Build_Per_Level_Subjects (All_Subjects, Result);

      for I in 1 .. Level_Count loop
         for Subj of Result(I) loop
            if Subj.Level/=I then
               OK:=False;
            end if;
         end loop;
      end loop;

      AUnit.Assertions.Assert
        (OK,
         "Initialization:Build_Subject_Level_Array failed");
--  begin read only
   end Test_Build_Per_Level_Subjects;
--  end read only

end Auxiliary.Initialization.Test_Data.Tests;
