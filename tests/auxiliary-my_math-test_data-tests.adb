--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Auxiliary.My_Math.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Auxiliary.My_Math.Test_Data.Tests is


--  begin read only
   procedure Test_Create_Permutation (Gnattest_T : in out Test);
   procedure Test_Create_Permutation_3517c8 (Gnattest_T : in out Test) renames Test_Create_Permutation;
--  id:2.2/3517c81db7b10f5a/Create_Permutation/1/0/
   procedure Test_Create_Permutation (Gnattest_T : in out Test) is
   --  auxiliary-my_math.ads:23:4:Create_Permutation
--  end read only

      pragma Unreferenced (Gnattest_T);

      Permutation : Types.Integer_Vectors.Vector;
      Seen : Types.Integer_Vectors.Vector;
      OK : Boolean := True;
   begin
      Permutation := Create_Permutation (10);
      for I in 1..10 loop
         Seen.Append (0);
      end loop;

      for P of Permutation loop
         Seen (P) := Seen (P) + 1;
      end loop;

      for S of Seen loop
         if S /= 1 then
            OK := False;
         end if;
      end loop;

      AUnit.Assertions.Assert
        (OK,
         "My_Math:Create_Permutation failed");
--  begin read only
   end Test_Create_Permutation;
--  end read only


--  begin read only
   procedure Test_LCM (Gnattest_T : in out Test);
   procedure Test_LCM_7f5f09 (Gnattest_T : in out Test) renames Test_LCM;
--  id:2.2/7f5f09ba31d45829/LCM/0/0/
   procedure Test_LCM (Gnattest_T : in out Test) is
   --  auxiliary-my_math.ads:27:4:LCM
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (LCM(2.5,5.0) = 5.0,
         "My_Math:Float LCM failed");

--  begin read only
   end Test_LCM;
--  end read only

end Auxiliary.My_Math.Test_Data.Tests;
