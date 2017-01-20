--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Input.Cmd_Line.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Input.Cmd_Line.Test_Data.Tests is


--  begin read only
   procedure Test_Init (Gnattest_T : in out Test);
   procedure Test_Init_a69a58 (Gnattest_T : in out Test) renames Test_Init;
--  id:2.2/a69a5871ab5eef40/Init/1/0/
   procedure Test_Init (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:26:4:Init
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Strings.Unbounded.Unbounded_String;

      ----------------------------------------------------------------------

      procedure Invalid_Switch
      is
         Args : aliased GNAT.OS_Lib.Argument_List_Access
           := GNAT.OS_Lib.Argument_String_To_List (Arg_String => "-x");
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected (1)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Switch;

      ----------------------------------------------------------------------

      procedure Invalid_Parameter
      is
         Args : aliased GNAT.OS_Lib.Argument_List_Access
           := GNAT.OS_Lib.Argument_String_To_List
             (Arg_String => "-r raw -d deb -c -D plan.xml");
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected (2)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Invalid_Parameter;

      ----------------------------------------------------------------------

      procedure Null_Argument
      is
         Args : aliased GNAT.OS_Lib.Argument_List_Access
           := GNAT.OS_Lib.Argument_String_To_List
             (Arg_String => "-r raw -d deb -c 4");
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args);

         Parser := Test_Parser;

         begin
            Init (Description => "Test run");
            for A in Args'Range loop
               GNAT.OS_Lib.Free (X => Args (A));
            end loop;
            Assert (Condition => False,
                    Message   => "Exception expected (3)");

         exception
            when Invalid_Cmd_Line => null;
         end;

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;
      end Null_Argument;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Args : aliased GNAT.OS_Lib.Argument_List_Access
           := GNAT.OS_Lib.Argument_String_To_List
             (Arg_String => "-r raw -d deb -c 7 -D plan.xml");
         Test_Parser : GNAT.Command_Line.Opt_Parser;
      begin
         GNAT.Command_Line.Initialize_Option_Scan
           (Parser       => Test_Parser,
            Command_Line => Args);

         Parser := Test_Parser;

         Init (Description => "Test run");

         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         Assert (Condition => Plan_Raw = "raw",
                 Message   => "Raw plan mismatch");
         Assert (Condition => Debug_Out = "deb",
                 Message   => "Debug output mismatch");
         Assert (Condition => XML_Config = "plan.xml",
                 Message   => "XML config mismatch");
         Assert (Condition => CPUs = 7,
                 Message   => "CPU count mismatch");
         Assert (Condition => No_Min_Lenghts_Domains,
                 Message   => "Min. length and domains not disabled");

         CPUs := 4;

      exception
         when others =>
            CPUs := 4;
            raise;
      end Positive_Test;
   begin
      Invalid_Switch;
      Invalid_Parameter;
      Null_Argument;
      Positive_Test;
--  begin read only
   end Test_Init;
--  end read only


--  begin read only
   procedure Test_Plan_Raw_File (Gnattest_T : in out Test);
   procedure Test_Plan_Raw_File_fc254e (Gnattest_T : in out Test) renames Test_Plan_Raw_File;
--  id:2.2/fc254e30e0951fa9/Plan_Raw_File/1/0/
   procedure Test_Plan_Raw_File (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:29:4:Plan_Raw_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Plan_Raw := To_Unbounded_String ("t1");
      Assert (Condition => Plan_Raw_File = "t1",
              Message   => "Mismatch");
--  begin read only
   end Test_Plan_Raw_File;
--  end read only


--  begin read only
   procedure Test_Debug_Output_File (Gnattest_T : in out Test);
   procedure Test_Debug_Output_File_5d779f (Gnattest_T : in out Test) renames Test_Debug_Output_File;
--  id:2.2/5d779f4c13873649/Debug_Output_File/1/0/
   procedure Test_Debug_Output_File (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:32:4:Debug_Output_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Debug_Out := To_Unbounded_String ("t2");
      Assert (Condition => Debug_Output_File = "t2",
              Message   => "Mismatch");
--  begin read only
   end Test_Debug_Output_File;
--  end read only


--  begin read only
   procedure Test_CPU_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Count_190426 (Gnattest_T : in out Test) renames Test_CPU_Count;
--  id:2.2/19042678bd4b7254/CPU_Count/1/0/
   procedure Test_CPU_Count (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:35:4:CPU_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      CPUs := 12;
      Assert (Condition => CPU_Count = 12,
              Message   => "Mismatch");
      CPUs := 4;

   exception
      when others =>
         CPUs := 4;
         raise;
--  begin read only
   end Test_CPU_Count;
--  end read only


--  begin read only
   procedure Test_No_Min_Lenghts_And_Domains (Gnattest_T : in out Test);
   procedure Test_No_Min_Lenghts_And_Domains_e9c87e (Gnattest_T : in out Test) renames Test_No_Min_Lenghts_And_Domains;
--  id:2.2/e9c87e681765cfcb/No_Min_Lenghts_And_Domains/1/0/
   procedure Test_No_Min_Lenghts_And_Domains (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:38:4:No_Min_Lenghts_And_Domains
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      No_Min_Lenghts_Domains := True;
      Assert (Condition => No_Min_Lenghts_And_Domains,
              Message   => "Mismatch");
--  begin read only
   end Test_No_Min_Lenghts_And_Domains;
--  end read only


--  begin read only
   procedure Test_XML_Config_File (Gnattest_T : in out Test);
   procedure Test_XML_Config_File_79f3b0 (Gnattest_T : in out Test) renames Test_XML_Config_File;
--  id:2.2/79f3b01a932d4287/XML_Config_File/1/0/
   procedure Test_XML_Config_File (Gnattest_T : in out Test) is
   --  input-cmd_line.ads:41:4:XML_Config_File
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      XML_Config := To_Unbounded_String ("t3");
      Assert (Condition => XML_Config_File = "t3",
              Message   => "Mismatch");
--  begin read only
   end Test_XML_Config_File;
--  end read only

end Input.Cmd_Line.Test_Data.Tests;
