--
--  Copyright (C) 2016 Christiane Kuhn, secunet AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Input.Cmd_Line
is

   -------------------------------------------------------------------------

   function CPU_Count return Natural is (CPUs);

   -------------------------------------------------------------------------

   function Debug_Output_File return String is (To_String (Debug_Out));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline               : Mutools.Cmd_Line.Config_Type;
      Plan_R, Dbg_File      : aliased GNAT.Strings.String_Access;
      Dis_Minlength_Domains : aliased Boolean := False;
      CPUc                  : aliased Integer := 0;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <XML_config>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Plan_R'Access,
         Switch      => "-r:",
         Long_Switch => "--raw-plan-file:",
         Help        => "File to use for raw plan output");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Dbg_File'Access,
         Switch      => "-d:",
         Long_Switch => "--debug-file:",
         Help        => "Debug output destination");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => CPUc'Access,
         Switch      => "-c:",
         Long_Switch => "--cpu-count:",
         Help        => "Override CPU count defined in XML");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Dis_Minlength_Domains'Access,
         Switch      => "-D",
         Long_Switch => "--disable-min-lengths-domains",
         Help        => "Disable min. lengths and domains");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if Plan_R'Length /= 0 then
            Plan_Raw := To_Unbounded_String (Plan_R.all);
         end if;
         if Dbg_File'Length /= 0 then
            Debug_Out := To_Unbounded_String (Dbg_File.all);
         end if;
         GNAT.Strings.Free (X => Plan_R);
         GNAT.Strings.Free (X => Dbg_File);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      CPUs                   := CPUc;
      No_Min_Lenghts_Domains := Dis_Minlength_Domains;

      XML_Config := To_Unbounded_String
        (GNAT.Command_Line.Get_Argument
           (Parser => Parser));
      if Plan_Raw = Null_Unbounded_String
        or else Debug_Out = Null_Unbounded_String
        or else XML_Config_File = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

   -------------------------------------------------------------------------

   function No_Min_Lenghts_And_Domains return Boolean is
     (No_Min_Lenghts_Domains);

   -------------------------------------------------------------------------

   function Plan_Raw_File return String is (To_String (Plan_Raw));

   -------------------------------------------------------------------------

   function XML_Config_File return String is (To_String (XML_Config));

end Input.Cmd_Line;
