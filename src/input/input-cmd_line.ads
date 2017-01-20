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

with Ada.Strings.Unbounded;

with GNAT.Command_Line;

package Input.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return raw plan file.
   function Plan_Raw_File return String;

   --  Return debug output file.
   function Debug_Output_File return String;

   --  Return number of CPUs.
   function CPU_Count return Natural;

   --  Returns True if support for minimum lengths and domains is disabled.
   function No_Min_Lenghts_And_Domains return Boolean;

   --  Return XML config.
   function XML_Config_File return String;

   Invalid_Cmd_Line : exception;

private

   use Ada.Strings.Unbounded;

   Plan_Raw, Debug_Out, XML_Config : Unbounded_String;

   CPUs : Natural := 0;

   No_Min_Lenghts_Domains : Boolean := False;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Input.Cmd_Line;
