--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

package body Input.Checks
is

   -------------------------------------------------------------------------

   procedure Fixed_CPU_In_Range (Config : Muxml.XML_Data_Type)
   is
      subtype CPU_Range is Natural range 0 .. Input.CPU_Count - 1;

      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Config.Doc,
           XPath => "/mugenschedcfg/subjects/subject[@cpu]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            CPU : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "cpu"));
            Sname : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "name");
         begin
            if CPU not in CPU_Range then
               raise Validation_Error with "Fixed CPU value" & CPU'Img
                 & " of subject '" & Sname & "' not in allowed range"
                 & CPU_Range'First'Img & " .." & CPU_Range'Last'Img;
            end if;
         end;
      end loop;
   end Fixed_CPU_In_Range;

   -------------------------------------------------------------------------

   procedure Fixed_CPU_No_Domain (Config : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Config.Doc,
           XPath => "/mugenschedcfg/subjects/subject"
           & "[@cpu and (simultaneousDomain or sameCpuDomain)]");
   begin
      if DOM.Core.Nodes.Length (List => Subjects) > 0 then
         declare
            Sname : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Subjects,
                    Index => 0),
                 Name => "name");
         begin
            raise Validation_Error with "Subject '" & Sname & "' specifies "
              & "a fixed CPU and a security domain which is not allowed";
         end;
      end if;
   end Fixed_CPU_No_Domain;

   -------------------------------------------------------------------------

   procedure Run
   is
   begin
      Fixed_CPU_In_Range  (Config => Input.XML_Data);
      Fixed_CPU_No_Domain (Config => Input.XML_Data);
   end Run;

end Input.Checks;
