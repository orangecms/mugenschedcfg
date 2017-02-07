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

with Mulog;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Input.Cmd_Line;
private with Input.Checks;

package body Input
is

   --  Perform pre-precossing of XML data.
   procedure Preprocess;

   --  Return number of subjects defined in XML.
   function XML_Subject_Count return Positive;

   --  Return number of CPUs.
   function XML_CPU_Count return Positive;

   --  Return number of plans.
   function XML_Plan_Count return Positive;

   --  Return tick rate.
   function XML_Tick_Rate return Positive;

   --  Return plan weightings.
   function XML_Plan_Weighting return Types.Float_Vectors.Vector;

   --  Return maximum levels defined.
   function XML_Max_Levels return Positive;

   --  Return per-plan level sizes.
   function XML_Per_Plan_Level_Sizes
     return Types.Data_Structures.Per_Plan_Level_Sizes_Type;

   --  Return per-plan level count.
   function XML_Per_Plan_Level_Count return Types.Positive_Vectors.Vector;

   --  Return simultaneous domain count.
   function XML_Simultaneous_Domain_Count return Natural;

   --  Return same CPU count.
   function XML_Same_CPU_Count return Natural;

   --  Find largest ID attribute in given node list.
   function XML_Max_ID (Nodes : DOM.Core.Node_List) return Natural;

   --  Build restriction array from given exclude nodes and array dimension.
   function XML_Restrictions
     (Excludes  : DOM.Core.Node_List;
      Dimension : Natural)
      return Types.Basics.Execution_Restrictions_Array;

   -------------------------------------------------------------------------

   procedure Preprocess
   is
      All_Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/subjects/subject");
      Select_All : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan/subjects/selectAll");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Select_All) - 1 loop
         declare
            Select_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Select_All,
                                      Index => I);
            All_Level : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Select_Node,
                 Name => "setLevel");
            Parent : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Select_Node);

            Specific_Subjects : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Select_Node,
                 XPath => "subject");
         begin

            --  Append all subjects and set level if specified.

            for J in 0 .. DOM.Core.Nodes.Length (List => All_Subjects) - 1 loop
               declare
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Item
                         (List  => All_Subjects,
                          Index => J),
                       Name => "name");
                  New_Subj : constant DOM.Core.Node
                    := DOM.Core.Documents.Create_Element
                      (Doc      => XML_Data.Doc,
                       Tag_Name => "subject");
               begin
                  DOM.Core.Elements.Set_Attribute
                    (Elem  => New_Subj,
                     Name  => "ref",
                     Value => Subj_Name);

                  if All_Level'Length > 0 then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => New_Subj,
                        Name  => "level",
                        Value => All_Level);
                  end if;

                  Muxml.Utils.Append_Child (Node      => Parent,
                                            New_Child => New_Subj);
               end;
            end loop;

            --  Set level for explicitly specified subjects.

            for J in 0 .. DOM.Core.Nodes.Length (List => Specific_Subjects) - 1
            loop
               declare
                  Specific_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Specific_Subjects,
                                            Index => J);
                  Specific_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Specific_Node,
                       Name => "ref");
                  Specific_Level : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Specific_Node,
                       Name => "level");
                  Specific_Min_Len : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Specific_Node,
                       Name => "minLength");
                  Ref_Subj : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Parent,
                       XPath => "subject[@ref='" & Specific_Name & "']");
               begin
                  if Specific_Level'Length > 0 then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Ref_Subj,
                        Name  => "level",
                        Value => Specific_Level);
                  end if;

                  if Specific_Min_Len'Length > 0 then
                     DOM.Core.Elements.Set_Attribute
                       (Elem  => Ref_Subj,
                        Name  => "minLength",
                        Value => Specific_Min_Len);
                  end if;
               end;
            end loop;

            Muxml.Utils.Remove_Child (Node       => Parent,
                                      Child_Name => "selectAll");
         end;
      end loop;
   end Preprocess;

   -------------------------------------------------------------------------

   procedure Process (File : String)
   is
   begin
      Mulog.Log (Msg => "Processing scheduling configuration '" & File & "'");
      Muxml.Parse (Data => XML_Data,
                   Kind => Muxml.Mugenschedcfg,
                   File => File);

      if Cmd_Line.CPU_Count > 0 then
         CPUs := Cmd_Line.CPU_Count;
      else
         CPUs := XML_CPU_Count;
      end if;

      Checks.Run;

      Preprocess;

      Nr_Of_Subjects := XML_Subject_Count;
      Plans          := XML_Plan_Count;
      Plan_Weights   := XML_Plan_Weighting;
      Max_Levels     := XML_Max_Levels;
      Tickr          := XML_Tick_Rate;

      Per_Plan_Levels           := XML_Per_Plan_Level_Count;
      Per_Plan_Level_Sizes_Data := XML_Per_Plan_Level_Sizes;

      Simultaneous_Domains := XML_Simultaneous_Domain_Count;
      Same_CPU_Domains     := XML_Same_CPU_Count;

      Mulog.Log (Msg => "Configuration settings");
      Mulog.Log (Msg => "- CPUs                " & CPUs'Img);
      Mulog.Log (Msg => "- Tick rate           " & Tick_Rate'Img);
      Mulog.Log (Msg => "- Plans               " & Plans'Img);
      Mulog.Log (Msg => "- Subjects            " & Nr_Of_Subjects'Img);
      Mulog.Log (Msg => "- Max plan levels     " & Max_Levels'Img);
      Mulog.Log (Msg => "- Simultaneous domains"
                            & Simultaneous_Domains'Img);
      Mulog.Log (Msg => "- Same CPU domains    " & Same_CPU_Domains'Img);

      if Cmd_Line.No_Min_Lenghts_And_Domains then
         Mulog.Log (Msg => "WARNING: Domain support disabled");
         Mulog.Log (Msg => "WARNING: Min_Lengths not set");
      end if;
   end Process;

   -------------------------------------------------------------------------

   function Same_CPU_Allowed return Types.Basics.Execution_Restrictions_Array
   is (XML_Restrictions
       (Excludes  => McKae.XML.XPath.XIA.XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/mugenschedcfg/sameCpuDomains/exclude"),
        Dimension => Same_CPU_Domains));

   -------------------------------------------------------------------------

   function Simultaneous_Allowed
     return Types.Basics.Execution_Restrictions_Array
   is (XML_Restrictions
       (Excludes  => McKae.XML.XPath.XIA.XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/mugenschedcfg/simultaneousDomains/exclude"),
        Dimension => Simultaneous_Domains));

   -------------------------------------------------------------------------

   function XML_CPU_Count return Positive
   is (Positive'Value
       (Muxml.Utils.Get_Attribute
        (Doc   => XML_Data.Doc,
         XPath => "/mugenschedcfg",
         Name  => "cpuCount")));

   -------------------------------------------------------------------------

   function XML_Max_ID (Nodes : DOM.Core.Node_List) return Natural
   is
      Count : Natural := 0;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            ID : constant Positive
              := Positive'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => DOM.Core.Nodes.Item
                      (List  => Nodes,
                       Index => I),
                    Name => "id"));
         begin
            if ID > Count then
               Count := ID;
            end if;
         end;
      end loop;

      return Count;
   end XML_Max_ID;

   -------------------------------------------------------------------------

   function XML_Max_Levels return Positive
   is
      Result : Positive := 1;
      Levels : constant Types.Positive_Vectors.Vector
        := XML_Per_Plan_Level_Count;
   begin
      for I of Levels loop
         if I > Result then
            Result := I;
         end if;
      end loop;

      return Result;
   end XML_Max_Levels;

   -------------------------------------------------------------------------

   function XML_Per_Plan_Level_Count return Types.Positive_Vectors.Vector
   is
      Result : Types.Positive_Vectors.Vector;
      Plans  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Plans) - 1 loop
         declare
            Plan   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Plans,
                                      Index => I);
            Pcount : constant Positive
              := DOM.Core.Nodes.Length
                (List => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Plan,
                    XPath => "levels/level"));
         begin
            Result.Append (New_Item => Pcount);
         end;
      end loop;

      return Result;
   end XML_Per_Plan_Level_Count;

   -------------------------------------------------------------------------

   function XML_Per_Plan_Level_Sizes
     return Types.Data_Structures.Per_Plan_Level_Sizes_Type
   is
      subtype Level_Array is Types.Basics.Natural_Array (1 .. Max_Levels);

      Null_Levels : constant Level_Array := (others => 0);

      Result : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      Plans  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Plans) - 1 loop
         declare
            Plan : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Plans,
                                      Index => I);
            Level_Nodes : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Plan,
                 XPath => "levels/level");
            Levels : Level_Array := Null_Levels;
         begin
            for J in 1 .. DOM.Core.Nodes.Length (List => Level_Nodes) loop
               Levels (J) := Positive'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => DOM.Core.Nodes.Item
                         (List  => Level_Nodes,
                          Index => J - 1),
                     Name => "ticks"));
            end loop;

            Result.Append (New_Item => Levels);
         end;
      end loop;

      return Result;
   end XML_Per_Plan_Level_Sizes;

   -------------------------------------------------------------------------

   function XML_Plan_Count return Positive
   is (DOM.Core.Nodes.Length
       (List => McKae.XML.XPath.XIA.XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/mugenschedcfg/plans/plan")));

   -------------------------------------------------------------------------

   function XML_Plan_Weighting return Types.Float_Vectors.Vector
   is
      Result : Types.Float_Vectors.Vector;
      Plans  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Plans) - 1 loop
         Result.Append
           (New_Item => Float'Value (DOM.Core.Elements.Get_Attribute
            (Elem => DOM.Core.Nodes.Item
             (List  => Plans,
              Index => I),
             Name => "weighting")));
      end loop;

      return Result;
   end XML_Plan_Weighting;

   -------------------------------------------------------------------------

   function XML_Restrictions
     (Excludes  : DOM.Core.Node_List;
      Dimension : Natural)
      return Types.Basics.Execution_Restrictions_Array
   is
      Result : Types.Basics.Execution_Restrictions_Array
        (1 .. Dimension, 1 .. Dimension);
   begin
      if not Cmd_Line.No_Min_Lenghts_And_Domains then
         for I in 0 .. DOM.Core.Nodes.Length (List => Excludes) - 1 loop
            declare
               Domains1 : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => DOM.Core.Nodes.Item
                      (List  => Excludes,
                       Index => I),
                    XPath => "domain");
               Domains2 : constant DOM.Core.Node_List := Domains1;
            begin
               for J in 0 .. DOM.Core.Nodes.Length (List => Domains1) - 1
               loop
                  for K in 0 .. DOM.Core.Nodes.Length (List => Domains2) - 1
                  loop
                     declare
                        use type DOM.Core.Node;

                        Node1 : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item (List  => Domains1,
                                                  Index => J);
                        Node2 : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item (List  => Domains1,
                                                  Index => K);
                        Node1_ID : constant Positive
                          := Positive'Value (DOM.Core.Elements.Get_Attribute
                                             (Elem => Node1,
                                              Name => "id"));
                        Node2_ID : constant Positive
                          := Positive'Value (DOM.Core.Elements.Get_Attribute
                                             (Elem => Node2,
                                              Name => "id"));
                     begin
                        if Node1 /= Node2 then
                           Result (Node1_ID, Node2_ID) := False;
                        end if;
                     end;
                  end loop;
               end loop;
            end;
         end loop;
      end if;

      return Result;
   end XML_Restrictions;

   -------------------------------------------------------------------------

   function XML_Same_CPU_Count return Natural
   is (XML_Max_ID
       (Nodes => McKae.XML.XPath.XIA.XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/mugenschedcfg/sameCpuDomains/exclude/domain")));

   -------------------------------------------------------------------------

   function XML_Simultaneous_Domain_Count return Natural
   is (XML_Max_ID
       (Nodes => McKae.XML.XPath.XIA.XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/mugenschedcfg/simultaneousDomains/exclude/domain")));

   -------------------------------------------------------------------------

   function XML_Subject_Count return Positive
      is (DOM.Core.Nodes.Length
          (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/mugenschedcfg/subjects/subject")));

   -------------------------------------------------------------------------

   function XML_Tick_Rate return Positive
   is (Positive'Value
       (Muxml.Utils.Get_Attribute
        (Doc   => XML_Data.Doc,
         XPath => "/mugenschedcfg",
         Name  => "tickRate")));

end Input;
