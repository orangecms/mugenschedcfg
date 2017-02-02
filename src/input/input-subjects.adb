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

with Ada.Containers;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Auxiliary.Subjects;
with Types.Combined_With;
with Types.Positive_Set;
with Types.Metas.Meta_Subject_Package;
with Steps.Initial_Guess;
with Input.Chains;
with Input.Cmd_Line;

package body Input.Subjects
is

   use Ada.Strings.Unbounded;

   --  Initialize subjects from XML data.
   function XML_Subjects return TM.Meta_Subject_Vectors.Vector;

   --  Initialize per-plan subjects from XML data.
   function XML_Per_Plan_Subjects
     (All_Subjects : TM.Meta_Subject_Vectors.Vector)
      return TM.Meta_Subject_Vector_Vectors.Vector;

   -------------------------------------------------------------------------

   procedure Get_Input
     (Per_Plan_Subjects          : out TM.Meta_Subject_Vector_Vectors.Vector;
      All_Subjects               : out TM.Meta_Subject_Vectors.Vector;
      Per_Plan_Chains            : out TC.Per_Plan_Chain_Vectors.Vector;
      Per_Plan_Subject_Lengths   : out TD.Per_Plan_Subject_Lengths_Type;
      Per_Plan_Subject_Min_Sizes : out TD.Per_Plan_Subject_Lengths_Type)
   is
      subtype Plan_Range is Positive range 1 .. Input.Plan_Count;

      Per_Plan_Subject_Levels : TD.Per_Plan_Subject_Levels_Type;
   begin
      All_Subjects := XML_Subjects;

      for I in 1 .. Subject_Count loop
         All_Subjects (I).On_CPU_With_Number := I;
         All_Subjects (I).Combined_Subjects.Append (I);
      end loop;

      --  Chains.

      Per_Plan_Chains := Chains.Get_Input (All_Subjects => All_Subjects);

      Per_Plan_Subjects := XML_Per_Plan_Subjects
        (All_Subjects => All_Subjects);

      --  Adjust per-plan subjects.

      --  Set Plan Number
      for P in Plan_Range loop
         for A of Per_Plan_Subjects (P) loop
            A.Plan := P;
         end loop;
      end loop;

      declare
         Subject_Lengths : constant TB.Natural_Array (1 .. Subject_Count)
           := (others => 0);
      begin
         Per_Plan_Subject_Min_Sizes.Append
           (New_Item => Subject_Lengths,
            Count    => Ada.Containers.Count_Type (Plan_Count));
      end;
      for P in Plan_Range loop
         for S of Per_Plan_Subjects (P) loop
            Per_Plan_Subject_Min_Sizes (P)(S.Number)
              := S.Min_Length;
         end loop;
      end loop;

      declare
         Subject_Levels : constant TB.Natural_Array (1 .. Subject_Count)
           := (others => 1);
      begin
         Per_Plan_Subject_Levels.Append
           (New_Item => Subject_Levels,
            Count    => Ada.Containers.Count_Type (Plan_Count));
      end;
      for P in Plan_Range loop
         for S of Per_Plan_Subjects (P) loop
            Per_Plan_Subject_Levels (P)(S.Number) := S.Level;
         end loop;
      end loop;

      Steps.Initial_Guess.Guess_Length
        (Per_Plan_Chains      => Per_Plan_Chains,
         All_Levels           => Per_Plan_Subject_Levels,
         Level_Size_Array     => Per_Plan_Level_Sizes,
         Min_Sizes_List       => Per_Plan_Subject_Min_Sizes,
         Per_Plan_Level_Count => Per_Plan_Level_Count,
         Plans_Subject_Length => Per_Plan_Subject_Lengths);

      for P in Plan_Range loop
         for S in 1 .. Input.Subject_Count loop
            if S <= Per_Plan_Subjects (P).Last_Index then
               Per_Plan_Subjects (P).Reference (S).Length
                 := Per_Plan_Subject_Lengths (P)
                 (Per_Plan_Subjects (P)(S).Number);
            end if;
         end loop;
      end loop;
   end Get_Input;

   -------------------------------------------------------------------------

   function Get_Next_Meta_Number (Plan : Positive) return Positive
   is
      Res : constant Positive := Per_Plan_Meta_Subject_Numbers (Plan);
   begin
      Per_Plan_Meta_Subject_Numbers (Plan)
        := Per_Plan_Meta_Subject_Numbers (Plan) + 1;

      return Res;
   end Get_Next_Meta_Number;

   -------------------------------------------------------------------------

   procedure Get_On_Same_CPU (On_Same_CPU : out Types.CPU_Sets.Vector)
   is
      S : Types.Combined_With.Set;
   begin
      On_Same_CPU.Clear;

      for I in 1 .. Subject_Count loop
         S.Clear;
         S.Insert (I);
         On_Same_CPU.Append (S);
      end loop;
   end Get_On_Same_CPU;

   -------------------------------------------------------------------------

   procedure Init_Meta_Numbers
   is
   begin
      Per_Plan_Meta_Subject_Numbers.Clear;
      Per_Plan_Meta_Subject_Numbers.Append
        (New_Item => Subject_Count + 1,
         Count    => Ada.Containers.Count_Type (Plan_Count));
   end Init_Meta_Numbers;

   -------------------------------------------------------------------------

   function XML_Per_Plan_Subjects
     (All_Subjects : TM.Meta_Subject_Vectors.Vector)
      return TM.Meta_Subject_Vector_Vectors.Vector
   is
      Result   : TM.Meta_Subject_Vector_Vectors.Vector;
      Subjects : TM.Meta_Subject_Vectors.Vector;

      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan/subjects");
   begin
      Result.Append
        (New_Item => Subjects,
         Count    => Ada.Containers.Count_Type (Input.Plan_Count));

      for I in 1 .. DOM.Core.Nodes.Length (List => Nodes) loop
         declare
            Subj_Refs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I - 1),
                 XPath => "subject");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Refs) - 1 loop
               declare
                  Subj_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subj_Refs,
                       Index => J);
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Node,
                       Name => "ref");
                  Level_Str : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Node,
                       Name => "level");
                  Min_Len_Str : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Node,
                       Name => "minLength");
                  Subj : TM.Meta_Subject_Package.Meta_Subject_Type
                    := Auxiliary.Subjects.Subject_By_Name
                      (Subjects => All_Subjects,
                       Name     => Subj_Name);
               begin
                  if Level_Str'Length > 0 then
                     Subj.Level := Positive'Value (Level_Str);
                  end if;

                  if Min_Len_Str'Length > 0
                    and then not Cmd_Line.No_Min_Lenghts_And_Domains
                  then
                     Subj.Min_Length := Positive'Value (Min_Len_Str);
                  end if;

                  Result (I).Append (New_Item => Subj);
               end;
            end loop;
         end;
      end loop;

      return Result;
   end XML_Per_Plan_Subjects;

   -------------------------------------------------------------------------

   function XML_Subjects return TM.Meta_Subject_Vectors.Vector
   is
      Result : TM.Meta_Subject_Vectors.Vector;
      Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/subjects/subject");
   begin
      for I in 1 .. DOM.Core.Nodes.Length (List => Nodes) loop
         declare

            --  Return positive set from node attributes given by XPath
            --  relative to node and attribute name.
            function Get_Positive_Set
              (Node  : DOM.Core.Node;
               XPath : String;
               Attr  : String)
               return Types.Positive_Set.Set;

            ----------------------------------------------------------------

            function Get_Positive_Set
              (Node  : DOM.Core.Node;
               XPath : String;
               Attr  : String)
               return Types.Positive_Set.Set
            is
               Result : Types.Positive_Set.Set;
               Nodes  : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Node,
                    XPath => XPath);
            begin
               for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
                  Result.Insert
                    (New_Item => Positive'Value
                       (DOM.Core.Elements.Get_Attribute
                            (Elem => DOM.Core.Nodes.Item
                                 (List  => Nodes,
                                  Index => I),
                             Name => Attr)));
               end loop;

               return Result;
            end Get_Positive_Set;

            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I - 1);
            Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Level_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "level");
            Level : constant Natural
              := (if Level_Str'Length > 0 then
                     Positive'Value (Level_Str) else 0);
            CPU_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "cpu");
            CPU : constant Natural
              := (if CPU_Str'Length > 0 then
                     Positive'Value (CPU_Str) + 1 else 0);
         begin
            Result.Append
              (New_Item =>
                 (Number           => I,
                  Name             => To_Unbounded_String (Name),
                  Level            => Level,
                  CPU              => CPU,
                  Min_Length       => 1,
                  Length           => 1,
                  Simultaneous_Set => Get_Positive_Set
                    (Node  => Subj,
                     XPath => "simultaneousDomain",
                     Attr  => "ref"),
                  Same_CPU_Set     => Get_Positive_Set
                    (Node  => Subj,
                     XPath => "sameCpuDomain",
                     Attr  => "ref"),
                  others           => <>));
         end;
      end loop;

      return Result;
   end XML_Subjects;

end Input.Subjects;
