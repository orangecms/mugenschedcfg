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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Types.Chain.Chains_Vectors;
with Types.Chain.Chains;
with Types.Chain.Chains_With_Score;
with Types.Chain.Chain_Link;
with Auxiliary.Subjects;

package body Input.Chains
is

   --  Return graph of function given by name.
   --  TODO: Cache results.
   function To_Points
     (Name : String)
      return Types.Chain.Chains_With_Score.Points_Type;

   -------------------------------------------------------------------------

   function Get_Input
     (All_Subjects : Types.Metas.Meta_Subject_Vectors.Vector)
      return Types.Chain.Per_Plan_Chain_Vectors.Vector
   is
      Plans  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/plans/plan");
      Result : Types.Chain.Per_Plan_Chain_Vectors.Vector;
   begin
      for P in 0 .. DOM.Core.Nodes.Length (List => Plans) - 1 loop
         declare
            CV     : Types.Chain.Chains_Vectors.Vector;
            Chains : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                   (List  => Plans,
                    Index => P),
                 XPath => "chains/chain");
         begin
            for I in 0 .. DOM.Core.Nodes.Length (List => Chains) - 1 loop
               declare
                  Chain_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Chains,
                       Index => I);
                  Subjects : constant DOM.Core.Node_List
                    := McKae.XML.XPath.XIA.XPath_Query
                      (N     => Chain_Node,
                       XPath => "subject");
                  Funcname : constant String
                    := Muxml.Utils.Get_Attribute
                      (Doc   => Chain_Node,
                       XPath => "scoreFunction",
                       Name  => "ref");

                  List  : Types.Chain.Chains.List;
                  Score : Types.Chain.Chains_With_Score.Chains_With_Score_Type;
               begin
                  for S in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1
                  loop
                     declare
                        package AUX renames Auxiliary.Subjects;
                        package TC  renames Types.Chain;

                        Subj_Node : constant DOM.Core.Node
                          := DOM.Core.Nodes.Item (List  => Subjects,
                                                  Index => S);
                        Subj_Name : constant String
                          := DOM.Core.Elements.Get_Attribute
                            (Elem => Subj_Node,
                             Name => "ref");
                        Speed : constant Float
                          := Float'Value (DOM.Core.Elements.Get_Attribute
                                          (Elem => Subj_Node,
                                           Name => "speed"));

                        C_Speed : constant TC.Chain_Link.Chain_Link_Type
                          := (Subject          => AUX.Subject_By_Name
                              (Subjects => All_Subjects,
                               Name     => Subj_Name).Number,
                              Processing_Speed => Speed);
                     begin
                        List.Append (New_Item => C_Speed);
                     end;
                  end loop;

                  CV.Append (New_Item => Score);
                  CV (CV.Last).Chain      := List;
                  CV (CV.Last).Datapoints := To_Points
                    (Name => Funcname);
               end;
            end loop;

            Result.Append (New_Item => CV);
         end;
      end loop;

      return Result;
   end Get_Input;

   -------------------------------------------------------------------------

   function To_Points
     (Name : String)
      return Types.Chain.Chains_With_Score.Points_Type
   is
      Result : Types.Chain.Chains_With_Score.Points_Type;
      Points : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/mugenschedcfg/scoreFunctions/function"
           & "[@name='" & Name & "']/point");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Points) - 1 loop
         declare
            Point : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Points,
                                      Index => I);
            X : constant Float := Float'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Point,
                  Name => "x"));
            Y : constant Float := Float'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Point,
                  Name => "y"));
         begin
            Result.Insert
              (New_Item => Types.Chain.Chains_With_Score.Point_Type'
                 (X => X, Y => Y));
         end;
      end loop;

      return Result;
   end To_Points;

end Input.Chains;
