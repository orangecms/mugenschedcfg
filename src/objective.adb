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

with Input;
with Types.Float_Vectors;

--  ATTENTION: Objective independent of MajorframeLength
--  NO checking if MajorframeLength is exceeded

--  ATTENTION DONT USE Subject.Length WITHOUT CHANGINGS IN MAIN!

package body Objective
is

   type Ticks_Per_Subject_Bonus_Type is array (Positive range <>) of Float
     with Default_Component_Value => 0.0;

   --  Amount of ticks that will be added if no context switch is needed
   --  (i.e. the tick before it had the same subject)
   Bonus_Factor : constant Float := 0.1;

   --  Without splitting cost.
   procedure Add_Time
     (Meta_Number                :        Integer;
      Subjects_and_Meta_Subjects :
        Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Sizes                :        Types.Basics.Natural_Array;
      Majorframe                 :        Integer;
      Ticks_per_Subject          : in out Ticks_Per_Subject_Type);

   -------------------------------------------------------------------------
   --  With splitting costs
   -------------------------------------------------------------------------

   function Calc_Ticks_Per_Subject_Bonus
     (Plan : Types.Data_Structures.Level_Type)
      return Ticks_Per_Subject_Bonus_Type;

   function Calc_Score_Of_One_Plan_Bonus
     (Ticks_Per_Subject : Ticks_Per_Subject_Bonus_Type;
      Chains_Vector     : Types.Chain.Chains_Vectors.Vector)
      return Float;

   ------------------------------------------------------------------------

   procedure Add_Time
     (Meta_Number                :        Integer;
      Subjects_and_Meta_Subjects :
        Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Sizes                :        Types.Basics.Natural_Array;
      Majorframe                 :        Integer;
      Ticks_per_Subject          : in out Ticks_Per_Subject_Type)
   is
      I : Positive := 1;
   begin
      for S of Subjects_and_Meta_Subjects (Meta_Number).Combined_Subjects loop
         if S <= Input.Subject_Count then
            Ticks_per_Subject (S) := Ticks_per_Subject (S)
              + Subjects_and_Meta_Subjects (Meta_Number).Tick_Vector (I)
              * Majorframe
              / Level_Sizes ((Subjects_and_Meta_Subjects (Meta_Number)
                             .Level) + 1);
         else
            Add_Time (Ticks_per_Subject          => Ticks_per_Subject,
                      Meta_Number                => S,
                      Subjects_and_Meta_Subjects => Subjects_and_Meta_Subjects,
                      Level_Sizes                => Level_Sizes,
                      Majorframe                 => Majorframe);
         end if;
         I := I + 1;
      end loop;
   end Add_Time;

   ------------------------------------------------------------------------

   function Calc_Objective_Score_Simple
     (Plans                  : Types.Data_Structures.All_Plans_Type;
      All_Subjects           : Types.Metas.Meta_Subject_Vectors.Vector;
      Per_Plan_Meta_Subjects : Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      Per_Plan_Level_Sizes   : Types.Data_Structures.Per_Plan_Level_Sizes_Type;
      Per_Plan_Chains        : Types.Chain.Per_Plan_Chain_Vectors.Vector;
      Per_Plan_Level_Count   : Types.Positive_Vectors.Vector)
      return Float
   is
      Result                     : Float := 0.0;
      Current                    : Float;
      Ticks_Per_Subject          : Ticks_Per_Subject_Type
        (1 .. Input.Subject_Count);
      Subjects_and_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Sum_of_Plan_Weighting      : Float := 0.0;
      Plan_Weighting             : constant Types.Float_Vectors.Vector
        := Input.Plan_Weighting;
   begin
      for P of Plan_Weighting loop
         Sum_of_Plan_Weighting := Sum_of_Plan_Weighting + P;
      end loop;

      --  Sum score over all plans.

      for P in Plans.Iterate loop
         declare
            Idx : constant Positive
              := Types.Data_Structures.All_Plans_Package.To_Index
                (Position => P);
         begin
            Subjects_and_Meta_Subjects.Clear;
            Subjects_and_Meta_Subjects := All_Subjects;
            Subjects_and_Meta_Subjects.Append (Per_Plan_Meta_Subjects (Idx));
            Ticks_Per_Subject := Calc_Ticks_Per_Subject
              (Plan                       => Plans (P),
               Subjects_And_Meta_Subjects => Subjects_and_Meta_Subjects,
               Majorframe                 =>
                 Per_Plan_Level_Sizes (Idx)(Per_Plan_Level_Count (Idx)),
               Level_Sizes                => Per_Plan_Level_Sizes (Idx));

            Current := Calc_Score_Of_One_Plan
              (Ticks_Per_Subject => Ticks_Per_Subject,
               Chains_Vector     => Per_Plan_Chains (Idx));

            Result := Result + (Plan_Weighting (Idx) / Sum_of_Plan_Weighting)
              * Current;
         end;
      end loop;

      return Result;
   end Calc_Objective_Score_Simple;

   ------------------------------------------------------------------------

   function Calc_Objective_Score_Split
     (Plans                  : Types.Data_Structures.All_Plans_Type;
      All_Subjects           : Types.Metas.Meta_Subject_Vectors.Vector;
      Per_Plan_Meta_Subjects : Types.Metas.Meta_Subject_Vector_Vectors.Vector;
      Per_Plan_Chains        : Types.Chain.Per_Plan_Chain_Vectors.Vector)
      return Float
   is
      Result                     : Float := 0.0;
      Current                    : Float;
      Ticks_Per_Subject          : Ticks_Per_Subject_Bonus_Type
        (1 .. Input.Subject_Count);
      Subjects_and_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Sum_of_Plan_Weighting      : Float := 0.0;
      Plan_Weighting             : constant Types.Float_Vectors.Vector
        := Input.Plan_Weighting;
   begin
      for P of Plan_Weighting loop
         Sum_of_Plan_Weighting := Sum_of_Plan_Weighting + P;
      end loop;

      --  Sum score over all plans.

      for P in Plans.Iterate loop
         declare
            Idx : constant Positive
              := Types.Data_Structures.All_Plans_Package.To_Index
                (Position => P);
         begin
            Subjects_and_Meta_Subjects.Clear;
            Subjects_and_Meta_Subjects := All_Subjects;
            Subjects_and_Meta_Subjects.Append (Per_Plan_Meta_Subjects (Idx));
            Ticks_Per_Subject := Calc_Ticks_Per_Subject_Bonus
              (Plan => Plans (P));

            Current := Calc_Score_Of_One_Plan_Bonus
              (Ticks_Per_Subject => Ticks_Per_Subject,
               Chains_Vector     => Per_Plan_Chains (Idx));

            Result := Result + (Plan_Weighting (Idx) / Sum_of_Plan_Weighting)
              * Current;
         end;
      end loop;

      return Result;
   end Calc_Objective_Score_Split;

   ------------------------------------------------------------------------

   function Calc_Score_Of_One_Plan
     (Ticks_Per_Subject : Ticks_Per_Subject_Type;
      Chains_Vector     : Types.Chain.Chains_Vectors.Vector)
      return Float
   is
      Sum : Float := 0.0;
      Min : Float; --  Minimal throughput
   begin
      for C of Chains_Vector loop
         Min := Float'Last;
         --  Calc throughput = min of all subject throughputs
         for Link of C.Chain loop
            if Float (Ticks_Per_Subject (Link.Subject)) * Link.Processing_Speed
              < Min
            then
               Min :=
                 Float (Ticks_Per_Subject (Link.Subject))
                   * Link.Processing_Speed;
            end if;

         end loop;

         if Sum /= Float'Last and then Sum /= Float'First then
            Sum := Sum + C.Score (Min);
         end if;
      end loop;

      return Sum;
   end Calc_Score_Of_One_Plan;

   ------------------------------------------------------------------------

   function Calc_Score_Of_One_Plan_Bonus
     (Ticks_Per_Subject : Ticks_Per_Subject_Bonus_Type;
      Chains_Vector     : Types.Chain.Chains_Vectors.Vector)
      return Float
   is
      Sum : Float := 0.0;
      Min : Float; --  Minimal throughput
   begin
      for C of Chains_Vector loop
         Min := Float'Last;
         --  Calc throughput = min of all subject throughputs
         for Link of C.Chain loop
            if Ticks_Per_Subject (Link.Subject) * Link.Processing_Speed
              < Min
            then
               Min :=
                 Ticks_Per_Subject (Link.Subject) * Link.Processing_Speed;
            end if;
         end loop;

         if Sum /= Float'Last and then Sum /= Float'First then
            Sum := Sum + C.Score (Min);
         end if;
      end loop;

      return Sum;
   end Calc_Score_Of_One_Plan_Bonus;

   ------------------------------------------------------------------------

   function Calc_Ticks_Per_Subject
     (Plan                       : Types.Data_Structures.Level_Type;
      Subjects_And_Meta_Subjects : Types.Metas.Meta_Subject_Vectors.Vector;
      Level_Sizes                : Types.Basics.Natural_Array;
      Majorframe                 : Integer)
      return Ticks_Per_Subject_Type
   is
      Ticks_Per_Subject : Ticks_Per_Subject_Type (1 .. Input.Subject_Count);
   begin
      --  For every minorframe
      for P of Plan loop
         for M of P loop
            --  If minorframe is subject then add time
            --  else find subjects of metasubject and add time there
            if M.Subject <= Input.Subject_Count then
               Ticks_Per_Subject (M.Subject) := Ticks_Per_Subject (M.Subject)
                 + (M.Endtime - M.Starttime + 1)
                 * Majorframe / Level_Sizes (1);
            else
               Add_Time
                 (Ticks_per_Subject          => Ticks_Per_Subject,
                  Meta_Number                => M.Subject,
                  Subjects_and_Meta_Subjects => Subjects_And_Meta_Subjects,
                  Level_Sizes                => Level_Sizes,
                  Majorframe                 => Majorframe);
            end if;
         end loop;
      end loop;
      return Ticks_Per_Subject;
   end Calc_Ticks_Per_Subject;

   ------------------------------------------------------------------------

   --  Maybe a good point to improve runtime: make use of repeating subjects...
   function Calc_Ticks_Per_Subject_Bonus
     (Plan : Types.Data_Structures.Level_Type)
      return Ticks_Per_Subject_Bonus_Type
   is
      Ticks_per_Subject : Ticks_Per_Subject_Bonus_Type
        (1 .. Input.Subject_Count);
   begin
      --  For every minorframe
      for P of Plan loop
         for M of P loop
            --  For every minorframe add time
            --  if time is more than one ticks a bonus per extra tick is added
            --  (this equals a penalty for every split)
            if M.Subject <= Input.Subject_Count then
               Ticks_per_Subject (M.Subject) := Ticks_per_Subject (M.Subject)
                 + (Float (M.Endtime - M.Starttime + 1))
                 + (Bonus_Factor * Float (M.Endtime - M.Starttime));
            end if;
         end loop;
      end loop;
      return Ticks_per_Subject;
   end Calc_Ticks_Per_Subject_Bonus;

end Objective;
