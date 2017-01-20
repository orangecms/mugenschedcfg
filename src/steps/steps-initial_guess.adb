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
with Types.Basics; use Types.Basics;
with Types.Chain.Chains_Vectors;
with Auxiliary.My_Math; use Auxiliary.My_Math;

package body Steps.Initial_Guess
is

   --  ASSUMES: Processing Speeds are as little as they can be
   --  f.e. no Chain: c1@2-> c2@2 -> c3@4
   --  this would be translated to: c1@1->c2@1->c3@2
   --  What if Current_Ticks> Left ticks... -> suboptimal improvement might
   --  be better than optimal improvement on another chain
   --  If two chains bring the same gain, the one with the shorter first
   --  element will be preferred.
   procedure Choose_Chain
     (All_Chains            :     Types.Chain.Chains_Vectors.Vector;
      All_Chains_Throughput :     Types.Float_Vectors.Vector;
      All_Subject_Lengths   :     Natural_Array;
      My_Levels             :     Natural_Array;
      Level_Sizes           :     Natural_Array;
      Majorframe            :     Integer;
      Left_Ticks            :     Integer;
      This_Chain            : out Integer;
      How_Many              : out Float;
      Ticks                 : out Integer);

   -------------------------------------------------------------------------

   procedure Choose_Chain
     (All_Chains            :     Types.Chain.Chains_Vectors.Vector;
      All_Chains_Throughput :     Types.Float_Vectors.Vector;
      All_Subject_Lengths   :     Natural_Array;
      My_Levels             :     Natural_Array;
      Level_Sizes           :     Natural_Array;
      Majorframe            :     Integer;
      Left_Ticks            :     Integer;
      This_Chain            : out Integer;
      How_Many              : out Float;
      Ticks                 : out Integer)
   is
      Max_Improve              : Float := 0.0;
      Current_Throughput       : Float;
      All_Subject_Lengths_Copy : Natural_Array (All_Subject_Lengths'Range);

      Diff, Too_Big_Subject, Current_Ticks : Integer;
   begin
      How_Many := 0.0;

      for I in 1 .. All_Chains.Last_Index loop
         --  Calc how much new throughput in how many new ticks
         Too_Big_Subject := -2;
         while Too_Big_Subject /= -1 loop
            All_Subject_Lengths_Copy := All_Subject_Lengths;
            if Too_Big_Subject = -2 then
               --  this is the first try
               Too_Big_Subject := -1;
               Current_Throughput := 1.0;

               --  Calc the gain in throughput (current_throughput) as the LCM
               --  of the processing speeds
               for C of All_Chains (I).Chain loop
                  Current_Throughput
                    := LCM (Current_Throughput,
                            C.Processing_Speed *
                              Float
                                (Majorframe / Level_Sizes
                                     (My_Levels (C.Subject))));
               end loop;

               Current_Ticks := 0;
               for C of All_Chains (I).Chain loop
                  --  For every link of Chain check if Subject can get the
                  --  needed ticks for this
                  Diff := Integer
                    (Float'Ceiling ((All_Chains_Throughput (I) +
                         Current_Throughput) / (C.Processing_Speed *
                         Float (Majorframe / Level_Sizes
                           (My_Levels (C.Subject))))))
                    - All_Subject_Lengths_Copy (C.Subject);
                  if Diff > 0 then
                     All_Subject_Lengths_Copy (C.Subject) :=
                       All_Subject_Lengths_Copy (C.Subject)
                       + Diff;
                     Current_Ticks := Current_Ticks + Diff *
                       Majorframe / Level_Sizes
                         (My_Levels ((C.Subject)));
                  end if;

                  if All_Subject_Lengths_Copy (C.Subject) > Level_Sizes
                    (My_Levels (C.Subject))
                  then
                     Too_Big_Subject := C.Subject;
                  end if;
               end loop;
            else
               --  There is a subject too big
               --  -> calc throughput based on this subject:
               Current_Ticks := (Level_Sizes
                                 (My_Levels (Too_Big_Subject)))
                 - All_Subject_Lengths (Too_Big_Subject);
               Current_Throughput := Float'Last;
               for C of All_Chains (I).Chain loop

                  if C.Subject = Too_Big_Subject and then Current_Throughput >
                    Float (Current_Ticks) * C.Processing_Speed *
                    Float (Majorframe / Level_Sizes (My_Levels (C.Subject)))
                  then
                     Current_Throughput := Float (Current_Ticks)
                       * C.Processing_Speed
                       * Float (Majorframe / Level_Sizes
                                (My_Levels (C.Subject)));
                  end if;
               end loop;

               Too_Big_Subject := -1;
               Current_Ticks := 0;
               for C of All_Chains (I).Chain loop
                  Diff := Integer
                    (Float'Ceiling ((All_Chains_Throughput (I)
                     + Current_Throughput) / (C.Processing_Speed *
                       (Float (Majorframe / Level_Sizes (My_Levels
                          (C.Subject)))))))
                    - All_Subject_Lengths_Copy (C.Subject);
                  if Diff > 0 then
                     All_Subject_Lengths_Copy (C.Subject) :=
                       All_Subject_Lengths_Copy (C.Subject)
                       + Diff;

                     Current_Ticks := Current_Ticks + Diff *
                       Majorframe / Level_Sizes
                         (My_Levels (C.Subject));
                  end if;

                  if All_Subject_Lengths_Copy (C.Subject) >
                    Level_Sizes (My_Levels (C.Subject))
                  then
                     Too_Big_Subject := C.Subject;
                  end if;

               end loop;

            end if;

            if Current_Ticks > Left_Ticks
            then
               --  Not enough ticks left
               All_Subject_Lengths_Copy := All_Subject_Lengths;

               for C of All_Chains (I).Chain loop
                  if Current_Throughput > C.Processing_Speed *
                    Float (Majorframe / Level_Sizes (My_Levels
                           (C.Subject)))
                  then
                     Current_Throughput := C.Processing_Speed *
                       Float (Majorframe / Level_Sizes (My_Levels
                              (C.Subject)));
                  end if;
               end loop;

               Too_Big_Subject := -1;
               Current_Ticks   := 0;

               for C of All_Chains (I).Chain loop
                  Diff := Integer
                    (Float'Ceiling ((All_Chains_Throughput (I) +
                         Current_Throughput) / (C.Processing_Speed *
                         Float (Majorframe / Level_Sizes (My_Levels
                           (C.Subject))))))
                    - All_Subject_Lengths_Copy (C.Subject);
                  if Diff > 0 then
                     All_Subject_Lengths_Copy (C.Subject) :=
                       All_Subject_Lengths_Copy (C.Subject)
                       + Diff;
                     Current_Ticks := Current_Ticks + Diff *
                       (Majorframe / Level_Sizes (My_Levels (C.Subject)));
                  end if;

                  if All_Subject_Lengths_Copy (C.Subject) >
                    Level_Sizes (My_Levels (C.Subject))
                  then
                     Too_Big_Subject := C.Subject;
                  end if;

               end loop;
            end if;
         end loop;

         --  Better than all other chains so far?
         if (Current_Ticks = 0 and then Current_Throughput > 0.0) or else
           (Current_Ticks > 0 and then Current_Ticks <= Left_Ticks and then
              (All_Chains (I).Score (All_Chains_Throughput (I)
                                     + Current_Throughput) -
                 All_Chains (I).Score (All_Chains_Throughput (I)))
            / Float (Current_Ticks) > 0.0 and then
                (All_Chains (I).Score (All_Chains_Throughput (I)
                                       + Current_Throughput) -
                   All_Chains (I).Score (All_Chains_Throughput (I)))
            / Float (Current_Ticks)
            >= Max_Improve)
         then
            if Current_Ticks = 0 then
               Max_Improve := Float'Last;

               This_Chain := I;
               How_Many := Current_Throughput;
               Ticks := Current_Ticks;
            else
               if Max_Improve =
                 (All_Chains (I).Score (All_Chains_Throughput (I)
                  + Current_Throughput) - All_Chains (I).Score
                  (All_Chains_Throughput (I))) / Float (Current_Ticks)
               then
                  --  MAYBE exchange with a random choice
                  if All_Subject_Lengths
                    (All_Chains (I).Chain.First_Element.Subject) <
                      All_Subject_Lengths
                        (All_Chains (This_Chain).Chain
                         .First_Element.Subject)
                  then
                     This_Chain := I;
                     How_Many := Current_Throughput;
                     Ticks := Current_Ticks;
                  end if;
               else
                  Max_Improve := (All_Chains
                                  (I).Score (All_Chains_Throughput (I)
                                    + Current_Throughput)
                                  - All_Chains (I)
                                  .Score (All_Chains_Throughput (I)))
                    / Float (Current_Ticks);

                  This_Chain := I;
                  How_Many   := Current_Throughput;
                  Ticks      := Current_Ticks;
               end if;
            end if;
         end if;
      end loop;
   end Choose_Chain;

   -------------------------------------------------------------------------

   procedure Guess_Length
     (Per_Plan_Chains      :     Types.Chain.Per_Plan_Chain_Vectors.Vector;
      Level_Size_Array     :     TD.Per_Plan_Level_Sizes_Type;
      Min_Sizes_List       :     TD.Per_Plan_Subject_Lengths_Type;
      All_Levels           :     TD.Per_Plan_Subject_Levels_Type;
      Per_Plan_Level_Count :     Types.Positive_Vectors.Vector;
      Plans_Subject_Length : out TD.Per_Plan_Subject_Lengths_Type)
   is
      Ticks, Time_Left, Chain_Number : Integer;

      How_Many              : Float;
      All_Chains_Troughtput : Types.Float_Vectors.Vector;
   begin
      Plans_Subject_Length := Min_Sizes_List;

      for P in Positive range 1 .. Input.Plan_Count loop

         --  Calc all_Chains_throughput
         All_Chains_Troughtput.Clear;
         for C of Per_Plan_Chains (P) loop
            All_Chains_Troughtput.Append (Float'Last);
            for CL of C.Chain loop
               if Float (Plans_Subject_Length (P)(CL.Subject))
                 * CL.Processing_Speed * Float
                 (Level_Size_Array (P)
                  (Per_Plan_Level_Count (P)) / Level_Size_Array
                  (P)(All_Levels (P) (CL.Subject)))
                   < All_Chains_Troughtput.Last_Element
               then
                  All_Chains_Troughtput.Replace_Element
                    (Index    => All_Chains_Troughtput.Last_Index,
                     New_Item => Float
                       (Plans_Subject_Length (P)(CL.Subject)) *
                         CL.Processing_Speed *
                           Float (Level_Size_Array (P)
                       (Per_Plan_Level_Count (P))
                       / Level_Size_Array (P)(All_Levels (P) (CL.Subject))));
               end if;
            end loop;
         end loop;

         Time_Left := Level_Size_Array (P)(Per_Plan_Level_Count (P))
           * Input.CPU_Count;

         --  Plan minimum for every subject
         for S in 1 .. Input.Subject_Count loop
            Time_Left := Time_Left -
              (Min_Sizes_List (P)(S) *
                   Level_Size_Array (P)(Per_Plan_Level_Count (P))
               / Level_Size_Array (P)(All_Levels (P)(S)));
         end loop;

         How_Many := 1.0;

         --  Plan time of chain with biggest increase until no time is left or
         --  no one can improve
         while Time_Left > 0 and How_Many /= 0.0 loop
            Choose_Chain (All_Chains            => Per_Plan_Chains (P),
                          All_Chains_Throughput => All_Chains_Troughtput,
                          All_Subject_Lengths   => Plans_Subject_Length (P),
                          Level_Sizes           => Level_Size_Array (P),
                          Majorframe            => Level_Size_Array (P)
                          (Per_Plan_Level_Count (P)),
                          Left_Ticks            => Time_Left,
                          My_Levels             => All_Levels (P),
                          This_Chain            => Chain_Number,
                          How_Many              => How_Many,
                          Ticks                 => Ticks);

            if How_Many /= 0.0 then
               All_Chains_Troughtput (Chain_Number) :=
                 All_Chains_Troughtput (Chain_Number) + How_Many;
               Time_Left := Time_Left - Ticks;
               for C of Per_Plan_Chains (P)(Chain_Number).Chain loop
                  if Plans_Subject_Length (P)(C.Subject) <
                    Integer
                      (Float'Ceiling
                         (All_Chains_Troughtput (Chain_Number) /
                          (C.Processing_Speed * Float (Level_Size_Array (P)
                               (Per_Plan_Level_Count (P)) /
                                 Level_Size_Array (P)
                               (All_Levels (P) (C.Subject))))))
                  then
                     Plans_Subject_Length (P)(C.Subject) :=
                       Integer (Float'Ceiling
                                (All_Chains_Troughtput (Chain_Number)
                                   / (C.Processing_Speed *
                                       Float (Level_Size_Array (P)
                                         (Per_Plan_Level_Count (P))
                                         / Level_Size_Array (P)(All_Levels (P)
                                             (C.Subject))))));
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
   end Guess_Length;

end Steps.Initial_Guess;
