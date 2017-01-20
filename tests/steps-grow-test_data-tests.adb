--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Steps.Grow.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Steps.Grow.Test_Data.Tests is


--  begin read only
   procedure Test_Grow_One_Level_Steady_Order (Gnattest_T : in out Test);
   procedure Test_Grow_One_Level_Steady_Order_c16b03 (Gnattest_T : in out Test) renames Test_Grow_One_Level_Steady_Order;
--  id:2.2/c16b0351b00678d2/Grow_One_Level_Steady_Order/1/0/
   procedure Test_Grow_One_Level_Steady_Order (Gnattest_T : in out Test) is
   --  steps-grow.ads:30:4:Grow_One_Level_Steady_Order
--  end read only

      pragma Unreferenced (Gnattest_T);

      Subject_Count : constant := 47;
      subtype Subject_Range is Positive range 1 .. Subject_Count;

      A_Meta                : Meta_Subject_Type;
      Plan                  : Types.Data_Structures.Level_Type (0 .. 4);
      Meta_and_Subjects     : Types.Metas.Meta_Subject_Vectors.Vector;
      Ticks_Per_Subject     : Objective.Ticks_Per_Subject_Type (Subject_Range);
      Ticks_Per_Subject_Old : Objective.Ticks_Per_Subject_Type (Subject_Range);
      Level_Sizes           : Types.Basics.Natural_Array (1 .. 4);
      Simultaneous_Allowed  : Types.Basics.Execution_Restrictions_Array
        (1 .. 5, 1 .. 5);
      OK                    : Boolean := True;
      A_Vector              : Types.Integer_Vectors.Vector;
      Minf                  : Types.Minfs.Minorframe_Type;

      Chains_Vector  : Types.Chain.Chains_Vectors.Vector;
      Chain          : Types.Chain.Chains.List;
      Chain_Score    : Chains_With_Score_Type;
      Chain_Link_all : Chain_Link_Type
        := (Subject          => 2,
            Processing_Speed => 1.0);
      Iterator       : Types.Minfs.Minorframes.Cursor;
   begin
      Simultaneous_Allowed (1, 2) := False;
      Simultaneous_Allowed (2, 1) := False;

      Chain_Link_all.Subject := 1;
      Chain.Append (Chain_Link_all);
      Chains_Vector.Append (Chain_Score);
      Chains_Vector (1).Datapoints := Double_Score;
      Chains_Vector (1).Chain := Chain;

      Chain.Clear;
      Chain_Link_all.Subject := 2;
      Chain.Append (Chain_Link_all);
      Chains_Vector.Append (Chain_Score);
      Chains_Vector (2).Datapoints := Identity_Score;
      Chains_Vector (2).Chain := Chain;

      Chain.Clear;
      Chain_Link_all.Subject := 3;
      Chain.Append (Chain_Link_all);
      Chains_Vector.Append (Chain_Score);
      Chains_Vector (3).Datapoints := Half_Score;
      Chains_Vector (3).Chain := Chain;

      --  Subject 1
      A_Meta.Number := 1;
      A_Meta.Length := 2;
      A_Vector.Append (2);
      A_Meta.CPU := 1;
      A_Meta.Level := 1;
      A_Meta.Simultaneous_Set.Insert (1);
      A_Meta.Combined_Subjects := A_Vector;
      A_Meta.Tick_Vector := A_Vector;
      Meta_and_Subjects.Append (A_Meta);


      A_Meta.Level := 2;
      A_Meta.Length := 1;
      for I in 2 .. Subject_Count loop
         if I = 5 then
            --  Subject 5
            A_Meta.Number := 5;
            A_Meta.Length := 2;
            A_Vector.Append (5);
            A_Meta.Level := 1;
            A_Meta.CPU := 2;
            A_Meta.Simultaneous_Set.Clear;
            A_Meta.Simultaneous_Set.Insert (2);
            A_Meta.Combined_Subjects := A_Vector;
            A_Meta.Tick_Vector := A_Vector;
            Meta_and_Subjects.Append (A_Meta);
         elsif I = 6 then
            A_Meta.Number := 6;
            Meta_and_Subjects.Append (A_Meta);
         else
            A_Meta.CPU := 1;
            Meta_and_Subjects.Append (A_Meta);
         end if;
      end loop;

      A_Meta.Level := 0;
      --  MetaSubject 1
      A_Meta.Number := Subject_Count + 1;
      A_Meta.Length := 1;
      A_Meta.CPU := 1;
      A_Vector.Append (4);
      A_Meta.Combined_Subjects := A_Vector;
      A_Meta.Tick_Vector := A_Vector;
      Meta_and_Subjects.Append (A_Meta);

      A_Meta.Level := 1;
      --  MetaSubject 2
      A_Meta.Number := Subject_Count + 2;
      A_Meta.Length := 2;
      A_Vector.Clear;
      A_Vector.Append (2);
      A_Meta.Combined_Subjects := A_Vector;
      A_Meta.Tick_Vector := A_Vector;
      Meta_and_Subjects.Append (A_Meta);

      --  MetaSubject 3
      A_Meta.Number := Subject_Count + 3;
      A_Meta.Length := 1;
      A_Meta.Simultaneous_Set.Clear;
      A_Vector.Clear;
      A_Vector.Append (3);
      A_Meta.Combined_Subjects := A_Vector;
      A_Vector.Clear;
      A_Vector.Append (2);
      A_Meta.Tick_Vector := A_Vector;
      Meta_and_Subjects.Append (A_Meta);

      Minf.Starttime := 0;
      Minf.Endtime := 1;
      Minf.Subject := 1;
      Minf.Simultaneous_Set.Insert (1);
      Plan (1).Append (Minf);

      Minf.Simultaneous_Set.Clear;
      Minf.Starttime := 4;
      Minf.Endtime := 5;
      Minf.Subject := Subject_Count + 2;
      Plan (1).Append (Minf);

      Minf.Starttime := 7;
      Minf.Endtime := 7;
      Minf.Subject := Subject_Count + 1;
      Plan (1).Append (Minf);

      Minf.Starttime := 9;
      Minf.Endtime := 9;
      Minf.Subject := Subject_Count + 3;
      Plan (1).Append (Minf);

      Minf.Starttime := 0;
      Minf.Endtime := 2;
      Minf.Subject := 6;
      Plan (2).Append (Minf);

      Minf.Starttime := 3;
      Minf.Endtime := 4;
      Minf.Subject := 5;
      Minf.Simultaneous_Set.Insert (2);
      Plan (2).Append (Minf);

      Ticks_Per_Subject (1) := 2;
      Ticks_Per_Subject (2) := 4;
      Ticks_Per_Subject (3) := 2;
      Ticks_Per_Subject (4) := 2;
      Ticks_Per_Subject (5) := 2;
      Ticks_Per_Subject (6) := 3;

      Ticks_Per_Subject_Old := Ticks_Per_Subject;

      Level_Sizes :=
        (1      => 10,
         2      => 20,
         others => 0
        );

      Grow_One_Level_Steady_Order
        (All_Chains           => Chains_Vector,
         Plan                 => Plan,
         Meta_and_Subjects    => Meta_and_Subjects,
         Ticks_Per_Subject    => Ticks_Per_Subject,
         Level_Sizes          => Level_Sizes,
         Majorframe           => 20,
         Simultaneous_Allowed => Simultaneous_Allowed);


      for I in 1 .. 6 loop
         if Ticks_Per_Subject_Old (I) > Ticks_Per_Subject (I) then
            -- it was shortened
            OK := False;
         end if;
      end loop;

      if Plan (1).First_Element.Endtime /= Plan (2).Last_Element.Starttime - 1
      then
         --1 wasnt expanded enough or expanded too far
         OK := False;
      end if;

      Iterator := Plan (1).First;
      if Types.Minfs.Minorframes.Has_Element
        (Types.Minfs.Minorframes.Next (Iterator) )then
         if Types.Minfs.Minorframes.Element (Iterator).Endtime /=
           Types.Minfs.Minorframes.Element
             (Types.Minfs.Minorframes.Next (Iterator)).Starttime - 1
         then
            OK := False;
         end if;
      end if;

      Assert (Condition => OK,
              Message   => "Steps.Grow:Grow_One_Level_Steady_Order failed");
--  begin read only
   end Test_Grow_One_Level_Steady_Order;
--  end read only

end Steps.Grow.Test_Data.Tests;
