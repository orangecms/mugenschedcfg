--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Auxiliary.XML.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Auxiliary.XML.Test_Data.Tests is


--  begin read only
   procedure Test_To_XML (Gnattest_T : in out Test);
   procedure Test_To_XML_9720ec (Gnattest_T : in out Test) renames Test_To_XML;
--  id:2.2/9720ece3b2fc3aac/To_XML/1/0/
   procedure Test_To_XML (Gnattest_T : in out Test) is
   --  auxiliary-xml.ads:25:4:To_XML
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Plan : Types.Data_Structures.All_Plans_Type
        := Types.Data_Structures.Create
          (Plan_Count => 2,
           CPU_Count  => 2);
      Subjects : Types.Metas.Meta_Subject_Vectors.Vector;

      Ref : constant String :=
        "<scheduling tickRate=""2000"">" & ASCII.LF
        & " <majorFrame>" & ASCII.LF
        & "  <cpu id=""0"">" & ASCII.LF
        & "   <minorFrame subject=""s1"" ticks=""13""/>" & ASCII.LF
        & "   <minorFrame subject=""s2"" ticks=""5""/>" & ASCII.LF
        & "  </cpu>" & ASCII.LF
        & "  <cpu id=""1"">" & ASCII.LF
        & "   <minorFrame subject=""s3"" ticks=""101""/>" & ASCII.LF
        & "  </cpu>" & ASCII.LF
        & " </majorFrame>" & ASCII.LF
        & " <majorFrame>" & ASCII.LF
        & "  <cpu id=""0"">" & ASCII.LF
        & "   <minorFrame subject=""s1"" ticks=""101""/>" & ASCII.LF
        & "  </cpu>" & ASCII.LF
        & "  <cpu id=""1"">" & ASCII.LF
        & "   <minorFrame subject=""s2"" ticks=""1101""/>" & ASCII.LF
        & "  </cpu>" & ASCII.LF
        & " </majorFrame>" & ASCII.LF
        & "</scheduling>";
   begin
      Subjects.Append
        (New_Item =>
           (Number => 1,
            Name   => To_Unbounded_String ("s1"),
            others => <>));
      Subjects.Append
        (New_Item =>
           (Number => 2,
            Name   => To_Unbounded_String ("s2"),
            others => <>));
      Subjects.Append
        (New_Item =>
           (Number => 2,
            Name   => To_Unbounded_String ("s3"),
            others => <>));

      Plan (1) (1).Append
        (New_Item =>
           (Starttime => 0,
            Endtime   => 12,
            Subject   => 1,
            others    => <>));
      Plan (1) (1).Append
        (New_Item =>
           (Starttime => 13,
            Endtime   => 17,
            Subject   => 2,
            others    => <>));
      Plan (1) (2).Append
        (New_Item =>
           (Starttime => 100,
            Endtime   => 200,
            Subject   => 3,
            others    => <>));
      Plan (2) (1).Append
        (New_Item =>
           (Starttime => 900,
            Endtime   => 1000,
            Subject   => 1,
            others    => <>));
      Plan (2) (2).Append
        (New_Item =>
           (Starttime => 0,
            Endtime   => 1100,
            Subject   => 2,
            others    => <>));

      Assert (Condition => Ref = To_XML
              (Plan      => Plan,
               Subjects  => Subjects,
               Tick_Rate => 2000),
              Message   => "Plan mismatch");

--  begin read only
   end Test_To_XML;
--  end read only


--  begin read only
   procedure Test_Add_Idle_Subjects (Gnattest_T : in out Test);
   procedure Test_Add_Idle_Subjects_54ecaf (Gnattest_T : in out Test) renames Test_Add_Idle_Subjects;
--  id:2.2/54ecafe41fc56bdb/Add_Idle_Subjects/1/0/
   procedure Test_Add_Idle_Subjects (Gnattest_T : in out Test) is
   --  auxiliary-xml.ads:32:4:Add_Idle_Subjects
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Types.Data_Structures;

      Level_Size : constant Types.Basics.Natural_Array
        := (1 => 10,
            2 => 160,
            3 => 1920,
            4 => 0);
      Per_Plan_Level_Sizes : Per_Plan_Level_Sizes_Type;

      Plan : All_Plans_Type := Create
        (Plan_Count => 1,
         CPU_Count  => 1);
   begin
      Per_Plan_Level_Sizes.Append (New_Item => Level_Size);

      Plan (1) (1).Append
        (New_Item =>
           (Starttime => 0,
            Endtime   => 12,
            Subject   => 1,
            others    => <>));
      Plan (1) (1).Append
        (New_Item =>
           (Starttime => 520,
            Endtime   => 533,
            Subject   => 2,
            others    => <>));
      Plan (1) (1).Append
        (New_Item =>
           (Starttime => 1023,
            Endtime   => 1023,
            Subject   => 3,
            others    => <>));

      declare
         use type Ada.Containers.Count_Type;

         Res : constant All_Plans_Type
           := Add_Idle_Subjects
             (Plan  => Plan,
              Sizes => Per_Plan_Level_Sizes);
         LS  : constant Types.Minfs.Minorframe_Type
           := Res (1) (1).Last_Element;

         Last_Endtime : Natural := 0;
      begin
         Assert (Condition => Res (1) (1).Length = 6,
                 Message   => "Frame count mismatch");
         Assert (Condition => LS.Starttime = 1024,
                 Message   => "Starttime mismatch");
         Assert (Condition => LS.Endtime = 1919,
                 Message   => "Endtime mismatch");
         Assert (Condition => LS.Subject = Positive'Last,
                 Message   => "Idle subject mismatch");

         --  Check that there are no gaps.

         for M of Res (1) (1) loop
            if Last_Endtime /= 0 then
               Assert (Condition => Last_Endtime + 1 = M.Starttime,
                       Message   => "Gap detected");
            end if;
            Last_Endtime := M.Endtime;
         end loop;
      end;
--  begin read only
   end Test_Add_Idle_Subjects;
--  end read only

end Auxiliary.XML.Test_Data.Tests;
