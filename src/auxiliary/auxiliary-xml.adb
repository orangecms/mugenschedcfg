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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Types.Basics;
with Types.Minfs.Minorframes;

package body Auxiliary.XML
is

   use Ada.Strings.Unbounded;

   Idle_Subject_Number : constant := Positive'Last;
   Idle_Subject_Prefix : constant Unbounded_String
     := To_Unbounded_String ("mugenschedcfg_auto_idle_");

   -------------------------------------------------------------------------

   function Add_Idle_Subjects
     (Plan  : Types.Data_Structures.All_Plans_Type;
      Sizes : Types.Data_Structures.Per_Plan_Level_Sizes_Type)
      return Types.Data_Structures.All_Plans_Type
   is

      use Types.Data_Structures;

      --  Return plan size.
      function Get_Size (NA : Types.Basics.Natural_Array) return Positive;

      ----------------------------------------------------------------------

      function Get_Size (NA : Types.Basics.Natural_Array) return Positive
      is
         Res : Positive;
      begin
         for S in NA'Range loop
            exit when NA (S) = 0;
            Res := NA (S);
         end loop;

         return Res;
      end Get_Size;

      Result : All_Plans_Type := Plan;
   begin
      for Plan_Cursor in Plan.Iterate loop
         declare
            Plan_Idx  : constant Positive
              := All_Plans_Package.To_Index (Position => Plan_Cursor);
            Plan_Size : constant Positive
              := Get_Size (NA => Sizes (Plan_Idx));
            Level     : constant Level_Type := Plan (Plan_Cursor);
         begin
            for CPU_Idx in 1 .. Level'Last loop
               declare
                  use type Ada.Containers.Count_Type;
                  use type Types.Minfs.Minorframes.Cursor;

                  Mins : constant Types.Minfs.Minorframes.List
                    := Level (CPU_Idx);
               begin
                  if Mins.Length = 0 then

                     --  CPU is completely idle.

                     Result (Plan_Idx) (CPU_Idx).Append
                       (New_Item =>
                          (Starttime => 0,
                           Endtime   => Plan_Size - 1,
                           Subject   => Idle_Subject_Number,
                           others    => <>));
                  else

                     --  Fill gaps with idle subjects.

                     Result (Plan_Idx) (CPU_Idx).Clear;

                     declare
                        use type Types.Minfs.Minorframe_Type;

                        Last_End : Integer := -1;
                     begin
                        for Frame of Mins loop
                           if Last_End < Frame.Starttime - 1 then
                              Result (Plan_Idx) (CPU_Idx).Append
                                (New_Item =>
                                   (Starttime => Last_End + 1,
                                    Endtime   => Frame.Starttime - 1,
                                    Subject   => Idle_Subject_Number,
                                    others    => <>));
                           end if;

                           Result (Plan_Idx) (CPU_Idx).Append
                             (New_Item => Frame);

                           if Frame = Mins.Last_Element
                             and then Frame.Endtime /= Plan_Size - 1
                           then
                              Result (Plan_Idx) (CPU_Idx).Append
                                (New_Item =>
                                   (Starttime => Frame.Endtime + 1,
                                    Endtime   => Plan_Size - 1,
                                    Subject   => Idle_Subject_Number,
                                    others    => <>));
                           end if;

                           Last_End := Frame.Endtime;
                        end loop;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Result;
   end Add_Idle_Subjects;

   -------------------------------------------------------------------------

   function To_XML
     (Plan      : Types.Data_Structures.All_Plans_Type;
      Subjects  : Types.Metas.Meta_Subject_Vectors.Vector;
      Tick_Rate : Positive)
      return String
   is
      Res : Unbounded_String;
   begin
      Res := Res & "<scheduling tickRate="""
        & Ada.Strings.Fixed.Trim
        (Source => Tick_Rate'Img,
         Side   => Ada.Strings.Left)
        & """>" & ASCII.LF;

      for P of Plan loop
         Res := Res & " <majorFrame>" & ASCII.LF;
         declare
            use type Ada.Containers.Count_Type;

            CPU_ID : Natural := 0;
         begin
            for L of P loop
               if L.Length > 0 then
                  declare
                     CPU_Str : constant String := Ada.Strings.Fixed.Trim
                       (Source => CPU_ID'Img,
                        Side   => Ada.Strings.Left);
                  begin
                     Res := Res & "  <cpu id=""" & CPU_Str & """>" & ASCII.LF;
                     for M of L loop
                        declare
                           Subj : constant Unbounded_String
                             := (if M.Subject /= Idle_Subject_Number then
                                    Subjects (M.Subject).Name
                                 else
                                    Idle_Subject_Prefix & CPU_Str);
                           Ticks : constant String := Ada.Strings.Fixed.Trim
                             (Source => Positive'Image
                                (M.Endtime - M.Starttime + 1),
                              Side   => Ada.Strings.Left);
                        begin
                           Res := Res & "   <minorFrame subject="""
                             & Subj & """";
                           Res := Res & " ticks=""" & Ticks & """/>"
                             & ASCII.LF;
                        end;
                     end loop;
                     Res := Res & "  </cpu>" & ASCII.LF;

                     CPU_ID := CPU_ID + 1;
                  end;
               end if;
            end loop;
         end;
         Res := Res & " </majorFrame>" & ASCII.LF;
      end loop;
      Res := Res & "</scheduling>";

      return To_String (Res);
   end To_XML;

end Auxiliary.XML;
