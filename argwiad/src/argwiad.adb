------------------------------------------------------------------------------
--                                  Gwiad                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                            Olivier Ramonat                               --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Server;

with GNAT.Case_Util;

with Gwiad.Dynamic_Libraries.Manager;
with Gwiad.Web;

procedure Argwiad is

   use Ada;
   use Ada.Exceptions;
   use Ada.Command_Line;
   use GNAT;
   use Gwiad;

   type Cmd_Options is (No_Server, Q_Key_Pressed);

   procedure Usage;
   --  Displays usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Text_IO.Put ("usage: argwiad ");
      for Opt in Cmd_Options'Range loop
         declare
            Option_Name : String := Cmd_Options'Image (Opt);
         begin
            GNAT.Case_Util.To_Lower (Option_Name);
            if Opt /= Cmd_Options'Last then
               Text_IO.Put ("-" & Option_Name & "|");
            else
               Text_IO.Put_Line (Option_Name);
            end if;
         end;
      end loop;
   end Usage;

   Termination : AWS.Server.Termination := AWS.Server.Forever;

begin
   if Argument_Count /= 0 then
      for K in 1 .. Argument_Count loop
         if Argument (K) = "-h" or else Argument (K) = "-help"
           or else Argument (K) = "--help" then
            Usage;
            return;

         elsif Argument (K)'Length > 1 then
            case Cmd_Options'Value (Argument (K) (2 .. Argument (K)'Length)) is
            when No_Server | Q_Key_Pressed =>
                  Termination := AWS.Server.Termination'Value
                    (Argument (K) (2 .. Argument (K)'Length));
            end case;
         end if;
      end loop;
   end if;

   Start_Argwiad : declare
      Discover_Library : Dynamic_Libraries.Manager.Discover;
   begin
      Web.Start;

      Web.Wait (Termination);

      --  Then exit

      Web.Stop;
      Discover_Library.Stop;
   end Start_Argwiad;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Exception_Information (E));
end Argwiad;
