------------------------------------------------------------------------------
--                                 Gwiad                                    --
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

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;

--  with Gwiad.Web;
with Gwiad.Websites.Register;
with Gwiad.Dynamic_Libraries.Manager;

procedure Check_Mem is

   use Ada;
   use Ada.Text_IO;

   use Gwiad;

   use Gwiad.Dynamic_Libraries.Manager;

   Iteration : Positive;

   Lib_Hello_World_Website : constant String :=
     "../lib/websites/libhello_world_website.so";

   procedure Load;

   procedure Unload;

   procedure Unload_Websites;

   ----------
   -- Load --
   ----------

   procedure Load is
   begin
      Manager.Load (Lib_Hello_World_Website);
      Put_Line ("loaded");
   end Load;

   ------------
   -- Unload --
   ------------

   procedure Unload is
   begin
      Manager.Unload (Lib_Hello_World_Website);
      Put_Line ("unloaded");

      --  Rename the disabled library

      Directories.Rename (Lib_Hello_World_Website & ".disabled",
                          Lib_Hello_World_Website);
   end Unload;

   ---------------------
   -- Unload_Websites --
   ---------------------

   procedure Unload_Websites is
      use Gwiad.Websites.Register;

      Position     : Cursor := First;
   begin
      while Has_Element (Position) loop
         declare
            Last_Position : constant Cursor := Position;
         begin
            Next (Position);
            Unregister (Name (Last_Position));
         end;
      end loop;
      Put_Line ("websites unloaded");

   end Unload_Websites;

begin
   Put_Line ("Start main, wait for server to start...");

   Iteration := Integer'Value (Command_Line.Argument (1));

   --  This is the main loop. Be sure to run everything inside this
   --  loop. Check_Mem is checked between 2 runs with a different number of
   --  iterations.

   for K in 1 ..  Iteration loop
      Load;
      Unload_Websites;
      Unload;
   end loop;

   delay 2.0;

   Command_Line.Set_Exit_Status (Command_Line.Success);
exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Check_Mem;
