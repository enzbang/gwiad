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

with Gwiad.Plugins.Register;
with Ada.Text_IO;

package body Hello_World_Plugin is

   -------------
   -- Builder --
   -------------

   function Builder return access Plugin'Class is
      Test : constant Hello_World_Plugin_Access := new Hello_World_Plugin;
   begin
      return Test;
   end Builder;

   -----------------
   -- Hello_World --
   -----------------

   overriding function Hello (P : Hello_World_Plugin) return String is
      pragma Unreferenced (P);
   begin
      return "hello_world_plugin says Hello World";
   end Hello;

begin
   Gwiad.Plugins.Register.Register
     (Name        => "hello_world_plugin",
      Description => "A simple hello world for gwiad",
      Builder     => Builder'Access);

exception
   when others =>
      Ada.Text_IO.Put_Line ("hello_world_plugin registration failed");
end Hello_World_Plugin;
