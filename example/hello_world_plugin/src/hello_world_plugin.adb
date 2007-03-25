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

with Ada.Text_IO;
with Gwiad.Plugins.Register;

package body Hello_World_Plugin is

   function Builder return access Plugin'Class is
      Test : constant Test_Plugin_Access := new Test_Plugin;
   begin
      Ada.Text_IO.Put_Line ("return test_plugin");
      return Test;
   end Builder;

   -----------------
   -- Hello_World --
   -----------------

   function Hello_World (Plug : Test_Plugin) return String is
      pragma Unreferenced (Plug);
   begin
      return "Hello World";
   end Hello_World;

begin

   Gwiad.Plugins.Register.Register
     (Name        => "hello_world_plugin",
      Description => "This is just a test for gwiad application",
      Builder     => Builder'Access);

end Hello_World_Plugin;
