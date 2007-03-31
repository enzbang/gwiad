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

with Gwiad.Services.Register;
with Ada.Text_IO;

package body Hello_World_Service is

   use Gwiad.Services;

   function Builder return access Service'Class;
   --  Build a new test plugin

   -------------
   -- Builder --
   -------------

   function Builder return access Service'Class is
      Test : constant Hello_World_Service_Access := new Hello_World_Service;
   begin
      return Test;
   end Builder;

   -----------------
   -- Hello_World --
   -----------------

   overriding function Hello (S : Hello_World_Service) return String is
      pragma Unreferenced (S);
   begin
      return "hello_world_service says Hello World";
   end Hello;

begin
   Gwiad.Services.Register.Register
     (Name        => "hello_world_service",
      Description => "A simple hello world service for gwiad",
      Builder     => Builder'Access);

exception
   when others =>
      Ada.Text_IO.Put_Line ("hello_world_service registration failed");
end Hello_World_Service;
