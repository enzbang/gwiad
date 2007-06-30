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

with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;

generic
   use Ada;

   type Plugin_Name is new String;
   type Registered_Plugin is new Plugin with private;

   with function Hash (Key : Plugin_Name) return Containers.Hash_Type;

package Gwiad.Plugins.Registry is

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Name : in Plugin_Name) return Registered_Plugin;
   --  Returns plugin with given name

   function Element (Position : in Cursor) return Registered_Plugin;
   --  Returns plugin at position

   function Exists (Name : in Plugin_Name) return Boolean;
   --  Returns true if a service with the given name is registered

   procedure Insert (Name : in Plugin_Name; Plugin : in Registered_Plugin);
   --  Insert a new plugin

   procedure Delete (Name : in Plugin_Name);
   --  Delete a plugin

   procedure Delete (Position : in out Cursor);
   --  Delete a plugin

   function First return Cursor;
   --  Returns a cursor to the first registered service

   function Find (Key : in Plugin_Name) return Cursor;
   --  Returns the cursor pointing to element with the given key

   procedure Next (Position : in out Cursor);
   --  Select the next element

   function Has_Element (Position : in Cursor) return Boolean;
   --  Returns true if cursor is not No_Element

   function Name (Position : in Cursor) return Plugin_Name;
   --  Returns the name of the service

   function Description (Position : in Cursor) return String;
   --  Returns the description of the service

   function Path (Position : in Cursor) return String;
   --  Returns the path of the shared library providing the service

private

   package Register_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Plugin_Name, Registered_Plugin, Hash, "=", "=");

   type Cursor is new Register_Maps.Cursor;

   No_Element : constant Cursor := Cursor (Register_Maps.No_Element);

end Gwiad.Plugins.Registry;
