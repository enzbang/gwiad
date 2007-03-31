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

with Gwiad.Services;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package Gwiad.Services.Register is

   use Services;
   use Ada.Strings.Unbounded;

   procedure Register (Library_Path : in String);
   --  Registers a new dynamic library
   --  This must be called before registering the plugin to set the plugin path
   --  before the plugin registration

   procedure Register
     (Name           : in String;
      Description    : in String;
      Builder        : in Plugin_Builder);
   --  Registers a new service
   --  This is called by a plugin after that the plugin path has been set by
   --  the dynamic library manager (as the plugin has no knowlegde of its path)
   --  Raise plugin error if plugin with the same name is registered or
   --  if no dynamic library is registered

   procedure Unregister (Name : in String);
   --  Unregisters a service

   function Exists (Name : in String) return Boolean;
   --  Returns true if a service with the given name is registered

   function Get (Name : in String) return Plugin_Access;
   --  Returns the plugin

   type Cursor is private;

   function First return Cursor;
   --  Returns a cursor to the first registered plugin

   function Find (Key : in String) return Cursor;
   --  Returns the cursor pointing to element with the given key

   procedure Next (Position : in out Cursor);
   --  Select the next element

   function Has_Element (Position : Cursor) return Boolean;
   --  Returns true if cursor is not No_Element

   function Name (Position : Cursor) return String;
   --  Returns the name of the plugin

   function Description (Position : Cursor) return String;
   --  Returns the description of the plugin

   function Path (Position : Cursor) return String;
   --  Returns the path of the shared library providing the plugin

private

   type Registered_Plugin is record
      Builder     : Plugin_Builder;
      Path        : Unbounded_String;
      Description : Unbounded_String;
   end record;

   package Register_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Registered_Plugin, Ada.Strings.Hash, "=", "=");

   type Cursor is new Register_Maps.Cursor;

end Gwiad.Services.Register;
