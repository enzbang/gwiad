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

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Unbounded;

with Gwiad.Plugins.Services;

package Gwiad.Plugins.Services.Register is

   use Services;

   type Service_Name is new String;

   procedure Register (Library_Path : in String);
   --  Registers a new dynamic library
   --  This must be called before registering the service to set the library
   --  path before the service registration

   procedure Register
     (Name        : in Service_Name;
      Description : in String;
      Builder     : in Service_Builder);
   --  Registers a new service
   --  This is called by a service library after that the library path has been
   --  set by the dynamic library manager (as the library has no knowlegde of
   --  its path)
   --  Raise service error if service with the same name is registered or
   --  if no dynamic library is registered

   procedure Unregister (Name : in Service_Name);
   --  Unregisters a service

   function Exists (Name : in Service_Name) return Boolean;
   --  Returns true if a service with the given name is registered

   function New_Service (Name : in Service_Name) return Service_Access;
   --  Returns a new service

   type Cursor is private;

   function First return Cursor;
   --  Returns a cursor to the first registered service

   function Find (Key : in Service_Name) return Cursor;
   --  Returns the cursor pointing to element with the given key

   procedure Next (Position : in out Cursor);
   --  Select the next element

   function Has_Element (Position : in Cursor) return Boolean;
   --  Returns true if cursor is not No_Element

   function Name (Position : in Cursor) return Service_Name;
   --  Returns the name of the service

   function Description (Position : in Cursor) return String;
   --  Returns the description of the service

   function Path (Position : in Cursor) return String;
   --  Returns the path of the shared library providing the service

   function Hash (Key : Service_Name) return Ada.Containers.Hash_Type;
   --  Hash function for service name

private
   use Ada.Strings.Unbounded;

   type Registered_Service is record
      Builder     : Service_Builder;
      Path        : Unbounded_String;
      Description : Unbounded_String;
   end record;

   package Register_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Service_Name, Registered_Service, Hash, "=", "=");

   type Cursor is new Register_Maps.Cursor;

end Gwiad.Plugins.Services.Register;
