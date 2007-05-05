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

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with Ada.Strings.Unbounded;

with Gwiad.Services;

package Gwiad.Services.Register is

   use Services;

   type Service_Id is new String (1 .. 11);

   Null_Service_Id : Service_Id := (others => ' ');

   procedure Register (Library_Path : in String);
   --  Registers a new dynamic library
   --  This must be called before registering the service to set the library
   --  path before the service registration

   procedure Register
     (Name        : in String;
      Description : in String;
      Builder     : in Service_Builder);
   --  Registers a new service
   --  This is called by a service library after that the library path has been
   --  set by the dynamic library manager (as the library has no knowlegde of
   --  its path)
   --  Raise service error if service with the same name is registered or
   --  if no dynamic library is registered

   procedure Unregister (Name : in String);
   --  Unregisters a service

   function Exists (Name : in String) return Boolean;
   --  Returns true if a service with the given name is registered

   function Get
     (Name : in String;
      Id   : in Service_Id := Null_Service_Id) return Service_Access;
   --  Returns the service

   function Set (Name : in String; Item : in Service_Access) return Service_Id;
   --  Adds the service to cache

   type Cursor is private;

   function First return Cursor;
   --  Returns a cursor to the first registered service

   function Find (Key : in String) return Cursor;
   --  Returns the cursor pointing to element with the given key

   procedure Next (Position : in out Cursor);
   --  Select the next element

   function Has_Element (Position : in Cursor) return Boolean;
   --  Returns true if cursor is not No_Element

   function Name (Position : in Cursor) return String;
   --  Returns the name of the service

   function Description (Position : in Cursor) return String;
   --  Returns the description of the service

   function Path (Position : in Cursor) return String;
   --  Returns the path of the shared library providing the service

private
   use Ada.Strings.Unbounded;

   type Registered_Service is record
      Builder     : Service_Builder;
      Path        : Unbounded_String;
      Description : Unbounded_String;
   end record;

   package Register_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Registered_Service, Ada.Strings.Hash, "=", "=");

   type Cursor is new Register_Maps.Cursor;

end Gwiad.Services.Register;
