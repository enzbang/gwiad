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

with Gwiad.Plugins.Registry;

package Gwiad.Plugins.Services.Registry is

   use Ada;
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
   --  This is called by a service library after that the library path
   --  has been set by the dynamic library manager (as the library has
   --  no knowlegde of its path)
   --  Raise service error if service with the same name is registered or
   --  if no dynamic library is registered

   procedure Unregister (Name : in Service_Name);
   --  Unregisters a service

   function New_Service (Name : in Service_Name) return Service_Access;
   --  Returns a new service

   type Registered_Service is new Plugin with record
      Builder : Service_Builder;
   end record;

   function Hash (Key : in Service_Name) return Containers.Hash_Type;

   package Map is new Gwiad.Plugins.Registry
     (Plugin_Name       => Service_Name,
      Registered_Plugin => Registered_Service,
      Hash              => Hash);

   function Exists (Name : in Service_Name) return Boolean
     renames Map.Exists;

end Gwiad.Plugins.Services.Registry;
