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

package Gwiad.Plugins.Websites.Registry is

   use Ada;

   procedure Register (Library_Path : in String);
   --  Registers a new website
   --  This must be called before registering the service to set the library
   --  path before the service registration

   function Library_Path return String;
   --  Returns the current library path
   --  This me be called on library init

   procedure Register
     (Name         : in Website_Name;
      Description  : in String;
      Unregister   : in Unregister_CB;
      Library_Path : in String);
   --  Registers a new website
   --  This is called by a website after that the library path has been
   --  set by the dynamic library manager (as the library has no knowlegde of
   --  its path)
   --  Raise website error if website with the same name is registered or
   --  if no dynamic library is registered

   procedure Unregister (Name : in Website_Name);
   --  Unregisters a website

   type Registered_Website is new Plugin with record
      Unregister_CB : Gwiad.Plugins.Websites.Unregister_CB;
   end record;

   function Hash (Key : in Website_Name) return Containers.Hash_Type;

   package Map is new Gwiad.Plugins.Registry
     (Plugin_Name       => Website_Name,
      Registered_Plugin => Registered_Website,
      Hash              => Hash);

end Gwiad.Plugins.Websites.Registry;
