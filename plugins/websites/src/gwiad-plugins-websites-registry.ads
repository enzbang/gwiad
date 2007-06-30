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

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;

package Gwiad.Plugins.Websites.Registry is

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

   type Cursor is private;

   function First return Cursor;
   --  Returns a cursor to the first registered service

   function Find (Key : in Website_Name) return Cursor;
   --  Returns the cursor pointing to element with the given key

   procedure Next (Position : in out Cursor);
   --  Select the next element

   function Has_Element (Position : in Cursor) return Boolean;
   --  Returns true if cursor is not No_Element

   function Name (Position : in Cursor) return Website_Name;
   --  Returns the name of the service

   function Description (Position : in Cursor) return String;
   --  Returns the description of the service

   function Path (Position : in Cursor) return String;
   --  Returns the path of the shared library providing the service

private

   use Ada;
   use Ada.Strings.Unbounded;

   type Registered_Website is record
      Path          : Unbounded_String;
      Description   : Unbounded_String;
      Unregister_CB : Gwiad.Plugins.Websites.Unregister_CB;
   end record;

   function Hash (Key : in Website_Name) return Containers.Hash_Type;

   package Register_Maps is new Containers.Indefinite_Hashed_Maps
     (Website_Name, Registered_Website, Hash, "=", "=");
   --  Store each website information : website name, path of the library
   --  providing the website, website description and unregister callback
   --  Unregister callback will be called when disabling website or unloading
   --  the library providing it.

   type Cursor is new Register_Maps.Cursor;

end Gwiad.Plugins.Websites.Registry;
