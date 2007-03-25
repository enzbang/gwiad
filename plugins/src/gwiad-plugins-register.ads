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

with Gwiad.Plugins;

package Gwiad.Plugins.Register is

   procedure Register (Library_Path : in String);
   --  Register a new dynamic library
   --  This must be called before registering the plugin to set the plugin name

   procedure Register
     (Name           : in String;
      Description    : in String;
      Builder        : in Plugin_Builder);
   --  Register a new service
   --  Raise plugin error if plugin with the same name is registered

   procedure Unregister (Name : in String);
   --  Register a new service
   --  Raise plugin error if plugin with the same name is registered

   function Exists (Name : in String) return Boolean;
   --  Returns true if a service named "name" is registered

   function Image (Name : in String) return String;
   --  Returns description and library path of the plugin

   function Get (Name : in String) return Plugin_Access;
   --  Returns the plugin

end Gwiad.Plugins.Register;
