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

with AWS.Dispatchers;
with Ada.Strings.Unbounded;

package Gwiad.Web.Virtual_Host is

   use Ada.Strings.Unbounded;

   type Virtual_Host_Directory is record
      Document_Root : Unbounded_String;
      Default_Page  : Unbounded_String;
      Secure        : Boolean;
   end record;

   procedure Register
     (Hostname : in String; Action : in AWS.Dispatchers.Handler'Class);
   --  Registers a new virtual host handler

   procedure Register
     (Host : in String; Redirected_Hostname : in String);
   --  Registers a new redirection

   procedure Register
     (Hostname : in String;
      VH_Dir   : in Virtual_Host_Directory);
   --  Registers a new virtual host to serve a document root

   procedure Unregister (Hostname : in String);
   --  Unregisters a virtual host

   function Get_Hostname (Hostname : in String) return String;
   --  Get hostname

end Gwiad.Web.Virtual_Host;
