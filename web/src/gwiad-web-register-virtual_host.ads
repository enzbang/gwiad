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

package Gwiad.Web.Register.Virtual_Host is

   procedure Register
     (Hostname : in String; Action : in AWS.Dispatchers.Handler'Class);
   --  Registers a new virtual host handler

   procedure Register
     (Host : in String; Redirected_Hostname : in String);
   --  Register a new redirection

   procedure Unregister (Hostname : in String);
   --  Unregister a virtual host

end Gwiad.Web.Register.Virtual_Host;