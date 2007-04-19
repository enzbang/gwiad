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

private with AWS.Services.Dispatchers.Virtual_Host;

package Gwiad.Web is

   procedure Start;

   procedure Wait;

   procedure Register
     (Host : in String; Redirected_Hostname : in String);
   --  Registers a new virtual host redirection

   procedure Register
     (Hostname : in String; Action : in AWS.Dispatchers.Handler'Class);
   --  Registers a new virtual host handler

   procedure Register_Web_Directory
     (Web_Dir : in String; Action : in AWS.Dispatchers.Handler'Class);
   --  Registers a new web directory

   procedure Unregister_Web_Directory (Web_Dir : in String);
   --  Unregister a web directory

private

   Virtual_Hosts_Dispatcher : AWS.Services.Dispatchers.Virtual_Host.Handler;

end Gwiad.Web;