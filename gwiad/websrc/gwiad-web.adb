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

with Ada.Text_IO;

with AWS.Server.Log;
with AWS.Config;

with Gwiad.Web.Main_Host;

package body Gwiad.Web is

   use AWS;

   Configuration : Config.Object;
   HTTP          : Server.HTTP;

   --------------
   -- Register --
   --------------

   procedure Register
     (Host : in String; Redirected_Hostname : in String)
   is
   begin
      Services.Dispatchers.Virtual_Host.Register
        (Dispatcher       => Virtual_Hosts_Dispatcher,
         Virtual_Hostname => Host,
         Hostname         => Redirected_Hostname);
      Server.Set (HTTP, Virtual_Hosts_Dispatcher);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Hostname : in String; Action : in AWS.Dispatchers.Handler'Class)
   is
   begin
      Ada.Text_IO.Put_Line ("Virtual Host " & Hostname & " registered. ");
      Services.Dispatchers.Virtual_Host.Register
        (Dispatcher       => Virtual_Hosts_Dispatcher,
         Virtual_Hostname => Hostname,
         Action           => Action);
      Server.Set (HTTP, Virtual_Hosts_Dispatcher);
   end Register;

   ----------------------------
   -- Register_Web_Directory --
   ----------------------------

   procedure Register_Web_Directory
     (Web_Dir : in String; Action : in AWS.Dispatchers.Handler'Class)
   is
   begin
      Ada.Text_IO.Put_Line ("Web Directory " & Web_Dir & " registered. ");
      Main_Host.Register (Web_Dir, Action);
      Server.Set (HTTP, Virtual_Hosts_Dispatcher);
   end Register_Web_Directory;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      --  Log control

      Server.Log.Start (HTTP, Auto_Flush => True);
      Server.Log.Start_Error (HTTP);

      --  Main host start

      Gwiad.Web.Main_Host.Start;

      --  Server configuration

      Server.Start (HTTP, Virtual_Hosts_Dispatcher, Configuration);
   end Start;

   ------------------------------
   -- Unregister_Web_Directory --
   ------------------------------

   procedure Unregister_Web_Directory (Web_Dir : in String) is
   begin
      Main_Host.Unregister (Web_Dir);
      Server.Set (HTTP, Virtual_Hosts_Dispatcher);
   end Unregister_Web_Directory;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Server.Wait (Server.Forever);
   end Wait;

end Gwiad.Web;
