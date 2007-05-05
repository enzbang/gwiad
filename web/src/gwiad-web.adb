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

with AWS.Server.Log;
with AWS.Config.Set;

with Gwiad.Web.Main_Host;

package body Gwiad.Web is

   use AWS;

   Configuration : Config.Object;
   HTTP          : Server.HTTP;

   task Reload_Dispatcher;

   ------------
   -- Reload --
   ------------

   protected body Reload is

      ------------
      --  Done  --
      ------------

      procedure Done is
      begin
         Reload_Required := False;
      end Done;

      ---------------
      -- Do_Reload --
      ---------------

      function Is_Required return Boolean is
      begin
         return Reload_Required;
      end Is_Required;

      --------------
      -- Required --
      --------------

      procedure Require is
      begin
         Reload_Required := True;
      end Require;

   end Reload;

   -----------------------
   -- Reload_Dispatcher --
   -----------------------

   task body Reload_Dispatcher is
   begin
      loop
         delay 1.0;
         if Reload.Is_Required then
            Server.Set (HTTP, Virtual_Hosts_Dispatcher);
            Reload.Done;
         end if;
      end loop;
   end Reload_Dispatcher;

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

      Config.Set.Session (Configuration, True);
      Config.Set.Upload_Directory (Configuration, Upload_Directory);
      Config.Set.Admin_URI (Configuration, Admin_URI);

      Server.Start (HTTP, Virtual_Hosts_Dispatcher, Configuration);
   end Start;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Server.Wait (Server.Forever);
   end Wait;

end Gwiad.Web;
