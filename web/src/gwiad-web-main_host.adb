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
with AWS.Status;
with AWS.Response;
with AWS.MIME;
with AWS.Dispatchers.Callback;
with AWS.Services.Dispatchers.URI;

with Gwiad.Config.Settings;

package body Gwiad.Web.Main_Host is

   use AWS;

   Main_Host_Dispatcher : Services.Dispatchers.URI.Handler;

   function Default_Callback (Request : in Status.Data) return Response.Data;
   --  Default callback

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.File
        (Filename     => Gwiad.Config.Settings.Web_Default_Page,
         Content_Type => MIME.Text_HTML);
   end Default_Callback;

   --------------
   -- Register --
   --------------

   procedure Register
     (Web_Dir : in String; Action : in AWS.Dispatchers.Handler'Class) is
   begin
      Services.Dispatchers.URI.Register
        (Dispatcher => Main_Host_Dispatcher,
         URI        => Web_Dir,
         Action     => Action,
         Prefix     => True);

      --  Call register default callback to update Main_Host_Dispatcher

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Virtual_Hosts_Dispatcher,
         Main_Host_Dispatcher);

   end Register;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      --  Adds default callback to main host dispatcher

      Services.Dispatchers.URI.Register_Default_Callback
        (Main_Host_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Virtual_Hosts_Dispatcher,
         Main_Host_Dispatcher);
   end Start;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Web_Dir : in String) is
   begin
      Services.Dispatchers.URI.Unregister (Main_Host_Dispatcher, Web_Dir);

      --  Call register default callback to update Main_Host_Dispatcher

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Virtual_Hosts_Dispatcher,
         Main_Host_Dispatcher);
   end Unregister;

end Gwiad.Web.Main_Host;
