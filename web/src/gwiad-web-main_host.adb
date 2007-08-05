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

with AWS.Status;
with AWS.Response;
with AWS.MIME;
with AWS.Dispatchers.Callback;
with AWS.Services.Dispatchers.URI;

with Ada.Directories;

with Morzhol.OS;
with Gwiad.Config.Settings;

package body Gwiad.Web.Main_Host is

   use Ada;
   use AWS;
   use Morzhol.OS;

   Main_Host_Dispatcher : Services.Dispatchers.URI.Handler;

   function Default_Callback (Request : in Status.Data) return Response.Data;
   --  Default callback

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      use Ada.Directories;
      URI      : constant String := Status.URI (Request);
      Filename : constant String :=
                   Gwiad.Config.Settings.Web_Default_Directory
                     & Directory_Separator
                     & URI (URI'First + 1 .. URI'Last);
      Data     : Response.Data;
   begin

      if Exists (Filename)
        and then Kind (Filename) = Ordinary_File then
         Data := Response.File
           (Filename     => Filename,
            Content_Type => MIME.Content_Type (Filename));
      elsif Exists (Gwiad.Config.Settings.Web_Default_Directory
                    & Directory_Separator
                    & Gwiad.Config.Settings.Web_Default_Page) then
         Data := Response.Moved
           (Location => "/" &  Gwiad.Config.Settings.Web_Default_Page);
      else
         Data := Response.Build
           (Content_Type  => MIME.Text_HTML,
            Message_Body  => "<h1>Welcome to gwiad</h1>");
      end if;
      return Data;
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

--        Call register default callback to update Main_Host_Dispatcher

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Virtual_Hosts_Dispatcher,
         Main_Host_Dispatcher);

      --  Reload the dispatchers

      Gwiad.Web.Reload;
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
      --  Call register default callback to update Main_Host_Dispatcher

      Services.Dispatchers.URI.Unregister (Main_Host_Dispatcher, Web_Dir);

      --  Call register default callback to update Main_Host_Dispatcher

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Virtual_Hosts_Dispatcher,
         Main_Host_Dispatcher);

      --  Reload the dispatchers

      Gwiad.Web.Reload;
   end Unregister;

end Gwiad.Web.Main_Host;
