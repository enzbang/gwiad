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

with AWS.Services.Dispatchers.Virtual_Host;
with AWS.Messages;
with AWS.Templates;
with AWS.Response;
with AWS.Dispatchers.Callback;
with AWS.Services.ECWF.Registry;
with AWS.Status;
with AWS.Server.Log;
with AWS.Config;
with Ada.Text_IO;
with AWS.MIME;

package body Gwiad.Web is

   use AWS;

   Main_Dispatcher : Services.Dispatchers.Virtual_Host.Handler;
   Configuration   : Config.Object;
   HTTP            : Server.HTTP;

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback
     (Request : in Status.Data) return Response.Data
   is
      use type Messages.Status_Code;
      URI          : constant String := Status.URI (Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin
      Web_Page := Services.ECWF.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         --           return Services.ECWF.Registry.Build
         --             ("error", Request, Translations);
         return Response.Build (MIME.Text_HTML, "<h1>Error</h1>");

      else
         return Web_Page;
      end if;
   end Default_Callback;

   --------------
   -- Register --
   --------------

   procedure Register
     (Host : in String; Redirected_Hostname : in String)
   is
   begin
      Services.Dispatchers.Virtual_Host.Register
        (Dispatcher       => Main_Dispatcher,
         Virtual_Hostname => Host,
         Hostname         => Redirected_Hostname);
      Server.Set (HTTP, Main_Dispatcher);
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
        (Dispatcher       => Main_Dispatcher,
         Virtual_Hostname => Hostname,
         Action           => Action);
      Server.Set (HTTP, Main_Dispatcher);
   end Register;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      --  Log control

      Server.Log.Start (HTTP, Auto_Flush => True);
      Server.Log.Start_Error (HTTP);

      Services.Dispatchers.Virtual_Host.Register_Default_Callback
        (Main_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));

      --  Server configuration

      Server.Start (HTTP, Main_Dispatcher, Configuration);
   end Start;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Server.Wait (Server.Forever);
   end Wait;


end Gwiad.Web;
