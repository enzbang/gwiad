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

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Digest;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.Response;
with AWS.MIME;
with AWS.Services.Dispatchers.URI;
with AWS.Services.ECWF.Registry;
with AWS.Services.ECWF.Context;
with AWS.Templates;
with AWS.Parameters;

with Gwiad.Web.Main_Host;
with Gwiad.Config.Settings;
with Gwiad.Plugins.Services.Registry;
with Gwiad.Dynamic_Libraries.Manager;

package body Services_Admin is

   use Gwiad;
   use Gwiad.Plugins.Services;

   use AWS;
   use AWS.Templates;

   Services_Admin_URL : constant String := "/admin/services/";

   Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;

   function Default_Callback (Request : in Status.Data) return Response.Data;
   --  Registers default callback

   procedure List_Services
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Lists all services

   procedure Stop_Service
     (Request      : in Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Stop a gwiad service

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      use type Messages.Status_Code;
      use type AWS.Status.Authorization_Type;
      URI          : constant String := Status.URI (Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;

      Username    : constant String := AWS.Status.Authorization_Name (Request);
      Client_Mode : constant AWS.Status.Authorization_Type
        := AWS.Status.Authorization_Mode (Request);

   begin

      if Client_Mode = Status.Digest
        and then Username = Config.Settings.Auth_Username
        and then Status.Check_Digest (Request, Config.Settings.Auth_Password)
      then
         if Digest.Check_Nonce
           (Digest.Nonce (AWS.Status.Authorization_Nonce (Request)))
         then
            Web_Page := AWS.Services.ECWF.Registry.Build
              (URI, Request, Translations,
               Cache_Control => Messages.Prevent_Cache);

            if Response.Status_Code (Web_Page) = Messages.S404 then
               --  Page not found
               return Response.Build
                 (Content_Type  => MIME.Text_HTML,
                  Message_Body  => "<p>Service admin error</p>");
            else
               return Web_Page;
            end if;
         else
            --  Nonce is stale

            return AWS.Response.Authenticate
              ("Gwiad restricted usage", Response.Digest, Stale => True);
         end if;
      end if;

      --  Unauthorized

      return Response.Authenticate ("Gwiad restricted usage", Response.Digest);

   end Default_Callback;

   -------------------
   -- List_Services --
   -------------------

   procedure List_Services
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
      use Gwiad.Plugins.Services.Registry.Map;

      Position : Cursor := First;

      Tag_Name : Templates.Tag;
      Tag_Description : Templates.Tag;

   begin
      while Has_Element (Position) loop
         Tag_Name        := Tag_Name & String (Name (Position));
         Tag_Description := Tag_Description & Description (Position);
         Next (Position);
      end loop;

      Templates.Insert (Translations, Templates.Assoc ("NAME", Tag_Name));
      Templates.Insert (Translations,
                        Templates.Assoc ("DESCRIPTION", Tag_Description));
      Templates.Insert
        (Translations,
         Templates.Assoc ("SERVICES_ADMIN_URL", Services_Admin_URL));

   end List_Services;

   ------------------
   -- Stop_Service --
   ------------------

   procedure Stop_Service
     (Request      : in Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);

      use Gwiad.Plugins.Services.Registry;
      use Dynamic_Libraries.Manager;
      use Ada.Strings.Unbounded;

      P            : constant Parameters.List := Status.Parameters (Request);
      Name         : constant String          := Parameters.Get (P, "service");
      Library_Path : Unbounded_String         := Null_Unbounded_String;

   begin

      declare
         Position : constant Map.Cursor := Map.Find (Service_Name (Name));
      begin
         if Map.Has_Element (Position) then
            Library_Path := To_Unbounded_String (Map.Path (Position));
         end if;
      end;

      if Library_Path /= "" then
         Unregister (Service_Name (Name));
         Manager.Unload (To_String (Library_Path));
      end if;

      Templates.Insert
        (Translations,
         Templates.Assoc ("NAME", Name));

      Templates.Insert
        (Translations,
         Templates.Assoc ("SERVICES_ADMIN_URL", Services_Admin_URL));

   end Stop_Service;

begin

   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));
   --  This default callback will handle all ECWF callbacks

   --  Register ECWF pages

   AWS.Services.ECWF.Registry.Register
     (Services_Admin_URL & "list",
      "templates/services_admin/list.thtml",
      List_Services'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Services_Admin_URL & "stop",
      "templates/services_admin/stop.thtml",
      Stop_Service'Access,
      MIME.Text_HTML);

   Gwiad.Web.Main_Host.Register (Web_Dir => Services_Admin_URL,
                                 Action  => Main_Dispatcher);

end Services_Admin;
