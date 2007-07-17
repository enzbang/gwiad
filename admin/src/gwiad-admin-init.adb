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

with AWS.Services.Dispatchers.URI;
with AWS.Services.ECWF.Registry;
with AWS.Services.ECWF.Context;
with AWS.Dispatchers.Callback;
with AWS.Status;
with AWS.Response;
with AWS.Messages;
with AWS.Templates;
with AWS.Digest;
with AWS.MIME;

with Gwiad.Config.Settings;
with Gwiad.Web.Main_Host;
with Gwiad.Admin.Services;
with Gwiad.Admin.Websites;

package body Gwiad.Admin.Init is

   use AWS;
   use Gwiad;

   Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;

   function Default_Callback (Request : in Status.Data) return Response.Data;
   --  Registers default callback

   procedure Menu
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Displays Gwiad admin menu

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
               --  Redirect to gwiad admin page
               Web_Page := Response.URL (Location => Admin_URL);
            end if;
         else
            --  Nonce is stale

            Web_Page := AWS.Response.Authenticate
              (Realm => "Gwiad restricted usage",
               Mode  => Response.Digest,
               Stale => True);
         end if;
      else

         --  Unauthorized

         Web_Page := Response.Authenticate
           (Realm => "Gwiad restricted usage", Mode => Response.Digest);
      end if;

      return Web_Page;

   end Default_Callback;

   procedure Menu
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);

   begin
      Templates.Insert
        (Translations,
         Templates.Assoc ("SERVICES_ADMIN_URL_LIST",
           Admin_URL & Admin.Services.Services_URL & "list"));

      Templates.Insert
        (Translations,
         Templates.Assoc ("WEBSITES_ADMIN_URL_LIST",
           Admin_URL & Admin.Websites.Websites_URL & "list"));
   end Menu;

begin --  Gwiad.Admin : Register pages

   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));
   --  This default callback will handle all ECWF callbacks

   --  Register ECWF Pages

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL,
      Template     => "templates/admin.thtml",
      Data_CB      => Menu'Access,
      Content_Type => MIME.Text_HTML);


   --  Register ECWF pages (Services Admin)

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Services.Services_URL & "list",
      Template     => "templates/services_admin/list.thtml",
      Data_CB      => Admin.Services.List_Services'Access,
      Content_Type => MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Services.Services_URL & "stop",
      Template     => "templates/services_admin/stop.thtml",
      Data_CB      => Admin.Services.Stop_Service'Access,
      Content_Type => MIME.Text_HTML);

   --  Register ECWF pages (Websites Admin)

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Websites.Websites_URL & "list",
      Template     => "templates/websites_admin/list.thtml",
      Data_CB      => Admin.Websites.List_Websites'Access,
      Content_Type => MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Websites.Websites_URL & "stop",
      Template     => "templates/websites_admin/stop.thtml",
      Data_CB      => Admin.Websites.Stop_Website'Access,
      Content_Type => MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Websites.Websites_URL & "unload",
      Template     => "templates/websites_admin/unload.thtml",
      Data_CB      => Admin.Websites.Unload_Websites'Access,
      Content_Type => MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (Key          => Admin_URL & Admin.Websites.Websites_URL & "find_vhd",
      Template     => "templates/websites_admin/list.thtml",
      Data_CB      => Admin.Websites.Virtual_Host_Directories'Access,
      Content_Type => MIME.Text_HTML);


   Gwiad.Web.Main_Host.Register (Web_Dir => Admin_URL,
                                 Action  => Main_Dispatcher);

   --  Discover virtual host directories

   Admin.Websites.Discover_Virtual_Host_Directories;

end Gwiad.Admin.Init;
