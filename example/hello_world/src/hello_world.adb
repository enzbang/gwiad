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
with AWS.Status;
with AWS.Response;
with AWS.Dispatchers.Callback;
with AWS.MIME;

with Gwiad.Plugins.Services.Cache;
with Gwiad.Plugins.Services.Registry;
with Gwiad.Plugins.Websites;
with Gwiad.Plugins.Websites.Registry;

with Gwiad.Web.Main_Host;

with Hello_World_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Hello_World is

   use AWS;

   use Gwiad;
   use Gwiad.Plugins.Websites;

   use Hello_World_Interface;

   Hello_Web_Dir : constant String := "/hello/";

   Main_Dispatcher : AWS.Dispatchers.Handler_Class_Access
     := new AWS.Services.Dispatchers.URI.Handler;

   Path : String_Access :=
            new String'(Gwiad.Plugins.Get_Last_Library_Path);
   --  Get Library Path (used for register process)

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function Hello_World (Request : in Status.Data) return Response.Data;
   --  Hello world

   procedure Unregister (Name : in Website_Name);
   --  Unregister website

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Build (MIME.Text_HTML, "Hello World 404 Error");
   end Default_Callback;

   -----------------
   -- Hello_World --
   -----------------

   function Hello_World (Request : in Status.Data) return Response.Data is
      pragma Unreferenced (Request);

      Service_Name : constant Plugins.Services.Registry.Service_Name :=
                       "hello_world_service";
   begin
      if not Plugins.Services.Registry.Map.Exists (Name => Service_Name) then
         return Response.Build (MIME.Text_HTML,
                                "<p>Service down</p>");
      end if;

      Build_Response :
      declare
         Hello_World_Service_Access : HW_Service_Access
           := HW_Service_Access
             (Gwiad.Plugins.Services.Cache.Get (Service_Name));
         Hello_World_Service        : HW_Service'Class
           := Hello_World_Service_Access.all;
         Hello_Response : constant String := Hello_World_Service.Hello;
      begin
         Gwiad.Plugins.Services.Free
           (Gwiad.Plugins.Services.Service_Access
              (Hello_World_Service_Access));
         return Response.Build (MIME.Text_HTML, Hello_Response);
      end Build_Response;
   end Hello_World;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      pragma Unreferenced (Name);
   begin
      Gwiad.Web.Main_Host.Unregister (Web_Dir => Hello_Web_Dir);
   end Unregister;

begin  --  Hello_World register website

   AWS.Services.Dispatchers.URI.Register
     (Dispatcher => AWS.Services.Dispatchers.URI.Handler (Main_Dispatcher.all),
      URI        => Hello_Web_Dir & "world",
      Action     => Dispatchers.Callback.Create (Hello_World'Access));

   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (AWS.Services.Dispatchers.URI.Handler (Main_Dispatcher.all),
      Dispatchers.Callback.Create (Default_Callback'Access));

   Gwiad.Web.Main_Host.Register
     (Web_Dir => Hello_Web_Dir,
      Action  => AWS.Services.Dispatchers.URI.Handler (Main_Dispatcher.all));

   AWS.Dispatchers.Free (Main_Dispatcher);

   Gwiad.Plugins.Websites.Registry.Register
     (Name        => "Hello web site",
      Description => "A test for gwiad using hello world service",
      Unregister  => Unregister'Access,
     Library_Path => Path.all);

   Free (Path);
end Hello_World;
