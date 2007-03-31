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

with Gwiad.Services.Register;
with Gwiad.Web;

with Hello_World_Interface;

package body Hello_World is

   use AWS;

   use Gwiad;
   use Gwiad.Services;

   use Hello_World_Interface;

   Hello_Web_Dir : constant String := "/hello/";

   Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default callback

   function Hello_World (Request : in Status.Data) return Response.Data;
   --  Hello world
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

      Service_Name : constant String := "hello_world_service";

   begin

      if not Register.Exists (Name => Service_Name) then
         return Response.Build (MIME.Text_HTML,
                                "<p>Service down</p>");
      end if;

      declare
         Hello_World_Service_Access : constant HW_Service_Access :=
                                       HW_Service_Access
                                         (Register.Get (Service_Name));
         Hello_World_Service        : HW_Service'Class :=
                                       Hello_World_Service_Access.all;
      begin
         return Response.Build (MIME.Text_HTML, Hello_World_Service.Hello);
      end;
   end Hello_World;

begin

   AWS.Services.Dispatchers.URI.Register
     (Dispatcher => Main_Dispatcher,
      URI        => Hello_Web_Dir & "world",
      Action     => Dispatchers.Callback.Create (Hello_World'Access));

   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));


   Gwiad.Web.Register_Web_Directory (Web_Dir => Hello_Web_Dir,
                                     Action  => Main_Dispatcher);

end Hello_World;
