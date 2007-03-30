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

with Gwiad.Plugins.Register;
with Gwiad.Web;

with Hello_World_Interface;

with AWS.Dispatchers.Callback;
with AWS.MIME;

package body Hello_World is

   use Gwiad;
   use Gwiad.Plugins;

   use Hello_World_Interface;

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

      Plugin_Name : constant String := "hello_world_plugin";

   begin

      if not Plugins.Register.Exists (Name => Plugin_Name) then
         return Response.Build (MIME.Text_HTML,
                                "<p>Service down</p>");
      end if;

      declare
         Hello_World_Plugin_Access : constant HW_Plugin_Access
           := HW_Plugin_Access (Plugins.Register.Get ("hello_world_plugin"));
         Hello_World_Plugin        : HW_Plugin'Class :=
                                       Hello_World_Plugin_Access.all;
      begin
         return Response.Build (MIME.Text_HTML, Hello_World_Plugin.Hello);
      end;
   end Hello_World;

begin

   Services.Dispatchers.URI.Register
     (Dispatcher => Main_Dispatcher,
      URI        => "/hello",
      Action     => Dispatchers.Callback.Create (Hello_World'Access));

   Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));
   --  This Default Callback Will Handle all ECWF Callbacks

   Gwiad.Web.Register (Hostname => "127.0.0.1",
                       Action   => Main_Dispatcher);

end Hello_World;
