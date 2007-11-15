------------------------------------------------------------------------------
--                                 Gwiad                                    --
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
with Ada.Command_Line;
with Ada.Exceptions;

with Gwiad.Web;

with Gwiad.Dynamic_Libraries.Manager;

with AWS.Client;
with AWS.Response;
with AWS.Utils;
with AWS.Config.Set;

procedure Check_Mem is

   use Ada;
   use Ada.Text_IO;

   use Gwiad;
   use AWS;

   use Gwiad.Dynamic_Libraries.Manager;

   Host : constant String   := "localhost";
   Port : constant Positive := 8042;

   Lib_Hello_World_Service : constant String :=
                               "./libhello_world_service.so";

   Lib_Hello_World_Website : constant String :=
                               "./libhello_world_website.so";

   Lib_Gwiad_Admin          : constant String :=
                                "./libgwiad_admin.so";

   --  Path to hello world website and service library

   Iteration : Positive;
   --  Control the number of iteration (for mem check)

   Connection : Client.HTTP_Connection;
   --  AWS Client Connection to check generated pages

   Result     : Response.Data;
   --  AWS Client response

   Configuration : Config.Object;
   --  AWS Server config

   procedure Load (Library : in String);

   procedure Unregister (Library : in String);

   ----------
   -- Load --
   ----------

   procedure Load (Library : in String) is
   begin
      Manager.Load (Library);
   end Load;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Library : in String) is
   begin
      Manager.Unregister (Library);
   end Unregister;

   Welcome_Message     : constant String := "<h1>Welcome to gwiad</h1>";
   Hello_World_Message : constant String :=
                           "hello_world_service says Hello World";

begin

   --  Server configuration

   Configuration := Config.Get_Current;

   Config.Set.Session (O => Configuration, Value => True);
   Config.Set.Server_Host (O => Configuration, Value => Host);
   Config.Set.Server_Port (O => Configuration, Value => Port);

   Web.Start (Configuration);
   Put_Line ("Start main, wait for server to start...");

   Client.Create (Connection, "http://" & Host & ":" & Utils.Image (Port),
                 User => "admin", Pwd => "admin");

   Client.Get (Connection, Result, URI => "/");

   if Response.Message_Body (Result) /= Welcome_Message then
      Ada.Text_IO.Put_Line ("Error get " & Response.Message_Body (Result)
                              & " waiting " & Welcome_Message);
   end if;

   Iteration := Integer'Value (Command_Line.Argument (1));

   Load (Lib_Hello_World_Service);
   Load (Lib_Gwiad_Admin);
   Load (Lib_Hello_World_Website);

   --  This is the main loop. Be sure to run everything inside this
   --  loop. Check_Mem is checked between 2 runs with a different number of
   --  iterations.

   for K in 1 ..  Iteration loop

      Client.Get (Connection, Result, URI => "/hello/world");

      if Response.Message_Body (Result) /= Hello_World_Message
        and then Response.Message_Body (Result) /= Welcome_Message
      then
         --  An error as occurred. The message could be the Hello_World_Message
         --  or the Welcome_Message (if the website is not loaded)
         Ada.Text_IO.Put_Line ("Unknown error !");
      end if;

      Client.Get (Connection, Result, URI => "/hello/world");

      if Response.Message_Body (Result) /= Hello_World_Message then
         Ada.Text_IO.Put_Line ("Error get " & Response.Message_Body (Result)
                                 & "waiting "& Hello_World_Message);
      end if;
   end loop;

   Unregister (Lib_Hello_World_Website);

   for K in 1 ..  Iteration loop

      Client.Get (Connection, Result, URI => "/hello/world");

      if Response.Message_Body (Result) /= Welcome_Message then
         Ada.Text_IO.Put_Line ("Error get " & Response.Message_Body (Result)
                                 & " waiting " & Welcome_Message);
      end if;
   end loop;

   Ada.Text_IO.Put_Line ("Exit : unload libraries and stop web server");

   Client.Close (Connection);

   Web.Stop;

   Manager.Unregister_All;

   Command_Line.Set_Exit_Status (Command_Line.Success);
exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Check_Mem;
