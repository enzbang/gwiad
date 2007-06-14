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

with Ada.Directories;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Hashed_Maps;

with AWS.Dispatchers.Callback;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Response;
with AWS.MIME;

with Morzhol.OS;

package body Gwiad.Web.Register.Virtual_Host is

   use Ada;
   use AWS;
   use Morzhol.OS;

   package Config_Maps is new Containers.Indefinite_Hashed_Maps
     (String, Virtual_Host_Directory, Strings.Hash, "=", "=");
   use Config_Maps;

   Configs : Map;

   function Default_Callback
     (Request : in Status.Data) return Response.Data;
   --  Default and unique callback for this dispatcher

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback
     (Request : in Status.Data) return Response.Data
   is
      use Ada.Directories;

      function Get_Hostname (Hostname : String) return String;
      --  Get hostname

      ------------------
      -- Get_Hostname --
      ------------------

      function Get_Hostname (Hostname : String) return String is
         K : Natural;
      begin
         K := Strings.Fixed.Index (Hostname, ":");

         if K = 0 then
            K := Hostname'Last;
         else
            K := K - 1;
         end if;
         return Hostname (Hostname'First .. K);
      end Get_Hostname;


      URI      : constant String := Status.URI (Request);
      Hostname : constant String := Get_Hostname (AWS.Status.Host (Request));
   begin

      if Configs.Contains (Hostname) then
         declare
            VH_Dir : constant Virtual_Host_Directory :=
                       Configs.Element (Hostname);
            Filename : constant String
              := To_String (VH_Dir.Document_Root)
              & Directory_Separator
              & URI (URI'First + 1 .. URI'Last);
         begin
            if Exists (Filename)
              and then Kind (Filename) = Ordinary_File then
               return Response.File
                 (Filename     => Filename,
                  Content_Type => MIME.Content_Type (Filename));
            end if;
            if Exists (To_String (VH_Dir.Document_Root)
                       & Directory_Separator &
                       To_String (VH_Dir.Default_Page)) then
               return Response.Moved
                 (Location => "/" &  To_String (VH_Dir.Default_Page));
            end if;
         end;
      end if;
      return Response.Build (Content_Type  => MIME.Text_HTML,
                             Message_Body  => "<h1>Coming soon...</h1>");
   end Default_Callback;

   --------------
   -- Register --
   --------------

   procedure Register
     (Hostname : in String; Action : in AWS.Dispatchers.Handler'Class)
   is
   begin
      Services.Dispatchers.Virtual_Host.Register
        (Dispatcher       => Virtual_Hosts_Dispatcher,
         Virtual_Hostname => Hostname,
         Action           => Action);
      Gwiad.Web.Reload.Require;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Host : in String; Redirected_Hostname : in String)
   is
   begin
      Services.Dispatchers.Virtual_Host.Register
        (Dispatcher       => Virtual_Hosts_Dispatcher,
         Virtual_Hostname => Host,
         Hostname         => Redirected_Hostname);
      Gwiad.Web.Reload.Require;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Hostname : in String;
      VH_Dir   : in Virtual_Host_Directory)
   is
      Hostname_Dispatcher : Services.Dispatchers.URI.Handler;
   begin

      Configs.Insert (Hostname, VH_Dir);

      Services.Dispatchers.URI.Register_Default_Callback
        (Hostname_Dispatcher,
         Dispatchers.Callback.Create (Default_Callback'Access));

      Register (Hostname => Hostname,
                Action   => Hostname_Dispatcher);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Hostname : in String) is
   begin
      Services.Dispatchers.Virtual_Host.Unregister
        (Dispatcher       => Virtual_Hosts_Dispatcher,
         Virtual_Hostname => Hostname);
      Gwiad.Web.Reload.Require;

      if Configs.Contains (Hostname) then
         Configs.Delete (Hostname);
      end if;
   end Unregister;

end Gwiad.Web.Register.Virtual_Host;
