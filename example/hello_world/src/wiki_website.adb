------------------------------------------------------------------------------
--                               Diouzhtu                                   --
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

with AWS.Services.Dispatchers.URI;
with AWS.Dispatchers.Callback;
with AWS.Services.ECWF.Registry;
with AWS.MIME;

with Gwiad.Services.Register;
with Gwiad.Websites.Register;
with Gwiad.Web;

with Wiki_Website.Config;
with Wiki_Website.Callbacks;

package body Wiki_Website is

   use AWS;
   use Ada.Strings.Unbounded;

   use Gwiad;
   use Gwiad.Services;
   use Gwiad.Services.Register;

   use Wiki_Website.Config;
   use Wiki_Website.Callbacks;

   Wiki_Service_Id     : Service_Id       := Null_Service_Id;

   Main_Dispatcher : AWS.Services.Dispatchers.URI.Handler;

   procedure Unregister;
   --  Unregister website

   ----------------------
   -- Get_Wiki_Service --
   ----------------------

   function Get_Wiki_Service return Wiki_Interface.GW_Service'Class is
      use Wiki_Interface;
   begin
      if Wiki_Service_Id = Null_Service_Id then
         declare
            Wiki_World_Service_Access : constant GW_Service_Access
              := GW_Service_Access (Gwiad.Services.Register.Get
                                    (Wiki_Service_Name));
            Get_Service               : GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            Wiki_Service_Id := Gwiad.Services.Register.Set
              (Wiki_Service_Name, Service_Access (Wiki_World_Service_Access));

            Get_Service.Base_Directory := To_Unbounded_String (Wiki_Text_Dir);
            return Get_Service;
         end;
      else
         declare
            Wiki_World_Service_Access : constant GW_Service_Access
              := GW_Service_Access
                (Gwiad.Services.Register.Get (Wiki_Service_Name,
                 Wiki_Service_Id));
            Get_Service               : GW_Service'Class :=
                                          Wiki_World_Service_Access.all;
         begin
            return Get_Service;
         end;
      end if;
   end Get_Wiki_Service;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister is
   begin
      Gwiad.Web.Unregister_Web_Directory
        (Web_Dir => To_String (Wiki_Web_Root));
   end Unregister;

begin

   --  Read configuration file to get Wiki_Root and Wiki_Web_Root

   Wiki_Root :=  To_Unbounded_String ("/tmp");
   Wiki_Web_Root := To_Unbounded_String ("/wiki");


   AWS.Services.Dispatchers.URI.Register_Default_Callback
     (Main_Dispatcher,
      Dispatchers.Callback.Create (Default_Callback'Access));
   --  This default callback will handle all ECWF callbacks

   --  Register ECWF pages

   AWS.Services.ECWF.Registry.Register
     (To_String (Wiki_Web_Root) & "/" & Wiki_Web_Edit,
      Edit_Template'Access,
      Edit_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (To_String (Wiki_Web_Root) & "/" & Wiki_Web_Preview,
      Preview_Template'Access,
      Preview_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     (To_String (Wiki_Web_Root) & "/" & Wiki_Web_View,
      View_Template'Access,
      View_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     ("LAZY_VIEW",
      View_Template'Access,
      View_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     ("LAZY_PREVIEW",
      Preview_Template'Access,
      Preview_Page'Access,
      MIME.Text_HTML);

   AWS.Services.ECWF.Registry.Register
     ("LAZY_EDIT",
      Edit_Template'Access,
      Edit_Page'Access,
      MIME.Text_HTML);

   Gwiad.Web.Register_Web_Directory (Web_Dir => To_String (Wiki_Web_Root),
                                     Action  => Main_Dispatcher);

   Gwiad.Websites.Register.Register
     (Name        => "wiki",
      Description => "A test for diouzhtu integration in gwiad",
      Unregister  => Unregister'Access);

end Wiki_Website;
