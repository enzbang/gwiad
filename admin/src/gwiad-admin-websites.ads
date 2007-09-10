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
with AWS.Templates;
with AWS.Services.Web_Block.Context;

with Gwiad.Plugins.Websites;

package Gwiad.Admin.Websites is

   use AWS;

   Websites_URL : constant String := "websites/";

   procedure Discover_Virtual_Host_Directories;
   --  Search wiki website on plugin root path

   procedure List_Websites
     (Request      : in     Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Lists all websites

   procedure Stop_Website
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Stop a website

   procedure Unregister_Website_Library
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Unregister a library and all associated websites

   ------------------------------
   -- Virtual_Host_Directories --
   ------------------------------

   procedure Virtual_Host_Directories
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set);
   --  Search for virtual host directories

   procedure Virtual_Host_Unregister
     (Name : in Gwiad.Plugins.Websites.Website_Name);
   --  Unregister a virtual host. The website_name is the

end Gwiad.Admin.Websites;
