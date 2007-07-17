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

with AWS.Parameters;

with Gwiad.Plugins.Services.Registry;
with Gwiad.Dynamic_Libraries.Manager;

package body Gwiad.Admin.Services is

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
      use AWS.Templates;

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
         Templates.Assoc ("SERVICES_ADMIN_URL", Admin_URL & Services_URL));
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
         Templates.Assoc ("SERVICES_ADMIN_URL", Admin_URL & Services_URL));
   end Stop_Service;

end Gwiad.Admin.Services;
