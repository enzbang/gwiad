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

with Ada.Strings.Hash;

with Morzhol.Strings;

package body Gwiad.Plugins.Websites.Registry is

   use Morzhol.Strings;

   Last_Library_Path : Unbounded_String;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Website_Name) return Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   ------------------
   -- Library_Path --
   ------------------

   function Library_Path return String is
   begin
      return To_String (Last_Library_Path);
   end Library_Path;

   --------------
   -- Register --
   --------------

   procedure Register (Library_Path : in String) is
   begin
      Last_Library_Path := +Library_Path;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name         : in Website_Name;
      Description  : in String;
      Unregister   : in Unregister_CB;
      Library_Path : in String)
   is
   begin
      Map.Insert
        (Name,
         (Unregister_CB => Unregister,
          Path          => +Library_Path,
          Description   => +Description));
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      use Map;

      Position : Cursor := Find (Name);
   begin
      if Position = No_Element then
         raise Website_Error;
      end if;

      declare
         RW : Registered_Website := Element (Position);
      begin
         RW.Unregister_CB.all (Name);
      end;

      Map.Delete (Position);
   end Unregister;

end Gwiad.Plugins.Websites.Registry;
