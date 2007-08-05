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

with Gwiad.Plugins.Services.Cache;

package body Gwiad.Plugins.Services.Registry is

   procedure Unload (Library_Path : in String);
   --  Unload service library

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Service_Name) return Ada.Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   -----------------
   -- New_Service --
   -----------------

   function New_Service
     (Name : in Service_Name) return not null Service_Access is
   begin
      return Service_Access (Map.Element (Name).Builder.all);
   end New_Service;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name        : in Service_Name;
      Description : in String;
      Builder     : in Service_Builder) is

      Last_Library_Path : constant String := Get_Last_Library_Path;
      Last_Unload_CB    : constant Unload_CB_Access := Get_Last_Unload_CB;

   begin
      if Last_Library_Path = ""
        or else Last_Unload_CB.Callback = null
      then
         raise Service_Error;
      end if;

      declare
         New_Service : Registered_Service :=
                         (Builder       => Builder,
                          Path          =>
                            To_Unbounded_String (Last_Library_Path),
                          Description   => To_Unbounded_String (Description));
      begin
         Map.Insert (Name, New_Service);
      end;
      Last_Unload_CB.Internal_Callback := Unload'Access;
   end Register;

   ------------
   -- Unload --
   ------------

   procedure Unload (Library_Path : in String) is
      use Map;
      Position : Cursor := First;
   begin
      Search_In_Map :
      while Has_Element (Position) loop
         if Element (Position).Path = Library_Path then
            exit Search_In_Map;
         end if;
         Next (Position);
      end loop Search_In_Map;

      Cache.Delete (Name (Position));
      Delete (Position);

   end Unload;
end Gwiad.Plugins.Services.Registry;
