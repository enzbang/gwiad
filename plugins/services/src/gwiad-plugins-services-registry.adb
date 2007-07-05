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

   Last_Library_Path : Unbounded_String;

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

   procedure Register (Library_Path : in String) is
   begin
      Last_Library_Path := To_Unbounded_String (Library_Path);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name        : in Service_Name;
      Description : in String;
      Builder     : in Service_Builder) is
   begin
      if Last_Library_Path = Null_Unbounded_String then
         raise Service_Error;
      end if;

      declare
         New_Service : Registered_Service :=
                         (Builder     => Builder,
                          Path        => Last_Library_Path,
                          Description => To_Unbounded_String (Description));
      begin
         Map.Insert (Name, New_Service);
      end;

      Last_Library_Path := Null_Unbounded_String;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Service_Name) is
      use Gwiad.Plugins.Services;
   begin
      Cache.Delete (Name);
      Map.Delete (Name);
   exception
         when others => raise Service_Error;
   end Unregister;

end Gwiad.Plugins.Services.Registry;
