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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package body Gwiad.Plugins.Register is

   use Plugins;
   use Ada.Strings.Unbounded;

   type Registered_Plugin is record
      Builder     : Plugin_Builder;
      Path        : Unbounded_String;
      Description : Unbounded_String;
   end record;

   Last_Library_Path : Unbounded_String;

   package Register_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Registered_Plugin, Ada.Strings.Hash, "=", "=");

   Plugin_Map : Register_Maps.Map;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in String) return Boolean is
   begin
      return Register_Maps.Contains (Plugin_Map, Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Name : in String) return Plugin_Access is
      Get_Registered_Plugin : Registered_Plugin :=
                                Register_Maps.Element
                                  (Container => Plugin_Map,
                                   Key       => Name);
   begin
      return Plugin_Access (Get_Registered_Plugin.Builder.all);
   end Get;

   -----------
   -- Image --
   -----------

   function Image (Name : in String) return String is
      P : Registered_Plugin;
   begin
      P := Register_Maps.Element (Container => Plugin_Map,
                                  Key       => Name);

      return To_String (Name & P.Description & ", path : " & P.Path);
   end Image;

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
     (Name           : in String;
      Description    : in String;
      Builder        : in Plugin_Builder) is
   begin
      if Last_Library_Path = Null_Unbounded_String then
         raise Plugin_Error;
      end if;

      Register_Maps.Insert
        (Plugin_Map,
         Name,
         (Builder, Last_Library_Path,
          To_Unbounded_String (Description)));

      Last_Library_Path := Null_Unbounded_String;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in String) is
   begin
      Register_Maps.Delete (Container => Plugin_Map,
                            Key       => Name);
   end Unregister;

end Gwiad.Plugins.Register;
