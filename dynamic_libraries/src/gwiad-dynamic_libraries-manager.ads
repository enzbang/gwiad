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

package Gwiad.Dynamic_Libraries.Manager is

   Dynamic_Library_Manager_Error : exception renames Dynamic_Library_Error;

   type Library_Type is (Website_Library, Service_Library);

   task type Discover is

      entry Stop;
      --  Stop discover task

   end Discover;

   --  Active task used to check for new services/websites plugins

   package Hashed_Strings is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Dynamic_Library_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   use Hashed_Strings;

   protected Manager is

      procedure Discover_Libraries;
      --  Discovers new libraries to load

      procedure Load (Path : in String);
      --  Load a library

      procedure Unload (Path : in String);
      --  Unregister a library

      procedure Unload_All;
      --  Unregister all libraries

   private
      Loaded_Libraries : Hashed_Strings.Map := Hashed_Strings.Empty_Map;
   end Manager;

end Gwiad.Dynamic_Libraries.Manager;
