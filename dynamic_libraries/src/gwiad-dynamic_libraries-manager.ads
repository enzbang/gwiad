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
with Ada.Containers.Indefinite_Hashed_Maps;

package Gwiad.Dynamic_Libraries.Manager is

   task type Discover;

   package Hashed_Strings is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Dynamic_Library, Ada.Strings.Hash, "=", "=");
   use Hashed_Strings;

   protected type Manager is
      procedure Discover_Libraries;
      --  Discovers new libraries to load

      procedure Load (Path : in String);
      --  Load a library

      entry Unload (Path : in String);
      --  Unload a library

   private
      Loaded_Libraries : Hashed_Strings.Map := Hashed_Strings.Empty_Map;
   end Manager;

end Gwiad.Dynamic_Libraries.Manager;
