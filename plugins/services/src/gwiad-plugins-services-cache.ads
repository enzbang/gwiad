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

with AWS.Digest;

with Gwiad.Plugins.Services.Registry;

package Gwiad.Plugins.Services.Cache is

   --  This package provides a cache for gwiad services
   --  A unique service id is generated on cache insertion
   --  When a service plugin is unloaded, all the services refering to
   --  it are removed from cache

   use Gwiad.Plugins.Services.Registry;

   type Service_Id is new AWS.Digest.Nonce;
   --  Ensure that service_id is unique by using AWS digest nonce

   function Get (Name : in Service_Name) return not null Service_Access;
   --  Gets a new service

   function Get (Id : in Service_Id) return not null Service_Access;
   --  Returns the service from cache.
   --  When no service with the given id is found raise Service_Error

   function Set
     (Name : in Service_Name; Item : in not null Service_Access)
      return Service_Id;
   --  Adds the service to cache

   procedure Delete (Name : in Service_Name);
   --  Deletes all services in cache having the given name

   ------------------
   -- Service_Maps --
   ------------------

   package Service_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type     => String,
      Element_Type => Service_Id,
      Hash         => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Service_Map_Access is access all Service_Maps.Map;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Service_Maps.Map, Name => Service_Map_Access);

end Gwiad.Plugins.Services.Cache;
