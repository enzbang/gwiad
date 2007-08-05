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

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Containers.Vectors;

package body Gwiad.Plugins.Services.Cache is

   use Ada;
   use Ada.Exceptions;

   function Hash (Key : in Service_Id) return Containers.Hash_Type;
   --  Hash function for Service_Id (using Strings.Hash)

   package Services_Cache_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => Service_Id,
      Element_Type    => Service_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --  Store all services index by service_id

   package Ids_Vectors is new Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Service_Id);

   package Running_Services_Map is new Containers.Indefinite_Hashed_Maps
     (Key_Type        => Service_Name,
      Element_Type    => Ids_Vectors.Vector,
      Hash            => Gwiad.Plugins.Services.Registry.Hash,
      Equivalent_Keys => "=",
      "="             => Ids_Vectors."=");
   --  Store all service_id index by service_name

   procedure Insert (Name : in Service_Name; Id : in Service_Id);
   --  Insert a new service in cache

   Services_Cache   : Services_Cache_Map.Map;
   Running_Services : Running_Services_Map.Map;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : in Service_Name) is
   begin
      if Running_Services_Map.Contains (Running_Services, Name) then
         declare
            Service_Ids : Ids_Vectors.Vector :=
                            Running_Services_Map.Element
                              (Running_Services, Name);
            Id          : Service_Id;
         begin
            for K in Service_Ids.First_Index .. Service_Ids.Last_Index loop
               Id := Service_Ids.First_Element;

               --  Delete service_access

               declare
                  Position : Services_Cache_Map.Cursor :=
                               Services_Cache.Find (Id);
                  Service  : Service_Access            :=
                               Services_Cache_Map.Element (Position);
               begin
                  Delete (Service);
                  Free (Service);
                  Services_Cache.Delete (Position);
               end;

               Service_Ids.Delete_First;
            end loop;
         end;
      end if;
   end Delete;

   ---------
   -- Get --
   ---------

   function Get (Name : in Service_Name) return not null Service_Access is
   begin
      return New_Service (Name);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Id : in Service_Id) return not null Service_Access is
   begin
      return Services_Cache.Element (Key => Id);
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         raise Service_Error;
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Service_Id) return Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   ------------
   -- Insert --
   ------------

   procedure Insert (Name : in Service_Name; Id : in Service_Id) is
      use Running_Services_Map;
      Position : constant Running_Services_Map.Cursor
        := Running_Services.Find (Key => Name);
      Services : Ids_Vectors.Vector := Ids_Vectors.Empty_Vector;
   begin
      if Position /=  No_Element then
         Services := Element (Position);
         Ids_Vectors.Append (Services, Id);
      else
         Services.Append (Id);
         Running_Services.Insert (Name, Services);
      end if;
   end Insert;

   ---------
   -- Set --
   ---------

   function Set
     (Name : in Service_Name; Item : in not null Service_Access)
      return Service_Id
   is

      SID     : constant Service_Id := Service_Id (AWS.Digest.Create_Nonce);
      Cursor  : Services_Cache_Map.Cursor;
      Success : Boolean := False;

   begin
      Insert (Name, SID);
      Services_Cache.Insert
        (SID, Item, Cursor, Success);
      return SID;
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         raise Service_Error;
   end Set;

end Gwiad.Plugins.Services.Cache;
