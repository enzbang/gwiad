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

   use Ada;

   Last_Library_Path : Unbounded_String;
   Service_Map       : Register_Maps.Map;

   -----------------
   -- Description --
   -----------------

   function Description (Position : in Cursor) return String is
      RS : constant Registered_Service :=
             Register_Maps.Element
               (Position => Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Description);
   end Description;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in Service_Name) return Boolean is
   begin
      return Register_Maps.Contains (Service_Map, Name);
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Key : in Service_Name) return Cursor is
   begin
      return Cursor (Register_Maps.Find (Service_Map, Key));
   end Find;

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      return Cursor (Service_Map.First);
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : in Cursor) return Boolean is
   begin
      return Register_Maps.Has_Element (Register_Maps.Cursor (Position));
   end Has_Element;

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Service_Name) return Ada.Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

   ----------
   -- Name --
   ----------

   function Name (Position : in Cursor) return Service_Name is
   begin
      return  Register_Maps.Key (Position => Register_Maps.Cursor (Position));
   end Name;

   -----------------
   -- New_Service --
   -----------------

   function New_Service (Name : Service_Name) return Service_Access is
   begin
      return Service_Access (Service_Map.Element (Name).Builder.all);
   end New_Service;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Register_Maps.Next (Register_Maps.Cursor (Position));
   end Next;

   ----------
   -- Path --
   ----------

   function Path (Position : in Cursor) return String is
      RS : constant Registered_Service :=
             Register_Maps.Element (Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Path);
   end Path;

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

      Register_Maps.Insert
        (Service_Map,
         Name,
         (Builder     => Builder,
          Path        => Last_Library_Path,
          Description => To_Unbounded_String (Description)));

      Last_Library_Path := Null_Unbounded_String;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Service_Name) is
      use Register_Maps;
      use Gwiad.Plugins.Services;
   begin
      Cache.Delete (Name);
      Service_Map.Delete (Name);
   exception
         when others => raise Service_Error;
   end Unregister;

end Gwiad.Plugins.Services.Registry;
