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

package body Gwiad.Registry.Websites.Register is

   use Morzhol.Strings;

   Last_Library_Path : Unbounded_String;
   Website_Map       : Register_Maps.Map;

   -----------------
   -- Description --
   -----------------

   function Description (Position : in Cursor) return String is
      RW : constant Registered_Website :=
             Register_Maps.Element
               (Position => Register_Maps.Cursor (Position));
   begin
      return -RW.Description;
   end Description;

   ----------
   -- Find --
   ----------

   function Find (Key : in Website_Name) return Cursor is
   begin
      return Cursor (Register_Maps.Find (Website_Map, Key));
   end Find;

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      return Cursor (Website_Map.First);
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

   ----------
   -- Name --
   ----------

   function Name (Position : in Cursor) return Website_Name is
   begin
      return Register_Maps.Key (Position => Register_Maps.Cursor (Position));
   end Name;

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
      RW : constant Registered_Website :=
             Register_Maps.Element (Register_Maps.Cursor (Position));
   begin
      return -RW.Path;
   end Path;

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
      Register_Maps.Insert
        (Website_Map,
         Name,
         (Unregister_CB => Unregister,
          Path          => +Library_Path,
          Description   => +Description));
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      use Register_Maps;

      Position : Register_Maps.Cursor := Website_Map.Find (Name);
   begin
      if Position = No_Element then
         raise Website_Error;
      end if;

      declare
         RW : Registered_Website := Element (Position);
      begin
         RW.Unregister_CB.all (Name);
      end;

      Website_Map.Delete (Position);
   end Unregister;

end Gwiad.Registry.Websites.Register;
