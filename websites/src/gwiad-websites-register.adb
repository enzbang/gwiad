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

package body Gwiad.Websites.Register is

   Last_Library_Path : Unbounded_String;

   Website_Map : Register_Maps.Map;

      -----------------
   -- Description --
   -----------------

   function Description (Position : Cursor) return String is
      RW : Registered_Website;
   begin
      RW := Register_Maps.Element
        (Position => Register_Maps.Cursor (Position));
      return To_String (RW.Description);
   end Description;

   ----------
   -- Find --
   ----------

   function Find (Key : in String) return Cursor is
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

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Register_Maps.Has_Element (Register_Maps.Cursor (Position));
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name (Position : Cursor) return String is
   begin
      return  Register_Maps.Key (Position => Register_Maps.Cursor (Position));
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

   function Path (Position : Cursor) return String is
      RW : Registered_Website := Register_Maps.Element
        (Register_Maps.Cursor (Position));
   begin
      return To_String (RW.Path);
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
     (Name        : in String;
      Description : in String;
      Unregister  : in Unregister_CB) is
   begin
      if Last_Library_Path = Null_Unbounded_String then
         raise Website_Error;
      end if;

      Register_Maps.Insert
        (Website_Map,
         Name,
         (Unregister_CB => Unregister,
          Path          => Last_Library_Path,
          Description   => To_Unbounded_String (Description)));

      Last_Library_Path := Null_Unbounded_String;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in String) is
      use Register_Maps;

      Position : Register_Maps.Cursor := Website_Map.Find (Name);
   begin
      if Position = No_Element then
         raise Website_Error;
      end if;

      Website_Map.Delete (Position);
   end Unregister;

end Gwiad.Websites.Register;
