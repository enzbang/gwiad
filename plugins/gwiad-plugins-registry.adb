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

package body Gwiad.Plugins.Registry is

   Plugin_Map : Register_Maps.Map;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : in Plugin_Name) is
   begin
      Register_Maps.Delete (Plugin_Map, Name);
   end Delete;

   ------------
   -- Delete --
   ------------

   procedure Delete (Position : in out Cursor) is
   begin
      Register_Maps.Delete
        (Container => Plugin_Map,
         Position  => Register_Maps.Cursor (Position));
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Name : in Plugin_Name) return Registered_Plugin is
   begin
      return Register_Maps.Element (Plugin_Map, Name);
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Registered_Plugin is
   begin
      return Register_Maps.Element (Register_Maps.Cursor (Position));
   end Element;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in Plugin_Name) return Boolean is
   begin
      return Register_Maps.Contains (Plugin_Map, Name);
   end Exists;

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      return Cursor (Register_Maps.First (Plugin_Map));
   end First;

   ----------
   -- Find --
   ----------

   function Find (Key : in Plugin_Name) return Cursor is
   begin
      return Cursor (Register_Maps.Find (Plugin_Map, Key));
   end Find;

   ------------
   -- Insert --
   ------------

   procedure Insert (Name : in Plugin_Name; Plugin : in Registered_Plugin) is
   begin
      Register_Maps.Insert (Plugin_Map, Name, Plugin);
   end Insert;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Register_Maps.Next (Register_Maps.Cursor (Position));
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : in Cursor) return Boolean is
   begin
      return Register_Maps.Has_Element (Register_Maps.Cursor (Position));
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name (Position : in Cursor) return Plugin_Name is
   begin
      return Register_Maps.Key (Position => Register_Maps.Cursor (Position));
   end Name;

   -----------------
   -- Description --
   -----------------

   function Description (Position : in Cursor) return String is
      RS : constant Registered_Plugin :=
             Register_Maps.Element
               (Position => Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Description);
   end Description;

   ----------
   -- Path --
   ----------

   function Path (Position : in Cursor) return String is
      RS : constant Registered_Plugin :=
             Register_Maps.Element
               (Position => Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Path);
   end Path;

end Gwiad.Plugins.Registry;
