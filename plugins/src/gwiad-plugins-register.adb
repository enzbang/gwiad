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

package body Gwiad.Plugins.Register is

   Last_Library_Path : Unbounded_String;

   Plugin_Map : Register_Maps.Map;

   -----------------
   -- Description --
   -----------------

   function Description (Position : Cursor) return String is
      P : Registered_Plugin;
   begin
      P := Register_Maps.Element (Position => Register_Maps.Cursor (Position));

      return To_String (P.Description);
   end Description;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in String) return Boolean is
   begin
      return Register_Maps.Contains (Plugin_Map, Name);
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Key : in String) return Cursor is
   begin
      return Cursor (Register_Maps.Find (Plugin_Map, Key));
   end Find;

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      return Cursor (Plugin_Map.First);
   end First;

   ---------
   -- Get --
   ---------

   function Get (Name : in String) return Plugin_Access is
      Get_Registered_Plugin : Registered_Plugin;
   begin
      Get_Registered_Plugin := Register_Maps.Element (Container => Plugin_Map,
                                                      Key       => Name);

      return Plugin_Access (Get_Registered_Plugin.Builder.all);
   exception
      when others =>
         raise Plugin_Error;
   end Get;

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
      P : Registered_Plugin := Register_Maps.Element
        (Register_Maps.Cursor (Position));
   begin
      return To_String (P.Path);
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
         (Builder     => Builder,
          Path        => Last_Library_Path,
          Description => To_Unbounded_String (Description)));

      Last_Library_Path := Null_Unbounded_String;

   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in String) is
      use Register_Maps;

      Position : Register_Maps.Cursor := Plugin_Map.Find (Name);
   begin
      if Position = No_Element then
         raise Plugin_Error;
      end if;

      Plugin_Map.Delete (Position);
   end Unregister;

end Gwiad.Plugins.Register;
