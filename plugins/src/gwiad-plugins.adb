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

with Ada.Unchecked_Deallocation;

package body Gwiad.Plugins is

   Last_Unload_CB    : Unload_CB_Access := null;
   Last_Reload_CB    : Reload_CB_Access := null;
   Last_Library_Path : Unbounded_String;

   ----------
   -- Call --
   ----------

   procedure Call (Unload_CB : in out Unload_CB_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Plugins.Unload_CB,
         Name   => Plugins.Unload_CB_Access);
   begin
      if Unload_CB.Internal_Callback /= null then
         Unload_CB.Internal_Callback (Path => Unload_CB.Path.all);
      end if;

      if Unload_CB.Callback /= null then
         Unload_CB.Callback.all;
      end if;

      Free (Unload_CB.Path);
      Free (Unload_CB);
   end Call;

   ----------
   -- Call --
   ----------

   procedure Call (Reload_CB : in out Reload_CB_Access) is
   begin
      if Reload_CB.Callback /= null then
         Reload_CB.Callback.all;
      end if;
   end Call;

   ---------------------------
   -- Get_Last_Library_Path --
   ---------------------------

   function Get_Last_Library_Path return String is
   begin
      return To_String (Last_Library_Path);
   end Get_Last_Library_Path;

   ------------------------
   -- Get_Last_Reload_CB --
   ------------------------

   function Get_Last_Reload_CB return Reload_CB_Access is
   begin
      return Last_Reload_CB;
   end Get_Last_Reload_CB;

   ------------------------
   -- Get_Last_Unload_CB --
   ------------------------

   function Get_Last_Unload_CB return Unload_CB_Access is
   begin
      return Last_Unload_CB;
   end Get_Last_Unload_CB;

   -------------------
   -- New_Reload_CB --
   -------------------

   function New_Reload_CB return Reload_CB_Access is
   begin
      return new Reload_CB;
   end New_Reload_CB;

   -------------------
   -- New_Unload_CB --
   -------------------

   function New_Unload_CB (Path : in String) return Unload_CB_Access is
   begin
      return new Unload_CB'(Path   => new String'(Path),
                            others => <>);
   end New_Unload_CB;

   --------------
   -- Register --
   --------------

   procedure Register
     (Path      : in String;
      Unload_CB : in Gwiad.Plugins.Unload_CB_Access;
      Reload_CB : in Gwiad.Plugins.Reload_CB_Access)
   is
   begin
      Last_Library_Path := To_Unbounded_String (Path);
      Last_Unload_CB    := Unload_CB;
      Last_Reload_CB    := Reload_CB;

      if Last_Unload_CB.Path /= null then
         Free (Last_Unload_CB.Path);
      end if;
   end Register;

   ----------------------------
   -- Set_Internal_Unload_CB --
   ----------------------------

   procedure Set_Internal_Unload_CB
     (Callback : access procedure (Path : in String))
   is
   begin
      Last_Unload_CB.Internal_Callback := Callback;
   end Set_Internal_Unload_CB;

   -------------------
   -- Set_Reload_CB --
   -------------------

   procedure Set_Reload_CB (Callback : access procedure) is
   begin
      Last_Reload_CB.Callback := Callback;
   end Set_Reload_CB;

   -------------------
   -- Set_Unload_CB --
   -------------------

   procedure Set_Unload_CB (Callback : access procedure) is
   begin
      Last_Unload_CB.Callback := Callback;
   end Set_Unload_CB;

end Gwiad.Plugins;
