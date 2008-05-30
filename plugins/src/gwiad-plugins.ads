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

with Ada.Strings.Unbounded;
--  with Ada.Unchecked_Deallocation;

package Gwiad.Plugins is

   use Ada.Strings.Unbounded;

   type Callback is access procedure;

   type Plugin is tagged record
      Description   : Unbounded_String;
      Path          : Unbounded_String;
   end record;

   ---------------
   -- Unload_CB --
   ---------------

   type Unload_CB is private;
   type Unload_CB_Access is access all Unload_CB;

   function New_Unload_CB (Path : in String) return Unload_CB_Access;
   --  Create a new unload callback

   procedure Call (Unload_CB : in out Unload_CB_Access);
   --  Call unload callback

   ---------------
   -- Reload_CB --
   ---------------

   type Reload_CB is private;
   type Reload_CB_Access is access all Reload_CB;

   function New_Reload_CB return Reload_CB_Access;
   --  Create a new reload callback

   procedure Call (Reload_CB : in Reload_CB_Access);
   --  Call reload callback

   --------------
   -- Register --
   --------------

   procedure Register
     (Path      : in String;
      Unload_CB : in Gwiad.Plugins.Unload_CB_Access;
      Reload_CB : in Gwiad.Plugins.Reload_CB_Access);
   --  Registers a new plugin
   --  This must be called before registering the plugin to set the library
   --  path before the service registration
   --  The unload callback procedure is called by dynamic plugin manager on
   --  library unload
   --  The reload callback is called on "gwiad reload"

   procedure Set_Unload_CB (Callback : in Plugins.Callback);
   --  Set plugin library unload CB (must be called by the plugin library)

   procedure Set_Reload_CB (Callback : in Plugins.Callback);
   --  Set plugin library reload CB (must be called by the plugin library)

   function Get_Last_Library_Path return String;
   --  Returns last library path

private

   function Get_Last_Unload_CB return Unload_CB_Access;
   --  Returns the current library path
   --  This must be called on library init

   function Get_Last_Reload_CB return Reload_CB_Access;
   --  Returns the current library path
   --  This must be called on library init

   type Internal_Unload_CB is access procedure (Path : in String);

   type Unload_CB is record
      Path              : String_Access;
      Callback          : Plugins.Callback;
      Internal_Callback : Internal_Unload_CB;
   end record;

   type Reload_CB is record
      Callback : Plugins.Callback;
   end record;

   procedure Set_Internal_Unload_CB
     (Callback : in Internal_Unload_CB);
   --  Set internal unload callback

end Gwiad.Plugins;
