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

private with Gwiad.Plugins;

package Gwiad.Dynamic_Libraries is

   Dynamic_Library_Error : exception;

   type Dynamic_Library is tagged limited private;

   type Dynamic_Library_Access is access all Dynamic_Library;

   function Load (Path : in String) return Dynamic_Library_Access;
   --  Load the dynamic library located at path.
   --  Raises Dynamic_Library_Error if it fails.

   procedure Init (Library : in Dynamic_Library; Path : in String);
   --  Init the dynamic library
   --  As the library is compile with Library_Auto_Init set as false
   --  it has to be manually initialized
   --  The initialization procedure is named by the catentation of the
   --  Library name (without the leading "lib") and "init"
   --  Raises Dynamic_Library_Error if fails.

   generic
      type Call_Function_Access is private;
   function Call
     (Library       : in Dynamic_Library;
      Function_Name : in String)
      return Call_Function_Access;
   --  Returns an access to the function Function_Name within the
   --  given dynamic library.
   --  Raise Dynamic_Library_Error if no such function is present.

--     procedure Unload (Library : in out Dynamic_Library_Access);
--     --  Unloads the dynamic library

private

   type Implementation;

   type Reference is access Implementation;

   type Dynamic_Library is tagged limited record
      Ref                 : Reference;
      Unregister_Callback : Gwiad.Plugins.Unload_CB_Access;
      Reload_Callback     : Gwiad.Plugins.Reload_CB_Access;
   end record;

   function Get_Library_Extension return String;
   --  Returns the library extension

end Gwiad.Dynamic_Libraries;
