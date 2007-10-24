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

package body Gwiad.Plugins.Websites.Registry is

   use Morzhol.Strings;

   procedure Unload (Library_Path : in String);
   --  Unload a website library

   ----------
   -- Hash --
   ----------

   function Hash (Key : in Website_Name) return Containers.Hash_Type is
   begin
      return Strings.Hash (String (Key));
   end Hash;

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
      Register_Unload_Callback :
      declare
         use Map;
         Position  : Cursor  := First;
         Is_In_Map : Boolean := False;
      begin
         Search_In_Map :
         while Has_Element (Position) loop
            if Path (Position) = Library_Path then
               Is_In_Map := True;
               exit Search_In_Map;
            end if;
            Map.Next (Position);
         end loop Search_In_Map;

         if not Is_In_Map then
            Set_Internal_Unload_CB (Unload'Access);
         end if;
      end Register_Unload_Callback;

      Map.Insert
        (Name,
         Registered_Website'(Unregister_CB => Unregister,
                             Path          => +Library_Path,
                             Description   => +Description));
   end Register;

   ------------
   -- Unload --
   ------------

   procedure Unload (Library_Path : in String) is
   begin
      Map_Search :
      declare
         Position : Map.Cursor := Map.First;
      begin
         while Map.Has_Element (Position) loop
            if Map.Path (Position) /= Library_Path then
               Map.Next (Position);
            else
               Unregister_Website :
               declare
                  Last_Position : constant Map.Cursor := Position;
               begin
                  Map.Next (Position);
                  Unregister (Map.Name (Last_Position));
               end Unregister_Website;
            end if;
         end loop;
      end Map_Search;

   end Unload;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in Website_Name) is
      use Map;
      Position : Cursor := Find (Name);
   begin
      if Position = No_Element then
         raise Website_Error;
      end if;

      Unregister_Website : declare
         RW : Registered_Website := Element (Position);
      begin
         RW.Unregister_CB.all (Name);
      end Unregister_Website;
      Map.Delete (Position);
   end Unregister;

end Gwiad.Plugins.Websites.Registry;
