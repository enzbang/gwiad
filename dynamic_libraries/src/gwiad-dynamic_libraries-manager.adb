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

with Ada.Directories;
with Ada.Text_IO;
with Ada.Exceptions;

with System.OS_Lib;

with Gwiad.Services.Register;
with Gwiad.Websites.Register;

package body Gwiad.Dynamic_Libraries.Manager is

   use Ada.Directories;
   use Ada.Containers;
   use Ada.Exceptions;

   --------------
   -- Discover --
   --------------

   task body Discover is
   begin
      loop
         delay 1.0;
         Manager.Discover_Libraries;
      end loop;
   end Discover;

   -------------
   -- Manager --
   -------------

   protected body Manager is

      ------------------------
      -- Discover_Libraries --
      ------------------------

      procedure Discover_Libraries is
         S : Search_Type;
         D : Directory_Entry_Type;
      begin
         Start_Search
           (S, "lib", "*." & Get_Library_Extension,
            (Ordinary_File => True, others => False));

         while More_Entries (S) loop
            Get_Next_Entry (S, D);
            declare
               Library : Dynamic_Library_Access := new Dynamic_Library;
               Path    : constant String        := Full_Name (D);
            begin
               if not Loaded_Libraries.Contains (Path) then
                  Ada.Text_IO.Put_Line (Path);

                  --  Set as read only to prevent file operation on runtine

                  System.OS_Lib.Set_Read_Only (Path);

                  Library := Dynamic_Libraries.Load (Path);

                  --  ??? As the type of library is not known (service or
                  --  website). Register both
                  --  Library having a different type must be stored in
                  --  separate directory

                  Services.Register.Register (Library_Path => Path);
                  Websites.Register.Register (Library_Path => Path);

                  Init (Library.all, Path);
                  Loaded_Libraries.Insert (Path, Library);
               end if;
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Exception_Information (E));
      end Discover_Libraries;

      ----------
      -- Load --
      ----------

      procedure Load (Path : in String) is
         Library : Dynamic_Library_Access;
      begin
         Library := Load (Path);
         Loaded_Libraries.Insert (Path, Library);
      end Load;

      ------------
      -- Unload --
      ------------

      entry Unload (Path : in String) when Loaded_Libraries.Length > 0 is
         Library : Dynamic_Library_Access;
      begin
         if not Loaded_Libraries.Contains (Path) then
            raise Dynamic_Library_Error with Path;
         end if;

         Library := Loaded_Libraries.Element (Path);
         Loaded_Libraries.Delete (Path);
         Dynamic_Libraries.Unload (Library.all);

         declare
            Path_Disabled : constant String := Path & ".disabled";
         begin
            if Exists (Path_Disabled) then
               Delete_File (Path_Disabled);
            end if;
            Rename (Path, Path_Disabled);
         end;
      end Unload;
   end Manager;

end Gwiad.Dynamic_Libraries.Manager;
