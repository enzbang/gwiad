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

with Gwiad.Plugins.Register;

package body Gwiad.Dynamic_Libraries.Manager is

   use Ada.Directories;
   use Ada.Containers;
   use Ada.Exceptions;

   Test_Manager : Manager;

   task body Discover is
   begin
      loop
         Test_Manager.Discover_Libraries;
         delay 1.0;
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
         Start_Search (S, "lib", "*." & Get_Library_Extension,
                       (Ordinary_File => True, others => False));

         while More_Entries (S) loop
            Get_Next_Entry (S, D);
            declare
               Library         : Dynamic_Library;

               Path            : constant String := Full_Name (D);
            begin
               if not Contains
                 (Container => Loaded_Libraries, Key => Path)
               then
                  Ada.Text_IO.Put_Line (Path);
                  Library := Load (Path);
                  Plugins.Register.Register (Library_Path => Path);
                  Init (Library, Path);
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
         Library : Dynamic_Library;
      begin
         Library := Load (Path);
         Loaded_Libraries.Insert (Path, Library);
      end Load;

      ------------
      -- Unload --
      ------------

      entry Unload (Path : in String) when Loaded_Libraries.Length > 0 is
         Library : Dynamic_Library;
      begin
         if Path /= "" then
            if not Loaded_Libraries.Contains (Path) then
               raise Dynamic_Library_Error with Path;
            end if;
            Ada.Text_IO.Put_Line ("Unload library at " & Path);
            Library := Loaded_Libraries.Element (Path);
            --  Loaded_Libraries.Delete (Name);
            Plugins.Register.Unregister ("name");
            Dynamic_Libraries.Unload (Library);
            Ada.Text_IO.Put_Line ("Done");
         end if;
      end Unload;
   end Manager;

end Gwiad.Dynamic_Libraries.Manager;
