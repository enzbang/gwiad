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

with GNAT.OS_Lib;

with Gwiad.Plugins;

package body Gwiad.Dynamic_Libraries.Manager is

   use Ada;
   use Ada.Directories;
   use Ada.Containers;
   use Ada.Exceptions;

   Ask_Reload_File : constant String := ".gwiad_do_reload";

   Websites_Lib_Dir : constant String :=
                        Compose (Containing_Directory => "lib",
                                 Name                 => "websites");

   Services_Lib_Dir : constant String :=
                        Compose (Containing_Directory => "lib",
                                 Name                 => "services");

   procedure Flag_As_Error (Path : in String);
   --  Renames a library by adding the given suffix (or .disabled)

   --------------
   -- Discover --
   --------------

   task body Discover is
      Discover_Delay : constant Duration := 5.0;
   begin
      Discover_Libraries :
      loop
         Manager.Discover_Libraries;

         --  Maybe reload
         if Exists (Ask_Reload_File) then
            Manager.Reload_All;
            Delete_File (Ask_Reload_File);
         end if;
         select
            accept Stop;
            Manager.Unregister_All;
            exit Discover_Libraries;
         or
            delay Discover_Delay;
         end select;
      end loop Discover_Libraries;
   end Discover;

   -------------------
   -- Flag_As_Error --
   -------------------

   procedure Flag_As_Error
     (Path : in String) is
      Path_Disabled : constant String := Path & ".error";
   begin
      if Exists (Path_Disabled) then
         Delete_File (Path_Disabled);
      end if;
      Rename (Path, Path_Disabled);
   end Flag_As_Error;

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

         procedure Discover_Libraries (From : in String);

         ------------------------
         -- Discover_Libraries --
         ------------------------

         procedure Discover_Libraries (From : in String) is
            use Gwiad.Plugins;
         begin
            Start_Search
              (Search    => S,
               Directory => From,
               Pattern   => "*." & Get_Library_Extension,
               Filter    => Filter_Type'(Ordinary_File => True,
                                         Directory     => False,
                                         Special_File  => False));

            Load_Libraries_Loop :
            while More_Entries (S) loop
               Get_Next_Entry (S, D);
               Load_Library :
               declare
                  Path       : constant String := Full_Name (D);
                  Library    : Dynamic_Library_Access;
               begin
                  if not Loaded_Libraries.Contains (Path) then
                     Text_IO.Put_Line (Path);

                     --  Set as read only to prevent file operation on runtine

                     GNAT.OS_Lib.Set_Read_Only (Path);

                     Initialization :
                     begin
                        Library := Dynamic_Libraries.Load (Path);
                     exception
                        when Dynamic_Library_Error =>
                           Text_IO.Put_Line ("Error when loading " & Path);
                           Flag_As_Error (Path => Path);
                           exit Load_Libraries_Loop;
                     end Initialization;

                     Library.Unregister_Callback :=
                       Plugins.New_Unload_CB (Path);
                     Library.Reload_Callback     := Plugins.New_Reload_CB;

                     Gwiad.Plugins.Register
                       (Path      => Path,
                        Unload_CB => Library.Unregister_Callback,
                        Reload_CB => Library.Reload_Callback);
                     Init (Library.all, Path);

                     Loaded_Libraries.Insert (Path, Library);
                     Registered_Libraries.Insert (Path, Library);
                  end if;
               end Load_Library;
            end loop Load_Libraries_Loop;
         end Discover_Libraries;

      begin
         --  Search for websites libraries

         Discover_Libraries (From => Websites_Lib_Dir);

         --  Search for services libraries

         Discover_Libraries (From => Services_Lib_Dir);

      exception
         when E : others =>
            Text_IO.Put_Line (Exception_Information (E));
      end Discover_Libraries;

      ----------
      -- Load --
      ----------

      procedure Load (Path : in String) is
         Library : Dynamic_Library_Access;
      begin
         Library := Load (Path);
         Library.Unregister_Callback := Plugins.New_Unload_CB (Path);
         Library.Reload_Callback     := Plugins.New_Reload_CB;
         Gwiad.Plugins.Register (Path      => Path,
                                 Unload_CB => Library.Unregister_Callback,
                                 Reload_CB => Library.Reload_Callback);
         Init (Library.all, Path);
         Loaded_Libraries.Insert (Path, Library);
         Registered_Libraries.Insert (Path, Library);
      end Load;

      ----------------
      -- Reload_All --
      ----------------

      procedure Reload_All is
         Position : Cursor := Registered_Libraries.First;
      begin
         while Has_Element (Position) loop
            Plugins.Call (Element (Position).Reload_Callback);
            Position := Next (Position);
         end loop;
      end Reload_All;

      ------------
      -- Unload --
      ------------

      procedure Unregister (Path : in String) is
         Library : Dynamic_Library_Access;
      begin
         if not Loaded_Libraries.Contains (Path) then
            raise Dynamic_Library_Manager_Error with Path;
         end if;

         Library := Loaded_Libraries.Element (Path);
         Plugins.Call (Library.Unregister_Callback);
         Registered_Libraries.Delete (Path);
      end Unregister;

      ----------------
      -- Unload_All --
      ----------------

      procedure Unregister_All is
         Position : Cursor := Registered_Libraries.First;
      begin
         while Has_Element (Position) loop
            Unload_Library :
            declare
               Path    : constant String                 := Key (Position);
               Library : constant Dynamic_Library_Access := Element (Position);
            begin
               Plugins.Call (Library.Unregister_Callback);
               Registered_Libraries.Delete (Path);
            end Unload_Library;
            Position := Registered_Libraries.First;
         end loop;
      end Unregister_All;
   end Manager;

end Gwiad.Dynamic_Libraries.Manager;
