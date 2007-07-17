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
with Ada.Strings.Unbounded;

with AWS.Parameters;

with Morzhol.Iniparser;
with Morzhol.Strings;

with Gwiad.Plugins.Websites.Registry;
with Gwiad.Dynamic_Libraries.Manager;
with Gwiad.Web.Virtual_Host;

with Gwiad.Admin.Template_Defs.Websites_List;
with Gwiad.Admin.Template_Defs.Websites_Stop;
with Gwiad.Admin.Template_Defs.Websites_Unload;


package body Gwiad.Admin.Websites is

   use Ada;
   use Ada.Strings.Unbounded;

   use Morzhol.Strings;

   use Gwiad;
   use Gwiad.Plugins.Websites;
   use Gwiad.Admin.Template_Defs;


   type Attribute is (Document_Root, Default_Page, Secure, Virtual_Host);
   package Conf is new Morzhol.Iniparser (Attribute);

   Config_Root : constant String := "config";

   ---------------------------------------
   -- Discover_Virtual_Host_Directories --
   ---------------------------------------

   procedure Discover_Virtual_Host_Directories is
      use Ada.Directories;
      S : Search_Type;
      D : Directory_Entry_Type;
   begin

      if not Directories.Exists (Name => Config_Root) then
         Directories.Create_Directory (New_Directory => Config_Root);
      end if;

      Start_Search (Search    => S,
                    Directory => Config_Root,
                    Pattern   => "*.ini",
                    Filter    => Filter_Type'(Ordinary_File => True,
                                              Special_File  => False,
                                              Directory     => False));

      while More_Entries (S) loop
         Get_Next_Entry (S, D);
         declare
            Name : constant String := Simple_Name (D);
         begin
            if Name /= "." and Name /= ".." then
               --  Now read the config file if any

               Conf.IO.Open (Full_Name (D));
               Conf.IO.Close;

               declare
                  use Gwiad.Plugins.Websites.Registry;
                  Conf_File_Document_Root : constant String :=
                                              Conf.Get_Value (Document_Root);
                  Conf_File_Default_Page  : constant String :=
                                              Conf.Get_Value (Default_Page);
                  Conf_File_Virtual_Host  : constant String :=
                                              Conf.Get_Value (Virtual_Host);

                  VH_Dir : constant Web.Virtual_Host.Virtual_Host_Directory
                    := Web.Virtual_Host.Virtual_Host_Directory'
                      (Document_Root => +Conf_File_Document_Root,
                       Default_Page  => +Conf_File_Default_Page,
                       Secure        => Conf.Get_Value (Secure));
               begin
                  Web.Virtual_Host.Register
                    (Hostname => Conf.Get_Value (Virtual_Host),
                     VH_Dir   => VH_Dir);
                  Gwiad.Plugins.Websites.Registry.Register
                    (Name         =>
                       Gwiad.Plugins.Websites.Website_Name
                         (Conf_File_Virtual_Host),
                     Description  =>
                       "Virtual Host " & Conf.Get_Value (Virtual_Host) &
                     " at Document Root = " & (-VH_Dir.Document_Root),
                     Unregister   =>
                       Virtual_Host_Unregister'Access,
                     Library_Path => "libgwiad_website_admin.so");
               end;
            end if;
         exception
            when Conf.IO.Uncomplete_Config =>
               Ada.Text_IO.Put_Line ("uncomplete");
               Conf.IO.Close;
            when UP : Conf.IO.Unknown_Parameter =>
               Text_IO.Put_Line ("Unknown Parameter : "
                                 & Exceptions.Exception_Message (UP));
               Conf.IO.Close;
            when Text_IO.Name_Error =>
               Ada.Text_IO.Put_Line ("Does not exit");
               null;
         end;
      end loop;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Discover_Virtual_Host_Directories;

   -------------------
   -- List_Websites --
   -------------------

   procedure List_Websites
     (Request      : in     Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Request, Context);
      use AWS.Templates;
      use Gwiad.Plugins.Websites.Registry.Map;

      Position     : Cursor := First;

      Names        : Templates.Tag;
      Simple_Paths : Templates.Tag;
      Paths        : Templates.Tag;
      Descriptions : Templates.Tag;

   begin
      while Has_Element (Position) loop
         Names        := Names & String (Name (Position));
         Paths        := Paths & Path (Position);
         Simple_Paths := Simple_Paths
           & Directories.Simple_Name (Path (Position));
         Descriptions := Descriptions & Description (Position);
         Next (Position);
      end loop;

      Templates.Insert (Translations,
                        Templates.Assoc (Websites_List.NAME, Names));

      Templates.Insert (Translations,
                        Templates.Assoc (Websites_List.PATH, Paths));

      Templates.Insert (Translations,
                        Templates.Assoc
                          (Websites_List.DESCRIPTION, Descriptions));

      Templates.Insert
        (Translations,
         Templates.Assoc (Websites_List.SIMPLE_PATH, Simple_Paths));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Websites_List.WEBSITES_ADMIN_URL, Admin_URL & Websites_URL));
   end List_Websites;

   ------------------
   -- Stop_Website --
   ------------------

   procedure Stop_Website
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context, Translations);
      use Gwiad.Plugins.Websites.Registry;

      P            : constant Parameters.List := Status.Parameters (Request);
      Name         : constant String          :=
                       Parameters.Get (P, Websites_Stop.HTTP.website);

      Library_Path : Unbounded_String := Null_Unbounded_String;

   begin

      declare
         Position : constant Map.Cursor := Map.Find (Website_Name (Name));
      begin
         if Map.Has_Element (Position) then
            Library_Path := +Map.Path (Position);
         end if;
      end;

      if Library_Path /= "" then
         Ada.Text_IO.Put_Line ("Unregister " & Name);
         Unregister (Website_Name (Name));
      else
         Ada.Text_IO.Put_Line ("No library path");
      end if;
   end Stop_Website;

   --------------------
   -- Unload_Website --
   --------------------

   procedure Unload_Websites
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use AWS.Templates;
      use Gwiad.Plugins.Websites.Registry;
      use Dynamic_Libraries.Manager;

      P            : constant Parameters.List := Status.Parameters (Request);
      Library_Path : constant String :=
                       Parameters.Get (P, Websites_Unload.HTTP.lib);
      Dry_Run      : constant String :=
                       Parameters.Get (P, Websites_Unload.HTTP.dry_run);

      Tag_Name : Templates.Tag;
   begin

      declare
         Position : Map.Cursor := Map.First;
      begin
         while Map.Has_Element (Position) loop
            if Map.Path (Position) /= Library_Path then
               Map.Next (Position);
            else
               if Dry_Run /= "" then
                  Tag_Name := Tag_Name & String (Map.Name (Position));
                  Map.Next (Position);
               else
                  declare
                     Last_Position : constant Map.Cursor := Position;
                  begin
                     Map.Next (Position);
                     Unregister (Map.Name (Last_Position));
                  end;
               end if;
            end if;
         end loop;
      end;

      Templates.Insert (Translations,
                        Templates.Assoc (Websites_Unload.PATH, Library_Path));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Websites_Unload.SIMPLE_PATH,
            Directories.Simple_Name (Library_Path)));

      Templates.Insert
        (Translations,
         Templates.Assoc
           (Websites_Unload.WEBSITES_ADMIN_URL, Admin_URL & Websites_URL));

      if Dry_Run /= "" then
         Templates.Insert (Translations,
                           Templates.Assoc (Websites_Unload.DRY_RUN, "yes"));
         Templates.Insert (Translations,
                           Templates.Assoc (Websites_Unload.NAME, Tag_Name));
      else
         Manager.Unload (Library_Path);
      end if;

   end Unload_Websites;

   ------------------------------
   -- Virtual_Host_Directories --
   ------------------------------

   procedure Virtual_Host_Directories
     (Request      : in Status.Data;
      Context      : access AWS.Services.Web_Block.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
   begin
      Discover_Virtual_Host_Directories;
      List_Websites (Request, Context, Translations);
   end Virtual_Host_Directories;

   -----------------------------
   -- Virtual_Host_Unregister --
   -----------------------------

   procedure Virtual_Host_Unregister (Name : in Website_Name) is
   begin
      Web.Virtual_Host.Unregister (String (Name));
   end Virtual_Host_Unregister;

end Gwiad.Admin.Websites;
