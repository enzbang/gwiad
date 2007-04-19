------------------------------------------------------------------------------
--                               Diouzhtu                                   --
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
with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;

with AWS.MIME;
with AWS.Messages;
with AWS.Parameters;
with AWS.Services.ECWF.Registry;

with Gwiad.Services.Register;

with Wiki_Interface;

with Wiki_Website.Config;

package body Wiki_Website.Callbacks is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use Wiki_Website;
   use Wiki_Website.Config;
   use Wiki_Interface;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : in Status.Data) return Response.Data is
      use type Messages.Status_Code;
      URI          : constant String := Status.URI (Request);
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin

      Web_Page := AWS.Services.ECWF.Registry.Build
        (URI, Request, Translations, Cache_Control => Messages.Prevent_Cache);

      if Response.Status_Code (Web_Page) = Messages.S404 then
         --  Page not found
         return Response.Build
           (Content_Type  => MIME.Text_HTML,
            Message_Body  => "<p>Error</p>");
      else
         return Web_Page;
      end if;
   end Default_Callback;

   ---------------
   -- Edit_Page --
   ---------------

   procedure Edit_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      use AWS.Status;
      pragma Unreferenced (Context);

      Get_URI  : constant String := URI (Request);
      Filename : constant String :=
                   Wiki_Text_Dir & "/" & Get_Filename (Get_URI);

      Text_Plain : Unbounded_String;
      Text_File  : File_Type;

   begin

      if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
         Templates.Insert (Translations,
                           Templates.Assoc ("ERROR", "<p>Service down</p>"));
         return;
      end if;

      if Ada.Directories.Exists (Filename) then
         Open (File => Text_File,
               Mode => In_File,
               Name => Filename);

         while not End_Of_File (File => Text_File) loop
            Append (Text_Plain, Get_Line (Text_File));
            Append (Text_Plain, ASCII.LF);
         end loop;

         Close (File => Text_File);
      end if;

      Templates.Insert (Translations,
                        Templates.Assoc ("TEXT_PLAIN", Text_Plain));
   end Edit_Page;

   -------------------
   -- Edit_Template --
   -------------------

   function Edit_Template (Request : in Status.Data) return String is
      pragma Unreferenced (Request);
   begin
      return Wiki_Template_Dir & "/edit.thtml";
   end Edit_Template;

   ------------------
   -- Preview_Page --
   ------------------

   procedure Preview_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      P          : constant Parameters.List := Status.Parameters (Request);
      Save       : constant String          := Parameters.Get (P, "save");
      Name       : constant String          := Parameters.Get (P, "filename");
      Text_Plain : constant String          := Parameters.Get
        (P, "text_plain");
   begin
      Ada.Text_IO.Put_Line (Text_Plain);

      if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
         Templates.Insert (Translations,
                           Templates.Assoc ("ERROR", "<p>Service down</p>"));
         return;
      end if;

      declare
         Get_Service : GW_Service'Class := Get_Wiki_Service;
      begin
         if Save /= "" then
            declare
               Text_File : File_Type;
               Filename  : constant String := Wiki_Text_Dir & "/" & Name;
            begin

               if Ada.Directories.Exists (Filename) then
                  --  Here we should add RCS

                  Ada.Directories.Delete_File (Filename);
               end if;

               Create (File => Text_File,
                       Mode => Out_File,
                       Name => Filename);

               Put (File => Text_File,
                    Item => Text_Plain);

               Close (File => Text_File);
            end;
         else
            Templates.Insert
              (Translations,
               Templates.Assoc
                 ("PREVIEW", HTML_Preview (Get_Service, Text_Plain)));
         end if;
      end;
   end Preview_Page;

   ----------------------
   -- Preview_Template --
   ----------------------

   function Preview_Template (Request : in Status.Data) return String is
      P    : constant Parameters.List := Status.Parameters (Request);
      Save : constant String          := Parameters.Get (P, "save");
   begin
      if Save /= "" then
         return Wiki_Template_Dir & "/view.thtml";
      else
         return Wiki_Template_Dir & "/preview.thtml";
      end if;
   end Preview_Template;

   ---------------
   -- View_Page --
   ---------------

   procedure View_Page
     (Request      : in     Status.Data;
      Context      : access AWS.Services.ECWF.Context.Object;
      Translations : in out Templates.Translate_Set)
   is
      pragma Unreferenced (Context);
      use AWS.Status;

      Get_URI   : constant String := URI (Request);
      Filename  : constant String := Get_Filename (Get_URI);
--                      Get_URI
--          (Wiki_Web_Dir'Length + Wiki_Web_View'Length + 1 .. Get_URI'Last);
      HTML_Text : Unbounded_String := Null_Unbounded_String;
      HTML_File : File_Type;
   begin

      Ada.Text_IO.Put_Line (Filename);

      if Ada.Directories.Exists (Wiki_HTML_Dir & "/" & Filename) then

         Open (File => HTML_File,
               Mode => In_File,
               Name => Wiki_HTML_Dir & "/" & Filename);

         while not End_Of_File (HTML_File) loop
            Append (HTML_Text, Get_Line (HTML_File));
            Append (HTML_Text, ASCII.LF);
         end loop;

         Close (HTML_File);

         Templates.Insert
           (Translations, Templates.Assoc ("VIEW", HTML_Text));

      else
         if not Gwiad.Services.Register.Exists (Wiki_Service_Name) then
            Templates.Insert
              (Translations,
               Templates.Assoc ("ERROR", "<p>Service down</p>"));
         end if;

         declare
            Get_Service : GW_Service'Class := Get_Wiki_Service;
            New_HTML    : constant String  := HTML (Get_Service, Filename);
         begin
            Templates.Insert
              (Translations,
               Templates.Assoc ("VIEW", New_HTML));

            Create (File => HTML_File,
                    Mode => Out_File,
                    Name => Wiki_HTML_Dir & "/" & Filename);

            Put (HTML_File, New_HTML);

            Close (HTML_File);
         end;
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Templates.Insert
           (Translations, Templates.Assoc ("ERROR", "<p>Service down</p>"));
   end View_Page;

   -------------------
   -- View_Template --
   -------------------

   function View_Template (Request : in Status.Data) return String is
      pragma Unreferenced (Request);
   begin
      return Wiki_Template_Dir & "/view.thtml";
   end View_Template;

end Wiki_Website.Callbacks;
