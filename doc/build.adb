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

with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS;
with Gwiad.Version;
with Templates_Parser;

procedure Build is

   use Ada;
   use Templates_Parser;

   File : Text_IO.File_Type;

   function Image (D : in Duration) return String;
   --  D string representation

   -----------
   -- Image --
   -----------

   function Image (D : in Duration) return String is
      D_S : constant String   := Duration'Image (D);
      I   : constant Positive := Strings.Fixed.Index (D_S, ".");
   begin
      return D_S (D_S'First + 1 .. I + 1);
   end Image;

   --  If a tag is added into this table make sure to update gen_doc.sed.tmplt

   T : constant Translate_Table
     := (Assoc ("GWIAD_VERSION", Gwiad.Version.Complete),
         Assoc ("AWS_VERSION", AWS.Version)
         );

begin
   --  Generates the documentation

   Text_IO.Put_Line
     (Parse ("gwiad.texi.tmplt", T, Keep_Unknown_Tags => True));

   --  Generates a script that can be used to create the documentation from
   --  the document template.

   Text_IO.Create (File, Text_IO.Out_File, "gen_doc.sed");

   declare
      use Strings.Fixed;
      Script : String :=
        Parse ("gen_doc.sed.tmplt", T, Keep_Unknown_Tags => True);
      I      : Natural;
   begin
      loop
         I := Index (Script, "x_");
         exit when I = 0;
         Replace_Slice (Script, I, I + 1, "@_");
      end loop;

      loop
         I := Index (Script, "_x");
         exit when I = 0;
         Replace_Slice (Script, I, I + 1, "_@");
      end loop;

      Text_IO.Put_Line (File, Script);
   end;
   Text_IO.Close (File);
end Build;
