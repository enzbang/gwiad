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

with Gwiad.Web;
with Gwiad.Dynamic_Libraries.Manager;
with Ada.Text_IO;
with Ada.Exceptions;

procedure Argwiad is

   use Gwiad;
   use Ada.Exceptions;

   Discover_Library : Dynamic_Libraries.Manager.Discover;

begin

   Web.Start;
   Web.Wait;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Exception_Information (E));
end Argwiad;
