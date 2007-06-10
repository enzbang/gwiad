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
with Ada.Exceptions;

with Gwiad.Iniparser;
with Gwiad.Config;

package body Gwiad.Config.Settings is

   use Ada;

   Config_Filename : constant String := "gwiad.ini";

   type Attributes is (Web_Default_Page);

   package Conf is new Gwiad.Iniparser (Attributes);

   ----------------------
   -- Web_Default_Page --
   ----------------------

   function Web_Default_Page return String is
   begin
      return Conf.Get_Value (Web_Default_Page);
   end Web_Default_Page;

begin
   --  Set default values

   Conf.Set_Value (Web_Default_Page, Gwiad.Config.Web_Default_Page);

   --  Now read the config file if any

   Conf.IO.Open (Config_Filename);
   Conf.IO.Close;

exception
   when Conf.IO.Uncomplete_Config =>
      Conf.IO.Close;
   when UP : Conf.IO.Unknown_Parameter =>
      Text_IO.Put_Line (Exceptions.Exception_Message (UP));
      Conf.IO.Close;
   when Text_IO.Name_Error =>
      null;
end Gwiad.Config.Settings;
