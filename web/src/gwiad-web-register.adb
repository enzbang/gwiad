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

with Gwiad.Web.Main_Host;

package body Gwiad.Web.Register is

   --------------
   -- Register --
   --------------

   procedure Register
     (Web_Dir : in String;
      Action  : in AWS.Dispatchers.Handler'Class)
   is
   begin
      Main_Host.Register (Web_Dir, Action);
      Gwiad.Web.Reload.Require;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Web_Dir : in String) is
   begin
      Main_Host.Unregister (Web_Dir);
      Gwiad.Web.Reload.Require;
   end Unregister;

end Gwiad.Web.Register;
