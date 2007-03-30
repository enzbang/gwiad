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

with System;
with Interfaces.C.Strings;
with Unchecked_Conversion;
with Unchecked_Deallocation;
with Ada.Directories;

package body Gwiad.Dynamic_Libraries is

   use Interfaces.C.Strings;
   use Interfaces.C;
   use System;

   type Implementation is new System.Address;

   function dlerror return chars_ptr;
   pragma Import (C, dlerror, "dlerror");

   procedure Free is
     new Unchecked_Deallocation (Object => Implementation,
                                 Name   => Reference);
   ----------
   -- Call --
   ----------

   procedure Call
     (Library       : in  Dynamic_Library;
      Function_Name : in  String;
      Call_Function : out Call_Function_Access)
   is

      function dlsym
        (Handle   : System.Address;
         Sym_Name : Interfaces.C.Strings.chars_ptr)
         return System.Address;
      pragma Import (C, dlsym, "dlsym");

      function Address_To_Access is new
        Unchecked_Conversion (Source => System.Address,
                              Target => Call_Function_Access);

      Addr : System.Address;

      C_Function_Name : chars_ptr := New_String (Function_Name);

   begin

      --  Get a pointer to the function within the dynamic library

      Addr := dlsym (System.Address (Library.Ref.all), C_Function_Name);

      Free (C_Function_Name);

      if Addr = System.Null_Address then
         raise Dynamic_Library_Error with Value (dlerror);
      end if;

      Call_Function := Address_To_Access (Addr);
   end Call;

   ---------------------------
   -- Get_Library_Extension --
   ---------------------------

   function Get_Library_Extension return String is
   begin
      return "so";
   end Get_Library_Extension;

   ----------
   -- Init --
   ----------

   procedure Init (Library : Dynamic_Library; Path : String) is
      use Interfaces;
      use Ada.Directories;

      Lib_Name  : constant String := Base_Name (Path);
      Init_Name : constant String :=
                    Lib_Name (Lib_Name'First + 3 .. Lib_Name'Last) & "init";

      type Library_Register is access procedure;
      procedure Register_Call is new Call (Library_Register);

      Access_Function : Library_Register;

   begin
      Register_Call (Library, Init_Name, Access_Function);
      Access_Function.all;
   end Init;

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Dynamic_Library_Access is

      function Dlopen
        (Lib_Name : chars_ptr; Mode : int)
        return System.Address;
      pragma Import (C, Dlopen, "dlopen");

      RTLD_LAZY : constant := 1;
      C_Path    : chars_ptr := New_String (Path);
      Result    : constant Dynamic_Library_Access := new Dynamic_Library;

   begin
      Result.Ref := new Implementation;
      Result.Ref.all := Implementation (Dlopen (C_Path, RTLD_LAZY));

      Free (C_Path);

      if Result.Ref.all = Implementation (System.Null_Address) then
         Free (Result.Ref);
         raise Dynamic_Library_Error with Value (dlerror);
      end if;
      return Result;
   end Load;

   ------------
   -- Unload --
   ------------

   procedure Unload (Library : in out Dynamic_Library) is
      function dlclose (Handle : System.Address) return Interfaces.C.int;
      pragma Import (C, dlclose, "dlclose");

      Result : int := 1;
   begin
      Result := dlclose (System.Address (Library.Ref.all));

      Free (Library.Ref);

      if Result /= 0 then
         raise Dynamic_Library_Error with Value (dlerror);
      end if;
   end Unload;

end Gwiad.Dynamic_Libraries;
