------------------------------------------------------------------------------
--                                  Gwiad                                   --
--                                                                          --
--                           Copyright (C) 2007                             --
--                              Pascal Obry                                 --
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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System;

package body Gwiad.Dynamic_Libraries is

   use Ada;
   use Interfaces.C.Strings;
   use System;

   type Implementation is new System.Address;

   type HMODULE is new System.Address;
   type DWORD is new Integer;

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   procedure Free is new Unchecked_Deallocation
     (Object => Implementation, Name => Reference);

   ----------
   -- Call --
   ----------

   function Call
     (Library       : in Dynamic_Library;
      Function_Name : in String) return Call_Function_Access
   is

      function GetProcAddress
        (Handle    : in System.Address;
         Proc_Name : in chars_ptr) return System.Address;
      pragma Import (Stdcall, GetProcAddress, "GetProcAddress");

      function Address_To_Access is new
        Unchecked_Conversion (Source => System.Address,
                              Target => Call_Function_Access);

      Addr            : System.Address;
      C_Function_Name : chars_ptr := New_String (Function_Name);

   begin
      --  Get a pointer to the function within the dynamic library

      Addr := GetProcAddress
        (System.Address (Library.Ref.all), C_Function_Name);

      Free (C_Function_Name);

      if Addr = System.Null_Address then
         raise Dynamic_Library_Error
           with "Call returned code " & DWORD'Image (GetLastError);
      end if;

      return Address_To_Access (Addr);
   end Call;

   ---------------------------
   -- Get_Library_Extension --
   ---------------------------

   function Get_Library_Extension return String is
   begin
      return "dll";
   end Get_Library_Extension;

   ----------
   -- Init --
   ----------

   procedure Init (Library : in Dynamic_Library; Path : in String) is
      use Interfaces;
      use Ada.Directories;

      Lib_Prefix_Length : constant := 3;
      Lib_Name          : constant String := Base_Name (Path);
      Init_Name         : constant String   :=
                            Lib_Name (Lib_Name'First
                                      + Lib_Prefix_Length .. Lib_Name'Last)
                            & "init";

      type Library_Register is access procedure;
      function Register_Call is new Call (Library_Register);

   begin
      Register_Call (Library, Init_Name).all;
   end Init;

   ----------
   -- Load --
   ----------

   function Load (Path : in String) return Dynamic_Library_Access is

      function LoadLibrary (FileName : in chars_ptr) return System.Address;
      pragma Import (Stdcall, LoadLibrary, "LoadLibraryA");

      Result : constant Dynamic_Library_Access := new Dynamic_Library;
      C_Path : chars_ptr := New_String (Path);
      Errno  : DWORD;

   begin
      Result.Ref := new Implementation;
      Result.Ref.all := Implementation (LoadLibrary (C_Path));
      Errno := GetLastError;

      Free (C_Path);

      if Result.Ref.all = Implementation (System.Null_Address) then
         Free (Result.Ref);
         raise Dynamic_Library_Error
           with "Load returned code " & DWORD'Image (Errno);
      end if;
      return Result;
   end Load;

end Gwiad.Dynamic_Libraries;
