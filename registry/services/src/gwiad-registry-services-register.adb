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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Utils;

package body Gwiad.Registry.Services.Register is

   use Ada;
   use Ada.Exceptions;

   package Service_Access_Cache is new Containers.Indefinite_Hashed_Maps
     (String, Service_Access, Ada.Strings.Hash, "=", "=");

   Last_Library_Path : Unbounded_String;
   Service_Map       : Register_Maps.Map;
   Cache             : Service_Access_Cache.Map;

   -----------------
   -- Description --
   -----------------

   function Description (Position : in Cursor) return String is
      RS : constant Registered_Service :=
             Register_Maps.Element
               (Position => Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Description);
   end Description;

   ------------
   -- Exists --
   ------------

   function Exists (Name : in String) return Boolean is
   begin
      return Register_Maps.Contains (Service_Map, Name);
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Key : in String) return Cursor is
   begin
      return Cursor (Register_Maps.Find (Service_Map, Key));
   end Find;

   -----------
   -- First --
   -----------

   function First return Cursor is
   begin
      return Cursor (Service_Map.First);
   end First;

   ---------
   -- Get --
   ---------

   function Get
     (Name : in String;
      Id   : in Service_Id := Null_Service_Id) return Service_Access is
   begin
      if Id /= Null_Service_Id then
         return Service_Access_Cache.Element
           (Container => Cache,
            Key       => Name & String (Id));
      end if;

      declare
         RS : constant Registered_Service :=
                Register_Maps.Element
                  (Container => Service_Map,
                   Key       => Name);
      begin
         return Service_Access (RS.Builder.all);
      end;
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         raise Service_Error;
   end Get;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : in Cursor) return Boolean is
   begin
      return Register_Maps.Has_Element (Register_Maps.Cursor (Position));
   end Has_Element;

   ----------
   -- Name --
   ----------

   function Name (Position : in Cursor) return String is
   begin
      return  Register_Maps.Key (Position => Register_Maps.Cursor (Position));
   end Name;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      Register_Maps.Next (Register_Maps.Cursor (Position));
   end Next;

   ----------
   -- Path --
   ----------

   function Path (Position : in Cursor) return String is
      RS : constant Registered_Service :=
             Register_Maps.Element (Register_Maps.Cursor (Position));
   begin
      return To_String (RS.Path);
   end Path;

   --------------
   -- Register --
   --------------

   procedure Register (Library_Path : in String) is
   begin
      Last_Library_Path := To_Unbounded_String (Library_Path);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name        : in String;
      Description : in String;
      Builder     : in Service_Builder) is
   begin
      if Last_Library_Path = Null_Unbounded_String then
         raise Service_Error;
      end if;

      Register_Maps.Insert
        (Service_Map,
         Name,
         (Builder     => Builder,
          Path        => Last_Library_Path,
          Description => To_Unbounded_String (Description)));

      Last_Library_Path := Null_Unbounded_String;
   end Register;

   ---------
   -- Set --
   ---------

   function Set
     (Name : in String; Item : in Service_Access) return Service_Id
   is
      function Generate_Id return Service_Id;
      --  Returns a session ID. This ID is not certified to be uniq in the
      --  system. It is required that the caller check for uniqness if
      --  necessary. (imported from AWS)

      -----------------
      -- Generate_Id --
      -----------------

      function Generate_Id return Service_Id is

         type NID is new AWS.Utils.Random_Integer;

         Chars : constant String
           := "0123456789"
             & "abcdefghijklmnopqrstuvwxyz"
             & "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
         Rand   : NID := 0;
         Result : Service_Id;

      begin
         for I in Service_Id'Range loop
            if Rand = 0 then
               Rand := Random;
            end if;

            Result (I) := Chars (Integer (Rand rem Chars'Length) + 1);
            Rand := Rand / Chars'Length;
         end loop;

         return Result;
      end Generate_Id;

      SID     : Service_Id;
      Cursor  : Service_Access_Cache.Cursor;
      Success : Boolean := False;

   begin
      loop
         SID := Generate_Id;

         if SID /= Null_Service_Id then
            Service_Access_Cache.Insert
              (Cache, Name & String (SID), Item, Cursor, Success);
         end if;

         exit when Success;
      end loop;
      return SID;
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Information (E));
         raise Service_Error;
   end Set;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Name : in String) is
      use Register_Maps;

      Position : Register_Maps.Cursor := Service_Map.Find (Name);
   begin
      if Position = No_Element then
         raise Service_Error;
      end if;

      Service_Map.Delete (Position);
   end Unregister;

end Gwiad.Registry.Services.Register;
