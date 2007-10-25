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
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;

with GNAT.OS_Lib;

with Gwiad;

procedure Argwiadctl is

   use Ada;

   Argwiadctl_Reload_File : String renames Gwiad.Reload_File;

   PID_Filename     : constant String := ".pid";
   Argwiad_Root_Env : constant String := "ARGWIAD_ROOT";

   type Options is (Start, Reload, Stop);

   procedure Reload;
   --  Ask gwiad to reload itself by creating a file named .gwiad_do_reload.
   --  Wait for the file deletion as it means that gwiad as properly
   --  been reloaded.

   procedure Start;
   --  Start argwiad web server

   procedure Stop;
   --  Stop argwiad web server

   procedure Usage;
   --  Show good command line usage

   ------------
   -- Reload --
   ------------

   procedure Reload is
      use Ada.Text_IO;
      Nb_Retry        : constant := 10;
      Ask_Reload_File : File_Type;
   begin
      Create (File => Ask_Reload_File,
              Mode => Out_File,
              Name => Argwiadctl_Reload_File);
      New_Line (Ask_Reload_File);
      Close (Ask_Reload_File);

      --  Wait for file deletion

      for K in 1 .. Nb_Retry loop
         if not Directories.Exists (Argwiadctl_Reload_File) then
            return;
         end if;
         delay 1.0;
         Text_IO.Put ("...");
      end loop;
      Text_IO.Put ("Timeout ! Abort");
   end Reload;

   -----------
   -- Start --
   -----------

   procedure Start is
      use GNAT;
      use Ada.Text_IO;
      use type OS_Lib.String_Access;

      Argwiad_Command : aliased String       := "./bin/argwiad";
      Gwiad_Arg       : constant OS_Lib.Argument_List :=
                          (1 => Argwiad_Command'Unchecked_Access);

      Gwiad_PID       : OS_Lib.Process_Id;
      Nohup_Command   : OS_Lib.String_Access :=
                          OS_Lib.Locate_Exec_On_Path ("nohup");
   begin
      --  Check if Gwiad PID file exists.

      if Nohup_Command = null then
         Put_Line ("No nohup command required by argwiadctl");
      else
         if Directories.Exists (Name => PID_Filename) then
            Put_Line (Directories.Current_Directory & "/" & PID_Filename
                      & " exists ! Gwiad may be running. If it not the case, "
                      & "please remove the pid file and "
                      & "try to restart argwiadctl");
         elsif not Directories.Exists (Argwiad_Command) then
            Put_Line ("Can not find Argwiad !");
         else
            Gwiad_PID := OS_Lib.Non_Blocking_Spawn
              (Program_Name => Nohup_Command.all,
               Args         => Gwiad_Arg,
               Output_File  => "gwiad.out",
               Err_To_Out   => True);

            Put_Line ("Start argwiad with PID : "
                      & Integer'Image (OS_Lib.Pid_To_Integer (Gwiad_PID)));
            delay 1.0;

            Save_PID : declare
               PID_File : File_Type;
            begin
               Create (File => PID_File,
                       Name => PID_Filename,
                       Mode => Out_File);

               Put_Line (PID_File, Integer'Image
                         (OS_Lib.Pid_To_Integer (Gwiad_PID)));

               Close (PID_File);
            end Save_PID;
         end if;
         OS_Lib.Free (Nohup_Command);
      end if;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
      use Ada.Text_IO;
      use GNAT;
      PID_File : File_Type;
   begin

      Open (File => PID_File,
            Mode => In_File,
            Name => PID_Filename);

      Kill_Process : declare
         PID : aliased String := Get_Line (PID_File);
         Result : Boolean;
      begin
         Put_Line (PID);
         GNAT.OS_Lib.Spawn (Program_Name => "/bin/kill",
                            Args         => OS_Lib.Argument_List'
                              (1 => PID'Unchecked_Access),
                            Success      => Result);

         if Result then
            Put_Line ("Kill succeed");
         else
            Put_Line ("Failed");
         end if;
      end Kill_Process;

      Close (PID_File);
      Directories.Delete_File (PID_Filename);
   end Stop;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Text_IO.Put ("argwiadctl ");
      for Opt in Options'Range loop
         Text_IO.Put (Options'Image (Opt) & " ");
      end loop;
   end Usage;

   use Ada.Command_Line;

begin
   --  Only one option for now

   if Argument_Count < 1 or else Argument_Count > 2 then
      Usage;
   else
      if Argument_Count = 2 and then Directories.Exists (Argument (2)) then

         --  If a directory is specified, use it as gwiad default directory

         Directories.Set_Directory (Argument (2));
      elsif Environment_Variables.Exists (Argwiad_Root_Env) then

         --  If no directory specified but ARGWIAD_ROOT env var not null then
         --  use it as gwiad default directory

         Directories.Set_Directory
           (Environment_Variables.Value (Argwiad_Root_Env));
      end if;

      Force_Valid_Option : begin
         case Options'Value (Argument (1)) is
            when Start =>
               Start;
            when Stop =>
               Stop;
            when Reload =>
               Reload;
         end case;
      exception
         when Constraint_Error =>
            Usage;
      end Force_Valid_Option;
   end if;

end Argwiadctl;