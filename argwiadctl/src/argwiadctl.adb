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
with GNAT.Case_Util;

with Gwiad;

procedure Argwiadctl is

   use Ada;

   Argwiadctl_Reload_File : String renames Gwiad.Reload_File;

   Argwiadctl_Version : constant String := "argwiadctl version 0.1";
   --  argwiadctl version

   PID_Filename     : constant String := ".pid";
   Argwiad_Root_Env : constant String := "ARGWIAD_ROOT";

   type Options is (Start, Reload, Stop, Restart, Version);
   subtype Cmd_Options is Options range Start .. Restart;
   subtype Others_Options is Options range Version .. Version;

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
      Text_IO.Put ("usage: argwiadctl [--version] ");
      for Opt in Cmd_Options'Range loop
         declare
            Option_Name : String := Cmd_Options'Image (Opt);
         begin
            GNAT.Case_Util.To_Lower (Option_Name);
            if Opt /= Cmd_Options'Last then
               Text_IO.Put (Option_Name & "|");
            else
               Text_IO.Put (Option_Name & " ");
            end if;
         end;
      end loop;

      Text_IO.Put ("[directory]");
   end Usage;

   use Ada.Command_Line;

begin
   for K in 1 .. Argument_Count loop
      if Argument (K)'Length > 2
        and then Argument (K)
        (Argument (K)'First .. Argument (K)'First + 1) = "--"
      then
         case Others_Options (Others_Options'Value
                              (Argument (K)
              (Argument (K)'First + 2 .. Argument (K)'Last))) is
            when Version => Text_IO.Put_Line (Argwiadctl_Version);
               exit;
         end case;
      else
         if K + 1 < Argument_Count then
            if Directories.Exists (Argument (K + 1)) then
               --  If a directory is specified, use it as gwiad default
               --  directory

               Directories.Set_Directory (Argument (K + 1));
            elsif Environment_Variables.Exists (Argwiad_Root_Env) then

               --  If no directory specified but ARGWIAD_ROOT env var not null
               --  then use it as gwiad default directory

               Directories.Set_Directory
                 (Environment_Variables.Value (Argwiad_Root_Env));
            end if;
         end if;

         case Cmd_Options (Cmd_Options'Value (Argument (K))) is
            when Start =>
               Start;
            when Stop =>
               Stop;
            when Reload =>
               Reload;
            when Restart =>
               Stop;
               Start;
         end case;
      end if;
   end loop;

exception
   when Constraint_Error =>
      --  Wrong options. Display usage info.
      Usage;
   when others =>
      Text_IO.Put_Line ("Unknown error");
      Usage;
end Argwiadctl;
