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

with Gwiad.Version;
with Morzhol.OS;

procedure Argwiadctl is

   use Ada;

   Argwiadctl_Reload_File : String renames Gwiad.Reload_File;
   DS : Character renames Morzhol.OS.Directory_Separator;

   Argwiadctl_Version : constant String :=
                          "argwiadctl " & Gwiad.Version.Simple;
   --  argwiadctl version

   Argwiad_Root_Env : constant String := "ARGWIAD_ROOT";
   Argwiad_Path_Env : constant String := "ARGWIAD_PATH";
   Argwiad_PID_Env  : constant String := "ARGWIAD_PID";
   Argwiad_Log_Env  : constant String := "ARGWIAD_LOG";

   Default_Tmp_Dir  : constant String := "/tmp";

   type Options is (Start, Reload, Stop, Restart, Version);
   subtype Cmd_Options is Options range Start .. Restart;
   subtype Others_Options is Options range Version .. Version;

   procedure Reload;
   --  Ask gwiad to reload itself by creating a file named .gwiad_do_reload.
   --  Wait for the file deletion as it means that gwiad as properly
   --  been reloaded.

   procedure Restart;
   --  Restart argwiad web server

   procedure Start;
   --  Start argwiad web server

   procedure Stop;
   --  Stop argwiad web server

   procedure Usage;
   --  Show good command line usage

   To_Run : access procedure := Usage'Access;

   function Log_Filename return String;
   --  Returns the log filename

   function PID_Filename return String;
   --  Returns the PID Filename

   ------------------
   -- Log_Filename --
   ------------------

   function Log_Filename return String is
   begin
      if Environment_Variables.Exists (Argwiad_Log_Env) then
         return Directories.Compose
           (Containing_Directory =>
              Environment_Variables.Value (Argwiad_Log_Env),
            Name                 => "argwiad",
            Extension            => "log");
      else
         return Directories.Compose
           (Containing_Directory => Default_Tmp_Dir,
            Name                 => "argwiad",
            Extension            => "log");
      end if;
   end Log_Filename;

   ------------------
   -- PID_Filename --
   ------------------

   function PID_Filename return String is
   begin
      if Environment_Variables.Exists (Argwiad_PID_Env) then
         return Directories.Compose
           (Containing_Directory =>
              Environment_Variables.Value (Argwiad_PID_Env),
            Name                 => "argwiad",
            Extension            => "pid");
      else
         return Directories.Compose
           (Containing_Directory => Default_Tmp_Dir,
            Name                 => "argwiad",
            Extension            => "pid");
      end if;
   end PID_Filename;

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

   -------------
   -- Restart --
   -------------

   procedure Restart is
   begin
      Stop;
      Start;
   end Restart;

   -----------
   -- Start --
   -----------

   procedure Start is
      use GNAT;
      use Ada.Text_IO;
      use type OS_Lib.String_Access;

      Argwiad_Command : aliased String := "./bin/argwiad";
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
            Put_Line (PID_Filename
                      & " exists ! Gwiad may be running. If it not the case, "
                      & "please remove the pid file and "
                      & "try to restart argwiadctl");

         elsif not Directories.Exists (Argwiad_Command) then
            Put_Line ("Can not find Argwiad ! Have you set "
                      & Argwiad_Root_Env & " ?");

         else
            if Morzhol.OS.Is_Windows then
               if Environment_Variables.Exists ("PATH") then
                  Environment_Variables.Set
                    (Name  => "PATH",
                     Value => Directories.Current_Directory & DS & "bin:"
                     & Environment_Variables.Value ("PATH"));
               else
                  Environment_Variables.Set
                    (Name  => "PATH",
                     Value => Directories.Current_Directory & DS & "bin");
               end if;

            else
               if  Environment_Variables.Exists ("LD_LIBRARY_PATH") then
                  Environment_Variables.Set
                    (Name  => "LD_LIBRARY_PATH",
                     Value => Directories.Current_Directory & DS & "bin:"
                     & Environment_Variables.Value ("LD_LIBRARY_PATH"));
               else
                  Environment_Variables.Set
                    (Name  => "LD_LIBRARY_PATH",
                     Value => Directories.Current_Directory & DS & "bin");
               end if;
            end if;

            Gwiad_PID := OS_Lib.Non_Blocking_Spawn
              (Program_Name => Nohup_Command.all,
               Args         => Gwiad_Arg,
               Output_File  => Log_Filename,
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

      --  Check if PID File exist

      if not Directories.Exists (Name => PID_Filename) then
         Put_Line (PID_Filename & " not found. Is argwiad running ?");
         return;
      end if;

      Open (File => PID_File,
            Mode => In_File,
            Name => PID_Filename);

      Kill_Process : declare
         PID : aliased String := Get_Line (PID_File);
         Result : Boolean;
      begin
         Put_Line (PID);
         GNAT.OS_Lib.Spawn
           (Program_Name => "/bin/kill",
            Args         => OS_Lib.Argument_List'(1 => PID'Unchecked_Access),
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
   if Argument_Count = 0 then
      Usage;
   end if;

   for K in 1 .. Argument_Count loop
      if Argument (K)'Length > 2
        and then Argument (K)
        (Argument (K)'First .. Argument (K)'First + 1) = "--"
      then
         case Others_Options
           (Others_Options'Value
              (Argument (K)
                 (Argument (K)'First + 2 .. Argument (K)'Last)))
         is
            when Version => Text_IO.Put_Line (Argwiadctl_Version);
               exit;
         end case;

      elsif Argument_Count > 1 and then Directories.Exists (Argument (K)) then
         --  If a directory is specified, use it as gwiad default
         --  directory

         Directories.Set_Directory (Argument (K));

      else
         if Environment_Variables.Exists (Argwiad_Root_Env) then
            --  If no directory specified but ARGWIAD_ROOT env var not null
            --  then use it as gwiad default directory.

            Directories.Set_Directory
              (Environment_Variables.Value (Argwiad_Root_Env));

         elsif Environment_Variables.Exists (Argwiad_Path_Env) then
            --  Otherwise test ARGWIAD_PATH env var for compatibility reasons.

            Directories.Set_Directory
              (Environment_Variables.Value (Argwiad_Path_Env));
         end if;

         if Environment_Variables.Exists (Argwiad_PID_Env) then
            Directories.Set_Directory
              (Environment_Variables.Value (Argwiad_PID_Env));
         end if;

         if Environment_Variables.Exists (Argwiad_Log_Env) then
            Directories.Set_Directory
              (Environment_Variables.Value (Argwiad_Log_Env));
         end if;

         case Cmd_Options (Cmd_Options'Value (Argument (K))) is
            when Start =>
               To_Run := Start'Access;
            when Stop =>
               To_Run := Stop'Access;
            when Reload =>
               To_Run := Reload'Access;
            when Restart =>
               To_Run := Restart'Access;
         end case;
      end if;
   end loop;

   To_Run.all;

exception
   when Constraint_Error =>
      Text_IO.Put_Line ("Wrong options !");
      --  Wrong options. Display usage info.
      Usage;
   when others =>
      Text_IO.Put_Line ("Unknown error");
      Usage;
end Argwiadctl;
