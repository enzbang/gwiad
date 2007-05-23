
generic

   type Parameter_Name is (<>);
   --  The list of parameter names to handle

   Max_Vector_Size : Positive := 1_024;
   --  Maximum size of a vector in the config file

package Gwiad.Iniparser is

   --------
   -- IO --
   --------

   package IO is

      procedure Open (Config_File_Name : in String);

      procedure Close;
      --  Close and discard changes

      procedure Save_Close;
      --  Close and save changes

      Unknown_Parameter : exception;
      --  Raised when a parameter name is not known

      Uncomplete_Config : exception;
      --  Raised when some parameters are not specified in the config file

   end IO;

   --  Accessor functions for simple types

   function Get_Value (Parameter : in Parameter_Name) return String;
   function Get_Value (Parameter : in Parameter_Name) return Integer;
   function Get_Value (Parameter : in Parameter_Name) return Float;
   function Get_Value (Parameter : in Parameter_Name) return Boolean;

   --  To change the value of a parameter of simple types

   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in String);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Integer);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Float);
   procedure Set_Value
     (Parameter : in Parameter_Name; Value : in Boolean);

   --  Handle enumeration types

   generic
      type Enum is (<>);
   package Enum_Values is

      function Get_Value (Parameter : in Parameter_Name) return Enum;

      procedure Set_Value
        (Parameter : in Parameter_Name; Value : in Enum);

   end Enum_Values;

   --  Handle vectors parameters

   generic
      type T is private;
      type Vector is array (Positive range <>) of T;
      with procedure Get
        (From : in     String;                               --  String -> T
         Item :    out T;
         Last :    out Positive) is <>;
      with function Image (Item : in T) return String is <>; --  T -> String
   package Vector_Values is

      function Get_Value (Parameter : in Parameter_Name) return Vector;

      procedure Set_Value
        (Parameter : in Parameter_Name; Value : in Vector);

   end Vector_Values;

end Gwiad.Iniparser;
