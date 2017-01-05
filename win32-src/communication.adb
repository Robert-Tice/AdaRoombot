------------------------------------------------------------------------------
--                              AdaRoombot                                  --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Strings;

with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package body Communication is

    Parity   : constant Parity_Check := None;
    Bits     : constant Data_Bits := CS8;
    End_Bits : constant Stop_Bits_Number := One;
    Control  : constant Flow_Control := None;
    G_Port   : aliased Serial_Port;

    function Communication_Init (BC      : Baud_Code := Default_Baud;
                                 COM_Num : Natural := Default_COM_Num)
                                 return access Ada.Streams.Root_Stream_Type'Class
    is
        PName : constant Port_Name := Name (Number => COM_Num);
        Rate  : Data_Rate;
    begin
        Open (Port => G_Port,
              Name => PName);
        case BC is
            when B300 =>
                Rate := B300;
            when B600 =>
                Rate := B600;
            when B1200 =>
                Rate := B1200;
            when B2400 =>
                Rate := B2400;
            when B4800 =>
                Rate := B4800;
            when B9600 =>
                Rate := B9600;
            when B19200 =>
                Rate := B19200;
            when B38400 =>
                Rate := B38400;
            when B57600 =>
                Rate := B57600;
            when others =>
                Rate := B115200;
        end case;

        Set (Port      => G_Port,
             Rate      => Rate,
             Bits      => Bits,
             Stop_Bits => End_Bits,
             Parity    => Parity,
             Block     => True,
             Local     => True,
             Flow      => Control);
        Is_Init := True;
        return G_Port'Access;
    end Communication_Init;

    procedure Set_Host_Baud (Port : access Ada.Streams.Root_Stream_Type'Class;
                             BC   : Integer)
    is
    begin
        null;
    end Set_Host_Baud;

    procedure Communications_Close (Port : access Ada.Streams.Root_Stream_Type'Class)
    is
        SPort : access Serial_Port := Serial_Port(Port.all)'Access;
    begin
        GNAT.Serial_Communications.Close (Port => SPort.all);
        Is_Init := False;
    end Communications_Close;

end Communication;
