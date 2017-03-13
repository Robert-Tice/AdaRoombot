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

with STM32.Device; use STM32.Device;
with HAL; use HAL;

with Peripherals_Streaming; use Peripherals_Streaming;
with Serial_IO.Streaming; use Serial_IO.Streaming;

package body Communication is

    with Last_Chance_Handler; pragma Unreferenced (Last_Chance_Handler);

    Parity    : constant Parity := No_Parity;
    Data_Bits : constant Word_Lengths := Word_Length_8;
    End_Bits  : constant Stop_Bits := Stopbits_1;
    Control   : constant Flow_Control := No_Flow_Control;


    function Communication_Init (BC : Baud_Code := Default_Baud;
                                 COM_Num : Natural := Default_COM_Num)
                                 return Comm_Port
    is
        Rate  : Baud_Rates;
    begin
        Initialize (COM);
        case BC is
            when B300 =>
                Rate := 300;
            when B600 =>
                Rate := 600;
            when B1200 =>
                Rate := 1200;
            when B2400 =>
                Rate := 2400;
            when B4800 =>
                Rate := 4800;
            when B9600 =>
                Rate := 9600;
            when B19200 =>
                Rate := 19_200;
            when B38400 =>
                Rate := 38_400;
            when B57600 =>
                Rate := 57_600;
            when others =>
                Rate := 115_200;
        end case;
        Configure (COM, Rate);
        Is_Init := True;
        return COM'Access;
    end Communication_Init;

    procedure Clear_Comm_Buffer (Port : in out Comm_Port)
    is
    begin
        null;
    end Clear_Comm_Buffer;

    procedure Set_Host_Baud (Port : in out Comm_Port)
    is
    begin
        null;
    end Set_Host_Baud;

    procedure Communications_Close (Port : Comm_Port)
    is
    begin
        Is_Init := False;
    end Communications_Close;

end Communication;
