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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Communication; use Communication;

package body Botstate is

    procedure Start (Self : in Bot)
    is
        Raw_RX    : UByte_Array (1 .. Sensor_Collection'Size / 8);
        Sensors   : Sensor_Collection
          with Address => Raw_RX'Address;
        Next_Read : Time := Clock;
        Period    : constant Time_Span := Milliseconds (15);

    begin
        loop
            Send_Command (Port   => Self.Port,
                          Rec    => Comm_Rec'(Op                 => Sensors_List,
                                              Num_Query_Packets  => 1),
                          Data   => (1 => 100));

            Read_Sensors (Port   => Self.Port,
                          Buffer => Raw_RX);

     --       Self.Algo.Safety_Check (Sensors => Sensors);
            Self.Algo.Process (Port    => Self.Port,
                               Sensors => Sensors);

            Next_Read := Next_Read + Period;
            delay until Next_Read;
        end loop;
    exception
        when Safety_Exception =>
            Put_Line ("Unhandled safety exception. Killing Control thread.");
        when Error : others =>
            Put ("Unexpected exception: ");
            Put_Line (Exception_Information (Error));

    end Start;

    procedure Init (Self      : in out Bot;
                    TTY_Name  : in String;
                    Algo_Type : in Algorithm_Type)
    is
    begin
        case Algo_Type is
            when Pong =>
                Self.Algo := new Pong_Algorithm;
                Self.Port := Communication_Init (Data_Rate => B115200,
                                                 Name      => TTY_Name);
                Send_Command (Port => Self.Port,
                              Rec  => Comm_Rec'(Op => Reset));

                delay 5.0;

                Send_Command (Port => Self.Port,
                              Rec  => Comm_Rec'(Op => Start));
                Clear_Comm_Buffer (Port => Self.Port);
                Send_Command (Port => Self.Port,
                              Rec  => Comm_Rec'(Op => Mode_Safe));
            when others =>
                null;
        end case;
    end Init;

    procedure Free_Algo is new Ada.Unchecked_Deallocation
      (Object => Abstract_Algorithm'Class, Name => Algorithm_Ptr);

    procedure Kill (Self : in out Bot)
    is
    begin
        Communications_Close (Port => Self.Port);
        Free_Algo (Self.Algo);
    end Kill;

end Botstate;
