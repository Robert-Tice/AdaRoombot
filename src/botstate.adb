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

with Ada.Real_Time; use Ada.Real_Time;

with Algorithm; use Algorithm;
with Commands; use Commands;
with System; use System;
with Communication; use Communication;

package body Botstate is

    protected body Bot_interface is
        procedure Set (Raw_Array : in Stream_Element_Array)
        is
            Elem_Array : Stream_Element_Array (1 .. Raw_Array'Length)
              with Address => Sensors'Address;
        begin
            Elem_Array := Raw_Array;
            Sem := True;
        end Set;

        entry Get (Collection : out Sensor_Collection)
          when Sem
        is
        begin
            Collection := Sensors;
            Sem := False;
        end Get;

    end Bot_Interface;

    task body Feedback
    is
        Raw_RX  : Stream_Element_Array (1 .. 80);
        Last    : Stream_Element_Offset := 0;

        Next_Read : Time := Clock;
        Period    : constant Time_Span := Milliseconds (20);

    begin
        loop
            Send_Command (Port   => Port,
                          Rec    => Comm_Rec'(Op                 => Sensors_List,
                                              Num_Query_Packets  => 1),
                          Data   => (0 => 100));
            Read (Stream => Port.all,
                  Item   => Raw_RX,
                  Last   => Last);
            Bot_Interface.Set (Raw_Array => Raw_RX);
            Next_Read := Next_Read + Period;
            delay until Next_Read;
        end loop;
    end Feedback;

    task body Control
    is
        Algo : Pong_Algorithm;
    begin
        Algo.Port := Port;
        loop
            Bot_Interface.Get (Collection => Algo.Sensors);
            Algo.Safety_Check;
            Algo.Process;
        end loop;
    exception
        when Safety_Exception =>
            raise Safety_Exception;
        when others =>
            null;
    end Control;

    procedure Init_Bot
    is
    begin
        Send_Command (Port => Port,
                      Rec  => Comm_Rec'(Op => Start));
        Clear_Comm_Buffer (Port => Port);
        Send_Command (Port => Port,
                      Rec  => Comm_Rec'(Op => Mode_Safe));
    end Init_Bot;


end Botstate;
