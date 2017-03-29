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

    protected body Bot_interface is
        procedure Set (Raw_Array : in UByte_Array)
        is
            Elem_Array : UByte_Array (1 .. Raw_Array'Length)
              with Address => Sensors.all'Address;
        begin
            Elem_Array := Raw_Array;
            Sem := True;
        end Set;

        entry Get (Collection : out Sensor_Collection)
          when Sem
        is
        begin
            Collection := Sensors.all;
            Sem := False;
        end Get;

    end Bot_Interface;

    task body Feedback
    is
        Raw_RX    : UByte_Array (1 .. 100);
        Next_Read : Time := Clock;
        Period    : constant Time_Span := Milliseconds (20);
    begin
        loop
            Send_Command (Port   => Algo.Port,
                          Rec    => Comm_Rec'(Op                 => Sensors_List,
                                              Num_Query_Packets  => 1),
                          Data   => (1 => 100));

            Read_Sensors (Port   => Algo.Port,
                          Buffer => Raw_RX);

            Next_Read := Next_Read + Period;
            delay until Next_Read;
        end loop;
    end Feedback;

    task body Control
    is
    begin
        loop
            Bot_Interface.Get (Collection => Algo.Sensors.all);
            Algo.Safety_Check;
            Algo.Process;
        end loop;
    exception
        when Safety_Exception =>
	    Put_Line ("Unhandled safety exception. Killing Control thread.");
        when Error: others =>
            Put ("Unexpected exception: ");
	    Put_Line (Exception_Information(Error));
    end Control;

    procedure Init_Bot (TTY_Name  : String;
                        Algo_Type : Algorithm_Type)
    is
    begin
        case Algo_Type is
            when Pong =>
                Algo := new Pong_Algorithm;
                Algo.Init (TTY_Name => TTY_Name);
        end case;

    end Init_Bot;

    procedure Free_Algo is new Ada.Unchecked_Deallocation
      (Object => Abstract_Algorithm'Class, Name => Algorithm_Ptr);

    procedure Kill_Bot
    is
    begin
        Algo.Kill;
        Free_Algo (Algo);
    end Kill_Bot;


end Botstate;
