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

            Self.Algo.Safety_Check (Sensors => Sensors);
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
