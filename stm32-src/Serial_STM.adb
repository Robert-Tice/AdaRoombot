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

with Communication; use Communication;

with STM32; use STM32;
with STM32.Device; use STM32.Device;
with STM32.GPIO; use STM32.GPIO;
with STM32.USARTs; use STM32.USARTs;

package body Communication is

   Parity : constant Parity := No_Parity;
   Data_Bits : constant Word_Lengths := Word_Length_8;
   End_Bits : constant Stop_Bits := Stopbits_1;
   Control : constant Flow_Control := No_Flow_Control;


   procedure Communication_Init (BC : Integer := Default_Baud) is
      Config : GPIO_Port_Configuration;
      Device_Pins : constant GPIO_Points := GPIO_Point & GPIO_Point;
   begin
      -- Configure Pin muxing
      Enable_Clock(Device_Pins);
      Enable_Clock(USART.all);

      Config.Mode := Mode_AF;
      Config.Speed := Speed_50MHz;
      Config.Output_Type := Push_Pull;
      Config.Resistors := Pull_Up;

      Configure_IO(Device_Pins, Config);
      Configure_Alternate_Function(Device_Pins, GPIO_Alternate_Function);

      -- Setup peripheral
      Disable(USART);

      Set_Baud_Rate(USART, Baud_Rates(BC));
      Set_Mode(USART, Tx_Rx_Mode);
      Set_Stop_Bits(USART, End_Bits);
      Set_Word_Length(USART, Data_Bits);
      Set_Parity(USART, Parity);
      Set_Flow_Control(USART, Control);

      Enable(USART);

      Is_Init := True;
   end Communication_Init;

   procedure Serial_TX (Payload : Serial_Payload) is
   begin
      for I in 1 .. Payload'Length loop
	 Await_Send_Ready;
	 Transmit(USART, Payload(I));
      end loop;
   end Serial_TX;

   function Serial_RX (Msg_Size : Integer) return Serial_Payload is
      Ret : Serial_Payload(1 .. Msg_Size);
   begin
      for I in 1 .. Msg_Size loop
	 Await_Data_Available;
	 Receive(USART, Ret(I));
      end loop;
   end Serial_RX;

   procedure Set_Host_Baud (BC : Integer) is
   begin
      Disable(USART);
      Set_Baud_Rate(USART, Baud_Rates(BC));
      Enable(USART);
   end Set_Host_Baud;

end Communication;
