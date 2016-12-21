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
with Commands; use Commands;


package body Mode is

   procedure Read_Mode_From_Target is
      Pkt : Sensor_Data (S => OI_Mode);
      Comm : Comm_Rec (Op => Sensors_Single);
      Raw_TX : Serial_Payload (1 .. Comm'Size / 8)
        with Address => Comm'Address;
      Raw_RX : Serial_Payload (1 .. Pkt'Size / 8)
        with Address => Pkt'Address;
   begin
      --  Use raw commands to bypass mode checks associated with Get_Sensor calls
      Comm.Sensor_Packet_ID := OI_Mode;
      Serial_TX (Raw_TX);
      Raw_RX := Serial_RX (Pkt'Size / 8);
      Current_Mode := Interface_Mode'Val(Pkt.Current_Mode);
   end Read_Mode_From_Target;

   function Get_Mode return Interface_Mode is
   begin
      case Current_Mode is
      when Uninit =>
	 Read_Mode_From_Target;
      when others=>
	 null;
      end case;

      return Current_Mode;
   end Get_Mode;

   procedure Change_Mode (Set_Mode : Interface_Mode) is
  --    Rec : Comm_Rec (Op => OI_Mode);
   begin
      if Set_Mode /= Current_Mode then
         Current_Mode := Set_Mode;
      --   Rec.Current_Mode := Set_Mode;
--	 Send_Command (Rec);
      end if;

   end Change_Mode;

   procedure Effect_Mode_Changed (Set_Mode : Interface_Mode) is
   begin
      Current_Mode := Set_Mode;
   end Effect_Mode_Changed;


end Mode;
