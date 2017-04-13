with Algorithm; use Algorithm;
with Communication; use Communication;
with Types; use Types;

package Botstate is

    type Bot is tagged record
        Algo    : Algorithm_Ptr;
        Port    : Serial_Port;
    end record;

    procedure Init (Self      : in out Bot;
                    TTY_Name  : in String;
                    Algo_Type : in Algorithm_Type);
    procedure Start (Self : in Bot);
    procedure Kill (Self : in out Bot);

end Botstate;
