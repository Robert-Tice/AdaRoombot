with Types; use Types;

package Commands is

    type Drive_Special is
      (Straight,
       CW,
       CCW);

    --  Command Constructors
    function Construct_Date_Time (D : Day;
                                  H : Hour;
                                  M : Minute) return Comm_Rec;
    function Construct_Drive_Special (Special : Drive_Special;
                                      V       : Velocity)
                                      return Comm_Rec;


end Commands;
